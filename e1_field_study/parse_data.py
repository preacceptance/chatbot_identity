import zstandard
import os
import json
from datetime import datetime
import logging.handlers
import argparse
import time
from collections import deque, Counter
import multiprocessing as mp
import queue
from threading import Thread

log = logging.getLogger("bot")
log.setLevel(logging.DEBUG)
log.addHandler(logging.StreamHandler())


def subreddit_matches(subreddit_name, targets, mode):
    """Return True if subreddit_name matches targets under the given mode."""
    if not subreddit_name:
        return False

    if mode == "exact":
        return subreddit_name in targets

    if mode == "contains":
        return any(term in subreddit_name for term in targets)

    raise ValueError(f"Unknown subreddit match mode: {mode}")


def is_target_date(timestamp):
    """Check if timestamp falls within target date ranges."""
    try:
        dt = datetime.fromtimestamp(timestamp)
        year_month = (dt.year, dt.month)
        
        # Target periods: March 2023, May 2024, July 2024, August 2024
        target_periods = {
            #(2023, 2),
            #(2023, 3),   # March 2023
            #(2023, 4),
            #(2024, 4),
            #(2024, 5),   # May 2024
            #(2024, 6),   # June 2024
            #(2024, 7),   # July 2024
            #(2024, 8)    # August 2024
            #(2025, 11)
            (2025, 10)
        }
        
        return year_month in target_periods
    except (ValueError, OSError, OverflowError):
        return False


def read_and_decode(reader, chunk_size, max_window_size):
    """Read and decode a chunk safely (loop-based, no recursion)."""
    buffer = b""
    bytes_read = 0

    while True:
        chunk = reader.read(chunk_size)
        if not chunk:
            return None  # EOF

        buffer += chunk
        bytes_read += len(chunk)

        try:
            return buffer.decode()
        except UnicodeDecodeError:
            if bytes_read > max_window_size:
                raise UnicodeError(f"Unable to decode frame after reading {bytes_read:,} bytes")
            log.info(f"Decoding error with {bytes_read:,} bytes, reading more...")
            continue


def process_line_batch(batch_data):
    """Process a batch of lines in a separate process"""
    lines, subreddit_targets, field, subreddit_mode = batch_data
    results = []
    bad_count = 0
    matched_subreddits = Counter()
    
    for line in lines:
        try:
            obj = json.loads(line)
            subreddit_name = obj.get(field, '').lower()
            
            # Check both subreddit and date criteria
            if subreddit_matches(subreddit_name, subreddit_targets, subreddit_mode):
                # Check if the post/comment is from target time periods
                created_utc = obj.get('created_utc')
                if created_utc: # and is_target_date(created_utc):
                    results.append(line)
                    matched_subreddits[subreddit_name] += 1
        except (KeyError, json.JSONDecodeError, ValueError):
            bad_count += 1
            
    return results, bad_count, dict(matched_subreddits)


def read_lines_zst_batched(file_name, batch_size=20000):
    """Read lines in batches for multiprocessing"""
    with open(file_name, 'rb') as file_handle:
        buffer = ''
        reader = zstandard.ZstdDecompressor(max_window_size=2**31).stream_reader(file_handle)
        batch = []
        
        while True:
            chunk = read_and_decode(reader, 2**25, 2**29)

            if not chunk:
                if batch:
                    yield batch, file_handle.tell()
                break
                
            lines = (buffer + chunk).split("\n")

            for line in lines[:-1]:
                if line.strip():  # Skip empty lines
                    batch.append(line)
                    if len(batch) >= batch_size:
                        yield batch, file_handle.tell()
                        batch = []

            buffer = lines[-1]

        reader.close()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Process some integers.')
    parser.add_argument('--customname', type=str, required=False, help='Custom name for the output files')
    parser.add_argument('--year', type=str, required=False, help='Year in YYYY format')
    parser.add_argument('--month', type=str, required=False, help='Month in MM format')
    parser.add_argument('--ftype', type=str, required=True, help='File type (submissions or comments)')
    parser.add_argument('--subreddit', type=str, required=True, help='Subreddit name(s) - comma-separated for multiple (e.g., "cosmetic_surgery,plastic_surgery")')
    parser.add_argument(
        '--subreddit-mode',
        dest='subreddit_mode',
        choices=['exact', 'contains'],
        default='exact',
        help='How to match subreddits: exact (default) or contains (substring match)'
    )
    parser.add_argument('--processes', type=int, default=mp.cpu_count(), help=f'Number of processes to use (default: {mp.cpu_count()})')

    args = parser.parse_args()
    month = args.month
    year = args.year
    ftype = args.ftype
    num_processes = args.processes
    customname = args.customname
    subreddit_mode = args.subreddit_mode
    
    # Parse multiple subreddits
    subreddits = [sub.strip().lower() for sub in args.subreddit.split(',') if sub.strip()]

    if subreddit_mode == 'exact':
        subreddit_targets = set(subreddits)  # O(1) lookup
    else:
        # Keep order stable; substring checks are O(k) over provided terms.
        subreddit_targets = tuple(dict.fromkeys(subreddits))
    
    # Create filename-safe string for output
    subreddit_string = '_'.join(subreddits)
    print(f"Subreddit mode: {subreddit_mode} | Targets: {', '.join(subreddits)}")


    file_path = './data/raw/reddit_{}_{}/{}/R{}_{}-{}.zst'.format(year, month, ftype, ftype[0].upper(), year, month)

    if customname:
        file_path = f'./data/raw/{customname}.zst'
        print(f"Using custom file path: {file_path}")

    file_size = os.stat(file_path).st_size
    file_lines = 0
    file_bytes_processed = 0
    created = None
    field = "subreddit"
    bad_lines = 0
    matches_found = 0  # Track how many matching posts/comments found
    matched_subreddit_counts = Counter()

    fftype = 'comments'
    if ftype == 'submissions':
        fftype = 'posts'

    log.info(f"Starting processing with {num_processes} processes...")
    log.info(f"Subreddit mode: {subreddit_mode} | Targets: {', '.join(subreddits)}")
    log.info(f"Date filtering enabled for specific months.")
    
    start_time = time.time()
    output_file_path = './reddit_jsonl/reddit_{}_{}_{}_{}.jsonl'.format(subreddit_string, year, month, fftype)

    if customname:
        output_file_path = f'./reddit_jsonl/{customname}_{subreddit_string}_{fftype}.jsonl'
        print(f"Using custom output file path: {output_file_path}")
    
    # Create multiprocessing pool
    with mp.Pool(processes=num_processes) as pool, open(output_file_path, 'w', buffering=1024*1024) as json_file:

        pending_results = []
        batch_count = 0

        # Submit jobs
        for batch, file_bytes_processed in read_lines_zst_batched(file_path, batch_size=50000):
            batch_count += 1
            file_lines += len(batch)

            batch_data = (batch, subreddit_targets, field, subreddit_mode)
            result = pool.apply_async(process_line_batch, (batch_data,))
            pending_results.append((result, file_bytes_processed))

            # Optional: flush results every N batches to control memory
            if len(pending_results) >= num_processes * 2:
                for r, fbp in pending_results:
                    matching_lines, batch_bad_lines, batch_matched_counts = r.get()
                    bad_lines += batch_bad_lines
                    matches_found += len(matching_lines)
                    matched_subreddit_counts.update(batch_matched_counts)
                    if matching_lines:
                        json_file.writelines(line + '\n' for line in matching_lines)
                pending_results.clear()

            # Progress logging with ETA
            if batch_count % 10 == 0:
                elapsed = time.time() - start_time
                rate = file_lines / elapsed if elapsed > 0 else 0
                progress = (file_bytes_processed / file_size) * 100 if file_size > 0 else 0
                
                # Calculate ETA
                if progress > 0:
                    total_estimated_time = elapsed / (progress / 100)
                    remaining_time = total_estimated_time - elapsed
                    
                    # Format ETA nicely
                    if remaining_time > 3600:  # More than 1 hour
                        eta_str = f"{remaining_time/3600:.1f}h"
                    elif remaining_time > 60:  # More than 1 minute
                        eta_str = f"{remaining_time/60:.1f}m"
                    else:
                        eta_str = f"{remaining_time:.0f}s"
                        
                    log.info(f"Processed {file_lines:,} lines ({rate:.0f}/sec) : "
                            f"{matches_found:,} matches : {bad_lines:,} bad : "
                            f"{progress:.1f}% complete : ETA ~{eta_str}")
                else:
                    log.info(f"Processed {file_lines:,} lines ({rate:.0f}/sec) : "
                            f"{matches_found:,} matches : {bad_lines:,} bad : "
                            f"{progress:.1f}% complete : Calculating ETA...")

        # Collect any remaining results
        for r, fbp in pending_results:
            matching_lines, batch_bad_lines, batch_matched_counts = r.get()
            bad_lines += batch_bad_lines
            matches_found += len(matching_lines)
            matched_subreddit_counts.update(batch_matched_counts)
            if matching_lines:
                json_file.writelines(line + '\n' for line in matching_lines)

    # Final statistics
    elapsed_total = time.time() - start_time
    log.info(f"Complete : {file_lines:,} total lines : {matches_found:,} matches found : {bad_lines:,} bad lines")
    log.info(f"Total time: {elapsed_total:.1f}s : Average rate: {file_lines/elapsed_total:.0f} lines/sec")
    log.info(f"Target subreddits: {', '.join(subreddits)}")

    if matched_subreddit_counts:
        top = ', '.join(
            f"{name} ({count:,})" for name, count in matched_subreddit_counts.most_common(30)
        )
        log.info(
            f"Matched subreddits: {len(matched_subreddit_counts):,} unique. Top: {top}"
        )
    else:
        log.info("Matched subreddits: 0 unique (no matches)")