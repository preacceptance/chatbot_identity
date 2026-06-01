#!/usr/bin/env python3
"""
Produce a slim, de-identified version of the Study 1 Reddit dataset (d_all.csv).

d_all.csv is the raw PushShift/Reddit dump: 54,861 rows x 112 columns, ~218 MB.
The analysis (e1.R) only uses ~20 of those columns; the rest is unused Reddit API
metadata (media/preview/flair/awardings JSON blobs), a redundant raw-JSON
`classification` column (the parsed *_openai labels already exist separately),
and verbatim post text.

This script writes d_all_shareable.csv:
  * keeps only the 20 columns the analysis reads (verified: no other column is
    referenced in e1.R / robustness_checks.R),
  * replaces `author` (real Reddit username) with a stable salted hash, so the
    "posted both before and after" analysis still works without exposing identity
    ("[deleted]" is preserved, since e1.R filters on it),
  * blanks `titlencontent` (verbatim post title+body) -> removes the text an
    individual produced, while keeping the column so e1.R runs and the topic
    models reproduce from their cached fits (data/topic_cache/*.rds). A from-
    scratch LDA refit would need the text re-fetched from Reddit via post `id`.

The original d_all.csv is left untouched.
Result: ~218 MB -> ~8 MB.
"""
import csv, hashlib

SRC = "e1_field_study/d_all.csv"
DST = "e1_field_study/d_all_shareable.csv"
SALT = "ai_loss_study_v1"  # fixed so hashes are stable across rows/runs

KEEP = [
    "id", "author", "subreddit", "created_utc", "created_date", "app",
    "word_count", "titlencontent", "before_update",
    "sentiment_openai", "emotion_openai", "enjoyment_reason_openai",
    "community_support_expression_openai", "loss_type_openai",
    "emotion_due_to_loss_openai", "attachment_related_loss_openai",
    "negative_mental_health_expression_openai", "longing_for_restoration_openai",
    "update_related_openai", "blame_attribution_openai",
]


def hash_author(v):
    v = v.strip()
    if v in ("", "[deleted]", "NA"):
        return v
    return "r_" + hashlib.sha256((SALT + v).encode("utf-8")).hexdigest()[:12]


def main():
    csv.field_size_limit(10 ** 9)
    with open(SRC, newline="", encoding="utf-8", errors="replace") as f:
        r = csv.reader(f)
        header = next(r)
        idx = {c: i for i, c in enumerate(header)}
        keep = [c for c in KEEP if c in idx]
        ai, ti = idx["author"], idx["titlencontent"]
        keep_i = [idx[c] for c in keep]
        rows = 0
        with open(DST, "w", newline="", encoding="utf-8") as out:
            w = csv.writer(out, lineterminator="\n")
            w.writerow(keep)
            for row in r:
                row[ai] = hash_author(row[ai]) if ai < len(row) else ""
                if ti < len(row):
                    row[ti] = ""  # remove verbatim post text
                w.writerow([row[i] if i < len(row) else "" for i in keep_i])
                rows += 1
    print(f"Wrote {DST}: {rows} rows, {len(keep)} columns (kept), "
          f"{len(header) - len(keep)} columns dropped.")


if __name__ == "__main__":
    main()
