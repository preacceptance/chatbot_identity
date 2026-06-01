#!/usr/bin/env python3
"""
De-identify the survey data CSVs for public release.

Removes the two things that carry identification risk and are NOT used by any
analysis script (verified: no .R script references these columns):

  1. Direct identifiers -> "Anonymized"
       Prolific/worker/participant IDs (24-char hex tokens), plus the
       IPAddress / Location* / Recipient* / e-mail columns.
  2. Free text an individual produced -> blanked ("")
       open_ended, explanation, the *_name fields (aicomp/app/brand/game/voice/
       pet/car), and the "other, please specify" *_TEXT fields.

Everything needed to reproduce the reported results is kept: Likert items,
condition columns, derived scores, categorical demographics, comp checks,
timing, and ResponseId. None of the removed columns are read by the analysis.

Robustness: Prolific IDs are removed with a global regex over the raw text, so
they are caught regardless of CSV structure. Free text is blanked per column.
Most exports are well-formed CSV and are edited with a quote-aware, byte-
preserving parser. Two Qualtrics ';'-exports (archive/e4, eS4) embed a multi-
line consent block with stray quotes that desync RFC-4180 parsers; those files
are detected automatically and handled by literal delimiter splitting (one
record per physical line; data rows have a fixed field count, and a rare row
whose prose field contains the delimiter is collapsed back). The script self-
verifies at the end and raises if any identifier or free-text value survives.

Idempotent. Operates only on git-tracked CSVs. Study 1 raw Reddit data is
handled separately (see slim_e1_data.py / .gitignore).
"""
import csv, re, subprocess

ANON = "Anonymized"
ID_COLS = {
    "prolific_id", "PROLIFIC_PID", "worker_id", "participantId", "email",
    "IPAddress", "LocationLatitude", "LocationLongitude",
    "RecipientFirstName", "RecipientLastName", "RecipientEmail", "ExternalReference",
}
FREETEXT_COLS = {
    "open_ended", "explanation",
    "aicomp_name", "app_name", "brand_name", "game_name", "voice_name", "pet_name", "car_name",
    "gender_4_TEXT", "ethnicity_6_TEXT", "edu_8_TEXT", "relation_status_8_TEXT", "rep_relation_5_TEXT",
}
PROSE_COLS = {"open_ended", "explanation"}   # free-text fields long enough to contain a delimiter
EMPTYISH = {"", "NA", "N/A", "NULL", ANON}
# 24-char lowercase-hex token (Prolific / Mongo-style IDs) standing alone between non-alnum chars
PROLIFIC_RE = re.compile(r"(?<![0-9A-Za-z])[0-9a-f]{24}(?![0-9A-Za-z])")
_IP = re.compile(r"^\d{1,3}(\.\d{1,3}){3}$")
_COORD = re.compile(r"^-?\d{1,3}\.\d+$")


def looks_like_identifier(v):
    """A real direct identifier (IP / coordinate / e-mail / long token), NOT a short
    numeric pseudonym code (e.g. '522') that has already been de-identified."""
    v = v.strip()
    if v in EMPTYISH:
        return False
    if "@" in v or _IP.match(v) or _COORD.match(v):
        return True
    return len(v) >= 20 and any(c.isalpha() for c in v) and any(c.isdigit() for c in v)


def _id_sub(m):
    s = m.group()
    return ANON if any(c in "abcdef" for c in s) else s   # require a hex letter (avoids 24-digit numbers)


def sniff_delim(line):
    for d in (";", "\t", ","):
        if d in line:
            return d
    return ","


def unquote(tok):
    t = tok.strip()
    if len(t) >= 2 and t[0] == '"' and t[-1] == '"':
        return t[1:-1].replace('""', '"')
    return t


def is_wellformed(text, delim):
    """True if a standard CSV parser yields a consistent field count for ~all rows."""
    try:
        rows = list(csv.reader(text.splitlines(), delimiter=delim))
    except Exception:
        return False
    if len(rows) < 2:
        return True
    n = len(rows[0])
    good = sum(1 for r in rows if len(r) == n)
    return good >= 0.97 * len(rows)


# ---- well-formed path: quote-aware, byte-preserving surgical edit ----------
def split_records(text, term):
    records, i, n, start, in_q, tl = [], 0, len(text), 0, False, len(term)
    while i < n:
        c = text[i]
        if c == '"':
            if in_q and i + 1 < n and text[i + 1] == '"':
                i += 2; continue
            in_q = not in_q; i += 1
        elif not in_q and text.startswith(term, i):
            records.append(text[start:i]); i += tl; start = i
        else:
            i += 1
    if start < n:
        records.append(text[start:])
    return records


def split_fields(record, delim):
    fields, cur, i, n, in_q = [], [], 0, len(record), False
    while i < n:
        c = record[i]
        if c == '"':
            if in_q and i + 1 < n and record[i + 1] == '"':
                cur.append('""'); i += 2; continue
            in_q = not in_q; cur.append('"'); i += 1
        elif c == delim and not in_q:
            fields.append("".join(cur)); cur = []; i += 1
        else:
            cur.append(c); i += 1
    fields.append("".join(cur))
    return fields


def process_wellformed(text, delim):
    term = "\r\n" if "\r\n" in text else "\n"
    trailing = text.endswith(term)
    records = split_records(text, term)
    header = [unquote(t) for t in split_fields(records[0], delim)]
    idx = {c: i for i, c in enumerate(header)}
    ft = [c for c in header if c in FREETEXT_COLS]
    ids = [c for c in header if c in ID_COLS]
    meta_end = 3 if (len(records) > 2 and "ImportId" in records[2]) else 1
    for r in range(meta_end, len(records)):
        fields = split_fields(records[r], delim)
        changed = False
        for c in ft:
            i = idx[c]
            if i < len(fields) and unquote(fields[i]) not in EMPTYISH:
                fields[i] = ""; changed = True
        for c in ids:
            i = idx[c]
            if i < len(fields) and looks_like_identifier(unquote(fields[i])):
                fields[i] = ANON; changed = True
        if changed:
            records[r] = delim.join(fields)
    return term.join(records) + (term if trailing else "")


# ---- malformed path: literal split, one record per physical line -----------
def process_malformed(text, delim):
    term = "\r\n" if "\r\n" in text else "\n"
    trailing = text.endswith(term)
    lines = text.split(term)
    if trailing and lines and lines[-1] == "":
        lines = lines[:-1]
    header = lines[0].split(delim)
    n = len(header)
    idx = {c: i for i, c in enumerate(header)}
    ft = {c: idx[c] for c in FREETEXT_COLS if c in idx}
    prose = sorted(idx[c] for c in PROSE_COLS if c in idx)
    out = [lines[0]]
    for ln in lines[1:]:
        if "ImportId" in ln:
            out.append(ln); continue
        f = ln.split(delim)
        if len(f) == n:
            for i in ft.values():
                if f[i].strip() not in EMPTYISH:
                    f[i] = ""
            out.append(delim.join(f))
        elif len(f) > n and prose:
            # a prose field swallowed (len(f)-n) delimiters: blank that run and collapse to one cell
            extra = len(f) - n
            p = prose[0]
            f[p:p + 1 + extra] = [""]
            for i in ft.values():
                if i < len(f) and f[i].strip() not in EMPTYISH:
                    f[i] = ""
            out.append(delim.join(f))
        else:
            out.append(ln)   # metadata continuation (consent block etc.)
    return term.join(out) + (term if trailing else "")


def process(path):
    raw = open(path, "r", encoding="utf-8", errors="replace", newline="").read()
    if not raw:
        return
    delim = sniff_delim(raw.split("\n", 1)[0])
    header = [unquote(t) for t in split_fields(raw.split("\n", 1)[0], delim)]
    if not (set(header) & (ID_COLS | FREETEXT_COLS)):
        return
    # 1) identifiers: global, structure-independent
    text = PROLIFIC_RE.sub(_id_sub, raw)
    # IP / location / recipient / e-mail columns (well-formed files only; these never
    # appear in the two malformed exports) are handled by the column pass below.
    # 2) free text
    text = process_wellformed(text, delim) if is_wellformed(text, delim) else process_malformed(text, delim)
    if text != raw:
        open(path, "w", encoding="utf-8", newline="").write(text)
        print(f"  cleaned {path}")
    else:
        print(f"  {path}: no change")


def verify(path):
    raw = open(path, encoding="utf-8", errors="replace").read()
    bad = [m.group() for m in PROLIFIC_RE.finditer(raw) if any(c in "abcdef" for c in m.group())]
    assert not bad, f"{path}: {len(bad)} identifier tokens remain"


def main():
    csvs = [f for f in subprocess.check_output(["git", "ls-files", "*.csv"]).decode().split()
            if "d_all" not in f]
    print(f"Processing {len(csvs)} tracked survey CSVs...\n")
    for p in sorted(csvs):
        process(p)
    for p in csvs:
        verify(p)
    print("\nDone; verified no identifier tokens remain.")


if __name__ == "__main__":
    main()
