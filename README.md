# All Data and Code for: "Experience of Loss After Conversational AI Software Updates"

R version 4.4.1 was used, but other R versions will likely work as well.

## Data privacy

All data in this repository have been de-identified. Direct identifiers (Prolific
IDs, worker IDs, IP addresses, geolocation, e-mail addresses) and free-text that
participants produced (open-ended responses, "other, please specify" fields, and
the names participants supplied) have been removed from the survey data. Removal
is reproducible via `anonymize_for_release.py`. None of these fields are used by
the analysis scripts, so the reported results reproduce unchanged.

## Study 1 data

Study 1 (`e1_field_study/`) analyses run on `d_all_shareable.csv`, a de-identified
extract of the Reddit data: usernames are replaced with stable hashes and verbatim
post text is removed, keeping the derived emotion/sentiment classifications and the
post `id`. `e1.R` reads it automatically. The reported results and the cached LDA
topic models reproduce from this file; a from-scratch LDA refit would require
re-fetching the post text from Reddit via the `id` column. The full raw dump and
other Study 1 files are available on OSF at https://osf.io/825jk/ .