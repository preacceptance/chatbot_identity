

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('dplyr', 'effsize', 'lsr')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


REPLIKA_UPDATE_DATE <- "2023-02-03"
CHATGPT_UPDATE_DATE <- "2025-08-07"

################# READING THE DATA #################

d_replika <- read.csv('emotion_classifications_final_replika.csv', sep = ",")
d_chatgpt <- read.csv('emotion_classifications_final_chatgpt.csv', sep = ",")
d_robustness <- read.csv('emotion_classifications_final_robustness.csv', sep = ",")

d_all <- rbind(d_replika, d_chatgpt)

# Get common columns between d_all and d_robustness
common_cols <- intersect(colnames(d_all), colnames(d_robustness))

d_all <- d_all[, common_cols]
d_robustness <- d_robustness[, common_cols]

d_all <- rbind(d_all, d_robustness)
apps <- unique(d_all$app)

d_all$date <- substr(d_all$created_date, 1, 10)

################################################################################

did_daily_proportions <- function(
  data,
  event_date,
  window_days = 26,
  sentiment_var = "sentiment_openai",
  negative_label = "negative",
  positive_label = "positive"
) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  if (!sentiment_var %in% names(data)) {
    stop(sprintf("Column '%s' not found in `data`.", sentiment_var))
  }

  # --- Ensure Date column ---
  data <- data %>%
    dplyr::mutate(date = as.Date(substr(created_date, 1, 10)))

  event_date <- as.Date(event_date)

  # --- Restrict window ---
  data_window <- data %>%
    dplyr::filter(
      date >= event_date - window_days,
      date <= event_date + window_days
    )

  # --- Daily proportions ---
  daily <- data_window %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      prop_neg = mean(.data[[sentiment_var]] == negative_label, na.rm = TRUE),
      prop_pos = mean(.data[[sentiment_var]] == positive_label, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(rel_day = as.integer(date - event_date))

  # --- Split BEFORE / AFTER (exclude event day) ---
  before <- daily %>% dplyr::filter(rel_day < 0)
  after  <- daily %>% dplyr::filter(rel_day > 0)

  # --- Create symmetric matching index ---
  before <- before %>%
    dplyr::mutate(match_day = abs(rel_day))

  after <- after %>%
    dplyr::mutate(match_day = rel_day)

  # --- Match symmetric days ---
  matched <- dplyr::inner_join(
    after,
    before,
    by = "match_day",
    suffix = c("_after", "_before")
  )

  # --- Safety check ---
  if (nrow(matched) < 5) {
    stop("Not enough matched before–after days for DiD analysis.")
  }

  # --- Daily differences ---
  diffs_neg <- matched$prop_neg_after - matched$prop_neg_before
  diffs_pos <- matched$prop_pos_after - matched$prop_pos_before

  # --- Output ---
  list(
    n_days = length(diffs_neg),
    diffs_negative = diffs_neg,
    diffs_positive = diffs_pos,
    neg_test = t.test(diffs_neg),
    pos_test = t.test(diffs_pos),
    neg_d = lsr::cohensD(diffs_neg, mu = 0),
    pos_d = lsr::cohensD(diffs_pos, mu = 0)
  )
}

# Replika robustness checks

# Let's first compare 2022 vs. 2023.
d <- d_all[d_all$app == "replika", ]

erp_2022 <- did_daily_proportions(
  data = d %>% filter(date >= "2022-01-01" & date <= "2022-02-28"),
  event_date = "2022-02-03"
)

erp_2023 <- did_daily_proportions(
  data = d %>% filter(date >= "2023-01-01" & date <= "2023-02-28"),
  event_date = "2023-02-03"
)

# Between-year comparison
t.test(erp_2023$diffs_negative, erp_2022$diffs_negative)
cohen.d(erp_2023$diffs_negative, erp_2022$diffs_negative)

t.test(erp_2023$diffs_positive, erp_2022$diffs_positive)
cohen.d(erp_2023$diffs_positive, erp_2022$diffs_positive)

# Diff-in-diffs
did_neg <- erp_2023$diffs_negative - erp_2022$diffs_negative
t.test(did_neg)
cohensD(did_neg, mu = 0)

# Diff-in-diffs
did_pos <- erp_2023$diffs_positive - erp_2022$diffs_positive
t.test(did_pos)
cohensD(did_pos, mu = 0)

# Now, compare before-after June 15 2023, i.e., the "Ask Replika" update
# With before-after the ERP update
# Select 26 days before-after June 15 2023, so that it matches the 
# number of days before-after the ERP update
d <- d_all[d_all$app == "replika", ]

erp_2023 <- did_daily_proportions(
  data = d %>%
    filter(
      date >= as.Date("2023-01-08"),
      date <= as.Date("2023-02-28")
    ),
  event_date = "2023-02-03"
)

ask_replika <- did_daily_proportions(
  data = d %>%
    filter(
      date >= as.Date("2023-05-20"),
      date < as.Date("2023-07-11")
    ),
  event_date = "2023-06-15"
)

erp_2023$n_days
ask_replika$n_days

# Between-update comparison
t.test(erp_2023$diffs_negative, ask_replika$diffs_negative)
cohen.d(erp_2023$diffs_negative, ask_replika$diffs_negative)

# Difference-in-differences vs 0
did_neg_updates <- erp_2023$diffs_negative - ask_replika$diffs_negative
t.test(did_neg_updates)
cohensD(did_neg_updates, mu = 0)


#################### ChatGPT ####################

d <- d_all[d_all$app == "chatgpt", ]

# Control year - 2024
gpt_2024 <- did_daily_proportions(
  data = d %>%
    filter(
      date >= as.Date("2024-07-01"),
      date <= as.Date("2024-08-31")
    ),
  event_date = "2024-08-07"
)

# Event year - 2025
gpt_2025 <- did_daily_proportions(
  data = d %>%
    filter(
      date >= as.Date("2025-07-01"),
      date <= as.Date("2025-08-31")
    ),
  event_date = "2025-08-07"
)

t.test(gpt_2025$diffs_negative, gpt_2024$diffs_negative)
cohen.d(gpt_2025$diffs_negative, gpt_2024$diffs_negative)

did_neg_years <- gpt_2025$diffs_negative - gpt_2024$diffs_negative
t.test(did_neg_years)
cohensD(did_neg_years, mu = 0)

# Now, compare before-after March 14 2023, i.e., the "GPT-4" release
# With before-after the GPT-5 update
# Select 25 days before-after March 14 2023, so that it matches the 
# number of days before-after the GPT-5 update

gpt4 <- did_daily_proportions(
  data = d %>%
    filter(
      date >= as.Date("2023-02-17"),
      date <= as.Date("2023-04-07")
    ),
  event_date = "2023-03-14"
)

gpt5 <- did_daily_proportions(
  data = d %>%
    filter(
      date >= as.Date("2025-07-13"),
      date <= as.Date("2025-08-31")
    ),
  event_date = "2025-08-07"
)

t.test(gpt5$diffs_negative, gpt4$diffs_negative)
cohen.d(gpt5$diffs_negative, gpt4$diffs_negative)

did_neg_updates_gpt <- gpt5$diffs_negative - gpt4$diffs_negative
t.test(did_neg_updates_gpt)
cohensD(did_neg_updates_gpt, mu = 0)
