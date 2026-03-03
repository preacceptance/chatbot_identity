
## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ggpattern',       # patterns for ggplot2
               'ltm',             # cronbach's alpha
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'lsmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'sjstats',          # get eta squared for anova
               'filesstrings',    # create and move files
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'nlme',            # get p values for mixed effect model
               'DescTools',       # get Cramer's V
               'LDAvis',          # Topic model visualization
               'tm',
               'lda',
               'servr',
               'wordcloud',
               'vader',
               'topicmodels',
               'gridExtra',
               'syuzhet',
               'ggpubr',
               'rstatix',
               'textstem',
               'quanteda',
               'reticulate',
               'scales',
               'hash',
               'dplyr',
               'tidytext',
               'parallel',
               'lsr',
               'effectsize',
               'changepoint',
               'jsonlite',
               'irr',
               'emmeans'
)

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) == 1) {
    script_path <- sub("^--file=", "", file_arg)
    setwd(dirname(normalizePath(script_path)))
  }
}

get_stars <- function(p) {
  if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else if (p < 0.1) {
    return("+")
  } else {
    return("ns")
  }
}

REPLIKA_UPDATE_DATE <- "2023-02-03"
CHATGPT_UPDATE_DATE <- "2025-08-07"

# We want to have 30 days before and after the update for each app, so we set the date ranges accordingly
REPLIKA_START_DATE <- as.Date(REPLIKA_UPDATE_DATE) - 30
REPLIKA_END_DATE <- as.Date(REPLIKA_UPDATE_DATE) + 29

CHATGPT_START_DATE <- as.Date(CHATGPT_UPDATE_DATE) - 30
CHATGPT_END_DATE <- as.Date(CHATGPT_UPDATE_DATE) + 29

################# READING THE DATA #################

d_all <- read.csv('d_all.csv')
apps <- unique(d_all$app)

# Print dates for both apps
for (app in apps) {
  app_dates <- as.Date(substr(d_all$created_date[d_all$app == app], 1, 10))
  print(paste0("App: ", app, " | Date range: ", min(app_dates), " to ", max(app_dates)))
}

# Print number of unique days and ranges for both apps and before_update column
for (app in apps) {
  for (before_after in c("True", "False")) {
    app_dates <- as.Date(substr(d_all$created_date[d_all$app == app & d_all$before_update == before_after], 1, 10))
    print(paste0("App: ", app, " | Before update: ", before_after, " | Unique days: ", length(unique(app_dates)), " | Date range: ", min(app_dates), " to ", max(app_dates)))
  }
}


users_before <- unique(d_all$author[d_all$before_update == "True"])
users_after <- unique(d_all$author[d_all$before_update == "False"])
users_both <- intersect(users_before, users_after)

# Print # of users who posted both before and after, across both apps
print(paste0("Users who posted both before and after (Replika): ", length(unique(d_all[d_all$author %in% users_both & d_all$app == "replika", "author"]))))
print(paste0("Users who posted both before and after (ChatGPT): ", length(unique(d_all[d_all$author %in% users_both & d_all$app == "chatgpt", "author"]))))

ONLY_LOOK_WITHIN_USERS <- FALSE
if(ONLY_LOOK_WITHIN_USERS) {
  d_all <- d_all[d_all$author %in% users_both, ]
  d_all <- d_all[d_all$author != "[deleted]", ]
}

within_subjects_str <- ifelse(ONLY_LOOK_WITHIN_USERS, "_within_subjects", "")

## =============================================================================
##                             DESCRIPTIVE STATS
## =============================================================================

for (app in apps) {
  print(paste0("*-*-*-*-* App: ", app, " *-*-*-*-*"))

  print(paste0("Total number of posts: ", length(d_all[d_all$app == app, 'titlencontent'])))
  print(paste0("Number of distinct users in total: ", length(unique(d_all[d_all$app == app, 'author']))))

  # Number of posts before vs. after the update
  n_posts_before <- length(d_all[d_all$app == app & d_all$before_update == "True", 'titlencontent'])
  n_posts_after <- length(d_all[d_all$app == app & d_all$before_update == "False", 'titlencontent'])
  print(paste0("Number of posts before the update: ", n_posts_before))
  print(paste0("Number of posts after the update: ", n_posts_after))

  # Overall Mean and SD word_count before vs. after the update
  mean_n_words <- mean(d_all[d_all$app == app, 'word_count'], na.rm = TRUE)
  sd_n_words <- sd(d_all[d_all$app == app, 'word_count'], na.rm = TRUE)

  print(paste0("Overall mean number of words: ", round(mean_n_words, 2), " (SD = ", round(sd_n_words, 2), ")"))
}

## =============================================================================
##                              ANALYSIS
## =============================================================================

prepare_daily_summary <- function(d, app) {
  ############### Preparing daily summary dataframes ###############
  # Create a new variable d$date that contains only the date (YYYY-MM-DD) from d$created_date
  d$date <- substr(d$created_date, 1, 10)

  # Make a dataframe with d$date as one column, and number of positive, negative, neutral posts as other columns
  df_days <- data.frame(date = unique(d$date))

  table(d$sentiment_openai)

  # First calculate the counts for sentiment_label in one go
  sentiment_counts <- d %>%
    group_by(date, sentiment_openai) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = sentiment_openai, values_from = n, values_fill = list(n = 0)) %>%
    rename(n_positive = positive, n_negative = negative, n_neutral_sentiment = neutral)

  # Then calculate the counts for emotion in one go
  emotion_counts <- d %>%
    group_by(date, emotion_openai) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = emotion_openai, values_from = n, values_fill = list(n = 0)) %>%
    rename(n_anger = anger, n_disgust = disgust, n_fear = fear, n_enjoyment = enjoyment, n_sadness = sadness, n_surprise = surprise, n_neutral_emotion = neutral)

  # Calculate the % of negative posts across each day
  sentiment_percentages <- d %>%
    group_by(date) %>%
    summarise(perc_negative = sum(sentiment_openai == "negative") / n() * 100,
              perc_positive = sum(sentiment_openai == "positive") / n() * 100,
              perc_neutral = sum(sentiment_openai == "neutral") / n() * 100)

  emotion_percentages <- d %>%
    group_by(date) %>%
    summarise(n_enjoyment = sum(emotion_openai == "enjoyment"),
              n_total = n(),
              perc_enjoyment = sum(emotion_openai == "enjoyment") / n_total * 100, 
              perc_sadness = sum(emotion_openai == "sadness") / n_total * 100, 
              perc_anger = sum(emotion_openai == "anger") / n_total * 100,
              perc_fear = sum(emotion_openai == "fear") / n_total * 100,
              perc_disgust = sum(emotion_openai == "disgust") / n_total * 100,
              perc_surprise = sum(emotion_openai == "surprise") / n_total * 100,
              perc_neutral_emotion = sum(emotion_openai == "neutral") / n_total * 100,
              .groups = 'drop')

  emotion_percentages$n_total <- NULL
  emotion_percentages$n_enjoyment <- NULL

  # Calculate counts for boolean OpenAI variables
  boolean_counts <- d %>%
    group_by(date) %>%
    summarise(
      n_longing_for_restoration = sum(longing_for_restoration_openai == "True", na.rm = TRUE),
      n_negative_mental_health = sum(negative_mental_health_expression_openai == "True", na.rm = TRUE),
      n_emotion_due_to_loss = sum(emotion_due_to_loss_openai == "True", na.rm = TRUE),
      n_attachment_related_loss = sum(attachment_related_loss_openai == "True", na.rm = TRUE),
      n_loss_type_change = sum(loss_type_openai == "change", na.rm = TRUE),
      n_loss_type_total_loss = sum(loss_type_openai == "total_loss", na.rm = TRUE),
      n_loss_present = sum(loss_type_openai != "none", na.rm = TRUE),
      n_enjoyment_new_feature = sum(enjoyment_reason_openai == "new_feature", na.rm = TRUE),
      n_enjoyment_ongoing_use = sum(enjoyment_reason_openai == "ongoing_use", na.rm = TRUE),
      n_enjoyment_model_returned = sum(enjoyment_reason_openai == "model_returned_back", na.rm = TRUE),
      n_blame_attribution_company = sum(blame_attribution_openai == "company", na.rm = TRUE),
      n_blame_attribution_chatbot = sum(blame_attribution_openai == "chatbot", na.rm = TRUE),
      n_blame_attribution_ceo = sum(blame_attribution_openai == "ceo", na.rm = TRUE),
      n_update_related = sum(update_related_openai == "True", na.rm = TRUE),
      n_update_related_negative = sum(update_related_openai == "True" & sentiment_openai == "negative", na.rm = TRUE),
      perc_negative_mental_health = sum(negative_mental_health_expression_openai == "True", na.rm = TRUE) / n() * 100,
      perc_attachment_related_loss = sum(attachment_related_loss_openai == "True", na.rm = TRUE) / n() * 100,
      perc_longing_for_restoration = sum(longing_for_restoration_openai == "True", na.rm = TRUE) / n() * 100,
      perc_update_related = sum(update_related_openai == "True", na.rm = TRUE) / n() * 100,

      .groups = 'drop'
    )

  df_days <- merge(df_days, sentiment_counts, by="date")
  df_days <- merge(df_days, emotion_counts, by="date")
  df_days <- merge(df_days, sentiment_percentages, by="date")
  df_days <- merge(df_days, emotion_percentages, by="date")
  df_days <- merge(df_days, boolean_counts, by="date")
  df_days$date_var <- as.Date(df_days$date, format = "%Y-%m-%d") #%Y-

  update_date <- ifelse(app == "replika", REPLIKA_UPDATE_DATE, CHATGPT_UPDATE_DATE)
  df_days$before_after <- ifelse(df_days$date < update_date, "Before", "After")

  # Create folder if not exists
  if (!dir.exists("./data")) {
    dir.create("./data")
  }

  # write df_days to csv
  write.table(df_days, sep=";", paste0("./data/", app, "_daily_summary_", "posts", ".csv"), row.names=FALSE)
  return(df_days)
}

for (app in apps) {
  print(paste0("*-*-*-*-* App: ", app, " *-*-*-*-*"))
  d <- d_all[d_all$app == app, ]
  d_before_update <- d[d$before_update == "True", ]
  d_after_update <- d[d$before_update == "False", ]

  df_days <- prepare_daily_summary(d, app)
  update_date <- ifelse(app == "replika", REPLIKA_UPDATE_DATE, CHATGPT_UPDATE_DATE)
  df_days_before <- df_days[df_days$date < update_date, ]
  df_days_after <- df_days[df_days$date >= update_date, ]

  # Compare number of total posts before vs. after the update
  print("*-*-*-* Comparing total posts before v. after *-*-*-*")
  x <- na.omit(df_days_before$n_positive + df_days_before$n_negative + df_days_before$n_neutral_sentiment)
  y <- df_days_after$n_positive + df_days_after$n_negative + df_days_after$n_neutral_sentiment

  tt <- t.test(x, y, paired = FALSE)
  print(tt)
  print(cohen.d(x, y))

  # Find the days where most posts were made for each app
  max_day_after <- df_days_after$date[which.max(df_days_after$n_positive + df_days_after$n_negative + df_days_after$n_neutral_sentiment)]

  print(paste0("Day with most posts after the update: ", max_day_after))

  # In these days, what percentage of posts were update_related?
  n_posts_max_after <- df_days[df_days$date == max_day_after, "n_update_related"]
  total_posts_max_after <- rowSums(df_days[df_days$date == max_day_after, c("n_positive", "n_negative", "n_neutral_sentiment")])
  pct_update_related <- if (length(total_posts_max_after) > 0 && total_posts_max_after > 0) round(100 * n_posts_max_after / total_posts_max_after, 2) else NA_real_

  # What percentage of negative posts were update-related on that day?
  neg_posts_max_after <- df_days[df_days$date == max_day_after, "n_negative"]
  n_update_related_neg_max_after <- df_days[df_days$date == max_day_after, "n_update_related_negative"]
  pct_update_related_in_negative <- if (length(neg_posts_max_after) > 0 && neg_posts_max_after > 0) round(100 * n_update_related_neg_max_after / neg_posts_max_after, 2) else NA_real_

  print(paste0("Percentage of update-related posts on the day with most posts after the update: ", pct_update_related, "%"))
  print(paste0("Percentage of update-related posts within negative posts on that day: ", pct_update_related_in_negative, "%"))

}

###################################################################
######################### Sentiment analysis ######################
###################################################################

for (app in apps) {
  print(paste0("*-*-*-*-* App: ", app, " *-*-*-*-*"))
  d <- d_all[d_all$app == app, ]
  d_before_update <- d[d$before_update == "True", ]
  d_after_update <- d[d$before_update == "False", ]

  df_days <- prepare_daily_summary(d, app)
  update_date <- ifelse(app == "replika", REPLIKA_UPDATE_DATE, CHATGPT_UPDATE_DATE)
  df_days_before <- df_days[df_days$date < update_date, ]
  df_days_after <- df_days[df_days$date >= update_date, ]

  # Compare negative percentage before vs. after
  print("*-*-*-* Comparing negative percentage after v. before *-*-*-*")
  x <- df_days_after$perc_negative
  y <- df_days_before$perc_negative

  tt <- t.test(x, y, paired = FALSE)
  print(tt)
  print(cohen.d(x, y))
}

# Compare change in sentiment between Replika and ChatGPT
# We normalized each platform’s daily post-update sentiment scores by 
# subtracting its own pre-update mean—yielding the change in each sentiment 
# relative to baseline—and compared these post-update changes between Replika 
# and ChatGPT using independent-sample t-tests.
df_days_replika <- prepare_daily_summary(d_all[d_all$app == "replika", ], "replika")
df_days_chatgpt <- prepare_daily_summary(d_all[d_all$app == "chatgpt", ], "chatgpt")
update_date_replika <- REPLIKA_UPDATE_DATE
update_date_chatgpt <- CHATGPT_UPDATE_DATE
df_days_replika_before <- df_days_replika[df_days_replika$date < update_date_replika, ]
df_days_replika_after <- df_days_replika[df_days_replika$date >= update_date_replika, ]
df_days_chatgpt_before <- df_days_chatgpt[df_days_chatgpt$date < update_date_chatgpt, ]
df_days_chatgpt_after <- df_days_chatgpt[df_days_chatgpt$date >= update_date_chatgpt, ]

# Loop through sentiment types to reduce repetition
sentiments <- list(
  list(name = "negative", col = "perc_negative"),
  list(name = "positive", col = "perc_positive"),
  list(name = "neutral", col = "perc_neutral")
)

for (sent in sentiments) {
  print(paste0("*-*-*-* Comparing ", sent$name, " percentage before v. after *-*-*-*"))
  col <- sent$col

  mean_replika_before <- mean(df_days_replika_before[[col]])
  mean_chatgpt_before <- mean(df_days_chatgpt_before[[col]])

  norm_col <- paste0("norm_", col)
  df_days_replika_after[[norm_col]] <- df_days_replika_after[[col]] - mean_replika_before
  df_days_chatgpt_after[[norm_col]] <- df_days_chatgpt_after[[col]] - mean_chatgpt_before

  x <- df_days_replika_after[[norm_col]]
  y <- df_days_chatgpt_after[[norm_col]]
  tt <- t.test(x, y, paired = FALSE)
  print(tt)
  print(cohen.d(x, y))
}


##### Sentiment proportion tests for each day after the update #####
##### Results are saved in data/app_prop_results_posts.csv files #####
for (app in apps) {
  print(paste0("*-*-*-*-* App: ", app, " *-*-*-*-*"))
  d <- d_all[d_all$app == app, ]
  d_before_update <- d[d$before_update == "True", ]
  d_after_update <- d[d$before_update == "False", ]

  df_days <- prepare_daily_summary(d, app)
  update_date <- ifelse(app == "replika", REPLIKA_UPDATE_DATE, CHATGPT_UPDATE_DATE)
  df_days_before <- df_days[df_days$date < update_date, ]
  df_days_after <- df_days[df_days$date >= update_date, ]

  before_neg <- sum(df_days_before$n_negative)
  before_all <- sum(df_days_before$n_negative + df_days_before$n_positive + df_days_before$n_neutral_sentiment)

  print(paste0("Percentage of negative posts before: ", round(100 * sum(df_days_before$n_negative) / sum(df_days_before$n_negative + df_days_before$n_positive + df_days_before$n_neutral_sentiment), 2)))
  
  # Results container
  prop_results <- data.frame(result = character(), date = character(), eff_size = numeric(), p_value = numeric(), is_significant = logical())

  for (day in df_days_after$date) {
    print(paste0("Day: ", day))
    curr_day <- df_days[df_days$date == day, ]
    after_neg <- curr_day$n_negative
    after_all <- sum(curr_day$n_negative + curr_day$n_positive + curr_day$n_neutral_sentiment)

    print(paste0("Percentage of negative posts after: ", round(100 * after_neg / after_all, 2)))
    # Proportion test between before vs. after
    p_result <- prop.test(c(before_neg, after_neg), c(before_all, after_all))

    eff_size <- ES.h(after_neg / after_all, before_neg / before_all)
    result_str <- paste0(
      "%_After = ", round(100 * after_neg / after_all, 2),
      ", X^2(1, N=", before_all + after_all, ") = ", round(p_result$statistic, 2), ", ",
      ifelse(p_result$p.value < .001, "p < .001", paste0("p = ", round(p_result$p.value, 3)))
    )

    prop_results <- rbind(
      prop_results,
      data.frame(
        result = result_str,
        date = day,
        eff_size = eff_size,
        p_value = p_result$p.value,
        is_significant = p_result$p.value < 0.05,
        stringsAsFactors = FALSE
      )
    )
  }

  write.table(prop_results, sep = ";", paste0("./data/", app, "_prop_results_", "posts", ".csv"), row.names = FALSE)
}

###################################################################
######################### Emotion analyis #########################
###################################################################

for (app in apps) {
  print(paste0("*-*-*-*-* App: ", app, " *-*-*-*-*"))
  d <- d_all[d_all$app == app, ]
  d_before_update <- d[d$before_update == "True", ]
  d_after_update <- d[d$before_update == "False", ]
  df_days <- prepare_daily_summary(d, app)
  update_date <- ifelse(app == "replika", REPLIKA_UPDATE_DATE, CHATGPT_UPDATE_DATE)
  df_days_before <- df_days[df_days$date < update_date, ]
  df_days_after <- df_days[df_days$date >= update_date, ]

  # Compare % of each emotion before vs. after the update
  for(emotion in c('enjoyment', 'sadness', 'anger', 'fear', 'disgust', 'surprise')) {
    print(paste0("*-*-*-* Comparing ", emotion, " percentage after v. before *-*-*-*"))
    x <- df_days_after[, paste0("perc_", emotion)]
    y <- df_days_before[, paste0("perc_", emotion)]
    tt <- t.test(x, y, paired = FALSE)
    print(tt)
    print(cohen.d(x, y))
  }
}

#### Compare the percentage increase in emotions between the two apps
df_days_replika <- prepare_daily_summary(d_all[d_all$app == "replika", ], "replika")
df_days_chatgpt <- prepare_daily_summary(d_all[d_all$app == "chatgpt", ], "chatgpt")

df_days_replika_before <- df_days_replika[df_days_replika$before_after == "Before", ]
df_days_replika_after <- df_days_replika[df_days_replika$before_after == "After", ]
df_days_chatgpt_before <- df_days_chatgpt[df_days_chatgpt$before_after == "Before", ]
df_days_chatgpt_after <- df_days_chatgpt[df_days_chatgpt$before_after == "After", ]

# Calculate the increase in the percentage of posts expressing sadness
mean_sadness_replika_before <- mean(df_days_replika_before$perc_sadness)
mean_sadness_replika_after <- mean(df_days_replika_after$perc_sadness)
increase_sadness_replika <- mean_sadness_replika_after - mean_sadness_replika_before

# Define variables to analyze
variables <- c("perc_enjoyment", "perc_sadness", "perc_anger", 
               "perc_fear", "perc_disgust", "perc_surprise", "perc_neutral_emotion")

# Normalize all sentiment and emotion variables using a loop
for (var in variables) {
  df_days_replika[[paste0(var, "_normalized")]] <- df_days_replika[[var]] - mean(df_days_replika_before[[var]])
  df_days_chatgpt[[paste0(var, "_normalized")]] <- df_days_chatgpt[[var]] - mean(df_days_chatgpt_before[[var]])
}

# Compare all sentiment and emotion measures after the update between the two datasets
df_days_replika_after <- df_days_replika[df_days_replika$before_after == "After", ]
df_days_chatgpt_after <- df_days_chatgpt[df_days_chatgpt$before_after == "After", ]

# Run variance tests and t-tests for all variables using a loop
for (var in variables) {
  var_normalized <- paste0(var, "_normalized")
  
  cat("\n=== Analysis for", var, "===\n")
  
  ttest <- t.test(df_days_replika_after[[var_normalized]], df_days_chatgpt_after[[var_normalized]])
  
  print(ttest)
  print(cohen.d(df_days_replika_after[[var_normalized]], df_days_chatgpt_after[[var_normalized]]))
}

############ Combined Sentiment and Emotion Plot ##############

k_roll <- 3
within_subjects_str <- ifelse(ONLY_LOOK_WITHIN_USERS, "_within_subjects", "")
out_file <- paste0("images/panel_emotion_sentiment_stacked", within_subjects_str, ".pdf")

emotion_vars <- c(
  "n_anger", "n_disgust", "n_fear",
  "n_enjoyment", "n_sadness", "n_surprise"
)

sentiment_vars <- c(
  "n_positive", "n_negative", "n_neutral_sentiment"
)

label_map <- c(
  n_anger = "Anger",
  n_disgust = "Disgust",
  n_fear = "Fear",
  n_enjoyment = "Enjoyment",
  n_sadness = "Sadness",
  n_surprise = "Surprise",
  n_positive = "Positive sentiment",
  n_negative = "Negative sentiment",
  n_neutral_sentiment = "Neutral sentiment"
)

color_map <- c(
  "Anger"              = "#FF7F7F",
  "Disgust"            = "#7FCC7F",
  "Fear"               = "#B57FB5",
  "Enjoyment"          = "#d1d166",
  "Sadness"            = "#7F7FFF",
  "Surprise"           = "#FFBF7F",
  "Positive sentiment" = "#2ca02c",
  "Negative sentiment" = "#d62728",
  "Neutral sentiment"  = "gray40"
)

# ---------------- BUILD DAILY DATA ----------------
df_all_days <- bind_rows(
  prepare_daily_summary(d_all[d_all$app == "replika", ], "replika") %>%
    mutate(platform = "Replika", update_date = as.Date(REPLIKA_UPDATE_DATE)),
  prepare_daily_summary(d_all[d_all$app == "chatgpt", ], "chatgpt") %>%
    mutate(platform = "ChatGPT", update_date = as.Date(CHATGPT_UPDATE_DATE))
) %>%
  mutate(
    platform = factor(platform, levels = c("Replika", "ChatGPT")),
    date_var = as.Date(date_var),
    day_rel = as.numeric(date_var - update_date)
  ) %>%
  arrange(platform, day_rel)

# Keep only the overlapping day range so the two panels line up
shared_window <- df_all_days %>%
  group_by(platform) %>%
  summarise(
    min_day = min(day_rel, na.rm = TRUE),
    max_day = max(day_rel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summarise(
    overlap_min = max(min_day),
    overlap_max = min(max_day)
  )

df_all_days <- df_all_days %>%
  filter(day_rel >= shared_window$overlap_min, day_rel <= shared_window$overlap_max)

# ---------------- SMOOTHING ----------------
all_vars <- c(emotion_vars, sentiment_vars)

# Ensure a complete, consecutive day grid per platform so rolling windows are
# truly k calendar days (and not k available observations with gaps).
df_all_days <- df_all_days %>%
  group_by(platform) %>%
  tidyr::complete(
    day_rel = seq(shared_window$overlap_min, shared_window$overlap_max, by = 1)
  ) %>%
  tidyr::fill(update_date, .direction = "downup") %>%
  mutate(
    date_var = as.Date(update_date + day_rel),
    across(all_of(all_vars), ~ dplyr::coalesce(.x, 0))
  ) %>%
  ungroup()

df_all_days <- df_all_days %>%
  group_by(platform) %>%
  arrange(day_rel, .by_group = TRUE) %>%
  mutate(across(
    all_of(all_vars),
    ~ if (k_roll <= 1) .x else zoo::rollmean(.x, k = k_roll, fill = NA, align = "center"),
    .names = "smooth_{.col}"
  )) %>%
  ungroup()

# ---------------- LONG FORMAT ----------------
df_long <- df_all_days %>%
  select(platform, day_rel, starts_with("smooth_")) %>%
  pivot_longer(
    cols = starts_with("smooth_"),
    names_to = "raw",
    values_to = "value"
  ) %>%
  mutate(
    raw = sub("^smooth_", "", raw),
    label = label_map[raw],
    type = ifelse(raw %in% sentiment_vars, "Sentiment", "Emotion")
  ) %>%
  filter(!is.na(value))

  df_long <- df_long %>%
  mutate(platform = factor(platform, levels = c("Replika", "ChatGPT")))

# ---------------- SIGNIFICANCE DAYS ----------------
sig_df <- bind_rows(
  read.csv("./data/replika_prop_results_posts.csv", sep = ";") %>%
    mutate(
      platform = "Replika",
      date = as.Date(date),
      update_date = as.Date(REPLIKA_UPDATE_DATE)
    ),
  read.csv("./data/chatgpt_prop_results_posts.csv", sep = ";") %>%
    mutate(
      platform = "ChatGPT",
      date = as.Date(date),
      update_date = as.Date(CHATGPT_UPDATE_DATE)
    )
) %>%
  filter(is_significant) %>%
  mutate(day_rel = as.numeric(date - update_date)) %>%
  filter(
    day_rel >= shared_window$overlap_min,
    day_rel <= shared_window$overlap_max
  )

# Keep only days that are present in the plotted (non-NA) smoothed data.
sig_df <- sig_df %>%
  inner_join(
    df_long %>% distinct(platform, day_rel),
    by = c("platform", "day_rel")
  )

# Compute per-panel y-position (top 5%)
sig_y <- df_long %>%
  group_by(platform) %>%
  summarise(
    y_max = max(value, na.rm = TRUE),
    y_sig = y_max * 0.97
  )

sig_df <- left_join(sig_df, sig_y, by = "platform")

# ---------------- PLOT ----------------
plt <- ggplot(
  df_long,
  aes(
    x = day_rel,
    y = value,
    color = label,
    linetype = type
  )
) +

  # update marker
  geom_vline(
    xintercept = 0,
    color = "#f2c94c",
    linewidth = 9,
    alpha = 0.22
  ) +

  # significance markers (blue bars)
  geom_segment(
    data = sig_df,
    aes(
      x = day_rel - 0.5,
      xend = day_rel + 0.5,
      y = y_sig,
      yend = y_sig
    ),
    inherit.aes = FALSE,
    color = "#0b46a3",
    linewidth = 1.6,
    alpha = 0.7
  ) +

  geom_line(linewidth = 0.9, alpha = 0.9) +

  facet_wrap(~ platform, ncol = 1, scales = "free_y", as.table = FALSE) + 

  scale_x_continuous(
    labels = function(x) ifelse(x %in% c(-30, 30), "", x)
  ) +

  scale_color_manual(values = color_map, name = "Category") +

  scale_linetype_manual(
    values = c("Emotion" = "solid", "Sentiment" = "22"),
    name = "Type"
  ) +

  labs(
    x = "Days relative to update",
    y = "Number of posts"
  ) +

  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",

    plot.margin = unit(c(0.15, 0.2, 0.15, 0.2), "cm"),

    axis.title.x = element_text(size = 16, margin = margin(t = 6)),
    axis.title.y = element_text(size = 16, margin = margin(r = 6)),
    axis.text = element_text(size = 14),

    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(0.15, "lines")
  )

print(plt)


# ---------------- SAVE ----------------
ggsave(
  filename = out_file,
  plot = plt,
  width = 12,
  height = 7,
  units = "in",
  dpi = 400,
  useDingbats = FALSE
)


# To directly compare changes across apps, we ran a logistic regression model with platform 
# (Replika vs. ChatGPT), time (Before vs. After update), and their interaction as predictors.


########## AI loss mentions ##########

# Logistic regression on post-level AI loss mentions with platform x time
d_all$loss_present <- d_all$loss_type_openai != "none"

# Logistic regression on post-level AI loss mentions with platform x time
glm_df <- d_all %>%
  select(app, before_update, loss_present) %>%
  filter(!is.na(loss_present)) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"), levels = c("ChatGPT", "Replika")),
    time = factor(ifelse(before_update == "True", "Before", "After"), levels = c("Before", "After")),
    loss = loss_present == TRUE
  )

loss_glm <- glm(loss ~ platform * time, data = glm_df, family = binomial)
print(summary(loss_glm))

loss_rates <- glm_df %>%
  group_by(platform, time) %>%
  summarise(
    n_posts = n(),
    n_loss = sum(loss),
    perc_loss = round(100 * n_loss / n_posts, 2),
    .groups = "drop"
  )

print("AI loss mention rates (%):")
print(loss_rates)


########## Attachment-related loss mentions ##########
# Logistic regression on post-level attachment-related loss mentions with platform x time
glm_df <- d_all %>%
  select(app, before_update, attachment_related_loss_openai) %>%
  filter(!is.na(attachment_related_loss_openai)) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"), levels = c("ChatGPT", "Replika")),
    time = factor(ifelse(before_update == "True", "Before", "After"), levels = c("Before", "After")),
    att = attachment_related_loss_openai == "True"
  )

att_glm <- glm(att ~ platform * time, data = glm_df, family = binomial)
print(summary(att_glm))


attachment_loss_rates <- glm_df %>%
  group_by(platform, time) %>%
  summarise(
    n_posts = n(),
    n_attachment_loss = sum(att),
    perc_attachment_loss = round(100 * n_attachment_loss / n_posts, 2),
    .groups = "drop"
  )

print("Attachment-related loss mention rates (%):")
print(attachment_loss_rates)

########## Negative mental health mentions ##########

# Logistic regression on post-level negative mental health mentions with platform x time
glm_df <- d_all %>%
  select(app, before_update, negative_mental_health_expression_openai) %>%
  filter(!is.na(negative_mental_health_expression_openai)) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"), levels = c("ChatGPT", "Replika")),
    time = factor(ifelse(before_update == "True", "Before", "After"), levels = c("Before", "After")),
    neg_mh = negative_mental_health_expression_openai == "True"
  )

neg_mh_glm <- glm(neg_mh ~ platform * time, data = glm_df, family = binomial)
print(summary(neg_mh_glm))

# The increase in mental health mentions after the update was substantially greater for Replika
# Print the % of before vs after posts with mental health mentions for Replika and ChatGPT
neg_mh_rates <- glm_df %>%
  group_by(platform, time) %>%
  summarise(
    n_posts = n(),
    n_neg_mh = sum(neg_mh),
    perc_neg_mh = round(100 * n_neg_mh / n_posts, 2),
    .groups = "drop"
  )

print("Negative mental health mention rates (%):")
print(neg_mh_rates)


#### Compare the number of mental health posts before vs. after the update #####
for (app in apps) {
  print(paste0("*-*-*-*-* App: ", app, " *-*-*-*-*"))
  d <- d_all[d_all$app == app, ]
  d_before_erp <- d[d$before_update == "True", ]
  d_after_erp <- d[d$before_update == "False", ]

  df_days <- prepare_daily_summary(d, app)
  update_date <- ifelse(app == "replika", REPLIKA_UPDATE_DATE, CHATGPT_UPDATE_DATE)
  df_days_before <- df_days[df_days$date < update_date, ]
  df_days_after <- df_days[df_days$date >= update_date, ]

  x <- df_days_before$n_negative_mental_health 
  y <- df_days_after$n_negative_mental_health

  print(t.test(x, y, paired = FALSE))
  print(cohen.d(x, y))
}

########## Attachment loss → mental health link ##########

glm_df_link <- d_all %>%
  select(
    app,
    before_update,
    attachment_related_loss_openai,
    negative_mental_health_expression_openai
  ) %>%
  filter(
    !is.na(attachment_related_loss_openai),
    !is.na(negative_mental_health_expression_openai)
  ) %>%
  mutate(
    platform = factor(
      ifelse(app == "chatgpt", "ChatGPT", "Replika"),
      levels = c("ChatGPT", "Replika")
    ),
    time = factor(
      ifelse(before_update == "True", "Before", "After"),
      levels = c("Before", "After")
    ),
    attachment_loss = attachment_related_loss_openai == "True",
    neg_mh = negative_mental_health_expression_openai == "True"
  )

link_glm_platform <- glm(
  neg_mh ~ attachment_loss * platform + time,
  data = glm_df_link,
  family = binomial
)

print(summary(link_glm_platform))

link_rates <- glm_df_link %>%
  group_by(attachment_loss, platform) %>%
  summarise(
    n_posts = n(),
    n_neg_mh = sum(neg_mh),
    perc_neg_mh = round(100 * n_neg_mh / n_posts, 2),
    .groups = "drop"
  )

print("Negative mental health rates by attachment loss:")
print(link_rates)


########## Longing for restoration ##########

# Logistic regression on post-level longing for restoration mentions with platform x time
glm_df <- d_all %>%
  select(app, before_update, longing_for_restoration_openai) %>%
  filter(!is.na(longing_for_restoration_openai)) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"), levels = c("ChatGPT", "Replika")),
    time = factor(ifelse(before_update == "True", "Before", "After"), levels = c("Before", "After")),
    longing = longing_for_restoration_openai == "True"
  )

longing_glm <- glm(longing ~ platform * time, data = glm_df, family = binomial)
print(summary(longing_glm))

longing_rates <- glm_df %>%
  group_by(platform, time) %>%
  summarise(
    n_posts = n(),
    n_longing = sum(longing),
    perc_longing = round(100 * n_longing / n_posts, 2),
    .groups = "drop"
  )

print("Longing for restoration mention rates (%):")
print(longing_rates)

########## Attachment loss → longing for restoration link ##########

glm_df_link_longing <- d_all %>%
  select(
    app,
    before_update,
    attachment_related_loss_openai,
    longing_for_restoration_openai
  ) %>%
  filter(
    !is.na(attachment_related_loss_openai),
    !is.na(longing_for_restoration_openai)
  ) %>%
  mutate(
    platform = factor(
      ifelse(app == "chatgpt", "ChatGPT", "Replika"),
      levels = c("ChatGPT", "Replika")
    ),
    time = factor(
      ifelse(before_update == "True", "Before", "After"),
      levels = c("Before", "After")
    ),
    attachment_loss = attachment_related_loss_openai == "True",
    longing = longing_for_restoration_openai == "True"
  )

link_glm_platform_longing <- glm(
  longing ~ attachment_loss * platform + time,
  data = glm_df_link_longing,
  family = binomial
)

print(summary(link_glm_platform_longing))


link_rates <- glm_df_link_longing %>%
  group_by(attachment_loss, platform) %>%
  summarise(
    n_posts = n(),
    n_longing = sum(longing),
    perc_longing = round(100 * n_longing / n_posts, 2),
    .groups = "drop"
  )

print("Longing for restoration rates by attachment loss:")
print(link_rates)

########## Blame attribution mentions ##########

# If present, either "ceo" or "company"
d_all$blame_attribution_openai_present <- d_all$blame_attribution_openai == "company" | d_all$blame_attribution_openai == "ceo"

# Logistic regression on post-level blame attribution mentions with platform x time
glm_df <- d_all %>%
  select(app, before_update, blame_attribution_openai_present) %>%
  filter(!is.na(blame_attribution_openai_present)) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"), levels = c("ChatGPT", "Replika")),
    time = factor(ifelse(before_update == "True", "Before", "After"), levels = c("Before", "After")),
    blame = blame_attribution_openai_present == TRUE
  )

blame_glm <- glm(blame ~ platform * time, data = glm_df, family = binomial)
print(summary(blame_glm))

blame_rates <- glm_df %>%
  group_by(platform, time) %>%
  summarise(
    n_posts = n(),
    n_blame = sum(blame),
    perc_blame = round(100 * n_blame / n_posts, 2),
    .groups = "drop"
  )

print("Blame attribution mention rates (%):")
print(blame_rates)

########## Plot boolean OpenAI variables over time ##########

d_all$loss_present <- d_all$loss_type_openai != "none"

for (app in apps) {
  print(paste0("*-*-*-*-* App: ", app, " *-*-*-*-*"))
  d <- d_all[d_all$app == app, ]
  d_before_update <- d[d$before_update == "True", ]
  d_after_update <- d[d$before_update == "False", ]
  df_days <- prepare_daily_summary(d, app)
  update_date <- ifelse(app == "replika", REPLIKA_UPDATE_DATE, CHATGPT_UPDATE_DATE)
  df_days_before <- df_days[df_days$date < update_date, ]
  df_days_after <- df_days[df_days$date >= update_date, ]

  # Plot the boolean OpenAI variables
  plt_boolean <- ggplot(df_days, aes(x = date_var)) + 
    geom_line(aes(y = n_longing_for_restoration, color = "Longing for Restoration"), size = 1) +
    geom_line(aes(y = n_negative_mental_health, color = "Negative Mental Health"), size = 1) +
    geom_line(aes(y = n_attachment_related_loss, color = "Attachment-Related Loss"), size = 1) +
    geom_line(aes(y = n_loss_present, color = "AI Change or Loss"), size = 1) +
    scale_color_manual(values = c(
      "Longing for Restoration" = "#FF6B6B", 
      "Negative Mental Health" = "#1ed5c8ff", 
      "Attachment-Related Loss" = "#10f68aff",
      "AI Change or Loss" = "#ffa600ff"
    )) +
    labs(x = "Date",
          y = "Number of Posts",
          color = "Legend") +
    scale_x_date(breaks = df_days$date_var[seq(1, dim(df_days)[1], 2)], date_labels = "%m-%d", 
                limits = c(min(df_days$date_var), max(df_days$date_var))) +
    theme_minimal(base_size = 15) +
    theme_classic(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 20),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          plot.margin = unit(c(1, 1, 3, 1), "cm"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))

  plt_boolean

  # Save the boolean variables plot
  ggsave(paste0("./images/", app, "/perc_boolean_openai", within_subjects_str, ".pdf"), 
          width = 24, height = 20, units = "cm", dpi = 400, limitsize=FALSE)
}

###### Bar plots showing change in sentiment and emotion between the apps ######

# Helper to compute before/after percentage-point changes for sentiments
compute_sentiment_change <- function(df_days, app_label) {
  df_before <- df_days[df_days$before_after == "Before", ]
  df_after <- df_days[df_days$before_after == "After", ]
  data.frame(
    sentiment = c("Positive", "Neutral", "Negative"),
    change = c(
      mean(df_after$perc_positive, na.rm = TRUE) - mean(df_before$perc_positive, na.rm = TRUE),
      mean(df_after$perc_neutral, na.rm = TRUE) - mean(df_before$perc_neutral, na.rm = TRUE),
      mean(df_after$perc_negative, na.rm = TRUE) - mean(df_before$perc_negative, na.rm = TRUE)
    ),
    app = app_label
  )
}

prepare_daily_sentiment_change <- function(df_days, app_label) {
  df_before <- df_days[df_days$before_after == "Before", ]

  baseline <- colMeans(
    df_before[, c("perc_positive", "perc_neutral", "perc_negative")],
    na.rm = TRUE
  )

  df_days %>%
    filter(before_after == "After") %>%
    select(date, perc_positive, perc_neutral, perc_negative) %>%
    pivot_longer(
      cols = starts_with("perc_"),
      names_to = "sentiment",
      values_to = "perc"
    ) %>%
    mutate(
      sentiment = recode(
        sentiment,
        perc_positive = "Positive",
        perc_neutral  = "Neutral",
        perc_negative = "Negative"
      ),
      change = perc - baseline[paste0("perc_", tolower(sentiment))],
      app = app_label
    )
}


# Load sentiment change data for both apps
# df_sentiment_change means the change in percentage points (after - before)
df_days_chatgpt_change <- prepare_daily_summary(d_all[d_all$app == "chatgpt", ], "chatgpt")
df_days_erp_change <- prepare_daily_summary(d_all[d_all$app == "replika", ], "replika")

df_sentiment_daily_chatgpt <- prepare_daily_sentiment_change(df_days_chatgpt_change, "ChatGPT")
df_sentiment_daily_erp     <- prepare_daily_sentiment_change(df_days_erp_change, "Replika")

df_sentiment_daily <- rbind(df_sentiment_daily_chatgpt, df_sentiment_daily_erp)


df_sentiment_change_chatgpt <- compute_sentiment_change(df_days_chatgpt_change, "ChatGPT")
df_sentiment_change_erp <- compute_sentiment_change(df_days_erp_change, "Replika")

# Add app identifier to each dataset
df_sentiment_change_chatgpt$app <- "ChatGPT"
df_sentiment_change_erp$app <- "Replika"

# Combine the datasets
df_sentiment_change_combined <- rbind(df_sentiment_change_chatgpt, df_sentiment_change_erp)

# Sort emotions based on ERP highest change
df_sentiment_change_combined$sentiment <- factor(df_sentiment_change_combined$sentiment, levels = c("Positive", "Neutral", "Negative"))

# ----- SENTIMENT PLOT -----
ggplot(df_sentiment_change_combined, aes(x = change, y = sentiment, fill = sentiment, pattern = app)) +
  geom_col_pattern(
    position = position_dodge(width = 0.6),
    width = 0.6,
    pattern_colour = "black",
    pattern_fill = "black",
    color = "black",
    # make stripes visually thinner
    pattern_density = 0.1,   # fewer stripes overall
    pattern_spacing = 0.011 * 2,
    pattern_size = 0.2     # half as thick
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  scale_fill_manual(
    values = c("Negative" = "#b84b4b", "Positive" = "#4bb85f", "Neutral" = "gray"),
    name = "Sentiment"
  ) +
  scale_pattern_manual(values = c("ChatGPT" = "none", "Replika" = "stripe"), name = "App") +
  labs(
    y = "Sentiment",
    x = "Change in Percentage (After minus Before)"
  ) +
  theme_minimal(base_size = 16) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 15),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    # --- remove top & right borders completely ---
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.line.x.bottom = element_line(color = "black", size = 0.6),
    axis.line.y.left = element_line(color = "black", size = 0.6)
  ) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 2), "%")) +
  geom_point(
    data = df_sentiment_daily,
    aes(
      x = change,
      y = sentiment,
      group = app
    ),
    position = position_jitterdodge(
      dodge.width = 0.6,   # MUST match bar dodge
      jitter.height = 0.12,
      jitter.width = 0.1
    ),
    size = 1.7,
    alpha = 0.45,
    color = "black",
    inherit.aes = FALSE
  )

ggsave(paste0("./images/sentiment_change", within_subjects_str, ".pdf"), width = 25, height = 15, units = "cm", dpi = 400, limitsize = FALSE)

###### ----- EMOTION PLOT ----- #####


# Helper to compute before/after percentage-point changes for emotions
compute_emotion_change <- function(df_days, app_label) {
  df_before <- df_days[df_days$before_after == "Before", ]
  df_after <- df_days[df_days$before_after == "After", ]
  data.frame(
    emotion = c("Enjoyment", "Fear", "Neutral", "Surprise", "Disgust", "Anger", "Sadness"),
    change = c(
      mean(df_after$perc_enjoyment, na.rm = TRUE) - mean(df_before$perc_enjoyment, na.rm = TRUE),
      mean(df_after$perc_fear, na.rm = TRUE) - mean(df_before$perc_fear, na.rm = TRUE),
      mean(df_after$perc_neutral_emotion, na.rm = TRUE) - mean(df_before$perc_neutral_emotion, na.rm = TRUE),
      mean(df_after$perc_surprise, na.rm = TRUE) - mean(df_before$perc_surprise, na.rm = TRUE),
      mean(df_after$perc_disgust, na.rm = TRUE) - mean(df_before$perc_disgust, na.rm = TRUE),
      mean(df_after$perc_anger, na.rm = TRUE) - mean(df_before$perc_anger, na.rm = TRUE),
      mean(df_after$perc_sadness, na.rm = TRUE) - mean(df_before$perc_sadness, na.rm = TRUE)
    ),
    app = app_label
  )
}

prepare_daily_emotion_change <- function(df_days, app_label) {

  # Explicit emotion columns (NO sentiment)
  emotion_cols <- c(
    "perc_enjoyment",
    "perc_fear",
    "perc_neutral_emotion",
    "perc_surprise",
    "perc_disgust",
    "perc_anger",
    "perc_sadness"
  )

  # Baseline (pre-update means)
  baseline <- colMeans(
    df_days[df_days$before_after == "Before", emotion_cols],
    na.rm = TRUE
  )

  # Map column names to display labels
  emotion_labels <- c(
    perc_enjoyment = "Enjoyment",
    perc_fear = "Fear",
    perc_neutral_emotion = "Neutral",
    perc_surprise = "Surprise",
    perc_disgust = "Disgust",
    perc_anger = "Anger",
    perc_sadness = "Sadness"
  )

  df_days %>%
    filter(before_after == "After") %>%
    select(date, all_of(emotion_cols)) %>%
    pivot_longer(
      cols = all_of(emotion_cols),
      names_to = "emotion_col",
      values_to = "perc"
    ) %>%
    mutate(
      emotion = emotion_labels[emotion_col],
      change = perc - baseline[emotion_col],
      app = app_label
    ) %>%
    select(date, emotion, change, app)
}


# Calculate change in emotion percentages for ChatGPT (after - before)
df_emotion_change_chatgpt <- compute_emotion_change(df_days_chatgpt_change, "ChatGPT")
df_emotion_change_erp <- compute_emotion_change(df_days_erp_change, "Replika")
df_emotion_daily_chatgpt <- prepare_daily_emotion_change(df_days_chatgpt_change, "ChatGPT")
df_emotion_daily_erp     <- prepare_daily_emotion_change(df_days_erp_change, "Replika")
df_emotion_daily <- rbind(df_emotion_daily_chatgpt, df_emotion_daily_erp)

# Add app identifier to each dataset
df_emotion_change_chatgpt$app <- "ChatGPT"
df_emotion_change_erp$app <- "Replika"

# Combine the datasets
df_emotion_change_combined <- rbind(df_emotion_change_chatgpt, df_emotion_change_erp)

# Sort emotions based on ERP highest change
df_emotion_change_combined$emotion <- factor(df_emotion_change_combined$emotion, levels = c("Neutral", "Enjoyment", "Surprise", "Disgust", "Fear", "Sadness", "Anger"))

# Create a horizontal bar plot comparing both apps with patterns
ggplot(df_emotion_change_combined, aes(x = change, y = emotion, fill = emotion, pattern = app)) +
  geom_col_pattern(
    position = position_dodge(width = 0.6),
    width = 0.6,
    pattern_colour = "black",
    pattern_fill = "black",
    color = "black",
    # make stripes visually thinner
    pattern_density = 0.1,   # fewer stripes overall
    pattern_spacing = 0.015,  # narrower spacing between them
    pattern_size = 0.2     # half as thick
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  scale_fill_manual(values = c("Anger" = "#FF7F7F", "Disgust" = "#7FCC7F", "Fear" = "#B57FB5",
                               "Enjoyment" = "#d1d166", "Sadness" = "#7F7FFF", "Surprise" = "#FFBF7F"),
                    name = "Emotion") +
  scale_pattern_manual(values = c("ChatGPT" = "none", "Replika" = "stripe"), name = "App") +
  labs(y = "Emotion",
       x = "Change in Percentage (After minus Before)") +
  theme_minimal(base_size = 16) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 15),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    # --- remove top & right borders completely ---
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.line.x.bottom = element_line(color = "black", size = 0.6),
    axis.line.y.left = element_line(color = "black", size = 0.6)
  ) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 2), "%")) +
  geom_point(
    data = df_emotion_daily,
    aes(
      x = change,
      y = emotion,
      group = app
    ),
    position = position_jitterdodge(
      dodge.width = 0.6,
      jitter.height = 0.12,
      jitter.width = 0.1
    ),
    size = 0.8,
    alpha = 0.4,
    color = "black",
    inherit.aes = FALSE
  )

ggsave(paste0("./images/emotion_change", within_subjects_str, ".pdf"), 
        width = 25, height = 15, units = "cm", dpi = 400, limitsize=FALSE)

#########################################################
################## Remaining Bar plots ##################
#########################################################

#	•	Panel g: Mental health (Before/After × Platform)
#	•	Panel h: Attachment loss (Before/After × Platform)
#	•	Panel i: Mental health by attachment loss (No / Yes × Platform)

############################################
# Helper: prepare daily percentages
############################################
prepare_daily_percent <- function(data, outcome_var, group_vars) {
  data %>%
    mutate(
      date = if ("date" %in% names(.)) date else as.Date(substr(created_date, 1, 10)),
      outcome_flag = .data[[outcome_var]] %in% c(TRUE, "True")
    ) %>%
    group_by(date, !!!syms(group_vars)) %>%
    summarise(
      n_posts = n(),
      n_true = sum(outcome_flag, na.rm = TRUE),
      perc = 100 * n_true / n_posts,
      .groups = "drop"
    )
}

############################################
# Common bar + dot plotting function
############################################

plot_bar_with_daily_dots <- function(df_bar, df_daily, ylab, outfile) {

  # Robust upper limit for dots (99th percentile)
  y_max <- quantile(df_daily$perc, 0.99, na.rm = TRUE)

  p <- ggplot(df_bar, aes(x = category, y = value,
                          fill = platform, pattern = platform)) +

    ## Bars
    geom_bar_pattern(
      stat = "identity",
      width = 0.6,
      position = position_dodge(width = 0.7),
      pattern_colour = "black",
      pattern_fill = "black",
      color = "black",
      pattern_density = 0.05,
      pattern_spacing = 0.011 * 3.5,
      pattern_size = 0.2
    ) +

    ## Daily dots
    geom_point(
      data = df_daily,
      aes(x = category, y = perc, color = platform),
      position = position_jitterdodge(
        jitter.width = 0.1,
        dodge.width = 0.7
      ),
      size = 1.6,
      alpha = 0.45,
      inherit.aes = FALSE
    ) +

    scale_fill_manual(values = c("Replika" = "#d3d3d3ff",
                                 "ChatGPT" = "#d3d3d3ff")) +
    scale_pattern_manual(values = c("Replika" = "stripe",
                                    "ChatGPT" = "none")) +
    scale_color_manual(values = c("Replika" = "black",
                                  "ChatGPT" = "black"),
                       guide = "none") +

    labs(x = "", y = ylab) +

    theme_minimal(base_size = 18) +
    # Increase size of x axis labels
    theme_classic(base_size = 18) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      # remove legend
      legend.position = "none",
    ) +

    ## IMPORTANT: visual cap without changing data
    coord_cartesian(ylim = c(0, min(y_max * 1.05, 100))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

  ## Save as PDF
  ggsave(
    filename = outfile,
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )

  return(p)
}


############################################
# Panel g: Mental health (Before / After × Platform)
############################################

df_mh_daily <- prepare_daily_percent(
  d_all,
  "negative_mental_health_expression_openai",
  c("app", "before_update")
) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"),
                      levels = c("Replika", "ChatGPT")),
    category = factor(ifelse(before_update == "True", "Before", "After"),
                      levels = c("Before", "After"))
  )

df_mh_bar <- d_all %>%
  filter(!is.na(negative_mental_health_expression_openai)) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"),
                      levels = c("Replika", "ChatGPT")),
    category = factor(ifelse(before_update == "True", "Before", "After"),
                      levels = c("Before", "After")),
    neg_mh = negative_mental_health_expression_openai == "True"
  ) %>%
  group_by(platform, category) %>%
  summarise(
    value = 100 * mean(neg_mh, na.rm = TRUE),
    .groups = "drop"
  )

plt_panel_g <- plot_bar_with_daily_dots(
  df_mh_bar,
  df_mh_daily,
  "Mental health\nmentions (%)",
  paste0("images/panel_g_mental_health", within_subjects_str, ".pdf")
)

############################################
# Panel h: Attachment loss (Before / After × Platform)
############################################

df_attach_daily <- prepare_daily_percent(
  d_all,
  "attachment_related_loss_openai",
  c("app", "before_update")
) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"),
                      levels = c("Replika", "ChatGPT")),
    category = factor(ifelse(before_update == "True", "Before", "After"),
                      levels = c("Before", "After"))
  )

df_attach_bar <- d_all %>%
  filter(!is.na(attachment_related_loss_openai)) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"),
                      levels = c("Replika", "ChatGPT")),
    category = factor(ifelse(before_update == "True", "Before", "After"),
                      levels = c("Before", "After")),
    attach = attachment_related_loss_openai == "True"
  ) %>%
  group_by(platform, category) %>%
  summarise(
    value = 100 * mean(attach, na.rm = TRUE),
    .groups = "drop"
  )

plt_panel_h <- plot_bar_with_daily_dots(
  df_attach_bar,
  df_attach_daily,
  "Attachment-related\nloss (%)",
  paste0("images/panel_h_attachment_loss", within_subjects_str, ".pdf")
)

############################################
# Panel i: Mental health by attachment loss × Platform
############################################

df_conditional_daily <- prepare_daily_percent(
  d_all %>% filter(!is.na(attachment_related_loss_openai)),
  "negative_mental_health_expression_openai",
  c("app", "attachment_related_loss_openai")
) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"),
                      levels = c("Replika", "ChatGPT")),
    category = factor(
      ifelse(attachment_related_loss_openai %in% c(TRUE, "True"),
             "Attachment loss", "No attachment loss"),
      levels = c("No attachment loss", "Attachment loss")
    )
  )

df_conditional_bar <- d_all %>%
  filter(
    !is.na(attachment_related_loss_openai),
    !is.na(negative_mental_health_expression_openai)
  ) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"),
                      levels = c("Replika", "ChatGPT")),
    attachment_loss = attachment_related_loss_openai == "True",
    neg_mh = negative_mental_health_expression_openai == "True",
    category = factor(
      ifelse(attachment_loss,
             "Attachment loss", "No attachment loss"),
      levels = c("No attachment loss", "Attachment loss")
    )
  ) %>%
  group_by(platform, category) %>%
  summarise(
    value = 100 * mean(neg_mh, na.rm = TRUE),
    .groups = "drop"
  )

plt_panel_i <- plot_bar_with_daily_dots(
  df_conditional_bar,
  df_conditional_daily,
  "Mental health mentions (%)",
  paste0("images/fig_2_panel_a_conditional_mh", within_subjects_str, ".pdf")
)


############################################
# Figure 2 Panel b: Longing for restoration by attachment loss × Platform
############################################

df_longing_conditional_daily <- prepare_daily_percent(
  d_all %>% filter(!is.na(attachment_related_loss_openai)),
  "longing_for_restoration_openai",
  c("app", "attachment_related_loss_openai")
) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"),
                      levels = c("Replika", "ChatGPT")),
    category = factor(
      ifelse(attachment_related_loss_openai %in% c(TRUE, "True"),
             "Attachment loss", "No attachment loss"),
      levels = c("No attachment loss", "Attachment loss")
    )
  )

df_longing_conditional_bar <- d_all %>%
  filter(
    !is.na(attachment_related_loss_openai),
    !is.na(longing_for_restoration_openai)
  ) %>%
  mutate(
    platform = factor(ifelse(app == "chatgpt", "ChatGPT", "Replika"),
                      levels = c("Replika", "ChatGPT")),
    attachment_loss = attachment_related_loss_openai == "True",
    longing = longing_for_restoration_openai == "True",
    category = factor(
      ifelse(attachment_loss,
             "Attachment loss", "No attachment loss"),
      levels = c("No attachment loss", "Attachment loss")
    )
  ) %>%
  group_by(platform, category) %>%
  summarise(
    value = 100 * mean(longing, na.rm = TRUE),
    .groups = "drop"
  )

plt_fig2_panel_b <- plot_bar_with_daily_dots(
  df_longing_conditional_bar,
  df_longing_conditional_daily,
  "Longing for\nrestoration (%)",
  paste0("images/fig_2_panel_b_longing_by_attachment_loss_platform", within_subjects_str, ".pdf")
)


############################################

# Exploratory N words analysis
# Look at whether posts with sadness mention contain more N words
# For this, first calculate N word counts per post with the most optimized method
library(stringr)
d_all$n_word_count <- str_count(
  stringr::str_to_lower(dplyr::coalesce(d_all$titlencontent, "")),
  stringr::boundary("word")
)

# Loss or change mentions
x_loss <- d_all$n_word_count[d_all$loss_type_openai != "none"]
y_loss <- d_all$n_word_count[d_all$loss_type_openai == "none"]

t_test_result_loss <- t.test(x_loss, y_loss)
print(t_test_result_loss)

cohen.d_result_loss <- cohen.d(x_loss, y_loss)
print(cohen.d_result_loss)

# Mental health mentions
x_mh <- d_all$n_word_count[d_all$negative_mental_health_expression_openai == "True"]
y_mh <- d_all$n_word_count[d_all$negative_mental_health_expression_openai != "True"]

t_test_result_mh <- t.test(x_mh, y_mh)
print(t_test_result_mh)

cohen.d_result_mh <- cohen.d(x_mh, y_mh)
print(cohen.d_result_mh)

# Attachment loss
x_attach <- d_all$n_word_count[d_all$attachment_related_loss_openai == "True"]
y_attach <- d_all$n_word_count[d_all$attachment_related_loss_openai != "True"]

t_test_result_attach <- t.test(x_attach, y_attach)
print(t_test_result_attach)

cohen.d_result_attach <- cohen.d(x_attach, y_attach)
print(cohen.d_result_attach)

# Also longing for restoration
x_longing <- d_all$n_word_count[d_all$longing_for_restoration_openai == "True"]
y_longing <- d_all$n_word_count[d_all$longing_for_restoration_openai != "True"]

t_test_result_longing <- t.test(x_longing, y_longing)
print(t_test_result_longing)

cohen.d_result_longing <- cohen.d(x_longing, y_longing)
print(cohen.d_result_longing)

############################################################
# Side-by-side event-study plot (Replika vs ChatGPT)
############################################################

plot_event_study_side_by_side <- function(
  df_days_replika,
  df_days_chatgpt,
  outcome_var,          # e.g., "perc_negative_mental_health"
  ylab,
  outfile,
  smooth = TRUE
) {

  # Prepare Replika
  replika_df <- df_days_replika %>%
    mutate(
      app = "Replika",
      rel_day = as.numeric(as.Date(date) - as.Date(REPLIKA_UPDATE_DATE)),
      value = .data[[outcome_var]]
    ) %>%
    select(app, rel_day, value)

  # Prepare ChatGPT
  chatgpt_df <- df_days_chatgpt %>%
    mutate(
      app = "ChatGPT",
      rel_day = as.numeric(as.Date(date) - as.Date(CHATGPT_UPDATE_DATE)),
      value = .data[[outcome_var]]
    ) %>%
    select(app, rel_day, value)

  plot_df <- bind_rows(replika_df, chatgpt_df) %>%
    filter(!is.na(value)) %>%
    mutate(app = factor(app, levels = c("Replika", "ChatGPT")))

  # Robust y-limit (avoid single-day spikes)
  y_max <- quantile(plot_df$value, 0.99, na.rm = TRUE)

  p <- ggplot(plot_df, aes(x = rel_day, y = value)) +

    ## Daily dots
    geom_point(
      size = 1.5,
      alpha = 0.45,
      color = "black"
    ) +

    ## Smooth trend
    {if (smooth)
      geom_smooth(
        method = "loess",
        span = 0.35,
        se = FALSE,
        color = "black",
        linewidth = 1
      )
    } +

    ## Update marker
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.8,
      color = "black"
    ) +

    facet_wrap(~ app, nrow = 1) +

    labs(
      x = "Days relative to update",
      y = ylab
    ) +

    coord_cartesian(ylim = c(0, y_max * 1.05)) +

    theme_classic(base_size = 16) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      strip.text = element_text(size = 17, face = "bold"),
      strip.background = element_blank(),
      plot.margin = margin(6, 6, 6, 6, "mm")
    )

  ggsave(
    filename = outfile,
    plot = p,
    width = 10,
    height = 4,
    dpi = 300
  )

  return(p)
}


# Daily summaries (already in your pipeline)
df_days_replika <- prepare_daily_summary(d_all[d_all$app == "replika", ], "replika")
df_days_chatgpt <- prepare_daily_summary(d_all[d_all$app == "chatgpt", ], "chatgpt")

## --- Mental health ---
plot_event_study_side_by_side(
  df_days_replika,
  df_days_chatgpt,
  outcome_var = "perc_negative_mental_health",
  ylab = "Mental health mentions (%)",
  outfile = paste0("images/event_mh_side_by_side", within_subjects_str, ".pdf")
)

## --- Attachment-related loss ---
plot_event_study_side_by_side(
  df_days_replika,
  df_days_chatgpt,
  outcome_var = "perc_attachment_related_loss",
  ylab = "Attachment-related loss (%)",
  outfile = paste0("images/event_attachment_loss_side_by_side", within_subjects_str, ".pdf")
)

## --- Longing for restoration ---
plot_event_study_side_by_side(
  df_days_replika,
  df_days_chatgpt,
  outcome_var = "perc_longing_for_restoration",
  ylab = "Longing for restoration (%)",
  outfile = paste0("images/event_longing_for_restoration_side_by_side", within_subjects_str, ".pdf")
)

## --- Negative sentiment ---
if (FALSE) {
  plot_event_study_side_by_side(
    df_days_replika,
    df_days_chatgpt,
    outcome_var = "perc_negative",
    ylab = "Negative sentiment (%)",
    outfile = paste0("images/event_negative_sentiment_side_by_side", within_subjects_str, ".pdf")
  )

  ## --- Sadness ---
  plot_event_study_side_by_side(
    df_days_replika,
    df_days_chatgpt,
    outcome_var = "perc_sadness",
    ylab = "Sadness (%)",
    outfile = paste0("images/event_sadness_side_by_side", within_subjects_str, ".pdf")
  )
}


################## Check manual codings ##################

d_manual_replika <- read.csv('replika_25_posts_per_emotion_sample_final.csv', sep = ';')
d_manual_chatgpt <- read.csv('chatgpt_25_posts_per_emotion_sample_final.csv', sep = ';')

d_manual <- rbind(d_manual_replika, d_manual_chatgpt)

# Rename "sentiment" to "sentiment_roberta"
names(d_manual)[names(d_manual) == "sentiment"] <- "sentiment_roberta"

# Rename "emotion" to "emotion_roberta"
names(d_manual)[names(d_manual) == "emotion"] <- "emotion_roberta"

# Rename emotion_roberta values "joy" to "enjoyment"
d_manual$emotion_roberta[d_manual$emotion_roberta == "joy"] <- "enjoyment" 

length(unique(d_manual$post_id))

# Safely parse JSON and extract a single field
extract_field <- function(json_string, field) {
  tryCatch({
    parsed <- fromJSON(json_string)
    parsed[[field]]
  }, error = function(e) {
    NA
  })
}

fields_to_analyze <- c(
  "sentiment", "emotion", "enjoyment_reason", "community_support_expression",
  "loss_type", "emotion_due_to_loss", "attachment_related_loss",
  "negative_mental_health_expression", "longing_for_restoration",
  "update_related", "blame_attribution"
)

get_field_values <- function(field_name) {
  list(
    k = sapply(d_manual$classification_manual_k, extract_field, field = field_name),
    z = sapply(d_manual$classification_manual_z, extract_field, field = field_name),
    gpt = sapply(d_manual$classification, extract_field, field = field_name)
  )
}

compute_irr <- function(k_values, z_values) {
  valid <- !is.na(k_values) & !is.na(z_values)
  k_clean <- k_values[valid]
  z_clean <- z_values[valid]

  if (length(k_clean) == 0) {
    return(list(kappa = NA, p_value = NA, agreement_percent = NA))
  }

  agreement_pct <- sum(k_clean == z_clean, na.rm = TRUE) / length(k_clean) * 100
  if (length(unique(c(k_clean, z_clean))) <= 1) {
    return(list(kappa = NA, p_value = NA, agreement_percent = 100))
  }

  kappa_result <- tryCatch({
    kappa2(data.frame(k = k_clean, z = z_clean))
  }, error = function(e) {
    list(value = NA, p.value = NA)
  })

  list(
    kappa = ifelse(is.null(kappa_result$value), NA, kappa_result$value),
    p_value = ifelse(is.null(kappa_result$p.value), NA, kappa_result$p.value),
    agreement_percent = agreement_pct
  )
}

compute_accuracy_overall <- function(k_values, z_values, gpt_values) {
  gpt_classified <- !is.na(gpt_values)
  if (sum(gpt_classified) == 0) {
    return(list(common_cases = 0, accuracy_percent = NA))
  }

  human_consensus <- !is.na(k_values) & !is.na(z_values) & (k_values == z_values)
  correct <- gpt_classified & human_consensus & (gpt_values == k_values)
  list(
    common_cases = sum(human_consensus),
    accuracy_percent = sum(correct, na.rm = TRUE) / sum(human_consensus) * 100
  )
}

irr_results <- do.call(rbind, lapply(fields_to_analyze, function(field_name) {
  vals <- get_field_values(field_name)
  irr <- compute_irr(vals$k, vals$z)
  data.frame(
    field = field_name,
    kappa = irr$kappa,
    p_value = irr$p_value,
    agreement_percent = irr$agreement_percent,
    stringsAsFactors = FALSE
  )
}))

accuracy_results <- do.call(rbind, lapply(fields_to_analyze, function(field_name) {
  vals <- get_field_values(field_name)
  acc <- compute_accuracy_overall(vals$k, vals$z, vals$gpt)
  data.frame(
    field = field_name,
    common_cases = acc$common_cases,
    accuracy_percent = acc$accuracy_percent,
    stringsAsFactors = FALSE
  )
}))

cat("\n", strrep("=", 50), "\n", sep = "")
cat("INTER-RATER RELIABILITY SUMMARY\n")
cat(strrep("=", 50), "\n", sep = "")
print(irr_results)

cat("\n", strrep("=", 50), "\n", sep = "")
cat("ACCURACY SUMMARY\n")
cat(strrep("=", 50), "\n", sep = "")
print(accuracy_results)

cat("\n", strrep("=", 60), "\n", sep = "")
cat("DETAILED ACCURACY BREAKDOWN BY CATEGORY\n")
cat(strrep("=", 60), "\n", sep = "")

calculate_category_accuracy <- function(field_name, categories) {
  vals <- get_field_values(field_name)
  valid <- !is.na(vals$k) & !is.na(vals$z)
  human_consensus <- valid & (vals$k == vals$z)

  cases <- sapply(categories, function(cat) sum(!is.na(vals$gpt) & vals$gpt == cat & human_consensus))
  correct <- sapply(categories, function(cat) sum((!is.na(vals$gpt) & vals$gpt == cat) & human_consensus & (vals$k == cat)))
  accuracy <- ifelse(cases > 0, correct / cases * 100, NA)

  data.frame(
    category = categories,
    cases = as.numeric(cases),
    accuracy_percent = as.numeric(accuracy),
    stringsAsFactors = FALSE
  )
}

emotions <- c("anger", "sadness", "disgust", "neutral", "fear", "enjoyment", "surprise")
emotion_breakdown <- calculate_category_accuracy("emotion", emotions)
print(emotion_breakdown)

sentiments <- c("positive", "negative", "neutral")
sentiment_breakdown <- calculate_category_accuracy("sentiment", sentiments)
print(sentiment_breakdown)



######## Calculate accuracy of RoBERTa model "emotion_roberta", "sentiment_roberta" ########
######## against manual codings in classification_manual_k and classification_manual_z ########

standardize_labels <- function(x) {
  tolower(trimws(x))
}

compute_roberta_accuracy <- function(field_name, roberta_col) {
  vals <- get_field_values(field_name)
  k_vals <- standardize_labels(vals$k)
  z_vals <- standardize_labels(vals$z)
  roberta_vals <- standardize_labels(d_manual[[roberta_col]])

  human_consensus <- !is.na(k_vals) & !is.na(z_vals) & (k_vals == z_vals)
  if (sum(human_consensus) == 0) {
    return(list(common_cases = 0, accuracy_percent = NA))
  }

  correct <- human_consensus & !is.na(roberta_vals) & (roberta_vals == k_vals)
  list(
    common_cases = sum(human_consensus),
    accuracy_percent = sum(correct) / sum(human_consensus) * 100
  )
}

calculate_category_accuracy_roberta <- function(field_name, roberta_col, categories) {
  vals <- get_field_values(field_name)
  k_vals <- standardize_labels(vals$k)
  z_vals <- standardize_labels(vals$z)
  roberta_vals <- standardize_labels(d_manual[[roberta_col]])

  human_consensus <- !is.na(k_vals) & !is.na(z_vals) & (k_vals == z_vals)

  cases <- sapply(categories, function(cat) sum(roberta_vals == cat & human_consensus, na.rm = TRUE))
  correct <- sapply(categories, function(cat) sum(roberta_vals == cat & k_vals == cat & human_consensus, na.rm = TRUE))
  accuracy <- ifelse(cases > 0, correct / cases * 100, NA)

  data.frame(
    category = categories,
    cases = as.numeric(cases),
    accuracy_percent = as.numeric(accuracy),
    stringsAsFactors = FALSE
  )
}

roberta_accuracy_results <- rbind(
  data.frame(
    field = "emotion",
    t(compute_roberta_accuracy("emotion", "emotion_roberta")),
    stringsAsFactors = FALSE
  ),
  data.frame(
    field = "sentiment",
    t(compute_roberta_accuracy("sentiment", "sentiment_roberta")),
    stringsAsFactors = FALSE
  )
)

cat("\n", strrep("=", 50), "\n", sep = "")
cat("ROBERTA ACCURACY SUMMARY\n")
cat(strrep("=", 50), "\n", sep = "")
print(roberta_accuracy_results)

cat("\n", strrep("=", 60), "\n", sep = "")
cat("ROBERTA DETAILED ACCURACY BREAKDOWN\n")
cat(strrep("=", 60), "\n", sep = "")

roberta_emotion_breakdown <- calculate_category_accuracy_roberta("emotion", "emotion_roberta", emotions)
print(roberta_emotion_breakdown)

roberta_sentiment_breakdown <- calculate_category_accuracy_roberta("sentiment", "sentiment_roberta", sentiments)
print(roberta_sentiment_breakdown)
