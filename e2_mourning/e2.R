## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")
# Set the working directory to this script's folder. rstudioapi returns the
# *focused* editor tab, which may be a different file if e2.R was sourced while
# another script was focused; fall back to the sourced-file path in that case.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  this_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile), error = function(e) NULL)
  if (!is.null(this_file) && nzchar(this_file)) setwd(dirname(this_file))
}

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',
               'ggpattern',
               'stats',
               'lsr',
               'ggpubr',
               'sjstats',
               'pwr',
               'effectsize',
               'parameters',
               'ltm',
               'semTools',
               'lavaan',
               'tidyverse',
               'cSEM','seminr', 'interactions', 'dplyr', 'effsize', 'car')

### Read the data
d <- read.csv('./data.csv')

# Fail loudly if the wrong file loaded (e.g. wrong working directory / focused
# tab) instead of erroring cryptically later in the PLS block with
# "invalid 'row.names' length".
if (!"mourn_1_aicomp_1" %in% names(d)) {
  stop("data.csv in '", getwd(), "' is not the Study 2 data ",
       "(missing 'mourn_1_aicomp_1'). Check the working directory.")
}

# Remove NA rows
d <- d[d$Finished == 1, ]

################################## Exclusions ##################################

# Print IDs of failed attention check participants
print(paste0("IDS of participants failed attention check: ", paste(d[d$att_1 != 2 | d$att_2 != 2, 'prolific_id'], collapse = ", ")))

# Attention check
d <- d[d$att_1 == 2 & d$att_2 == 2,]

print(paste0("Number of participants after attention check: ", nrow(d)))

# Exclude bots
d <- d[d$Q_RecaptchaScore > 0.5,]

print(paste0("Number of participants after bot exclusion: ", nrow(d)))

# Exclude duplicates
d <- d[d$Q_RelevantIDDuplicate != "true",]

print(paste0("Number of participants after excluding duplicates: ", nrow(d)))

# Write emails of these participants to a csv file
#write.csv(d$email, file = "./participants_hired.csv", row.names = FALSE)

# Comprehension check:
d <- d[d$comp_1 == 1,]

print(paste0("Number of participants after comprehension check: ", nrow(d)))

############################ User characteristics ##############################

## user age
mean(as.numeric(d$age), trim = 0, na.rm = TRUE) ## mean age 
sd(d$age, na.rm = TRUE) ## standard deviation 

## user gender
table(d$gender)
100 * table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
100 * table(d$gender)[2]/sum(table(d$gender)) ## percentage of females
100 * table(d$gender)[3]/sum(table(d$gender)) ## percentage of not disclosed
100 * table(d$gender)[4]/sum(table(d$gender)) ## percentage of others

## user ethnicity
table(d$ethnicity)

sum(d$ethnicity == "2"); 100 * sum(d$ethnicity == "2") / dim(d)[1] ## percentage of asians
sum(d$ethnicity == "1"); 100 * sum(d$ethnicity == "1") / dim(d)[1] ## percentage of blacks
sum(d$ethnicity == "4"); 100 * sum(d$ethnicity == "4") / dim(d)[1] ## percentage of hispanics or latinos
sum(d$ethnicity == "3"); 100 * sum(d$ethnicity == "3") / dim(d)[1] ## percentage of whites
(sum(d$ethnicity == "5") + sum(nchar(d$ethnicity) > 1)); 100 * (sum(d$ethnicity == "5") + sum(nchar(d$ethnicity) > 1)) / dim(d)[1] ## percentage of mixed
sum(d$ethnicity == "6"); 100 * sum(d$ethnicity == "6") / dim(d)[1] ## percentage of others

## user education
table(d$edu)

100 * sum(d$edu == "1") / dim(d)[1] # Percentage of high school or equivalent
100 * sum(d$edu == "2") / dim(d)[1] # Percentage of vocational school
100 * sum(d$edu == "3") / dim(d)[1] # Percentage of some college
100 * sum(d$edu == "4") / dim(d)[1] # Percentage of college graduate (4 years)
100 * sum(d$edu == "5") / dim(d)[1] # Percentage of masters degree
100 * sum(d$edu == "6") / dim(d)[1] # Percentage of doctoral degree
100 * sum(d$edu == "7") / dim(d)[1] # Percentage of professional degree
100 * sum(d$edu == "8") / dim(d)[1] # Percentage of other

## ---- Replika companion characteristics

## replika gender
table(d$gender_rep)
100 * table(d$gender_rep)[1]/sum(table(d$gender_rep)) ## percentage of males
100 * table(d$gender_rep)[2]/sum(table(d$gender_rep)) ## percentage of females
100 * table(d$gender_rep)[3]/sum(table(d$gender_rep)) ## percentage of non-binary

## replika relationship
table(d$relationship_rep)
100 * table(d$relationship_rep)[1]/sum(table(d$relationship_rep)) ## percentage of friends
100 * table(d$relationship_rep)[2]/sum(table(d$relationship_rep)) ## percentage of partners
100 * table(d$relationship_rep)[3]/sum(table(d$relationship_rep)) ## percentage of mentors
100 * table(d$relationship_rep)[4]/sum(table(d$relationship_rep)) ## percentage of see how it goes


## replika subscription
table(d$subscription_rep)
100 * table(d$subscription_rep)[1]/sum(table(d$subscription_rep)) ## percentage of none
100 * table(d$subscription_rep)[2]/sum(table(d$subscription_rep)) ## percentage of monthly 
100 * table(d$subscription_rep)[3]/sum(table(d$subscription_rep)) ## percentage of yearly
100 * table(d$subscription_rep)[4]/sum(table(d$subscription_rep)) ## percentage of lifetime

# Remove prticipants without subscription
#d <- d[d$subscription_rep != "1",]

dim(d)

## replika months
mean(as.numeric(d$months_rep), trim = 0, na.rm = TRUE) ## mean number of months
sd(as.numeric(d$months_rep), na.rm = TRUE) ## standard deviation

###########

# Make all dv's numeric, i.e., all columns that contain 'mhealth', 'mourn', 'disap', 'freq'
for (col in colnames(d)) {
  if (grepl('mourn', col) | grepl('disap', col) | grepl('freq', col)) {
    d[[col]] <- as.numeric(d[[col]])
  }
}

CONDITIONS <- c('aicomp', 'app', 'brand', 'game', 'voice', 'pet', 'car')

# We have 2 questions for each DV, i.e., mhealth, mourn, disap
# We will calculate the mean of these 2 questions for each DV and condition
for (condition in CONDITIONS) {
  for (dv in c('mourn', 'disap')) {
    d[[paste0(dv, '_', condition)]] <- (d[[paste0(dv, '_1_', condition, '_1')]]
                                        + d[[paste0(dv, '_2_', condition, '_1')]]) / 2
  }
}

# Correlation table between each question
cor_matrix <- cor(d[, c('mourn_1_aicomp_1', 'mourn_2_aicomp_1', 'disap_1_aicomp_1', 'disap_2_aicomp_1')])
cor_matrix

############# AI Assistant Usage #############

# What % of users are from which app?
# First, lowercase all entries in the app column
d$ai_companion_app <- tolower(d$aicomp_name)

# Remove spaces from the app column
d$ai_companion_app <- gsub(" ", "", d$ai_companion_app)

# Now, let's search for AI companions apps in the column
# Do this for a vector of apps
apps <- c('replika', 'chatgpt', 'chatgbt', 'cahtgpt', 'chatbot', 'simsimi', 'cleverbot', 'cai', 'characterai', 'chai', 'woebot', 'siri', 'alexa', 'waifu', 'waze', 'snapchat', 'cortana', 'zoom', 'talkie', 'pi', 
          'openai', 'googleassistant', 'gemini', 'genesia', 'copilot', 'bard', 'bing', 'neverused', 'notaware', "don'thave", "donotinteract", "don'tuse")

# Now, let's iterate through all apps and see how many users are from each app. For this, create a new column for each app and indicate usage with 1 or 0
for(app in apps) {
  d[[app]] <- ifelse(grepl(app, d$ai_companion_app), 1, 0)
}

# Now, let's see how many users are from each app, along with their percentages.
# First sort the apps by the number of users
apps <- apps[order(colSums(d[apps]))]

for(app in apps) {
  print(paste0(app, ": ", sum(d[[app]]), " (", sum(d[[app]]) / dim(d)[1] * 100, "%)"))
}

table(d$ai_companion_app)

# Remove apps that are not AI companion apps, i.e., chatgpt, alexa, gemini, googleassistant, copilot, snapchat, zoom, chatgbt, cortana, siri
d <- d[(d$chatgpt == 0 & d$alexa == 0 & d$gemini == 0 & d$googleassistant == 0 & d$copilot == 0 & d$snapchat == 0 & d$zoom == 0 & d$chatgbt == 0 & d$cortana == 0 & d$siri == 0 & d$cahtgpt == 0 & d$neverused == 0 & d$notaware == 0 & d$donotinteract == 0 & d[, "don'tuse"] == 0 & d[, "don'thave"] == 0),]

# number of users after excluding AI assistant users
print(paste0("Number of participants after excluding AI assistant users: ", nrow(d)))

############### QUALITATIVE ANALYSIS ###############

# Look at the names of d$condition_name
d$app_name <- tolower(d$app_name)
d$brand_name <- tolower(d$brand_name)
d$game_name <- tolower(d$game_name)
d$pet_name <- tolower(d$pet_name)
d$car_name <- tolower(d$car_name)
d$voice_name <- tolower(d$voice_name)
d$aicomp_name <- tolower(d$aicomp_name)

d$owns

# Get how many rows include "5"
sum(grepl("5", d$owns)) / dim(d)[1]

# Get how many rows include "6"
sum(grepl("6", d$owns)) / dim(d)[1]

############### DESCRIPTIVE ANALYSIS ###############

# Print the mean (SD, n) for each DV, based on condition
for (condition in CONDITIONS) {
  print(paste0("*-*-*-*-* Condition: ", condition))
  for (dv in c('disap', 'mourn', 'freq')) {
    v <- d[[paste0(dv, '_', condition)]]
    print(paste0(dv, ": M = ", round(mean(v, na.rm = TRUE), 2),
                 ", SD = ", round(sd(v, na.rm = TRUE), 2),
                 ", n = ", sum(!is.na(v))))
  }
}

###############  ANCOVAS ###############

# Convert d to long format, with entity type as separate column
d_long <- d %>%
  pivot_longer(cols = c('mourn_aicomp', 'mourn_app', 'mourn_brand', 'mourn_game', 'mourn_voice', 'mourn_pet', 'mourn_car',
                        'disap_aicomp', 'disap_app', 'disap_brand', 'disap_game', 'disap_voice', 'disap_pet', 'disap_car',
                        'freq_aicomp', 'freq_app', 'freq_brand', 'freq_game', 'freq_voice', 'freq_pet', 'freq_car'
                        ),
               names_to = c('.value', 'entity_type'),
               names_sep = '_')

d_long$entity_type <- as.factor(d_long$entity_type)

# We will conduct two ANCOVAs to examine the effect of entity type on mourning, mental health, and disappointment, with “frequency of use” (hours per week interacting with the entity) as a covariate
for (dv in c('mourn', 'disap')) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  aov_result <- aov(d_long[[dv]] ~ d_long$freq + d_long$entity_type, data = d_long)
  print(summary(aov_result))
  print(anova_stats(aov_result))
  # Classical eta-squared with two-sided 95% CIs (effect sizes for each term)
  print(effectsize::eta_squared(aov_result, partial = FALSE, alternative = "two.sided"))
}

# --- Assumption checks for the ANCOVAs (normality, equal variance, equal slopes) ---
# and a non-parametric robustness check (Friedman test; entity type is within-subjects).
# Pre-registered analyses are the ANCOVAs/t-tests above; these confirm robustness.
for (dv in c('mourn', 'disap')) {
  print(paste0("*-*-*-*-* Assumption checks: ", dv, " *-*-*-*-*"))
  aov_result <- aov(d_long[[dv]] ~ d_long$freq + d_long$entity_type, data = d_long)
  print(paste0("Residual normality (Shapiro-Wilk) p = ",
               signif(shapiro.test(residuals(aov_result))$p.value, 3)))
  print(paste0("Homogeneity of variance across entity_type (Levene) p = ",
               signif(car::leveneTest(d_long[[dv]] ~ d_long$entity_type)[1, "Pr(>F)"], 3)))
  aov_interaction <- aov(d_long[[dv]] ~ d_long$freq * d_long$entity_type, data = d_long)
  print(paste0("Homogeneity of regression slopes (freq x entity interaction) p = ",
               signif(summary(aov_interaction)[[1]]["d_long$freq:d_long$entity_type", "Pr(>F)"], 3)))
  # Friedman test (non-parametric, repeated-measures omnibus across the 7 entities)
  fried_mat <- as.matrix(d[, paste0(dv, '_', CONDITIONS)])
  ft <- friedman.test(fried_mat)
  print(paste0("Friedman chi2(", ft$parameter, ") = ", round(ft$statistic, 2),
               ", p = ", signif(ft$p.value, 3)))
}

# Now, run t-tests comparing AI companion condition to all other conditions
for (dv in c('mourn', 'disap')) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  for (condition in CONDITIONS) {
    if (condition != 'aicomp') {
      print(paste0("Condition: ", condition))
      x <- d_long[d_long$entity_type == 'aicomp',][[dv]]
      y <- d_long[d_long$entity_type == condition,][[dv]]
      vart <- var.test(x, y)
      tt <- t.test(x, y, paired = TRUE, var.equal = vart$p.value > 0.05); print(tt)
      print(cohen.d(x, y))

      # Print the mean difference with 95% CI
      mean_diff <- mean(x - y, na.rm = TRUE)
      ci <- t.test(x, y, paired = TRUE)$conf.int
      print(paste0("Mean difference: ", round(mean_diff, 2),
                   ", 95% CI [", round(ci[1], 2), ", ", round(ci[2], 2), "]"))

      # Assumption check + non-parametric robustness for the paired t-test
      print(paste0("Difference-score normality (Shapiro-Wilk) p = ",
                   signif(shapiro.test(x - y)$p.value, 3)))
      print(paste0("Wilcoxon signed-rank p = ",
                   signif(suppressWarnings(wilcox.test(x, y, paired = TRUE)$p.value), 3)))
    }
  }
}


#### Plotting ####

toLabel <- function(x) {
  labels <- c()
  for(label in x) {
    if(label == "aicomp") {
      labels <- c(labels, "AI\nCompanion")
    }
    if(label == "app") {
      labels <- c(labels, 'App')
    }
    if(label == "brand") {
      labels <- c(labels, 'Brand\nName')
    }
    if(label == "game") {
      labels <- c(labels, 'Game')
    }
    if(label == "voice") {
      labels <- c(labels, 'Voice\nAssistant')
    }
    if(label == "pet") {
      labels <- c(labels, 'Pet')
    }
    if(label == "car") {
      labels <- c(labels, 'Car')
    }
  }

  return(labels)
}

positions <- c('aicomp', 'app', 'brand', 'game', 'voice', 'pet', 'car')
labels <- c('AI Companion', 'App', 'Brand Name', 'Game', 'Voice Assistant', 'Pet', 'Car')

# Calculate mean mourning values and sort entity types
# Reshape the data to long format
d_longer <- d_long %>%
  pivot_longer(cols = c(mourn, disap), names_to = "dv", values_to = "value")

# Calculate mean mourning values and sort entity types
mean_mourning <- d_longer %>%
  filter(dv == "mourn") %>%
  group_by(entity_type) %>%
  summarise(mean_mourn = mean(value, na.rm = TRUE)) %>%
  arrange(mean_mourn) %>%
  pull(entity_type)

d_longer$dv <- factor(d_longer$dv, levels = c("mourn", "disap"))

# Map a p-value to a significance symbol (matches the convention in the figure).
sig_symbol <- function(p) {
  if (is.na(p)) return("")
  if (p < .001) return("***")
  if (p < .01)  return("**")
  if (p < .05)  return("*")
  if (p < .10)  return("+")
  return("ns")
}

plot_dv <- function() {
  # --- Significance annotations: each entity vs. the AI companion, tested
  # separately for mourning and disappointment with the same paired t-tests
  # reported in the text above. The AI companion is the reference and gets no star.
  sig_df <- d_longer %>%
    group_by(entity_type, dv) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
              .groups = "drop")
  sig_df$label <- ""
  for (i in seq_len(nrow(sig_df))) {
    et  <- as.character(sig_df$entity_type[i])
    dvv <- as.character(sig_df$dv[i])
    if (et != "aicomp") {
      x <- d_long[d_long$entity_type == "aicomp", ][[dvv]]
      y <- d_long[d_long$entity_type == et, ][[dvv]]
      sig_df$label[i] <- sig_symbol(t.test(x, y, paired = TRUE)$p.value)
    }
  }
  # Place labels in a clean row above the data (the overlaid dots span the full
  # 0-100 range, so per-bar placement would collide with them), dodged to align
  # each label with its bar.
  entity_levels <- as.character(mean_mourning)
  sig_df$x <- match(as.character(sig_df$entity_type), entity_levels) +
              ifelse(sig_df$dv == "mourn", -0.175, 0.175)
  sig_df$y <- 104
  # "ns" is italicised; stars and "+" upright, matching the figure convention.
  sig_df$face <- ifelse(sig_df$label == "ns", "italic", "plain")

  # Distinguish the two DVs by fill PATTERN (solid vs. striped) rather than by
  # colour/shade, so the figure is unambiguous in greyscale and for colour-blind
  # readers. Bars are white with black outlines; the pattern is the encoding.
  bar_func <- geom_bar_pattern(aes(pattern = dv),
                               position = "dodge", stat = "summary", width = 0.7,
                               colour = "black", linewidth = 0.4,
                               pattern_fill = NA, pattern_colour = "black",
                               pattern_density = 0.5, pattern_spacing = 0.065,
                               pattern_size = 0.4, pattern_key_scale_factor = 0.6)
    summary_func <- stat_summary(fun.data = "mean_se", color = "black",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.7),
                 geom = "errorbar", width = 0.2)

  # Overlay individual participant ratings (dot plot) so the data distribution
  # is shown clearly, per the reviewer's request. Jittered and dodged to align
  # with the corresponding bar; semi-transparent to remain legible at this n.
  point_func <- geom_point(position = position_jitterdodge(jitter.width = 0.12,
                                                           dodge.width = 0.7,
                                                           seed = 123),
                           size = 0.5, alpha = 0.25, shape = 16,
                           color = "black", show.legend = FALSE)

  # geom_text size is in mm; 2.1 mm * 2.845 ~ 6 pt (within Nature's 5-7 pt range).
  sig_func <- geom_text(data = sig_df,
                        aes(x = x, y = y, label = label, fontface = face),
                        inherit.aes = FALSE, vjust = 0, size = 2.1, family = "Helvetica")

  # Bold the AI Companion x-axis label (the reference condition); plain elsewhere.
  # Ordered to match the axis (factor levels = mean_mourning).
  x_face <- ifelse(as.character(mean_mourning) == "aicomp", "bold", "plain")

  plt <- ggplot(d_longer, aes(x = factor(entity_type, levels = mean_mourning), y = value, fill = dv))
  plt <- plt +
    bar_func +
    point_func +
    sig_func +
    labs(x = "Entity Type", y = "Mean rating", fill = NULL) +
    theme_classic(base_size = 7, base_family = "Helvetica") +
    scale_x_discrete(labels = function(x) toLabel(x)) +
    # Uniform white fill; the pattern (set below) carries the DV distinction.
    scale_fill_manual(values = c(mourn = "white", disap = "white"), guide = "none") +
    scale_pattern_manual(values = c(mourn = "none", disap = "stripe"),
                         labels = c(mourn = "Mourning", disap = "Disappointment"),
                         name = NULL) +
    guides(pattern = guide_legend(override.aes = list(fill = "white", colour = "black"))) +
    # All text within Nature's 5-7 pt range at the 180 mm final width.
    theme(text = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.text.x = element_text(size = 6, hjust=0.5, vjust=0.6, face = x_face),
          axis.text.y = element_text(size = 6),
          legend.text = element_text(size = 7),
          legend.position="top",
          legend.title = element_blank()) +
    summary_func +
    scale_y_continuous(breaks = seq(0, 100, 25)) +
    coord_cartesian(ylim = c(0, 110)) +
    xlab("")


  return(plt)
}

plt <- plot_dv()

# Arrange all plots: 2-column width (180 mm) per Nature artwork guidelines.
dev.new(width = 180 / 25.4, height = 108 / 25.4, noRStudioGD = TRUE)

figure <- ggarrange(plt, nrow = 1, ncol = 1, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, bottom = text_grob("Entity Type", color = "black", face = "plain", size = 7, family = "Helvetica", margin(b = 2), hjust = 0.25))

# Vector PDF, 180 mm (2-column) wide, RGB (ggplot PDF default) per Nature spec.
ggsave("plt.pdf", last_plot(), width = 180, height = 108, units = "mm")

############### DISCRIMINANT VALIDITY TESTS ###############

# Construct a dataframe, consisting of both mourning and disappointment questions for all entitites
mourn_1 <- c()
mourn_2 <- c()
disap_1 <- c()
disap_2 <- c()

for (condition in CONDITIONS) {
  mourn_1 <- c(mourn_1, d[[paste0('mourn_1_', condition, '_1')]])
  mourn_2 <- c(mourn_2, d[[paste0('mourn_2_', condition, '_1')]])
  disap_1 <- c(disap_1, d[[paste0('disap_1_', condition, '_1')]])
  disap_2 <- c(disap_2, d[[paste0('disap_2_', condition, '_1')]])
}

# Construct a dataframe
df <- data.frame(mourn_1, mourn_2, disap_1, disap_2)

# Calculate the correlation matrix
cor_matrix <- cor(df)
print(cor_matrix)

print(dim(df))
print(names(df))

# Define the measurement model
simple_mm <- constructs(
  composite("mourn", c("mourn_1", "mourn_2")),
  composite("disap", c("disap_1", "disap_2"))
)

simple_sm <- relationships(
  paths(from = "mourn", to = "disap")
)

# Estimate the model
simple_model <- estimate_pls(data = df, measurement_model = simple_mm, structural_model = simple_sm)
print(simple_model$construct_scores)

# Summarize the model results
summary_simple <- summary(simple_model)

# Iterations to converge
summary_simple$iterations

# To display all contents in the summary_simple object
summary_simple

# Inspect the indicator loadings
summary_simple$loadings

# Inspect the indicator reliability
summary_simple$loadings^2

#Inspect the composite reliability
summary_simple$reliability

# Plot the reliabilities of constructs
plot(summary_simple$reliability)

# Table of the FL criterion
# FL inside the measure itself should be greater than all other FLs
summary_simple$validity$fl_criteria
