## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',
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
               'cSEM','seminr', 'psych', 'interactions', 'splithalfr', 'dplyr')

### Read the data
d <- read.csv('./data.csv')

# Remove NA rows
d <- d[d$Finished == 1, ]

################################## Exclusions ##################################

# Print IDs of failed attention check participants
print(paste0("IDS of participants failed attention check: ", paste(d[d$att_1 != 2 | d$att_2 != 2, 'prolific_id'], collapse = ", ")))


# Attention check
d <- d[d$att_1 == 2 & d$att_2 == 2,]
print(paste0("Number of participants hired: ", nrow(d)))

d <- d[d$comp_1 == "1" ,]

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
d <- d[d$subscription_rep != "1",]

dim(d)

## replika months
mean(as.numeric(d$months_rep), trim = 0, na.rm = TRUE) ## mean number of months
sd(as.numeric(d$months_rep), na.rm = TRUE) ## standard deviation

###########

# Make all dv's numeric, i.e., all columns that contain 'mhealth', 'mourn', 'disap', 'freq'
for (col in colnames(d)) {
  if (grepl('mhealth', col) | grepl('mourn', col) | grepl('disap', col) | grepl('freq', col)) {
    d[[col]] <- as.numeric(d[[col]])
  }
}

# TODO: cronbach alpha for all dv's
CONDITIONS <- c('aicomp', 'app', 'brand', 'game', 'voice', 'pet', 'car')

# We have 2 questions for each DV, i.e., mhealth, mourn, disap
# We will calculate the mean of these 2 questions for each DV and condition
for (condition in CONDITIONS) {
  for (dv in c('mhealth', 'mourn', 'disap')) {
    d[[paste0(dv, '_', condition)]] <- (d[[paste0(dv, '_1_', condition, '_1')]]
                                        + d[[paste0(dv, '_2_', condition, '_1')]]) / 2
  }
}

# Correlation table between each question
cor_matrix <- cor(d[, c('mhealth_1_aicomp_1', 'mhealth_2_aicomp_1', 'mourn_1_aicomp_1', 'mourn_2_aicomp_1', 'disap_1_aicomp_1', 'disap_2_aicomp_1')])
cor_matrix

# Get cronbach alpha between the two questions for each DV. Combine each condition for this, for each DV. For this, create a new dataframe with two cols: Q1 and
for (dv in c('mhealth', 'mourn', 'disap')) {
  Q1 <- c()
  for (q in c(paste0(dv, '_1_', CONDITIONS, '_1'))) {
    Q1 <- c(Q1, d[[q]])
  }

  Q2 <- c()
  for (q in c(paste0(dv, '_2_', CONDITIONS, '_1'))) {
    Q2 <- c(Q2, d[[q]])
  }

  q1q2 <- data.frame(Q1, Q2)

  print(paste0(dv, ": ", cronbach.alpha(q1q2)))
}

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

############### DESCRIPTIVE ANALYSIS ###############

# Print the mean values for each DV, based on condition
for (condition in CONDITIONS) {
  print(paste0("*-*-*-*-* Condition: ", condition))
  for (dv in c('disap', 'mourn', 'mhealth', 'freq')) {
    print(paste0(dv, ": ", mean(d[[paste0(dv, '_', condition)]])))
  }
}

###############  ANCOVAS ###############

# Convert d to long format, with entity type as separate column
d_long <- d %>%
  pivot_longer(cols = c('mourn_aicomp', 'mourn_app', 'mourn_brand', 'mourn_game', 'mourn_voice', 'mourn_pet', 'mourn_car',
                        'mhealth_aicomp', 'mhealth_app', 'mhealth_brand', 'mhealth_game', 'mhealth_voice', 'mhealth_pet', 'mhealth_car',
                        'disap_aicomp', 'disap_app', 'disap_brand', 'disap_game', 'disap_voice', 'disap_pet', 'disap_car',
                        'freq_aicomp', 'freq_app', 'freq_brand', 'freq_game', 'freq_voice', 'freq_pet', 'freq_car'
                        ),
               names_to = c('.value', 'entity_type'),
               names_sep = '_')

d_long$entity_type <- as.factor(d_long$entity_type)

# We will conduct two ANCOVAs to examine the effect of entity type on mourning, mental health, and disappointment, with “frequency of use” (hours per week interacting with the entity) as a covariate
for (dv in c('mourn', 'mhealth', 'disap')) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  aov_result <- aov(d_long[[dv]] ~ d_long$freq + d_long$entity_type, data = d_long)
  print(summary(aov_result))
}


# Post-hoc pairwise comparisons will be conducted using Tukey’s HSD to identify specific differences between the AI companion condition and other conditions.
for (dv in c('mourn', 'mhealth', 'disap')) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  posthoc <- TukeyHSD(aov(d_long[[dv]] ~ d_long$entity_type, data = d_long))
  print(posthoc)
}

# Now, run t-tests comparing AI companion condition to all other conditions
for (dv in c('mourn', 'mhealth', 'disap')) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  for (condition in CONDITIONS) {
    if (condition != 'aicomp') {
      print(paste0("Condition: ", condition))
      print(t.test(d_long[d_long$entity_type == 'aicomp', dv], d_long[d_long$entity_type == condition, dv]))
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
  pivot_longer(cols = c(mourn, mhealth, disap), names_to = "dv", values_to = "value")

# Calculate mean mourning values and sort entity types
mean_mourning <- d_longer %>%
  filter(dv == "mourn") %>%
  group_by(entity_type) %>%
  summarise(mean_mourn = mean(value, na.rm = TRUE)) %>%
  arrange(mean_mourn) %>%
  pull(entity_type)

plot_dv <- function() {
  bar_func <- geom_bar(position="dodge", stat="summary", width = 0.7, size = 0.75)
    summary_func <- stat_summary(fun.data = "mean_se", color = "black",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.7),
                 geom = "errorbar", width = 0.2)

  plt <- ggplot(d_longer, aes(x = factor(entity_type, levels = mean_mourning), y = value, fill = dv))
  plt <- plt +
    bar_func +
    labs(x = "Entity Type", y = "Value", fill = "DV") +
    theme_classic() +
    scale_x_discrete(labels = function(x) toLabel(x)) +
    scale_fill_grey(start = 0.8, end = 0.2) +
    theme(text = element_text(size=22),
          axis.text.x = element_text(size = 20, hjust=0.5, vjust=0.6), 
          axis.text.y = element_text(size = 20),
          legend.position="top") +
    summary_func +
    xlab("")
  
  
  return(plt)
}

plot_dv()

plt1 <- plot_dv("mourn")
plt2 <- plot_dv("mhealth")
plt2 <- plot_dv("disap")

# Arrange all plots:
dev.new(width = 10, height = 12 * 3/5, noRStudioGD = TRUE)

figure <- ggarrange(plt1, plt2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, bottom = text_grob("Change Type", color = "black", face = "plain", size = 26, margin(b = 2), hjust = 0.25))

if(dim(d)[1] == 320) {
  ggsave("./combined_plot_ai_companion.pdf", last_plot(), dpi = 300, width = 15, height = 12 * 3/5)
} else {
  ggsave("./combined_plot.pdf", last_plot(), dpi = 300, width = 10, height = 12 * 3/5,)
}














############### DISCRIMINANT VALIDITY TESTS ###############

# Correlation matrix with all questions
cor_matrix_all <- cor(d[, c('identity_stability_1_1', 'identity_stability_2_1', 'value_1_1', 'value_2_1', 'mourn_1_1', 'mourn_2_1')])
print(cor_matrix_all)

# Define the measurement model
simple_mm <- constructs(
  composite("value", c("value_1_1", "value_2_1")),
  composite("mourn", c("mourn_1_1", "mourn_2_1")),
  composite("identity_stability", c("identity_stability_1_1", "identity_stability_2_1"))
)

# Define the structural model
simple_sm <- relationships(
  paths(from = "identity_stability", to = "mourn"),  
  paths(from = "mourn", to = "value")
)

# Estimate the model
simple_model <- estimate_pls(data = d,
                             measurement_model = simple_mm,
                             structural_model = simple_sm)

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