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
               'dplyr',
               'sjstats',
               'pwr',
               'effectsize',
               'parameters',
               'ltm',
               'semTools',
               'lavaan',
               'tidyverse',
               'cSEM','seminr', 'psych', 'interactions', 'splithalfr')

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

table(d$change_type)

# Comprehension check
change_conditions <- c('control', 'impacts_social', 'not_impacts_social')

# comp_1 is numeric, i.e., index of change_condition. Change comp_1 to string format according to change_conditions vector
# Apply to each row of d$comp_1, i.e., for each participant
d <- d[((d$comp_1 == "1" & d$change_type == "control") |
        (d$comp_1 == "2" & d$change_type == "impacts_social") |
        (d$comp_1 == "3" & d$change_type == "not_impacts_social")),]

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

## replika months
mean(as.numeric(d$months_rep), trim = 0, na.rm = TRUE) ## mean number of months
sd(as.numeric(d$months_rep), na.rm = TRUE) ## standard deviation

###########

# Make all dv's numeric
dvs <- c('value_1_1', 'value_2_1', 'mourn_1_1', 'mourn_2_1', 'identity_stability_1_1', 'identity_stability_2_1', 'app_improvement_1')
for( dv in dvs ) {
  d[, dv] <- as.numeric(d[, dv])
}

# Reliability between two value DVs
cronbach.alpha(d[, c("value_1_1", "value_2_1")])
spearman_brown(d$value_1_1, d$value_2_1)

cronbach.alpha(d[, c("mourn_1_1", "mourn_2_1")])
spearman_brown(d$mourn_1_1, d$mourn_2_1)

cronbach.alpha(d[, c("identity_stability_1_1", "identity_stability_2_1")])
spearman_brown(d$identity_stability_1_1, d$identity_stability_2_1)

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

############# AI Assistant Usage #############

# What % of users are from which app?
# First, lowercase all entries in the app column
d$ai_companion_app <- tolower(d$ai_companion_app)

# Remove spaces from the app column
d$ai_companion_app <- gsub(" ", "", d$ai_companion_app)

# Now, let's search for 'replika' in the column, and see how many users are from Replika
# Do this for a vector of apps
apps <- c('replika', 'chatgpt', 'chatgbt', 'chatbot', 'simsimi', 'cleverbot', 'cai', 'characterai', 'chai', 'woebot', 'siri', 'alexa', 'waifu', 'waze', 'snapchat', 'cortana', 'zoom', 'talkie', 'pi', 
          'openai', 'googleassistant', 'gemini', 'genesia', 'copilot', 'bard', 'bing')

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

# Remove apps that are not AI companion apps, i.e., chatgpt, alexa, gemini, googleassistant, copilot, snapchat, zoom, chatgbt, cortana, siri
d <- d[(d$chatgpt == 0 & d$alexa == 0 & d$gemini == 0 & d$googleassistant == 0 & d$copilot == 0 & d$snapchat == 0 & d$zoom == 0 & d$chatgbt == 0 & d$cortana == 0 & d$siri == 0),]

print(paste0("Number of participants using AI assistant apps: ", nrow(d[d$uses_ai_assistant == 1,])))

# number of users after excluding AI assistant users
print(paste0("Number of participants after excluding AI assistant users: ", nrow(d)))

# Combine measures
d$value <- (d$value_1_1 + d$value_2_1) / 2
d$mourn <- (d$mourn_1_1 + d$mourn_2_1) / 2
d$identity_stability <- (d$identity_stability_1_1 + d$identity_stability_2_1) / 2

############### DESCRIPTIVE ANALYSIS ###############

# Print the mean values for each DV, based on change_type
d %>% group_by(change_type) %>% summarise (x = mean(identity_stability))
d %>% group_by(change_type) %>% summarise (x = mean(value))
d %>% group_by(change_type) %>% summarise (x = mean(mourn))
d %>% group_by(change_type) %>% summarise (x = mean(app_improvement_1))

############### T-TESTS ###############

# the app was perceived as improving in both conditions 
x <- d[d$change_type == 'impacts_social', 'app_improvement_1']
t_test_result <- t.test(x, mu=50)
print(t_test_result)
cohens_d(x, 50)

x <- d[d$change_type == 'not_impacts_social', 'app_improvement_1']
t_test_result <- t.test(x, mu=50)
print(t_test_result)
cohens_d(x, 50)

# ANOVA
aov_result <- aov(mourn ~ change_type, data = d)
summary(aov_result)
anova_stats(aov_result)

aov_result <- aov(identity_stability ~ change_type, data = d)
summary(aov_result)
anova_stats(aov_result)

# We will run t-tests comparing identity discontinuity and mourning between the control condition and the two change type conditions separately.
for (dv in c('identity_stability', 'mourn', 'app_improvement_1')) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  print(paste0("Control vs. Impacts Social"))
  x <- d[d$change_type == 'control', dv]
  y <- d[d$change_type == 'impacts_social', dv]
  vart <- var.test(x, y)
  t_test_result <- t.test(x, y, var.equal = vart$p.value > 0.05)
  print(t_test_result)
  print(effectsize::cohens_d(x, y))
  
  print(paste0("Control vs. Not Impacts Social"))
  x <- d[d$change_type == 'control', dv]
  y <- d[d$change_type == 'not_impacts_social', dv]
  vart <- var.test(x, y)
  t_test_result <- t.test(x, y, var.equal = vart$p.value > 0.05)
  print(t_test_result)
  print(effectsize::cohens_d(x, y))

  print(paste0("Impacts Social vs. Not Impacts Social"))
  x <- d[d$change_type == 'impacts_social', dv]
  y <- d[d$change_type == 'not_impacts_social', dv]
  vart <- var.test(x, y)
  t_test_result <- t.test(x, y, var.equal = vart$p.value > 0.05)
  print(t_test_result)
  print(effectsize::cohens_d(x, y))
}

# Compare app_improvement in all conditions to the midpoint of the scale
print(paste0("Control vs. Midpoint"))
t_test_result <- t.test(d[d$change_type == 'control', 'app_improvement_1'], mu=50)
print(t_test_result)

print(paste0("Impacts Social vs. Midpoint"))
t_test_result <- t.test(d[d$change_type == 'impacts_social', 'app_improvement_1'], mu=50)
print(t_test_result)

print(paste0("Not Impacts Social vs. Midpoint"))
t_test_result <- t.test(d[d$change_type == 'not_impacts_social', 'app_improvement_1'], mu=50)
print(t_test_result)


#### Plotting ####
toLabel <- function(x) {
  labels <- c()
  for(label in x) {
    if(label == "control") {
      labels <- c(labels, "Control")
    }
    if(label == "impacts_social") {
      labels <- c(labels, 'Social\nImpact')
    }
    if(label == "not_impacts_social") {
      labels <- c(labels, 'No Social\nImpact')
    }
  }

  return(labels)
}

positions <- c('control', 'impacts_social', 'not_impacts_social')
labels <- c('Control', 'Social\nImpact', 'No Social\nImpact')
plot_dv <- function(dv) {

  if(dv == "identity_stability") {
    bar_func <- geom_bar(position="dodge", stat="summary", width = 0.7, size = 0.75)
    summary_func <- stat_summary(fun.data = "mean_se", color = "black",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.7),
                 geom = "errorbar", width = 0.2)
  } else {
    bar_func <- geom_bar(position="dodge", stat="summary", width = 0.7, size = 0.75)
    summary_func <- stat_summary(fun.data = "mean_se", color = "black",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.7),
                 geom = "errorbar", width = 0.2)
  }

  bar_func <- geom_bar(position="dodge", stat="summary", width = 0.7, size = 0.75)
    summary_func <- stat_summary(fun.data = "mean_se", color = "black",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.7),
                 geom = "errorbar", width = 0.2)

  plt <- ggplot(d, aes(x = change_type, y = !!rlang::sym(dv)))
  plt <- plt +
    bar_func +
    labs(x = "Change Type Condition", y = dv) +
    theme(legend.position = "none") +
    theme_classic() +
    scale_x_discrete(limits = positions, labels = function(x) toLabel(x)) +
    theme(text = element_text(size=22),
          axis.text.x = element_text(size = 20, hjust=0.5, vjust=0.6), 
          axis.text.y = element_text(size = 20),
          legend.position="top") +
    summary_func +
    xlab("") +
    coord_cartesian(ylim = c(0, 80))
  
  if(dv == "identity_stability") {
    plt <- plt + ylab("Identity Discontinuity")
  }

  if(dv == "value") {
    plt <- plt + ylab("Devaluation")
  }
  
  if(dv == "mourn") {
    plt <- plt + ylab("Mourning")
  }
  
  return(plt)
}

plt1 <- plot_dv("identity_stability")
plt2 <- plot_dv("mourn")
plt3 <- plot_dv("value")

# Arrange all plots:
dev.new(width = 10, height = 12 * 3/5, noRStudioGD = TRUE)

figure <- ggarrange(plt1, plt2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, bottom = text_grob("Change Type", color = "black", face = "plain", size = 26, margin(b = 2), hjust = 0.25))

if(dim(d)[1] == 320) {
  ggsave("./combined_plot_ai_companion.pdf", last_plot(), dpi = 300, width = 15, height = 12 * 3/5)
} else {
  ggsave("./combined_plot.pdf", last_plot(), dpi = 300, width = 10, height = 12 * 3/5,)
}

#### Mediation Analysis ####

source("process.R")
d_mediation <- d

d_mediation$change_type_num <- ifelse(d_mediation$change_type == 'control', 1, ifelse(d_mediation$change_type == 'impacts_social', 2, 3))

process(data = d_mediation, y = "mourn", x = "change_type_num",
        m = c("identity_stability"), model = 4, mcx = 1, effsize = 1, total = 1, stand = 1, contrast = 1,
        boot = 10000, modelbt = 1, seed = 654321)

process(data = d_mediation, y = "value", x = "change_type_num",
      m = c("identity_stability"), model = 4, mcx = 1, effsize = 1, total = 1, stand = 1, contrast = 1,
      boot = 10000, modelbt = 1, seed = 654321)
