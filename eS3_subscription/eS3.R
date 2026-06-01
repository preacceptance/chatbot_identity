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
               'cSEM','seminr', 'psych', 'splithalfr')

### Read the data
d <- read.csv('./data.csv')

# Remove NA rows
d <- d[d$Finished == 1, ]

################################## Exclusions ##################################

# Attention check
d <- d[d$att_1 == 2 & d$att_2 == 2,]
print(paste0("Number of participants hired: ", nrow(d)))

table(d$change_type, d$subscription_status)

# Comprehension check
change_conditions <- c('control', 'only_visual', 'erp', 'closeness')

# comp_1 is numeric, i.e., index of change_condition. Change comp_1 to string format according to change_conditions vector
# Apply to each row of d$comp_1, i.e., for each participant
d <- d[((d$comp_1 == "1" & d$change_type == "control") & (d$comp_2 == "2" & d$subscription_status == "no_subscription")) |
        ((d$comp_1 == "1" & d$change_type == "control") & (d$comp_2 == "4" & d$subscription_status == "lifetime")) |
        ((d$comp_1 == "4" & d$change_type == "coldness") & (d$comp_2 == "2" & d$subscription_status == "no_subscription")) |
        ((d$comp_1 == "4" & d$change_type == "coldness") & (d$comp_2 == "4" & d$subscription_status == "lifetime")),]

print(paste0("Number of participants after comprehension check: ", nrow(d)))

print(paste0("Number of participants using AI assistant apps: ", nrow(d[d$uses_ai_assistant == 1,])))

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

# Make all dv's numeric in 'd': value_1_1	value_2_1	mourn_1_1	mourn_2_1	identity_stability_1_1	identity_stability_2_1	abandonment_1	reassurance_need_1	closeness_scare_1	parasocial_1_1	parasocial_2_1
dvs <- c('value_1_1', 'value_2_1', 'mourn_1_1', 'mourn_2_1', 'identity_stability_1_1', 'identity_stability_2_1', 'investment_1_1', 'investment_2_1')
for( dv in dvs ) {
  d[, dv] <- as.numeric(d[, dv])
}

# Reliability between two value DVs
cronbach.alpha(d[, c("value_1_1", "value_2_1")])

cronbach.alpha(d[, c("mourn_1_1", "mourn_2_1")])
spearman_brown(d$mourn_1_1, d$mourn_2_1)

cronbach.alpha(d[, c("identity_stability_1_1", "identity_stability_2_1")])
spearman_brown(d$identity_stability_1_1, d$identity_stability_2_1)

cronbach.alpha(d[, c("investment_1_1", "investment_2_1")])
spearman_brown(d$investment_1_1, d$investment_2_1)

############### DISCRIMINANT VALIDITY TESTS ###############

# Correlation matrix with all questions
cor_matrix_all <- cor(d[, c('identity_stability_1_1', 'identity_stability_2_1', 'value_1_1', 'value_2_1', 'mourn_1_1', 'mourn_2_1')])
print(cor_matrix_all)

d$change_type_num <- ifelse(d$change_type == 'control', 1, 2)

# Define the measurement model
simple_mm <- constructs(
  composite("value", c("value_1_1", "value_2_1")),
  composite("mourn", c("mourn_1_1", "mourn_2_1")),
  composite("identity_stability", c("identity_stability_1_1", "identity_stability_2_1")),
  composite("investment", c("investment_1_1", "investment_2_1"))
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

print(paste0("Number of participants using AI assistant apps: ", nrow(d[d$uses_ai_assistant == 1,])))

# % of AI assistant users
print(paste0("Percentage of AI assistant users: ", nrow(d[d$uses_ai_assistant == 1,]) / nrow(d) * 100))

# Remove apps that are not AI companion apps
# COMMENT FOR RESULTS WITHOUT EXCLUSIONS
d <- d[d$uses_ai_assistant == 0,]
dim(d)

# number of users after excluding AI assistant users
print(paste0("Number of participants after excluding AI assistant users: ", nrow(d)))


# Combine measures
d$value <- (d$value_1_1 + d$value_2_1) / 2
d$mourn <- (d$mourn_1_1 + d$mourn_2_1) / 2
d$investment <- (d$investment_1_1 + d$investment_2_1) / 2
d$identity_stability <- (d$identity_stability_1_1 + d$identity_stability_2_1) / 2

# REPLICATING WITH ONLY THE FIRST IDENTITY STABILITY QUESTION
if(FALSE) {
  d$identity_stability <- d$identity_stability_1_1
}

# REPLICATING WITH ONLY THE SECOND IDENTITY STABILITY QUESTION
if(FALSE) {
  d$identity_stability <- d$identity_stability_2_1
}

############### DESCRIPTIVE ANALYSIS ###############

# Print the mean values for each DV, based on change_type
d %>% group_by(subscription_status, change_type) %>% summarise (x = mean(identity_stability))
d %>% group_by(subscription_status, change_type) %>% summarise (x = mean(value))
d %>% group_by(subscription_status, change_type) %>% summarise (x = mean(mourn))
d %>% group_by(subscription_status, change_type) %>% summarise (x = mean(investment))

mean(d[d$change_type == 'coldness', 'value'])
mean(d[d$change_type == 'control', 'value'])

mean(d[d$subscription_status == 'no_subscription', 'value'])
mean(d[d$subscription_status == 'lifetime', 'value'])

############### ANOVA TESTS ###############


# Manipulation check, i.e., whether subscription status affects investment
x <- d[d$subscription_status == 'no_subscription', 'investment']
y <- d[d$subscription_status == 'lifetime', 'investment']
vart <- var.test(x, y)
t_test_result <- t.test(x, y, var.equal = vart$p.value > 0.05)
print(t_test_result)
print(effectsize::cohens_d(x, y))


# Manipulation check, i.e., whether subscription status affects investment
x <- d[d$change_type == 'control', 'investment']
y <- d[d$change_type == 'coldness', 'investment']
vart <- var.test(x, y)
t_test_result <- t.test(x, y, var.equal = vart$p.value > 0.05)
print(t_test_result)
print(effectsize::cohens_d(x, y))

# Manip check with ANOVA
aov_mod <- aov(investment ~ subscription_status * change_type, data = d)
print(summary(aov_mod))
print(anova_stats(aov_mod))


dvs <- c('mourn')
for( dv in dvs ) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  formula_string <- paste(dv, "~ change_type * subscription_status + investment")
  
  aov_mod <- aov(as.formula(formula_string), data = d)
  print(summary(aov_mod))
  print(anova_stats(aov_mod))
}


dvs <- c('mourn')
for( dv in dvs ) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  formula_string <- paste(dv, "~ identity_stability * investment") # identity_stability * investment in appendix
  
  lm_mod <- lm(as.formula(formula_string), data = d)
  print(summary(lm_mod))
  print(anova_stats(lm_mod))
}


d$months_rep <- as.numeric(d$months_rep)

dvs <- c('mourn')
for( dv in dvs ) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  formula_string <- paste(dv, "~ identity_stability * months_rep") # identity_stability * investment in appendix
  
  lm_mod <- lm(as.formula(formula_string), data = d)
  print(summary(lm_mod))
  print(anova_stats(lm_mod))
}

# Also check identity_stability and months_rep

# T-tests comparing no subscription vs. lifetime, for each of control and coldness
for (dv in c('mourn')) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  print(paste0("No Subscription vs. Lifetime for Control"))
  x <- d[d$change_type == 'control' & d$subscription_status == 'no_subscription', dv]
  y <- d[d$change_type == 'control' & d$subscription_status == 'lifetime', dv]
  vart <- var.test(x, y)
  t_test_result <- t.test(x, y, var.equal = vart$p.value > 0.05)
  print(t_test_result)
  print(effectsize::cohens_d(x, y))
  
  print(paste0("No Subscription vs. Lifetime for Coldness"))
  x <- d[d$change_type == 'coldness' & d$subscription_status == 'no_subscription', dv]
  y <- d[d$change_type == 'coldness' & d$subscription_status == 'lifetime', dv]
  vart <- var.test(x, y)
  t_test_result <- t.test(x, y, var.equal = vart$p.value > 0.05)
  print(t_test_result)
  print(effectsize::cohens_d(x, y))
}

#### Plotting ####
toLabel <- function(x) {
  labels <- c()
  for(label in x) {
    if(label == "control") {
      labels <- c(labels, "Control")
    }
    if(label == "coldness") {
      labels <- c(labels, "Coldness")
    }
  }

  return(labels)
}

d$subscription_status <- factor(d$subscription_status, levels = c("no_subscription", "lifetime"))

###### APPENDIX
if(FALSE) {
  # Edit d$subscription status such that if investment is < 50, set it to "Low Investment (< 50)", else, set it to "High Investment (>= 50)"
  d$invest_binary <- ifelse(d$investment < 50, "Low Investment (< 50)", "High Investment (>= 50)")
  d$invest_binary <- factor(d$invest_binary, levels = c("Low Investment (< 50)", "High Investment (>= 50)"))


  for (dv in c('mourn')) {
    print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
    print(paste0("Control --- No Investment vs. High Investment "))
    x <- d[d$change_type == 'control' & d$invest_binary == 'Low Investment (< 50)', dv]
    y <- d[d$change_type == 'control' & d$invest_binary == 'High Investment (>= 50)', dv]
    vart <- var.test(x, y)
    t_test_result <- t.test(x, y, var.equal = vart$p.value > 0.05)
    print(t_test_result)
    print(effectsize::cohens_d(x, y))
    
    print(paste0("Coldness --- No Investment vs. High Investment "))
    x <- d[d$change_type == 'coldness' & d$invest_binary == 'Low Investment (< 50)', dv]
    y <- d[d$change_type == 'coldness' & d$invest_binary == 'High Investment (>= 50)', dv]
    vart <- var.test(x, y)
    t_test_result <- t.test(x, y, var.equal = vart$p.value > 0.05)
    print(t_test_result)
    print(effectsize::cohens_d(x, y))
  }
}


# Uncomment for results in appendix
#d$subscription_status <- d$invest_binary

#### Bar Plot For Devaluation and Mourning, Both for Free Subscription/Lifetime Subscription and Control/Coldness ####
positions <- c('control', 'coldness')
labels <- c('Control', 'Coldness')
plot_dv <- function(dv) {

  if(dv == "identity_stability") {
    bar_func <- geom_bar(position="dodge", stat="summary", width = 0.7, size = 0.75)
    summary_func <- stat_summary(fun.data = "mean_se", color = "black",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.7),
                 geom = "errorbar", width = 0.2)
  } else {
    bar_func <- geom_bar(aes(fill = subscription_status), position="dodge", stat="summary", width = 0.7, size = 0.75)
    summary_func <- stat_summary(fun.data = "mean_se", color = "black",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.7),
                 geom = "errorbar", width = 0.2,
                 aes(group=subscription_status))
  }

  bar_func <- geom_bar(aes(fill = subscription_status), position="dodge", stat="summary", width = 0.7, size = 0.75)
    summary_func <- stat_summary(fun.data = "mean_se", color = "black",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.7),
                 geom = "errorbar", width = 0.2,
                 aes(group=subscription_status))

  plt <- ggplot(d, aes(x = change_type, y = !!rlang::sym(dv)))
  plt <- plt +
    bar_func +
    scale_fill_manual("subscription_status", values = c("no_subscription" = "grey", "lifetime" = "#4a4a4a")) + # For Investment: c("Low Investment (< 50)" = "grey", "High Investment (>= 50)" = "#4a4a4a")
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

  if(dv == "investment") {
    plt <- plt + ylab("Investment")
  }
  
  return(plt)
}

plt1 <- plot_dv("identity_stability")
plt2 <- plot_dv("mourn")

# Arrange all plots:
dev.new(width = 15, height = 12 * 3/5, noRStudioGD = TRUE)

figure <- ggarrange(plt1, plt2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, bottom = text_grob("Change Type", color = "black", face = "plain", size = 26, margin(b = 2), hjust = 0.25))

if(dim(d)[1] == 340) {
  ggsave("./combined_plot_ai_companion.pdf", last_plot(), dpi = 300, width = 15 * 3/5, height = 12 * 3/5)
} else {
  ggsave("./combined_plot.pdf", last_plot(), dpi = 300, width = 15 * 3/5, height = 12 * 3/5)
}

#### Mediation Analysis ####

source("process.R")
d_mediation <- d

# If change_type is control, set it to 1. If it's ERP set it to 2. If it's closeness, set it to 3
d_mediation$change_type_num <- ifelse(d_mediation$change_type == 'control', 1, 2)
d_mediation$subscription_status <- ifelse(d_mediation$subscription_status == 'no_subscription', 1, 2)

# Run a serial mediation model with change_type -> identity_stability -> mourning -> devaluation
process(data = d_mediation, y = "mourn", x = "change_type_num",
      m = c("identity_stability"), model = 4, effsize = 1, total = 1, stand = 1, contrast = 1,
      boot = 10000, modelbt = 1, seed = 654321)


process(data = d_mediation, y = "value", x = "change_type_num",
      m = c("identity_stability"), model = 4, effsize = 1, total = 1, stand = 1, contrast = 1,
      boot = 10000, modelbt = 1, seed = 654321)

# On 'identity_stability' -> 'mourning
# w = "subscription_status" in manuscript
process(data = d_mediation, y = "mourn", x = "change_type_num",
    m = c("identity_stability"), w = "subscription_status", model = 14, effsize = 1, total = 1, stand = 1,
    boot = 10000, modelbt = 1, seed = 654321)

process(data = d_mediation, y = "value", x = "change_type_num",
    m = c("identity_stability"), w = "subscription_status", model = 14, effsize = 1, total = 1, stand = 1,
    boot = 10000, modelbt = 1, seed = 654321)

# w = "investment" in appendix
process(data = d_mediation, y = "mourn", x = "change_type_num",
    m = c("identity_stability"), w = "investment", model = 14, effsize = 1, total = 1, stand = 1,
    boot = 10000, modelbt = 1, seed = 654321)

process(data = d_mediation, y = "value", x = "change_type_num",
    m = c("identity_stability"), w = "investment", model = 14, effsize = 1, total = 1, stand = 1,
    boot = 10000, modelbt = 1, seed = 654321)

