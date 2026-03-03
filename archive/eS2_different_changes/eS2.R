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
               'cSEM','seminr', 'splithalfr')


# Power analysis in N. Strohminget, S. Nichols
# Converting t-values to Cohen's d: https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/td
t_value_apathy <- 3.16
cohen_d_apathy <- 2 * t_value_apathy / sqrt(50)
cohen_d_apathy

# Perform power analysis for apathy group
power_analysis_apathy <- pwr.t.test(d = cohen_d_apathy, power = 0.9, sig.level = 0.05)
power_analysis_apathy

### Read the data
d <- read.csv('./data.csv', sep = ",")

# Remove NA rows
d <- d[d$Finished == 1, ]

################################## Exclusions ##################################

# Attention check
d <- d[d$att_1 == 2 & d$att_2 == 2,]
print(paste0("Number of participants hired: ", nrow(d)))

table(d$change_type)

# Comprehension check
change_conditions <- c('control', 'only_visual', 'erp', 'closeness')

# comp_1 is numeric, i.e., index of change_condition. Change comp_1 to string format according to change_conditions vector
# Apply to each row of d$comp_1, i.e., for each participant
d$comp_1 <- change_conditions[as.numeric(d$comp_1)]

# Exclude participants who failed the comprehension check, i.e., comp_1 should be equal to change_type column
d <- d[(d$comp_1 == d$change_type) | (d$comp_1 == 'closeness' & d$change_type == 'closeness_revert'), ]

# Exclude those who answer yes to the second comprehension check question if their change_type isn't closeness_revert
d <- d[!(d$comp_2 == 1 & d$change_type != 'closeness_revert'), ]

print(paste0("Number of participants after comprehension check: ", nrow(d)))

t <- table(d$app_coding)

dim(d)

d$used_ai_companion <- d$app_coding != 'alexa' & d$app_coding != 'amazon' & d$app_coding != 'bard' & d$app_coding != 'bing' & d$app_coding != 'chatgpt' & d$app_coding != 'copilot' & d$app_coding != 'cortana' & d$app_coding != 'siri' & d$app_coding != 'snapchat' & d$app_coding != 'bind,chatgpt,bard' & d$app_coding != 'chatgpt,alexa' & d$app_coding != 'chatgpt,bard' & d$app_coding != 'chatgpt,bard,copilot' & d$app_coding != 'siri,alexa' & d$app_coding != 'snapchat,chatgpt'

# Make all dv's numeric in 'd': value_1_1	value_2_1	mourn_1_1	mourn_2_1	identity_stability_1_1	identity_stability_2_1	abandonment_1	reassurance_need_1	closeness_scare_1	parasocial_1_1	parasocial_2_1
dvs <- c('value_1_1', 'value_2_1', 'mourn_1_1', 'mourn_2_1', 'identity_stability_1_1', 'identity_stability_2_1', 'abandonment_1', 'reassurance_need_1', 'closeness_scare_1', 'parasocial_1_1', 'parasocial_2_1')
for( dv in dvs ) {
  d[, dv] <- as.numeric(d[, dv])
}

# Reverse code abandonement_1
d$abandonment_1 <- 100 - d$abandonment_1

# Combine measures
d$value <- (d$value_1_1 + d$value_2_1) / 2
d$mourn <- (d$mourn_1_1 + d$mourn_2_1) / 2
d$anxious_attachment <- (d$abandonment_1 + d$reassurance_need_1 + d$closeness_scare_1) / 3
d$parasocial_interaction <- (d$parasocial_1_1 + d$parasocial_2_1) / 2
d$identity_stability <- (d$identity_stability_1_1 + d$identity_stability_2_1) / 2

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


# Reliability between two value DVs
cronbach.alpha(d[, c("value_1_1", "value_2_1")])
spearman_brown(d$value_1_1, d$value_2_1)

cronbach.alpha(d[, c("mourn_1_1", "mourn_2_1")])
spearman_brown(d$mourn_1_1, d$mourn_2_1)

cronbach.alpha(d[, c("identity_stability_1_1", "identity_stability_2_1")])
spearman_brown(d$identity_stability_1_1, d$identity_stability_2_1)

cronbach.alpha(d[, c("parasocial_1_1", "parasocial_2_1")])
spearman_brown(d$parasocial_1_1, d$parasocial_2_1)
cor(d$parasocial_1_1, d$parasocial_2_1)

cronbach.alpha(d[, c('abandonment_1', 'reassurance_need_1', 'closeness_scare_1')])


############### DISCRIMINANT VALIDITY TESTS ###############

# Calculate cronbach's alpha between constructs
cronbach.alpha(d[, c('identity_stability_1_1', 'identity_stability_2_1')])
cronbach.alpha(d[, c('value_1_1', 'value_2_1')])
cronbach.alpha(d[, c('mourn_1_1', 'mourn_2_1')])

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
  paths(from = "identity_stability", to = "value"),
  paths(from = "identity_stability", to = "mourn")
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

# FL inside the measure itself should be greater than all other FLs
summary_simple$validity$fl_criteria

###########################################################################################

# Remove apps that are not AI companion apps, i.e., 'alexa', 'amazon', 'bard', 'bing', 'chatgpt', 'copilot', 'cortana', 'siri', 'snapchat', 'bind,chatgpt,bard', 'chatgpt,alexa', 'chatgpt,bard', 'chatgpt,bard,copilot', 'siri,alexa', 'snapchat,chatgpt'
d <- d[d$app_coding != 'alexa' & d$app_coding != 'amazon' & d$app_coding != 'bard' & d$app_coding != 'bing' & d$app_coding != 'chatgpt' & d$app_coding != 'copilot' & d$app_coding != 'cortana' & d$app_coding != 'siri' & d$app_coding != 'snapchat' & d$app_coding != 'bind,chatgpt,bard' & d$app_coding != 'chatgpt,alexa' & d$app_coding != 'chatgpt,bard' & d$app_coding != 'chatgpt,bard,copilot' & d$app_coding != 'siri,alexa' & d$app_coding != 'snapchat,chatgpt', ]
dim(d)

print(paste0("Number of participants after excluding non-AI companion users: ", nrow(d)))
134 / 403

############### DESCRIPTIVE ANALYSIS ###############

# Print the mean values for each DV, based on change_type
d %>% group_by(change_type) %>% summarise (x = mean(identity_stability))
d %>% group_by(change_type) %>% summarise (x = mean(value))
d %>% group_by(change_type) %>% summarise (x = mean(mourn))
d %>% group_by(change_type) %>% summarise (x = mean(anxious_attachment))
d %>% group_by(change_type) %>% summarise (x = mean(parasocial_interaction))

############### ANOVA TESTS ###############

dvs <- c('value', 'mourn')
d$change_type_factor <- factor(d$change_type)
for( dv in dvs ) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  formula_string <- paste(dv, "~ identity_stability") 
  
  aov_mod <- aov(as.formula(formula_string), data = d)
  print(summary(aov_mod))
  print(anova_stats(aov_mod))
}

# Print mean devaluation and mourning for each change type
for (dv in dvs) {
  print(paste0("******** ", dv, " ********"))
  print(tapply(d[, dv], d$change_type, mean))
}

############### T-TESTS ###############

change_types <- c('control', 'only_visual', 'erp', 'closeness', 'closeness_revert')

# Compare control to each change type
for (i in 2:5) {
  print(paste0("******** Comparing ", change_types[1], " vs. ", change_types[i], " ********"))
  
  ct1 <- d[d$change_type == change_types[1], 'identity_stability']
  ct2 <- d[d$change_type == change_types[i], 'identity_stability']
  
  # Do the t-test
  vart <- var.test(ct1, ct2)
  t_test_result <- t.test(ct1, ct2, var.equal = vart$p.value > 0.05)
  print(t_test_result)
  print(t_test_result$p.value) * 7
  print(effectsize::cohens_d(ct1, ct2))
}

for (i in 1:4) {
  print(paste0("******** Comparing Identity Loss in ", change_types[i], " vs. ", change_types[i+1], "********"))
  
  ct1 <- d[d$change_type == change_types[i], 'identity_stability']
  ct2 <- d[d$change_type == change_types[i+1], 'identity_stability']

  # Do the t-test
  t_test_result <- t.test(ct1, ct2)
  print(t_test_result)
  print(t_test_result$p.value * 7)
  print(effectsize::cohens_d(ct1, ct2))
}

# All possible comparisons:
p_values <- c()
p_labels <- c()
for (i in 1:5) {
  for (j in 1:5) {
    if(i != j) {
      print(paste0("******** Comparing ", change_types[i], " vs. ", change_types[j], " ********"))
      p_labels <- c(p_labels, paste0(change_types[i], " vs. ", change_types[j]))
      
      ct1 <- d[d$change_type == change_types[i], 'identity_stability']
      ct2 <- d[d$change_type == change_types[j], 'identity_stability']
      
      # Do the t-test
      t_test_result <- t.test(ct1, ct2)
      print(paste0("Adjusted p value: ", t_test_result$p.value * 10))

      p_values <- c(p_values, t_test_result$p.value)

      print(t_test_result)
      print(effectsize::cohens_d(ct1, ct2))
    }
  }
}

# Adjust p-values using Bonferroni correction
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

# Print adjusted p-values
print(p_values_adjusted)
print(p_labels)


# Check AI type as potential moderator
for( dv in dvs ) {
  print(paste0("*-*-*-*-*  ", dv, "  *-*-*-*-*"))
  formula_string <- paste(dv, "~ change_type_factor * used_ai_companion") 
  
  aov_mod <- aov(as.formula(formula_string), data = d)
  print(summary(aov_mod))
  print(anova_stats(aov_mod))
}


# Plot the moderation effect
plt <- ggplot(d, aes(x = change_type, y = mourn, fill = used_ai_companion)) +
  geom_boxplot() +
  labs(x = "Change Type", y = "Mourning") +
  theme_classic() +
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle = 30, size = 16, hjust=0.5, vjust=0.6), 
        axis.text.y = element_text(size = 16),
        legend.position="top") +
  scale_fill_manual(values = c("grey", "black")) +
  scale_x_discrete(labels = function(x) toLabel(x))
  
#### Plotting ####
toLabel <- function(x) {
  labels <- c()
  for(label in x) {
    if(label == "control") {
      labels <- c(labels, "Control")
    }
    if(label == "only_visual") {
      labels <- c(labels, "Only Visual")
    }
    if(label == "erp") {
      labels <- c(labels, "ERP")
    }
    if(label == "closeness_revert") {
      labels <- c(labels, "Revertible\nColdness")
    }
    if(label == "closeness") {
      labels <- c(labels, "Coldness")
    }
  }

  return(labels)
}

#### Bar Plot For Identity Loss ####
positions <- c('control', 'only_visual', 'erp', 'closeness', 'closeness_revert')
labels <- c('Control', 'Only Visual', 'ERP', 'Coldness', 'Revertible\nColdness')
plot_dv <- function(dv) {
  plt <- ggplot(d, aes(x = change_type, y = !!rlang::sym(dv)))
  plt <- plt +
    geom_bar(position="dodge", stat="summary", width = 0.7, alpha = 0.38, size = 0.75) +
    labs(x = "Change Type Condition", y = dv) +
    theme(legend.position = "none") +
    theme_classic() +
    scale_x_discrete(limits = positions, labels = function(x) toLabel(x)) +
    theme(text = element_text(size=18),
          axis.text.x = element_text(angle = 30, size = 16, hjust=0.5, vjust=0.6), 
          axis.text.y = element_text(size = 16),
          legend.position="top") +
    stat_summary(fun.data = "mean_se", color = "black",
                 fun.args = list(mult = 1),
                 position = position_dodge(width = 0.9),
                 geom = "errorbar", width = 0.2) +
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
plt2 <- plot_dv("value")
plt3 <- plot_dv("mourn")

# Arrange all plots:
dev.new(width = 30 * 3/5, height = 12 * 3/5, noRStudioGD = TRUE)

figure <- ggarrange(plt1, plt2, plt3, nrow = 1, ncol = 3, common.legend = TRUE, legend = "top", vjust = 1.0, hjust = 0.5)
annotate_figure(figure, bottom = text_grob("Change Type", color = "black", face = "plain", size = 26, margin(b = 2), hjust = 0.25))

ggsave("./combined_plot.pdf", last_plot(), dpi = 300, width = 30 * 3/5, height = 12 * 3/5)

#### Mediation Analysis ####

source("process.R")
d_mediation <- d[d$change_type %in% c('control', 'erp', 'closeness'), ]

# If change_type is control, set it to 1. If it's ERP set it to 2. If it's closeness, set it to 3
d_mediation$change_type_num <- ifelse(d_mediation$change_type == 'control', 1, ifelse(d_mediation$change_type == 'erp', 2, 3))

# 1 if true, 0 if false
d_mediation$used_ai_companion <- ifelse(d_mediation$used_ai_companion, 1, 0)

# Run a serial mediation model with change_type -> identity_stability -> mourning -> devaluation
process(data = d_mediation, y = "mourn", x = "change_type_num",
      m = c("identity_stability"), model = 4, effsize = 1, total = 1, stand = 1, mcx = 1,
      contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

process(data = d_mediation, y = "value", x = "change_type_num",
        m = c("identity_stability"), model = 4, effsize = 1, total = 1, stand = 1, mcx = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)


process(data = d_mediation, y = "value_1_1", x = "change_type_num",
        m = c("identity_stability"), model = 4, effsize = 1, total = 1, stand = 1, mcx = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

process(data = d_mediation, y = "value_2_1", x = "change_type_num",
        m = c("identity_stability"), model = 4, effsize = 1, total = 1, stand = 1, mcx = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)


######## EXPLORATORY ANALYSES ########

process(data = d_mediation, y = "value", x = "change_type_num",
    m = c("identity_stability"), w = "parasocial_interaction", model = 14, effsize = 1, total = 1, stand = 1, mcx = 1,
    contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

process(data = d_mediation, y = "mourn", x = "change_type_num",
        m = c("identity_stability"), w = "parasocial_interaction", model = 14, effsize = 1, total = 1, stand = 1, mcx = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)



process(data = d_mediation, y = "value", x = "change_type_num",
        m = c("identity_stability"), w = "anxious_attachment", model = 14, effsize = 1, total = 1, stand = 1, mcx = 1,
        contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

process(data = d_mediation, y = "mourn", x = "change_type_num",
    m = c("identity_stability"), w = "anxious_attachment", model = 14, effsize = 1, total = 1, stand = 1, mcx = 1,
    contrast = 1, boot = 10000, modelbt = 1, seed = 654321)

