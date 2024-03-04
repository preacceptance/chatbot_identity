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
               'cSEM')

install.packages("remotes")
remotes::install_github("LukasWallrich/rNuggets")


# Power analysis in N. Strohminget, S. Nichols
# Converting t-values to Cohen's d: https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/td
t_value_apathy <- 3.16
cohen_d_apathy <- 2 * t_value_apathy / sqrt(50)
cohen_d_apathy

# Perform power analysis for apathy group
power_analysis_apathy <- pwr.t.test(d = cohen_d_apathy, power = 0.9, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
power_analysis_apathy

source("process.R")

##### PILOT #####
d_pilot <- read.csv('./pilot.csv', sep = ",")
# Remove duplicate IDs
d_pilot <- d_pilot[!duplicated(d_pilot$prolific_id), ]
sum(d_pilot$Q10 == 'yes')

### Read the data
d <- read.csv('./data.csv', sep = ",")

# Remove NA rows
d <- d[d$Finished == 1, ]

################################## Exclusions ##################################

print(paste0("Number of participants before attention check: ", nrow(d)))

# Attention check
d <- d[d$att_1 == 2 & d$att_2 == 2,]
print(paste0("Number of participants after attention check: ", nrow(d)))

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

# Read all names in the app_coding column and take the first word before the first comma
d$app_coding <- sapply(strsplit(as.character(d$app_coding), ","), function(x) x[1])

d[d$app_coding == 'other', 'ai_companion_app']

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

# Reliability between two value DVs
cronbach.alpha(d[, c("value_1_1", "value_2_1")])
rNuggets::spearman_brown(d, items=c("value_1_1", "value_2_1"))

cronbach.alpha(d[, c("mourn_1_1", "mourn_2_1")])
rNuggets::spearman_brown(d, items=c("mourn_1_1", "mourn_2_1"))

cronbach.alpha(d[, c("identity_stability_1_1", "identity_stability_2_1")])
rNuggets::spearman_brown(d, items=c("identity_stability_1_1", "identity_stability_2_1"))

cor(d$parasocial_1_1, d$parasocial_2_1)
rNuggets::spearman_brown(d, items=c("parasocial_1_1", "parasocial_2_1"))

cronbach.alpha(d[, c('abandonment_1', 'reassurance_need_1', 'closeness_scare_1')])


## user age
mean(as.numeric(d$age), trim = 0, na.rm = TRUE) ## mean age 
sd(d$age, na.rm = TRUE) ## standard deviation 

## user gender
table(d$gender)
table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
table(d$gender)[2]/sum(table(d$gender)) ## percentage of females
table(d$gender)[3]/sum(table(d$gender)) ## percentage of not disclosed
table(d$gender)[4]/sum(table(d$gender)) ## percentage of others

## user ethnicity
table(d$ethnicity)

100 * sum(d$ethnicity == "2") / dim(d)[1] ## percentage of asians
100 * sum(d$ethnicity == "1") / dim(d)[1] ## percentage of blacks
100 * sum(d$ethnicity == "4") / dim(d)[1] ## percentage of hispanics or latinos
100 * sum(d$ethnicity == "3") / dim(d)[1] ## percentage of whites
100 * (sum(d$ethnicity == "5") + sum(nchar(d$ethnicity) > 1)) / dim(d)[1] ## percentage of mixed
100 * sum(d$ethnicity == "6") / dim(d)[1] ## percentage of others

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
table(d$gender_rep)[1]/sum(table(d$gender_rep)) ## percentage of males
table(d$gender_rep)[2]/sum(table(d$gender_rep)) ## percentage of females
table(d$gender_rep)[3]/sum(table(d$gender_rep)) ## percentage of non-binary

## replika relationship
table(d$relationship_rep)
table(d$relationship_rep)[1]/sum(table(d$relationship_rep)) ## percentage of friends
table(d$relationship_rep)[2]/sum(table(d$relationship_rep)) ## percentage of partners
table(d$relationship_rep)[3]/sum(table(d$relationship_rep)) ## percentage of mentors
table(d$relationship_rep)[4]/sum(table(d$relationship_rep)) ## percentage of see how it goes


## replika subscription
table(d$subscription_rep)
table(d$subscription_rep)[1]/sum(table(d$subscription_rep)) ## percentage of none
table(d$subscription_rep)[2]/sum(table(d$subscription_rep)) ## percentage of monthly 
table(d$subscription_rep)[3]/sum(table(d$subscription_rep)) ## percentage of yearly
table(d$subscription_rep)[4]/sum(table(d$subscription_rep)) ## percentage of lifetime

## replika months
mean(as.numeric(d$months_rep), trim = 0, na.rm = TRUE) ## mean number of months
sd(as.numeric(d$months_rep), na.rm = TRUE) ## standard deviation

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
  formula_string <- paste(dv, "~ change_type_factor") 
  
  aov_mod <- aov(as.formula(formula_string), data = d)
  print(summary(aov_mod))
  print(anova_stats(aov_mod))
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
  print(t.test(ct1, ct2, var.equal = vart$p.value > 0.05))
  print(cohens_d(ct1, ct2))
}


# We plan to compare each subsequent change type, i.e., condition 1 (smallest identity stability) v. 2; 2 v. 3; 3 v. 4; 4 v. 5 (largest identity stability).
for (i in 1:4) {
  print(paste0("******** Comparing Identity Loss in ", change_types[i], " vs. ", change_types[i+1], "********"))
  
  ct1 <- d[d$change_type == change_types[i], 'identity_stability']
  ct2 <- d[d$change_type == change_types[i+1], 'identity_stability']

  # Do the t-test
  vart <- var.test(ct1, ct2)

  alt <- "less"
  if(i == 4) {
    alt <- "greater"
  }

  print(t.test(ct1, ct2, var.equal = vart$p.value > 0.05, alternative = alt))
  print(effectsize::cohens_d(ct1, ct2, alternative = alt))
}

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

# Only call dev.off() if there's more than one graphics device open
if (dev.list() != NULL && length(dev.list()) > 1) {
  dev.off()
}


#### Mediation Analysis ####

d_mediation <- d[d$change_type %in% c('control', 'erp', 'closeness'), ]

# If change_type is control, set it to 1. If it's ERP set it to 2. If it's closeness, set it to 3
d_mediation$change_type_num <- ifelse(d_mediation$change_type == 'control', 1, ifelse(d_mediation$change_type == 'erp', 2, 3))

for(dv in c('value', 'mourn')) {
  print(paste0("*-*-*-*-*-*-*-*-*-*-*-*-*-*. ", dv, " .*-*-*-*-*-*-*-*-*-*-*-*-*-*"))
  process(data = d_mediation, y = dv, x = "change_type_num",
          m = "identity_stability", model = 4, effsize = 1, mcx = 1, total = 1, stand = 1,
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

  print("------- Anxious Attachment Moderation -------")
  process(data = d_mediation, y = dv, x = "change_type_num",
          m = "identity_stability", w = "anxious_attachment", model = 14, effsize =1, mcx = 1, total =1, stand =1,
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

  print("------- Parasocial Interaction Moderation -------")
  process(data = d_mediation, y = dv, x = "change_type_num",
          m = "identity_stability", w = "parasocial_interaction", model = 14, effsize =1, mcx = 1, total =1, stand =1,
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}
