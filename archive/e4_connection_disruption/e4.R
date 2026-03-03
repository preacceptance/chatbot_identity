if (!require(pwr)) install.packages("pwr")
library(pwr)

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
library(rstatix)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'filesstrings',    # create and move files
               'effsize',         # another effect size package
               'dplyr',
               'corrplot',        # correlation plots
               'RColorBrewer',    # color schemes for plots
               'tidytext',        # text mining
               'quanteda',        # text analysis
               'topicmodels',     # topic modeling
               'gridExtra',       # for combining plots
               'ggpubr',          # publication ready plots
               'rstatix',         # statistical tests
               'FactoMineR',      # for factor analysis
               'factoextra',      # for visualizing factor analysis
               'lpSolve',         # linear programming
               'tm',              # text mining
               'ltm',
               'lda',             # LDA topic models
               'vader',           # sentiment analysis
               'syuzhet',         # sentiment analysis
               'textstem',        # text stemming
               'seminr',          # SEM modeling
               'lavaan',          # SEM
               'semTools',        # SEM tools
               'sjPlot',          # regression and SEM plots
               'ggcorrplot',      # correlation plots
               'corrr',           # correlation analysis
               'tidyverse')       # for general data manipulation
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### POWER ANALYSIS #####

# power analysis
# u is the number of coefficients you'll have in your model (minus the intercept)
U = 5

# v is the number of error degrees of freedom: v = n - u - 1
pwr.f2.test(u = U, v = NULL, sig.level = 0.05, power = 0.8, f2 = 0.15)

## ================================================================================================================
##                                                   PRE-PROCESSING                
## ================================================================================================================

d <- read.csv('data.csv', sep = ";")

n_original <- dim(d)[1]
n_original 

## attention exclusions: 
d <- subset(d, (d$prime_vign == 5))
dim(d)
print(paste0("Number of participants hired: ", dim(d)[1]))

## ================================================================================================================
##                                              SCALES - RELIABILITY                
## ================================================================================================================

## well-being (converting to 0-100 scale)
d$well_1 <- as.numeric(d$well_1_1)*20
d$well_2 <- as.numeric(d$well_2_1)*20
d$well_3 <- as.numeric(d$well_3_1)*20
d$well_4 <- as.numeric(d$well_4_1)*20
d$well_5 <- as.numeric(d$well_5_1)*20

cronbach.alpha(d[,c("well_1", "well_2", "well_3", "well_4", "well_5")], na.rm = TRUE)
d$well_m <- rowMeans(d[,c("well_1", "well_2", "well_3", "well_4", "well_5")], na.rm = TRUE)
mean(d$well_m, trim = 0, na.rm = TRUE)
var(d$well_m, na.rm = TRUE)

d$value_1_1 <- as.numeric(d$value_1_1)
d$value_2_1 <- as.numeric(d$value_2_1)
d$value_3_1 <- as.numeric(d$value_3_1)

## autonomy
cronbach.alpha(d[,c("value_1_1", "value_2_1", "value_3_1")], na.rm = TRUE)
d$autonomy_m <- rowMeans(d[,c("value_1_1", "value_2_1", "value_3_1")], na.rm = TRUE)
mean(d$autonomy_m, trim = 0, na.rm = TRUE)
var(d$autonomy_m, na.rm = TRUE)

## ================================================================================================================
##                                            PARTICIPANT & REPLIKA CHARACTERISTICS                 
## ================================================================================================================

## ---- User characteristics

## user age
mean(as.numeric(d$age), trim = 0, na.rm = TRUE) ## mean age 
sd(d$age, na.rm = TRUE) ## standard deviation 

## user gender
table(d$gender)
100*table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
100*table(d$gender)[2]/sum(table(d$gender)) ## percentage of females
100*table(d$gender)[3]/sum(table(d$gender)) ## percentage of not disclosed
100*table(d$gender)[4]/sum(table(d$gender)) ## percentage of others

## user ethnicity
table(d$ethnicity)

100*sum(d$ethnicity == "2") / dim(d)[1] ## percentage of asians
100*sum(d$ethnicity == "1") / dim(d)[1] ## percentage of blacks
100*sum(d$ethnicity == "4") / dim(d)[1] ## percentage of hispanics or latinos
100*sum(d$ethnicity == "3") / dim(d)[1] ## percentage of whites
100*sum(d$ethnicity == "5") / dim(d)[1] ## percentage of mixed
100*sum(d$ethnicity == "7") / dim(d)[1] ## percentage of not disclosed
100*sum(d$ethnicity == "6") / dim(d)[1] ## percentage of others

## user education
table(d$edu)

sum(d$edu == "9"); 100 * sum(d$edu == "9") / dim(d)[1] # Percentage of no education
sum(d$edu == "1"); 100 * sum(d$edu == "1") / dim(d)[1] # Percentage of high school or equivalent
sum(d$edu == "2"); 100 * sum(d$edu == "2") / dim(d)[1] # Percentage of vocational school
sum(d$edu == "10"); 100 * sum(d$edu == "10") / dim(d)[1] # Percentage of associate's degree
sum(d$edu == "11"); 100 * sum(d$edu == "11") / dim(d)[1] # Percentage of bachelor's degree
sum(d$edu == "12"); 100 * sum(d$edu == "12") / dim(d)[1] # Percentage of master's degree
sum(d$edu == "7"); 100 * sum(d$edu == "7") / dim(d)[1] # Percentage of graduate degree (PhD, MD, JD, etc.)
sum(d$edu == "8"); 100 * sum(d$edu == "8") / dim(d)[1] # Percentage of other


## user relationship
table(d$relation_status)

sum(d$relation_status == "1"); 100*sum(d$relation_status == "1") / dim(d)[1] # Percentage of single
sum(d$relation_status == "7"); 100*sum(d$relation_status == "7") / dim(d)[1] # Percentage of in relationship
sum(d$relation_status == "2"); 100*sum(d$relation_status == "2") / dim(d)[1] # Percentage of married
sum(d$relation_status == "3"); 100*sum(d$relation_status == "3") / dim(d)[1] # Percentage of divorced
sum(d$relation_status == "4"); 100*sum(d$relation_status == "4") / dim(d)[1] # Percentage of separated
sum(d$relation_status == "5"); 100*sum(d$relation_status == "5") / dim(d)[1] # Percentage of widowed
sum(d$relation_status == "8"); 100*sum(d$relation_status == "8") / dim(d)[1] # Percentage of other

d$has_human_relationship <- ifelse(d$relation_status %in% c("1", "7", "2"), 1, 0)

## user friends
mean(as.numeric(d$close_friends), trim = 0, na.rm = TRUE) ## mean number of friends
sd(as.numeric(d$close_friends), na.rm = TRUE) ## standard deviation

## replika restore
table(d$restore)[1] / sum(table(d$restore)) ## percentage did not restore
table(d$restore)[2] / sum(table(d$restore)) ## percentage did restore

## ---- Replika companion characteristics

## replika age
mean(as.numeric(d$age_rep), trim = 0, na.rm = TRUE) ## mean age 
sd(as.numeric(d$age_rep), na.rm = TRUE) ## standard deviation

## replika gender
table(d$gender_rep)
100 * table(d$gender_rep)[1] / sum(table(d$gender_rep)) ## percentage of males
100 * table(d$gender_rep)[2] / sum(table(d$gender_rep)) ## percentage of females
100 * table(d$gender_rep)[3] / sum(table(d$gender_rep)) ## percentage of non-binary

## replika relationship
table(d$relationship_rep)
100 * table(d$relationship_rep)[1] / sum(table(d$relationship_rep)) ## percentage of friends
100 * table(d$relationship_rep)[2] / sum(table(d$relationship_rep)) ## percentage of partners
100 * table(d$relationship_rep)[3] / sum(table(d$relationship_rep)) ## percentage of see how it goes

## replika subscription
table(d$subscription_rep)
100 * table(d$subscription_rep)[1] / sum(table(d$subscription_rep)) ## percentage of none -- 1
100 * table(d$subscription_rep)[2] / sum(table(d$subscription_rep)) ## percentage of monthly -- 2
100 * table(d$subscription_rep)[3] / sum(table(d$subscription_rep)) ## percentage of yearly -- 3
100 * table(d$subscription_rep)[4] / sum(table(d$subscription_rep)) ## percentage of lifetime -- 5


## replika experience
mean(as.numeric(d$exp_level_rep), trim = 0, na.rm = TRUE) ## mean level
sd(as.numeric(d$exp_level_rep), na.rm = TRUE) ## standard deviation

mean(as.numeric(d$months_rep))
sd(as.numeric(d$months_rep))

# Limit dataframe to include only '1' and '5'
# Uncomment for appendix replication
#d <- d[d$relationship_rep %in% c(1, 5), ]

d$is_partner <- ifelse(d$relationship_rep == 2, 1, 0)

# Make DVs numeric
cols <- c("value_1", "essence_1", "invest_1", "petition_1", "cold_1", "mourn_1", "mental_health_1", "refund_1", 
          "close_friends", "age", "age_rep", "months_rep", "exp_level_rep")

d[cols] <- lapply(d[cols], as.numeric)

## ================================================================================================================
##                                              DATA ANALYSIS - PLOTS                
## ================================================================================================================

d <- d %>% mutate_at(c("age", "value_1", "refund_1", "petition_1", "cold_1", "mourn_1", "mental_health_1", 
                       "well_m", "autonomy_m", "essence_1", "invest_1"), as.numeric)

d5 <- d[,c("mourn_1", "mental_health_1", "well_m", "value_1", "refund_1", "petition_1", "cold_1", "essence_1", "invest_1", "autonomy_m")]

colnames(d5) <- c("a_mourn", "b_mental_health", "c_well_being", 'd_value', 'e_refund', 'f_petition', "g_rejection", 'h_essence', 'i_investment', "j_autonomy")

# Reverse code well being for this plot
d5$c_well_being <- 100 - d5$c_well_being

# Find the mean of all DV's, to draw a horizontal dotted line to show the midpoint of the scale
m_dvs <- mean(c(d5$a_mourn, d5$b_mental_health, d5$c_well_being, d5$d_value, d5$e_refund, d5$f_petition, d5$h_rejection, d5$g_essence, d5$i_investment, d5$j_autonomy))

pdf("dvs_bar.pdf", width=6, height=6)

p5 <- d5 %>% 
  gather(key = "Type", value = "Val") %>%
  ggplot(aes(x = Type, y = Val, fill = Type)) +
  geom_bar(stat = "summary", fun = "mean", fill = 'light grey', color = 'black', alpha = 0.7) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, width = 0.1, color = "black", linewidth = 0.75) +
  coord_cartesian(ylim=c(0, 100)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  xlab ("Measure") + ylab ("Mean Rating") +
  theme_classic() +
  ggtitle("") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 22)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_discrete(labels = c("Mourning", "Mental Health", "Well-being", "Devaluation", "Refund",
                              "Petition", "Rejection", "Identity\nDiscontinuity", "Investment", "Autonomy")) +
  theme(legend.position = "none") +
  geom_hline(yintercept=m_dvs, color="blue", linetype = "dashed") +
  theme(plot.margin = margin(1,1,1,1, "cm"))

p5

dev.off()

## ================================================================================================================
##                                              DATA ANALYSIS - CORRELATIONS                
## ================================================================================================================

library(corrplot)
library(RColorBrewer)


colMeans(d[c("age", "value_1", "refund_1", "petition_1", "cold_1", "mourn_1", "mental_health_1", 
    "well_m", "autonomy_m", "essence_1", "invest_1")], na.rm = TRUE)

sapply(d[c("age", "value_1", "refund_1", "petition_1", "cold_1", "mourn_1", "mental_health_1", 
             "well_m", "autonomy_m", "essence_1", "invest_1")], sd, na.rm = TRUE)


res1 <- cor.mtest(d[c("age", "value_1", "refund_1", "petition_1", "cold_1", "mourn_1", "mental_health_1", 
                      "well_m", "autonomy_m", "essence_1", "invest_1")], conf.level = .95)


corrplot(cor(d[c("age", "value_1", "refund_1", "petition_1", "cold_1", "mourn_1", "mental_health_1", 
                  "well_m", "autonomy_m", "essence_1", "invest_1")],
             method = "pearson", use = "pairwise.complete.obs"),
         method = "color",        # number, circle, square, ellipse, pie, or color
         p.mat = res1$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 0.8,
         pch.col = "yellow",
         tl.col="black",
         tl.cex=1,
         addCoef.col = "black")             # line width of the rectangles

## ================================================================================================================
##                                              DATA ANALYSIS - REGRESSIONS                
## ================================================================================================================

## multiple regressions
# Mean center invest_1, autonomy_m, and essence_1 before the analysis:

# is_partner is binary
d$is_partner <- as.factor(d$is_partner)

d$essence_mc <- d$essence_1 - mean(d$essence_1, na.rm = TRUE)

run_model <- function(dependent_var, dat) {
  print(paste0("*-*-*-*-*-*", dependent_var, "*-*-*-*-*-*"))
  formula <- as.formula(paste(dependent_var, "~ essence_mc * is_partner")) # essence_mc * is_partner
  model <- lm(formula, data = dat)
  print(summary(model))
}

variables <- c("mourn_1", "mental_health_1", "well_m", "value_1", "refund_1", "petition_1", "cold_1")
lapply(variables, run_model, d)


for (dv in c("value_1", "refund_1", "petition_1", "mourn_1", "mental_health_1", "well_m", "invest_1", "essence_1", "autonomy_m")) {
  print(paste0("*-*-*-*-*-*", dv, "*-*-*-*-*-*"))
  x <- d[d$is_partner == 1, dv]
  y <- d[d$is_partner == 0, dv]
  vart <- var.test(x, y)
  tt <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > 0.05); print(tt)
  print(cohen.d(x, y, paired = FALSE, hedges.correction = TRUE))
}

## ================================================================================================================
##                                              EXPLORATORY ANALYSES                
## ================================================================================================================

############## GENDER DIFFERENCES ##############

# Compare DVs for male v. female participants
d_male <- d[d$gender == "1", ]
d_female <- d[d$gender == "2", ]

# do t-tests comparing all DVs for two groups
for (dv in c("value_1", "refund_1", "petition_1", "mourn_1", "mental_health_1", "well_m", "invest_1", "essence_1", "autonomy_m")) {
  print(paste0("*-*-*-*-*-*", dv, "*-*-*-*-*-*"))
  x <- d_male[, dv]
  y <- d_female[, dv]  # Replika
  vart <- var.test(x, y)
  tt <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > 0.05); print(tt)
  print(cohen.d(x, y, paired = FALSE, hedges.correction = TRUE))
}

############## DEMOGRAPHIC DIFFERENCES ##############
# Do t-tests comparing is_partner condition for all DVs
for (dv in c("value_1", "refund_1", "petition_1", "mourn_1", "mental_health_1", "well_m", "invest_1", "essence_1", "autonomy_m")) {
  print(paste0("*-*-*-*-*-*", dv, "*-*-*-*-*-*"))
  x <- d[d$is_partner == 1, dv]
  y <- d[d$is_partner == 0, dv]
  vart <- var.test(x, y)
  tt <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > 0.05); print(tt)
  print(cohen.d(x, y, paired = FALSE, hedges.correction = TRUE))
}


############# MODERATION WITH AUTONOMY ##############

# Mean center autonomy and essence before the analysis:
d$autonomy_mc <- d$autonomy_m - mean(d$autonomy_m, na.rm = TRUE)
d$essence_mc <- d$essence_1 - mean(d$essence_1, na.rm = TRUE)

run_model <- function(dependent_var, dat) {
  print(paste0("*-*-*-*-*-*", dependent_var, "*-*-*-*-*-*"))
  formula <- as.formula(paste(dependent_var, "~ essence_mc * autonomy_mc"))
  model <- lm(formula, data = dat)
  print(summary(model))
}

variables <- c("mourn_1", "mental_health_1", "well_m", "value_1", "refund_1", "petition_1", "cold_1")
lapply(variables, run_model, d)


################ MODERATION WITH INVESTMENT ################

# Mean center invest_1, autonomy_m, and essence_1 before the analysis:
d$invest_mc <- d$invest_1 - mean(d$invest_1, na.rm = TRUE)
d$essence_mc <- d$essence_1 - mean(d$essence_1, na.rm = TRUE)

run_model <- function(dependent_var, dat) {
  print(paste0("*-*-*-*-*-*", dependent_var, "*-*-*-*-*-*"))
  formula <- as.formula(paste(dependent_var, "~ essence_mc * invest_mc"))
  model <- lm(formula, data = dat)
  print(summary(model))
}

variables <- c("mourn_1", "mental_health_1", "well_m", "value_1", "refund_1", "petition_1", "cold_1")
lapply(variables, run_model, d)

# Subscription types: none (1), monthly (2), yearly (3), lifetime (5)
################ MODERATION WITH SUBSCRIPTION ################

d$subscription_rep <- ifelse(d$subscription_rep == "1", "none", ifelse(d$subscription_rep == "2", "monthly", ifelse(d$subscription_rep == "3", "yearly", "lifetime")))

# Convert subscription to a factor for ANOVA
d$subscription_rep <- factor(d$subscription_rep, levels = c("none", "monthly", "yearly", "lifetime"))

# Check the distribution of mourn_1 by subscription level
table(d$subscription_rep)

# Run an ANOVA to test if subscription levels impact mourning levels
anova_model <- aov(mourn_1 ~ subscription_rep, data = d)
summary(anova_model)

anova_model <- aov(mental_health_1 ~ subscription_rep, data = d)
summary(anova_model)

# Print mean mourning for each subscription group
print(aggregate(d$mourn_1, by = list(d$subscription_rep), FUN = mean))
print(aggregate(d$mental_health_1, by = list(d$subscription_rep), FUN = mean))
print(aggregate(d$well_m, by = list(d$subscription_rep), FUN = mean))
print(aggregate(d$value_1, by = list(d$subscription_rep), FUN = mean))
print(aggregate(d$refund_1, by = list(d$subscription_rep), FUN = mean))
print(aggregate(d$petition_1, by = list(d$subscription_rep), FUN = mean))
print(aggregate(d$cold_1, by = list(d$subscription_rep), FUN = mean))
print(aggregate(d$essence_1, by = list(d$subscription_rep), FUN = mean))

# Compare mourning in monthly and yearly vs lifetime
for (Dv in c("mourn_1", "mental_health_1", "well_m", "value_1", "refund_1", "petition_1", "cold_1")) {
  print(paste0("*-*-*-*-*-*", Dv, "*-*-*-*-*-*"))
  print(t.test(d[d$subscription_rep %in% c("monthly", "yearly"), Dv], d[d$subscription_rep == "lifetime", Dv]))
}



# Plot each DV with each subscription group (mourning, deval, identity, etc.), with DV type on the legend with colors (e.g., mourning red and identity blue)
# and subscription groups on the x-axis
# We can plot the mean values with points
ggplot(d, aes(x = subscription_rep, y = mourn_1, color = "Mourning")) +
    geom_point() +
    theme_minimal() +
    labs(x = "Subscription Type", y = "Mourning Levels", title = "Mourning Levels by Subscription Type")

# Post-hoc test (Tukey's HSD) to identify which groups differ
TukeyHSD(anova_model)

# Correlation between subscription level (numeric) and mourning
# Assuming subscription_rep is coded as: none=0, monthly=1, yearly=2, lifetime=3
d$subscription_num <- as.numeric(d$subscription_rep) - 1
cor_test <- cor.test(d$subscription_num, d$mourn_1, method = "pearson")
print(cor_test)

# Visualization of the mean mourning levels by subscription group
library(ggplot2)
ggplot(d, aes(x = subscription_rep, y = mourn_1)) +
    geom_boxplot() +
    theme_minimal() +
    labs(x = "Subscription Type", y = "Mourning Levels", title = "Mourning Levels by Subscription Type")





# Make subscription_rep numeric with no=0, monthly=1, yearly=2, lifetime=3
d$subscription_rep_num <- as.numeric(d$subscription_rep) - 1

# Mean center invest_1, autonomy_m, and essence_1 before the analysis:
d$essence_mc <- d$essence_1 - mean(d$essence_1, na.rm = TRUE)

run_model <- function(dependent_var, dat) {
  print(paste0("*-*-*-*-*-*", dependent_var, "*-*-*-*-*-*"))
  formula <- as.formula(paste(dependent_var, "~ essence_mc * subscription_rep_num"))
  model <- lm(formula, data = dat)
  print(summary(model))
}

variables <- c("mourn_1", "mental_health_1", "well_m", "value_1", "refund_1", "petition_1", "cold_1")
lapply(variables, run_model, d)

## ================================================================================================================
##                                              DATA ANALYSIS - MANUAL RATINGS                
## ================================================================================================================

# IDENTITY
identity <- data.frame(X2=na.omit(d$rater_2_identity), X1=na.omit(d$rater_1_identity))
cronbach.alpha(identity)
mean(na.omit(d[d$rater_2_identity == d$rater_1_identity, 'rater_1_identity']))
table(d[(d$rater_2_identity == d$rater_1_identity) & d$rater_1_identity == 1, 'rater_1_identity_change_type'])

# DEVALUATION
devaluation <- data.frame(X2=na.omit(d$rater_2_devaluation), X1=na.omit(d$rater_1_devaluation))
cronbach.alpha(devaluation)
mean(na.omit(d[d$rater_2_devaluation == d$rater_1_devaluation, 'rater_1_devaluation']))
table(d[(d$rater_2_devaluation == d$rater_1_devaluation) & d$rater_1_devaluation == 1, 'rater_1_devaluation_type'])

# MOURNING
# For mourning, we have several columns that we want to combine: r1_mentalhealth, r2_mentalhealth, r1_senseofloss, r2_senseofloss, r1_wellbeing, r2_wellbeing
mentalhealth <- data.frame(X2=na.omit(d$r2_mentalhealth), X1=na.omit(d$r1_mentalhealth))
cronbach.alpha(mentalhealth)
mean(na.omit(d[d$r2_mentalhealth == d$r1_mentalhealth, 'r1_mentalhealth']))

wellbeing <- data.frame(X2=na.omit(d$r2_wellbeing), X1=na.omit(d$r1_wellbeing))
cronbach.alpha(wellbeing)
mean(na.omit(d[d$r2_wellbeing == d$r1_wellbeing, 'r1_wellbeing']))

# Get the OR for mental health and well being
# Create a data frame from the agreed-upon ratings for mental health and well-being
mhealth_or_wellbeing <- data.frame(
  rater2_mhealth = na.omit(d$r2_mentalhealth),
  rater1_mhealth = na.omit(d$r1_mentalhealth),
  rater2_wellbeing = na.omit(d$r2_wellbeing),
  rater1_wellbeing = na.omit(d$r1_wellbeing),
  rater1_senseofloss = na.omit(d$r1_senseofloss),
  rater2_senseofloss = na.omit(d$r2_senseofloss)
)

# Filter rows where both raters agreed on mental health and well-being ratings
# We use a logical OR to capture cases where either mental health or well-being is rated 1
agreed_mourning <- mhealth_or_wellbeing %>%
  filter(
    rater1_mhealth == rater2_mhealth | rater1_wellbeing == rater2_wellbeing,   # Agreement
    (rater1_mhealth == 1 | rater1_wellbeing == 1)                              # Either mental health or well-being is 1
  )

# How many percentage of cases have a rating of 1 for mental health or well-being?
prop_1 <- nrow(agreed_mourning) / nrow(mhealth_or_wellbeing)
prop_1

# How many percentage of cases that have a rating of 1 for mental health or well-being also have 1 for sense of loss?
prop_1_senseofloss <- nrow(agreed_mourning[agreed_mourning$rater1_senseofloss == 1, ]) / nrow(agreed_mourning)
prop_1_senseofloss


# An additional one is sense of loss
senseofloss <- data.frame(X2=na.omit(d$r2_senseofloss), X1=na.omit(d$r1_senseofloss))
cronbach.alpha(senseofloss)
mean(na.omit(d[d$r2_senseofloss == d$r1_senseofloss, 'r1_senseofloss']))



# DISAPPOINTMENT
disappointment <- data.frame(X2=na.omit(d$r2_disappointment), X1=na.omit(d$r1_disappointment))
cronbach.alpha(disappointment)
mean(na.omit(d[d$r2_disappointment == d$r1_disappointment, 'r1_disappointment']))






## =============================================================================
##                        DATA ANALYSIS - TOPIC MODELLING                
## =============================================================================

# Function for cleaning the posts/comments
clean_text <- function(text) {
  text <- gsub("'", "", text)  # remove apostrophes
  text <- gsub("http\\S+\\s*", "", text) # remove URLs
  text <- gsub("\\n", "", text) # remove line breaks
  text <- gsub("\\r", "", text) # remove line breaks
  text <- gsub("\\t", "", text) # remove line breaks
  text <- gsub("\\s+", " ", text) # remove extra spaces
  text <- gsub("^\\s+|\\s+$", "", text) # remove leading and trailing spaces
  text <- gsub("[[:punct:]]", "", text) # remove punctuation
  text <- gsub("[^[:alnum:]]", " ", text) # remove non-alphanumeric characters
  text <- gsub("[[:digit:]]", "", text) # remove numbers
  text <- gsub("^[[:space:]]+", "", text) # remove whitespace at beginning of documents
  text <- gsub("[[:space:]]+$", "", text) # remove whitespace at end of documents
  text <- gsub(" +", " ", text) # remove extra spaces
  text <- tolower(text) # convert to lowercase
  return(text)
}

explanations <- d[d$explanation != "", 'explanation']
emotions_l <- d[, 'emotion']
sentiments_l <- d[, 'sentiment']

# Clean text
explanations <- clean_text(explanations)

texts = corpus(explanations)
dfm = dfm(texts, remove=c(stopwords("smart"), "replika", "erp", "rep", "replikas", "erps", "reps", "people", "ai", "https", "im", "dont", "app", "users", "make", "things", "", "www"))
dtm = convert(dfm, to = "topicmodels") 

train = sample(rownames(dtm), nrow(dtm) * .8)
dtm_train = dtm[rownames(dtm) %in% train, ]
dtm_test = dtm[!rownames(dtm) %in% train, ]

## create a dataframe to store the perplexity scores for different values of k
p = data.frame(k = 2:20, perplexity = NA)

## loop over the values of k in data.frame p 
for (i in 1:nrow(p)) {
  print(p$k[i])
  
  ## replicate each topic perplexity 10 times and average the results
  perplexity_scores = rep(NA, 10)
  for (j in 1:10) {
    ## calculate perplexity for the given value of k
    m = LDA(dtm_train, method = "Gibbs", k = p$k[i], control=list(seed = 123))
    ## store result in our vector
    perplexity_scores[j] = perplexity(m, dtm_test)
  }
  
  ## compute the average of the perplexity scores
  p$perplexity[i] = mean(perplexity_scores)
}

# Plot p with dots conneting with lines, with x-axis as "Number of Topics", and y-axis as "Perplexity". Make the styling similar to matplotlib
png("perplexity.png", width=2400, height=1080, res=300)  
plt <- ggplot(p, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(x = "Number of Topics", y = "Perplexity") + theme(axis.line = element_line(colour = "black"))
plt
dev.off()

##### Frequency Bar Plot
set.seed(123)
model <- LDA(dtm, method = "Gibbs", k = 6, control=list(seed = 123))

# get the terms for the first topic
topics <- tidy(model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15, with_ties = FALSE) %>% 
  ungroup() %>%
  arrange(topic, -beta)


#top_terms <- top_terms %>%
#  filter(topic %in% c(1,4,6))

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term)) + # remove fill = factor(topic) to make all bars same color
  theme(text=element_text(size=14)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), aspect.ratio = 1, plot.title = element_text(hjust = 0.5)) +
  xlab("Topic Word Probability") +
  ylab("") +
  geom_col(show.legend = FALSE, fill = "#4f4f4f") + # set fill = "gray" in geom_col()
  facet_wrap(~ paste("Topic:", topic), scales = "free", labeller = label_parsed) +
  scale_x_continuous(limits = c(0, max(top_terms$beta))) + # limit x axis to the max x all bars have
  scale_y_reordered()

## ================================================================================================================
##                                              DATA ANALYSIS - SENTIMENT ANALYSIS                
## ================================================================================================================

title = "Emotions"
png("emotions.png", res = 300, width=2000, height=2000)

d_clean <- d[d$explanation != "", ]
print(sort(table(d_clean$emotion) / sum(table(d_clean$emotion))))
par(mar=c(5,6,4,1)+.1)
barplot(
  sort(table(d_clean$emotion) / sum(table(d_clean$emotion))), 
  horiz = TRUE, 
  las = 1, 
  main = title, xlab="Percentage",
  cex.main = 1.5, cex.axis=1.5, cex.names=1.5, cex.lab = 1.5, 
)

dev.off()

## ================================================================================================================
##                                              END                
## ================================================================================================================