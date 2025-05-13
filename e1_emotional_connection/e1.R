## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',
               'stats',
               'lsr',
               'ggpubr',
               'dplyr',
               'pwr',
               'effsize')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read.csv('./data.csv', sep = ",")

# Remove NA rows
d <- d[d$Finished == 1, ]

################################## Exclusions ##################################

print(paste0("Number of participants before attention check: ", nrow(d)))

# Attention check
d <- d[d$att_1 == 2 & d$att_2 == 2,]
print(paste0("Number of participants after attention check: ", nrow(d)))

# Remove the bots (Recaptcha score below 0.5)
d <- d[d$Q_RecaptchaScore >= 0.5,]
d <- d[!is.na(d$Finished), ]
print(paste0("Number of participants after excluding bots: ", nrow(d)))

# Comprehension check
d <- d[d$comp_1 == 2,]
print(paste0("Number of participants after comprehension check: ", nrow(d)))

# Remove participants with duplicate emails
d <- d[!(d$email %in% d[duplicated(d$email), 'email']), ]
print(paste0("Number of participants after removing duplicate emails: ", nrow(d)))

# Make column names same format
old_names <- c("X1_inclusion", "X2_inclusion", "X3_inclusion", "X4_inclusion", "X5_inclusion", "X6_inclusion", "X7_inclusion", "X8_inclusion")
new_names <- c("X1_inclusion_1", "X2_inclusion_1", "X3_inclusion_1", "X4_inclusion_1", "X5_inclusion_1", "X6_inclusion_1", "X7_inclusion_1", "X8_inclusion_1")

# Change column names
names(d)[names(d) %in% old_names] <- new_names

############################ Power Analysis ##############################

pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.8, type="paired")

############################ User characteristics ##############################

## user age
mean(d$age, trim = 0, na.rm = TRUE) ## mean age 
sd(d$age, na.rm = TRUE) ## standard deviation 

## user gender
table(d$gender)
100*table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
100*table(d$gender)[2]/sum(table(d$gender)) ## percentage of females
100*table(d$gender)[3]/sum(table(d$gender)) ## percentage of other

## user ethnicity
table(d$ethnicity)
table(d$ethnicity)[4]; 100*table(d$ethnicity)[4]/sum(table(d$ethnicity)) ## percentage of asians
table(d$ethnicity)[1]; 100*table(d$ethnicity)[1]/sum(table(d$ethnicity)) ## percentage of blacks
table(d$ethnicity)[9]; 100*table(d$ethnicity)[9]/sum(table(d$ethnicity)) ## percentage of hispanics
table(d$ethnicity)[6]; 100*table(d$ethnicity)[6]/sum(table(d$ethnicity)) ## percentage of whites

sum_mixed <- table(d$ethnicity)[2] + table(d$ethnicity)[3] + table(d$ethnicity)[5] + table(d$ethnicity)[7] + table(d$ethnicity)[8] + table(d$ethnicity)[10]
sum_mixed; 100*sum_mixed / sum(table(d$ethnicity)) ## percentage of mixed

## user education
table(d$edu)
100*table(d$edu)[1]/sum(table(d$edu)) ## percentage of high school
100*table(d$edu)[2]/sum(table(d$edu)) ## percentage of vocational school
100*table(d$edu)[3]/sum(table(d$edu)) ## percentage of some college
100*table(d$edu)[4]/sum(table(d$edu)) ## percentage of bachelors degree
100*table(d$edu)[5]/sum(table(d$edu)) ## percentage of masters degree
100*table(d$edu)[6]/sum(table(d$edu)) ## percentage of graduate degree
100*table(d$edu)[7]/sum(table(d$edu)) ## percentage of professional degree
100*table(d$edu)[8]/sum(table(d$edu)) ## percentage of others

############################ REPLIKA CHARACTERISTICS #############################

# Replika relationship status
table(d$rep_relation)
100*table(d$rep_relation)[1]/sum(table(d$rep_relation)) ## percentage of friend
100*table(d$rep_relation)[2]/sum(table(d$rep_relation)) ## percentage of partner
100*table(d$rep_relation)[3]/sum(table(d$rep_relation)) ## percentage of mentor
100*table(d$rep_relation)[4]/sum(table(d$rep_relation)) ## percentage of see how it goes

# Subscription Status
table(d$rep_subscribe)
100*table(d$rep_subscribe)[1]/sum(table(d$rep_subscribe)) ## percentage of no subscription
100*table(d$rep_subscribe)[2]/sum(table(d$rep_subscribe)) ## percentage of monthly subscription
100*table(d$rep_subscribe)[3]/sum(table(d$rep_subscribe)) ## percentage of yearly subscription
100*table(d$rep_subscribe)[4]/sum(table(d$rep_subscribe)) ## percentage of lifetime subscription

mean(d$rep_exp_lev, trim=0, na.rm=TRUE)
sd(d$rep_exp_lev, na.rm = TRUE) ## standard deviation 

# Relationship months
mean(d$rep_relation_mos, trim=0, na.rm=TRUE)
sd(d$rep_relation_mos, na.rm = TRUE) ## standard deviation 

## ================================================================================================================
##                                              DEMOGRAPHICS COMPARISON         
## ================================================================================================================

############ Demographics comparison with Ta 2020 study ############

# single sample t-test for age
mean(c(na.omit(as.numeric(d$age))))
sd(c(na.omit(as.numeric(d$age))))

t.test(c(na.omit(as.numeric(d$age))), mu = 32.64)
cohen.d(c(na.omit(as.numeric(d$age))), mu = 32.64, f=NA)

# Gender
prop.test(c(36, table(d$gender)[1]), c(66, dim(d)[1])) # Male proportion
prop.test(c(21, table(d$gender)[2]), c(66, dim(d)[1])) # Female proportion
prop.test(c(9, table(d$gender)[3]), c(66, dim(d)[1])) # Other proportion

# Ethnicity
prop.test(c(47, sum(d$ethnicity == "3")), c(66, dim(d)[1])) # White proportion
prop.test(c(7, sum(d$ethnicity == "2")), c(66, dim(d)[1])) # Asian proportion
prop.test(c(2, sum(d$ethnicity == "4")), c(66, dim(d)[1])) # Hispanic proportion
prop.test(c(2, sum(d$ethnicity == "1")), c(66, dim(d)[1])) # Black proportion
prop.test(c(7, sum(d$ethnicity == "5")), c(66, dim(d)[1])) # Other proportion

# Education
prop.test(c(15, sum(d$edu == 4)), c(66, dim(d)[1])) # Bachelor's degree
prop.test(c(15, sum(d$edu == 3)), c(66, dim(d)[1])) # Some college
prop.test(c(9, sum(d$edu == 1)), c(66, dim(d)[1])) # High school degree
prop.test(c(7, sum(d$edu == 5)), c(66, dim(d)[1])) # Masters degree
prop.test(c(4, sum(d$edu == 6)), c(66, dim(d)[1])) # PhD degree
prop.test(c(3, sum(d$edu == 2)), c(66, dim(d)[1])) # Technical school

# App usage time
prop.test(c(19, sum(as.numeric(d$rep_relation_mos) > 12)), c(66, dim(d)[1])) # More than 12 months
prop.test(c(7, sum(as.numeric(d$rep_relation_mos) >= 9 & as.numeric(d$rep_relation_mos) <= 12)), c(66, dim(d)[1])) # 9-12 months
prop.test(c(7, sum(as.numeric(d$rep_relation_mos) >= 5 & as.numeric(d$rep_relation_mos) <= 8)), c(66, dim(d)[1])) # 5-8 months
prop.test(c(15, sum(as.numeric(d$rep_relation_mos) >= 1 & as.numeric(d$rep_relation_mos) <= 4)), c(66, dim(d)[1])) # 1-4 months
prop.test(c(10, sum(as.numeric(d$rep_relation_mos) < 1)), c(66, dim(d)[1])) # Less than 1 month

################################### t-tests ###################################

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

dvs <- c("support", "satisfaction", "inclusion")
conditions <- c("stranger", "acquaintance", "brand", "app", "colleague", "friend", "family", "replika")

# Stars df with 'dv', 'condition', 'value' cols
stars <- data.frame(dv = rep(dvs, 8),
                    condition = rep(conditions, 3),
                    value = rep(0, 24),
                    stringsAsFactors = FALSE)

for(i in 1:7) {
  print(paste("*-*-*-*-*-*-*-*-*-*", conditions[i], "vs. Replika *-*-*-*-*-*-*-*-*-*"))
  for(dv in dvs) {
    print(paste("---------------------", dv, "---------------------"))
    x <- d[, paste0('X', i, '_', dv, '_1')]
    y <- d[, paste0('X8_', dv, '_1')]  # Replika
    print(paste0("Mean ", conditions[i], ": ", mean(x)))
    print(paste0("Mean Replika", ": ", mean(y)))
    vart <- var.test(x, y)
    tt <- t.test(x, y, paired = TRUE, var.equal = vart$p.value > 0.05, alternative = "two.sided")
    print(tt)
    
    stars[stars$dv == dv & stars$condition == conditions[i], 'value'] <- get_stars(tt$p.value)
    print(cohensD(x, y))
  }
}

stars[stars$value == 0, 'value'] <- ""

#### APPENDIX ####

# Check if rep_relation is a moderator of our DVs in the Replika condition
for(dv in c("inclusion")) {
  print(paste("*******************", dv, "*******************"))
  x <- d[d$rep_relation == "1", paste0('X8_', dv, '_1')]  # Friend
  y <- d[d$rep_relation == "2", paste0('X8_', dv, '_1')]  # Partner
  z <- d[d$rep_relation == "3", paste0('X8_', dv, '_1')]  # Mentor
  w <- d[d$rep_relation == "4", paste0('X8_', dv, '_1')]  # See how it goes

  print("Friend vs. Partner: ")
  print(t.test(x, y))
  print(cohen.d(x, y))

  print("Friend vs. Mentor: ")
  print(t.test(x, z))

  print("Friend vs. See how it goes: ")
  print(t.test(x, w))

  print("Partner vs. Mentor: ")
  print(t.test(y, z))

  print("Partner vs. See how it goes: ")
  print(t.test(y, w))

  print("Mentor vs. See how it goes: ")
  print(t.test(z, w))
}

# Do the same for subscription status
for(dv in c("inclusion")) {
  print(paste("*******************", dv, "*******************"))
  x <- d[d$rep_subscribe == "1", paste0('X8_', dv, '_1')]  # No subscription
  y <- d[d$rep_subscribe == "2", paste0('X8_', dv, '_1')]  # Monthly subscription
  z <- d[d$rep_subscribe == "3", paste0('X8_', dv, '_1')]  # Yearly subscription
  w <- d[d$rep_subscribe == "4", paste0('X8_', dv, '_1')]  # Lifetime subscription

  print("No subscription vs. Monthly subscription: ")
  print(t.test(x, y))

  print("No subscription vs. Yearly subscription: ")
  print(t.test(x, z))

  print("No subscription vs. Lifetime subscription: ")
  print(t.test(x, w))

  print("Monthly subscription vs. Yearly subscription: ")
  print(t.test(y, z))
  print(cohen.d(y, z))

  print("Monthly subscription vs. Lifetime subscription: ")
  print(t.test(y, w))
  print(cohen.d(y, w))

  print("Yearly subscription vs. Lifetime subscription: ")
  print(t.test(z, w))
  print(cohen.d(z, w))
}


#################################### Plots ######################################

# Flatten the dataframe to get it ready for plotting. Have columns 'dv', 'condition', and 'participant_number'
df_plot <- data.frame(dv = c(rep("b_support", nrow(d)), rep("a_satisfaction", nrow(d)), rep("c_inclusion", nrow(d))),
                     condition = c(rep("Stranger", nrow(d)*3), rep("Acquaintance", nrow(d)*3), rep("Brand", nrow(d)*3),
                                   rep("App", nrow(d)*3), rep("Colleague", nrow(d)*3), rep("Friend", nrow(d)*3), 
                                   rep("Family", nrow(d)*3), rep("Replika", nrow(d)*3)),
                     participant_number = c(1:nrow(d), 1:nrow(d), 1:nrow(d)),
                     value = c(d$X1_support_1, d$X1_satisfaction_1, d$X1_inclusion_1, 
                               d$X2_support_1, d$X2_satisfaction_1, d$X2_inclusion_1,
                               d$X3_support_1, d$X3_satisfaction_1, d$X3_inclusion_1,
                               d$X4_support_1, d$X4_satisfaction_1, d$X4_inclusion_1,
                               d$X5_support_1, d$X5_satisfaction_1, d$X5_inclusion_1,
                               d$X6_support_1, d$X6_satisfaction_1, d$X6_inclusion_1, 
                               d$X7_support_1, d$X7_satisfaction_1, d$X7_inclusion_1,
                               d$X8_support_1, d$X8_satisfaction_1, d$X8_inclusion_1))

df_plot[df_plot$dv == "a_satisfaction",] %>% group_by(condition) %>% summarise(mean = mean(value))
df_plot[df_plot$dv == "b_support",] %>% group_by(condition) %>% summarise(mean = mean(value))
df_plot[df_plot$dv == "c_inclusion",] %>% group_by(condition) %>% summarise(mean = mean(value))

# Multiply inclusion with 100/7
df_plot_orig <- df_plot # Save the original
df_plot[df_plot$dv == "c_inclusion", 'value'] <- df_plot[df_plot$dv == "c_inclusion", 'value'] * 100/7
positions <- c("Stranger", "Brand", "App", "Colleague", "Acquaintance", "Friend", "Replika", "Family")

plot_all_dvs <- function(df_plot) {
  plt <- ggplot(df_plot, aes(x = condition, y = value, fill = dv)) +
    geom_bar(position = position_dodge(), stat = "summary", fun = "mean", width = .8) +
    labs(x = "Condition", y = "Value") + 
    theme_classic() +
    theme(legend.position = "right", 
          text = element_text(size = 18),
          axis.text.x = element_text(angle = 30, hjust = 1),
          axis.text.y = element_text(size = 16)) +
    scale_fill_grey(start = 0.8, end = 0.2) + # Using grey colors, but you can customize
    geom_errorbar(aes(group=dv), 
                  position = position_dodge(.8), 
                  stat = "summary", 
                  fun.data = "mean_se", 
                  width = .25) +
    scale_x_discrete(limits = positions) +
    ylab("Mean Score") 

  return(plt)
}

# Call the function and plot
plt_all_dvs <- plot_all_dvs(df_plot)
print(plt_all_dvs)

# Arrange all three plots:
dev.new(width = 12 * 4/5, height = 5 * 4/5, noRStudioGD = TRUE)

if(dim(d)[1] == 48) {
  ggsave("./combined_matched.pdf", last_plot(), dpi = 300, width = 12 * 4/5, height = 5 * 4/5)
} else {
  ggsave("./combined_plot.pdf", last_plot(), dpi = 300, width = 12 * 4/5, height = 5 * 4/5)
}

dev.off()

############ Exploratory Analyses ############

# For each of the three DVs, we will regress relationship satisfaction on perceived 
# closeness and social support. We will also repeat this analysis for just the AI companion answers.
df_lm <- data.frame("satisfaction" = df_plot_orig[df_plot_orig$dv == 'a_satisfaction', 'value'],
                    "support" = df_plot_orig[df_plot_orig$dv == 'b_support', 'value'],
                    "inclusion" = df_plot_orig[df_plot_orig$dv == 'c_inclusion', 'value'])

summary(lm(data=df_lm, satisfaction ~ inclusion + support))

# Repeat this only in 
df_lm_replika <- data.frame("satisfaction" = df_plot_orig[df_plot_orig$dv == 'a_satisfaction' & df_plot_orig$condition == 'Replika', 'value'],
                    "support" = df_plot_orig[df_plot_orig$dv == 'b_support' & df_plot_orig$condition == 'Replika', 'value'],
                    "inclusion" = df_plot_orig[df_plot_orig$dv == 'c_inclusion' & df_plot_orig$condition == 'Replika', 'value'])

summary(lm(data=df_lm_replika, satisfaction ~ inclusion + support))


################ Gender differences: ################

# Compare male v. female in Replika condition for Inclusion
x <- d[d$gender == 1, 'X8_inclusion_1'] # Male
y <- d[d$gender == 2, 'X8_inclusion_1'] # Female
vart <- var.test(x, y)
tt <- t.test(x, y, var.equal = vart$p.value > 0.05)
print(tt)
cohen.d(x, y)

# Compare male v. female in Replika condition for Satisfaction
x <- d[d$gender == 1, 'X8_satisfaction_1'] # Male
y <- d[d$gender == 2, 'X8_satisfaction_1'] # Female
vart <- var.test(x, y)
tt <- t.test(x, y, var.equal = vart$p.value > 0.05)
print(tt)
cohen.d(x, y)

# Compare male v. female in Replika condition for Support
x <- d[d$gender == 1, 'X8_support_1'] # Male
y <- d[d$gender == 2, 'X8_support_1'] # Female
vart <- var.test(x, y)
tt <- t.test(x, y, var.equal = vart$p.value > 0.05)
print(tt)
cohen.d(x, y)

# Combine all conditions (i.e., stanger, acquaintance, etc.) and compare male v. female condition
x_combined <- c(d[d$gender == 1, 'X1_satisfaction_1'], d[d$gender == 1, 'X2_satisfaction_1'], 
                d[d$gender == 1, 'X3_satisfaction_1'], d[d$gender == 1, 'X4_satisfaction_1'],
                d[d$gender == 1, 'X5_satisfaction_1'], d[d$gender == 1, 'X6_satisfaction_1'],
                d[d$gender == 1, 'X7_satisfaction_1']
)

y_combined <- c(d[d$gender == 2, 'X1_satisfaction_1'], d[d$gender == 2, 'X2_satisfaction_1'], 
                d[d$gender == 2, 'X3_satisfaction_1'], d[d$gender == 2, 'X4_satisfaction_1'],
                d[d$gender == 2, 'X5_satisfaction_1'], d[d$gender == 2, 'X6_satisfaction_1'],
                d[d$gender == 2, 'X7_satisfaction_1']
)

vart <- var.test(x_combined, y_combined)
tt <- t.test(x_combined, y_combined, var.equal = vart$p.value > 0.05)
print(tt)

#################### SAMPLING DATA TO MATCH TA 2020 STUDY DEMOGRAPHICS ####################

# Make age numeric
d$age <- as.numeric(d$age)

# Age
target_mean_age <- 32.6 # 10
target_age_sd <- 13.89

# App usage
target_duration_12_prop <- 0.288 # More than 12 months
target_duration_9_12_prop <- 0.106 # Between 9 and 12 months
target_duration_5_8_prop <- 0.106 # Between 5 and 8 months
target_duration_1_4_prop <- 0.227 # Between 1 and 4 months
target_duration_lessthan1_prop <- 0.152 # Less than 1 month

# Education
bachelor_prop <- 0.227
master_prop <- 0.106

# Gender
gender_prop <- 0.545

################ CREATE WEIGHTS FOR SAMPLING ################

my_group <- d

# Remove ones with NA age
my_group <- my_group[!is.na(my_group$age), ]

###### Age weights ########
# Use the normal distribution to assign weights
age_weights <- dnorm(my_group$age, mean = target_mean_age, sd = target_age_sd)
age_weights <- age_weights / sum(age_weights)  # Normalize so they sum to 1

###### App usage weights ########
# Duration for more than 12 months
duration_12_weights <- as.numeric(my_group$rep_relation_mos) > 12
# Set the weights to the target proportion if they are 1, otherwise set them to 1 - target proportion
duration_12_weights <- ifelse(duration_12_weights == 1, target_duration_12_prop, 1 - target_duration_12_prop)
duration_12_weights <- 4 * (duration_12_weights / sum(duration_12_weights))  # Normalize so they sum to 1

bachelor_weights <- my_group$edu == 4
bachelor_weights <- ifelse(bachelor_weights, bachelor_prop, 1 - bachelor_prop)
bachelor_weights <- (bachelor_weights / sum(bachelor_weights))  # Normalize so they sum to 1

master_weights <- my_group$edu == 5
master_weights <- ifelse(master_weights, master_prop, 1 - master_prop)
master_weights <- (master_weights / sum(master_weights))  # Normalize so they sum to 1

gender_weights <- my_group$gender == 1
gender_weights <- ifelse(gender_weights, gender_prop, 1 - gender_prop)
gender_weights <- (gender_weights / sum(gender_weights))  # Normalize so they sum to 1

# Combine the weights
# Since we cannot guarantee exact proportions, we are creating a compromise by multiplying the weights together.
combined_weights <- age_weights * duration_12_weights * gender_weights * master_weights  * bachelor_weights

combined_weights <- combined_weights / sum(combined_weights)  # Normalize so they sum to 1

#---------------- Now we can sample from the data frame using the weights ----------------#

if(TRUE) {
n_iter <- 10000  # Number of iterations to run

# Vectorized conditions to avoid repetitive as.numeric conversions and repeated calculations
my_group$rep_relation_mos <- as.numeric(my_group$rep_relation_mos)

set.seed(111)

mean_diffs_all <- c()

# Iterate through all possible sample sizes, in order to find the biggest sample size that passes all tests
for(N in 50:30) {
  n_size <- N
  print(paste0("--- Sample size: ", n_size))

  all_age <- c(); all_duration_12_prop <- c(); all_duration_9_12_prop <- c(); all_duration_5_8_prop <- c(); 
  all_duration_1_4_prop <- c(); all_duration_lessthan1_prop <- c(); passed_tests <- c(); indices <- c();
  all_bachelors_prop <- c(); all_master_prop <- c()


  # Store which tests failed
  failed_tests <- c()

  for(i in 1:n_iter) { # Iterate through the sampling process
    # Randomly sample from the data frame
    sample_indices <- sample(nrow(my_group), size = n_size, replace = FALSE, prob = combined_weights)
    sampled_df <- my_group[sample_indices, ]

    indices <- append(indices, list(sample_indices))

    # Age
    all_age <- c(all_age, mean(na.omit(as.numeric(sampled_df$age))))

    # App usage
    all_duration_12_prop <- c(all_duration_12_prop, sum(sampled_df$rep_relation_mos > 12) / n_size)
    all_duration_9_12_prop <- c(all_duration_9_12_prop, sum(sampled_df$rep_relation_mos >= 9 & sampled_df$rep_relation_mos <= 12) / n_size)
    all_duration_5_8_prop <- c(all_duration_5_8_prop, sum(sampled_df$rep_relation_mos >= 5 & sampled_df$rep_relation_mos <= 8) / n_size)
    all_duration_1_4_prop <- c(all_duration_1_4_prop, sum(sampled_df$rep_relation_mos >= 1 & sampled_df$rep_relation_mos <= 4) / n_size)
    all_duration_lessthan1_prop <- c(all_duration_lessthan1_prop, sum(sampled_df$rep_relation_mos < 1) / n_size)
    
    # Education
    all_bachelors_prop <- c(all_bachelors_prop, sum(sampled_df$edu == 4) / n_size)
    all_master_prop <- c(all_master_prop, sum(sampled_df$edu == 5) / n_size)

    # Check if it passes the tests
    # More than 12 months
    test1 <- prop.test(c(sum(sampled_df$rep_relation_mos > 12), 19), c(n_size, 66)) # More than 12 months
    test2 <- prop.test(c(sum(sampled_df$rep_relation_mos >= 9 & sampled_df$rep_relation_mos <= 12), 7), c(n_size, 66)) # Between 9 and 12 months
    test3 <- prop.test(c(sum(sampled_df$rep_relation_mos >= 5 & sampled_df$rep_relation_mos <= 8), 7), c(n_size, 66)) # Between 5 and 8 months
    test4 <- prop.test(c(sum(sampled_df$rep_relation_mos >= 1 & sampled_df$rep_relation_mos <= 4), 15), c(n_size, 66)) # Between 1 and 4 months

    # Age
    test5 <- t.test(c(na.omit(as.numeric(sampled_df$age))), mu = 32.64)

    # Gender
    test6 <- prop.test(c(36, sum(sampled_df$gender == 1)), c(66, n_size)) # Gender

    # Education
    test7 <- prop.test(c(15, sum(sampled_df$edu == 4)), c(66, n_size)) # Bachelor's degree
    test8 <- prop.test(c(7, sum(sampled_df$edu == 5)), c(66, n_size)) # Masters degree    

    #test7$p.value > 0.1 & test8$p.value > 0.1
    # If all tests pass, we are done
    if(test1$p.value > 0.1 & test2$p.value > 0.1 & test3$p.value > 0.1 & test5$p.value > 0.1 & test6$p.value > 0.1
    & test7$p.value > 0.1 & test8$p.value > 0.1) {
      passed_tests <- c(passed_tests, 1)
    } else { 
      passed_tests <- c(passed_tests, 0)

      # Check each test and add the name of the test to failed_tests

      if(test1$p.value <= 0.1) { failed_tests <- c(failed_tests, "test1") }
      if(test2$p.value <= 0.1) { failed_tests <- c(failed_tests, "test2") }
      if(test3$p.value <= 0.1) { failed_tests <- c(failed_tests, "test3") }
      if(test4$p.value <= 0.1) { failed_tests <- c(failed_tests, "test4") }
      if(test5$p.value <= 0.1) { failed_tests <- c(failed_tests, "test5") }
      if(test6$p.value <= 0.1) { failed_tests <- c(failed_tests, "test6") }
      if(test7$p.value <= 0.1) { failed_tests <- c(failed_tests, "test7") }
      if(test8$p.value <= 0.1) { failed_tests <- c(failed_tests, "test8") }

    }
  }

  print(table(failed_tests))

  rnum <- (1:n_iter)
  df_all_samples <- data.frame(all_duration_12_prop, 
                          all_duration_1_4_prop, 
                          all_duration_lessthan1_prop, 
                          all_duration_5_8_prop, 
                          all_duration_9_12_prop, 
                          all_age,
                          passed_tests,
                          rnum,
                          all_bachelors_prop,
                          all_master_prop)

  # Only select the ones that passed all tests
  df_all_samples <- df_all_samples[df_all_samples$passed_tests == 1, ]

  # If there is at least 1 sample that passed all tests, we are done
  if(nrow(df_all_samples) > 0) {
    # If more than one, select the one with the smallest difference from the target values
    # Create a column that shows the cumulative difference from the target values
    df_all_samples$diff <- abs(df_all_samples$all_age - target_mean_age) / target_mean_age + 
                      abs(df_all_samples$all_duration_12_prop - target_duration_12_prop) + 
                      abs(df_all_samples$all_duration_9_12_prop - target_duration_9_12_prop) +
                      abs(df_all_samples$all_duration_5_8_prop - target_duration_5_8_prop) +
                      abs(df_all_samples$all_duration_1_4_prop - target_duration_1_4_prop) +
                      abs(df_all_samples$all_duration_lessthan1_prop - target_duration_lessthan1_prop) +
                      abs(df_all_samples$all_bachelors_prop - bachelor_prop) +
                      abs(df_all_samples$all_master_prop - master_prop) # Master's degree proportion

    # Store mean diff
    mean_diffs_all <- c(mean_diffs_all, mean(df_all_samples$diff))

    # Find the row number with the smallest difference
    if (nrow(df_all_samples) == 1) { min_row <- df_all_samples[1,'rnum'] } else { min_row <- df_all_samples[which.min(df_all_samples$diff), 'rnum'] }
    min_indices <- indices[[min_row]]
    sampled_df <- my_group[min_indices, ]

    print(paste0("Done! ", "Final sample size: ", n_size, " Rownum: ", min_row))
    break
  }
}

##### Uncomment this line if you want to replicate the results with this sample
#d <- sampled_df
 
}