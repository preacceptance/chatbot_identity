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
               'cSEM','seminr', 'interactions', 'dplyr', 'effsize')

### Read the data
d <- read.csv('./data.csv')

# Remove NA rows
d <- d[d$Finished == 1, ]

################################## Exclusions ##################################

# Print IDs of failed attention check participants
print(paste0("IDS of participants failed attention check: ", paste(d[d$att_1 != 2 | d$att_2 != 2, 'prolific_id'], collapse = ", ")))


# Attention check
d <- d[d$att_1 == 2 & d$att_2 == 2,]

# Exclude duplicates
d <- d[d$Q_RelevantIDDuplicate != "true",]

print(paste0("Number of participants hired: ", nrow(d)))

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
  if (grepl('mourn', col) | grepl('disap', col) | grepl('freq', col) | grepl('validated_grief', col)) {
    d[[col]] <- as.numeric(d[[col]])
  }
}

colnames(d)

d$mourn_old <- (d$mourn_old_1_1 + d$mourn_old_2_1) / 2
d$mourn_new <- (d$validated_grief_1_1 + d$validated_grief_2_1 + d$validated_grief_3_1) / 3

d$disap_old <- (d$disap_1_old_1 + d$disap_2_old_1) / 2
d$disap_2_1 <- 100 - d$disap_2_1  # reverse code
d$disap_4_1 <- 100 - d$disap_4_1  # reverse code
d$disap_new <- (d$disap_1_1 + d$disap_2_1 + d$disap_3_1 + d$disap_4_1) / 4

cor.test(d$mourn_old, d$mourn_new, method = "pearson")
cor.test(d$disap_old, d$disap_new, method = "pearson")

# Cronbach's alpha for all scales
cronbach.alpha(d[, c('mourn_old_1_1', 'mourn_old_2_1')])
cronbach.alpha(d[, c('validated_grief_1_1', 'validated_grief_2_1', 'validated_grief_3_1')])
cronbach.alpha(d[, c('disap_1_old_1', 'disap_2_old_1')])
cronbach.alpha(d[, c('disap_1_1', 'disap_2_1', 'disap_3_1', 'disap_4_1')])

# Exploratory factor analysis using all items to see if they load on separate factors
fa_data <- d[, c('mourn_old_1_1', 'mourn_old_2_1',
                 'validated_grief_1_1', 'validated_grief_2_1', 'validated_grief_3_1',
                 'disap_1_old_1', 'disap_2_old_1',
                 'disap_1_1', 'disap_2_1', 'disap_3_1', 'disap_4_1')]

fa_result <- factanal(fa_data, factors = 2, rotation = "promax")
print(fa_result, digits = 2, cutoff = 0.3, sort = TRUE)