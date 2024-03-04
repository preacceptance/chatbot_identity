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
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'effsize',         # another effect size package
               'LDAvis',          # Topic model visualization
               'tm',
               'lda',
               'ltm',
               'servr',
               'wordcloud',
               'vader',
               'dplyr',
               'topicmodels',
               'gridExtra',
               'textstem',
               'syuzhet',
               'ggpubr',
               'rstatix',
               'textstem', 'ggplot2','tidytext','quanteda', 'interactions',
               'seminr', 'lavaan', 'semTools', 'tidyverse',
               'sjPlot', 'corrr', 'ggcorrplot', 'FactoMineR', 'factoextra', 'lpSolve')
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

n_final <- dim(d)[1]
n_final 
percent_excluded <- (n_original - n_final)/n_original 
percent_excluded

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
table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
table(d$gender)[2]/sum(table(d$gender)) ## percentage of females
table(d$gender)[3]/sum(table(d$gender)) ## percentage of not disclosed
table(d$gender)[4]/sum(table(d$gender)) ## percentage of others

## user ethnicity
table(d$ethnicity)

sum(d$ethnicity == "1") / dim(d)[1] ## percentage of blacks
sum(d$ethnicity == "2") / dim(d)[1] ## percentage of asians
sum(d$ethnicity == "3") / dim(d)[1] ## percentage of whites
sum(d$ethnicity == "4") / dim(d)[1] ## percentage of hispanics or latinos
sum(d$ethnicity == "5") / dim(d)[1] ## percentage of mixed
sum(d$ethnicity == "7") / dim(d)[1] ## percentage of not disclosed
sum(d$ethnicity == "6") / dim(d)[1] ## percentage of others

## user education
table(d$edu)

100 * sum(d$edu == "9") / dim(d)[1] # Percentage of no education
100 * sum(d$edu == "1") / dim(d)[1] # Percentage of high school or equivalent
100 * sum(d$edu == "2") / dim(d)[1] # Percentage of vocational school
100 * sum(d$edu == "10") / dim(d)[1] # Percentage of associate's degree
100 * sum(d$edu == "11") / dim(d)[1] # Percentage of bachelor's degree
100 * sum(d$edu == "12") / dim(d)[1] # Percentage of master's degree
100 * sum(d$edu == "7") / dim(d)[1] # Percentage of graduate degree (PhD, MD, JD, etc.)
100 * sum(d$edu == "8") / dim(d)[1] # Percentage of other


## user relationship
table(d$relation_status)

sum(d$relation_status == "1") / dim(d)[1] # Percentage of single
sum(d$relation_status == "7") / dim(d)[1] # Percentage of in relationship
sum(d$relation_status == "2") / dim(d)[1] # Percentage of married
sum(d$relation_status == "3") / dim(d)[1] # Percentage of divorced
sum(d$relation_status == "4") / dim(d)[1] # Percentage of separated
sum(d$relation_status == "5") / dim(d)[1] # Percentage of widowed
sum(d$relation_status == "8") / dim(d)[1] # Percentage of other

## user friends
mean(as.numeric(d$close_friends), trim = 0, na.rm = TRUE) ## mean number of friends
sd(as.numeric(d$close_friends), na.rm = TRUE) ## standard deviation

## replika restore
table(d$restore)[1]/sum(table(d$restore)) ## percentage did not restore
table(d$restore)[2]/sum(table(d$restore)) ## percentage did restore

## ---- Replika companion characteristics

## replika age
mean(as.numeric(d$age_rep), trim = 0, na.rm = TRUE) ## mean age 
sd(as.numeric(d$age_rep), na.rm = TRUE) ## standard deviation

## replika gender
table(d$gender_rep)
table(d$gender_rep)[1]/sum(table(d$gender_rep)) ## percentage of males
table(d$gender_rep)[2]/sum(table(d$gender_rep)) ## percentage of females
table(d$gender_rep)[3]/sum(table(d$gender_rep)) ## percentage of non-binary

## replika relationship
table(d$relationship_rep)
table(d$relationship_rep)[1]/sum(table(d$relationship_rep)) ## percentage of friends
table(d$relationship_rep)[2]/sum(table(d$relationship_rep)) ## percentage of partners
table(d$relationship_rep)[3]/sum(table(d$relationship_rep)) ## percentage of see how it goes

## replika months
mean(as.numeric(d$months_rep), trim = 0, na.rm = TRUE) ## mean number of months
sd(as.numeric(d$months_rep), na.rm = TRUE) ## standard deviation

## replika hours
mean(as.numeric(d$hours_rep), trim = 0, na.rm = TRUE) ## mean number of total hours per day 
sd(as.numeric(d$hours_rep), na.rm = TRUE) ## standard deviation

## replika subscription
table(d$subscription_rep)
table(d$subscription_rep)[1]/sum(table(d$subscription_rep)) ## percentage of none
table(d$subscription_rep)[2]/sum(table(d$subscription_rep)) ## percentage of monthly 
table(d$subscription_rep)[3]/sum(table(d$subscription_rep)) ## percentage of yearly
table(d$subscription_rep)[4]/sum(table(d$subscription_rep)) ## percentage of lifetime

## replika experience
mean(as.numeric(d$exp_level_rep), trim = 0, na.rm = TRUE) ## mean level
sd(as.numeric(d$exp_level_rep), na.rm = TRUE) ## standard deviation

mean(as.numeric(d$months_rep))
sd(as.numeric(d$months_rep))

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


corrplot(cor(d[c("age", "value_1", "refund_1", "petition_1", "cold_1", "mourn_1", "mental_health_1", 
                   "well_m", "autonomy_m", "essence_1", "invest_1")],
              method = "pearson", use = "pairwise.complete.obs"), type = "upper", order = "hclust",
         col = brewer.pal(n = 4, name = "RdYlBu"))


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
d$invest_mc <- d$invest_1 - mean(d$invest_1, na.rm = TRUE)
d$autonomy_mc <- d$autonomy_m - mean(d$autonomy_m, na.rm = TRUE)
d$essence_mc <- d$essence_1 - mean(d$essence_1, na.rm = TRUE)

run_model <- function(dependent_var) {
  print(paste0("*-*-*-*-*-*", dependent_var, "*-*-*-*-*-*"))
  formula <- as.formula(paste(dependent_var, "~ essence_mc * (invest_mc + autonomy_mc)"))
  model <- lm(formula, data = d)
  print(summary(model))
}

variables <- c("cold_1", "mourn_1", "mental_health_1", "well_m", "value_1", "refund_1", "petition_1")
lapply(variables, run_model)

# Interaction Plots
# Plot the interaction effect of invest_mc and essence_mc on value_1
mi1 <- lm(cold_1 ~ essence_1 * (invest_1 + autonomy_m), data = d)
i_cold <- interact_plot(mi1, pred = essence_1, modx = invest_1, interval = TRUE, 
              plot.points = TRUE, point.args = list(size = 6, alpha = 0.5)) +
              theme(legend.position = "none")

mi2 <- lm(well_m ~ essence_1 * (invest_1 + autonomy_m), data = d)
i_well <- interact_plot(mi2, pred = essence_1, modx = invest_1, interval = TRUE, 
              plot.points = TRUE, point.args = list(size = 6, alpha = 0.5)) +
              theme(legend.position = "none")

mi3 <- lm(value_1 ~ essence_1 * (invest_1 + autonomy_m), data = d)
i_value <- interact_plot(mi3, pred = essence_1, modx = invest_1, interval = TRUE, 
              plot.points = TRUE, point.args = list(size = 6, alpha = 0.5)) +
              theme(legend.position = "none")


# Combine all plots into a single plot
pdf("interaction_plots.pdf", width=9, height=4.5)
grid.arrange(i_cold, i_well, i_value, ncol = 3)
dev.off()

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


## ================================================================================================================
##                                              DATA ANALYSIS - MANUAL RATINGS                
## ================================================================================================================

identity <- data.frame(X2=na.omit(d$rater_2_identity), X1=na.omit(d$rater_1_identity))
devaluation <- data.frame(X2=na.omit(d$rater_2_devaluation), X1=na.omit(d$rater_1_devaluation))
mourning <- data.frame(X2=na.omit(d$rater_2_mourning), X1=na.omit(d$rater_1_mourning))

# calculate cronbach's alpha
cronbach.alpha(identity)
cronbach.alpha(devaluation)
cronbach.alpha(mourning)

# IDENTITY
mean(na.omit(d[d$rater_2_identity == d$rater_1_identity, 'rater_1_identity']))

# DEVALUATION
mean(na.omit(d[d$rater_2_devaluation == d$rater_1_devaluation, 'rater_1_devaluation']))

# MOURNING
mean(na.omit(d[d$rater_2_mourning == d$rater_1_mourning, 'rater_1_mourning']))


table(d[(d$rater_2_identity == d$rater_1_identity) & d$rater_1_identity == 1, 'rater_1_identity_change_type'])
table(d[(d$rater_2_devaluation == d$rater_1_devaluation) & d$rater_1_devaluation == 1, 'rater_1_devaluation_type'])
table(d[(d$rater_2_mourning == d$rater_1_mourning) & d$rater_1_mourning == 1, 'rater_1_mourning_type'])

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
