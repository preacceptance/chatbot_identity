
## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # cronbach's alpha
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'lsmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'nlme',            # get p values for mixed effect model
               'DescTools',       # get Cramer's V
               'LDAvis',          # Topic model visualization
               'tm',
               'lda',
               'servr',
               'wordcloud',
               'vader',
               'topicmodels',
               'gridExtra',
               'syuzhet',
               'ggpubr',
               'rstatix',
               'textstem',
               'quanteda',
               'reticulate',
               'scales',
               'hash',
               'dplyr',
               'tidytext',
               'parallel',
               'lsr'
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

################# READING THE DATA #################

d_all <- hash()

######################### Read comments #########################

# For Replika
d_1 <- read.csv('./data/replika/comments_2023_01.csv', sep = ",")  
d_2 <- read.csv('./data/replika/comments_2023_02.csv', sep = ",")  

#d_2$expression_asset_data <- NULL
#d_1$editable <- NULL
  
# Combine the two dataframes
d <- rbind(d_1, d_2)
print(paste0("Number of comments: ", nrow(d)))

# Load the arrays in 'sentiment' column. The first object in the python array is the sentiment label and the second one is sentiment score
d$sentiment_label <- sapply(d$sentiment, function(x) {reticulate::py_eval(x)[[1]]})
d$sentiment_score <- sapply(d$sentiment, function(x) {reticulate::py_eval(x)[[2]]})
d$sentiment <- sapply(d$sentiment, function(x) {reticulate::py_eval(x)[[1]]})

print(paste0("Number of comments in January: ", nrow(d_1)))
print(paste0("Number of comments in February: ", nrow(d_2)))

d_all[['comments']] <- d

######################### Read posts #########################

# For Replika
d_1 <- read.csv('./data/replika/posts_2023_01.csv', sep = ",")  
d_2 <- read.csv('./data/replika/posts_2023_02.csv', sep = ",")

d_2$crosspost_parent <- NULL
d_2$crosspost_parent_list <- NULL

# Combine the two dataframes
d <- rbind(d_1, d_2)

print(paste0("Number of posts: ", nrow(d)))

# Load the arrays in 'sentiment' column. The first object in the python array is the sentiment label and the second one is sentiment score
d$sentiment_label <- sapply(d$sentiment, function(x) {reticulate::py_eval(x)[[1]]})
d$sentiment_score <- sapply(d$sentiment, function(x) {reticulate::py_eval(x)[[2]]})
d$sentiment <- sapply(d$sentiment, function(x) {reticulate::py_eval(x)[[1]]})

print(paste0("Number of posts in January: ", nrow(d_1)))
print(paste0("Number of posts in February: ", nrow(d_2)))

d_all[['posts']] <- d

# Get number of all sadness posts after update
print(paste0("Number of sadness posts after the update: ", sum(d_all[['posts']]$emotion == "sadness" & d_all[['posts']]$sentiment == "negative")))
# Get number of unique users who posted sadness posts after the update
print(paste0("Number of unique users who posted sadness posts after the update: ", length(unique(d_all[['posts']][d_all[['posts']]$emotion == "sadness" & d_all[['posts']]$sentiment == "negative", 'author']))))
      


# Randomly select and save posts from after the ERP update that are sadness with negative sentiment
# First remove posts with selftext = "[removed]" and "[deleted]"
# Then, remove those with selftext = "" or NA
# Finally, randomly select 100 posts
set.seed(123)
d_after_erp <- d_all[['posts']][d_all[['posts']]$before_erp_removal == "False" & d_all[['posts']]$emotion == "sadness" & d_all[['posts']]$sentiment == "negative", ]
d_after_erp <- d_after_erp[!d_after_erp$selftext %in% c("[removed]", "[deleted]"), ]
d_after_erp <- d_after_erp[!is.na(d_after_erp$selftext) & d_after_erp$selftext != "", ]
d_after_erp <- d_after_erp[sample(nrow(d_after_erp), 100), ]

write.csv(d_after_erp, './data/replika/posts_after_erp_sample.csv', row.names=FALSE)

######################### Descriptive Stats #########################

# How many distinct users are there before the ERP update?
print(paste0("Number of distinct users before the ERP update: ", length(unique(d_all[['posts']][d_all[['posts']]$before_erp_removal == "True", 'author']))))

# How many distinct users are there after the ERP update?
print(paste0("Number of distinct users after the ERP update: ", length(unique(d_all[['posts']][d_all[['posts']]$before_erp_removal == "False", 'author']))))

# Get the number of users in common before and after the ERP update
print(paste0("Number of users in common before and after the ERP update: ", length(intersect(unique(d_all[['posts']][d_all[['posts']]$before_erp_removal == "True", 'author']), unique(d_all[['posts']][d_all[['posts']]$before_erp_removal == "False", 'author'])))))

# How many distinct users are there in total?
print(paste0("Number of distinct users in total: ", length(unique(d_all[['posts']][, 'author']))))

################################################################################

# Same for comments
# How many distinct users are there before the ERP update?
print(paste0("Number of distinct users before the ERP update: ", length(unique(d_all[['comments']][d_all[['comments']]$before_erp_removal == "True", 'author']))))

# How many distinct users are there after the ERP update?
print(paste0("Number of distinct users after the ERP update: ", length(unique(d_all[['comments']][d_all[['comments']]$before_erp_removal == "False", 'author']))))

# Get the number of users in common before and after the ERP update
print(paste0("Number of users in common before and after the ERP update: ", length(intersect(unique(d_all[['comments']][d_all[['comments']]$before_erp_removal == "True", 'author']), unique(d_all[['comments']][d_all[['comments']]$before_erp_removal == "False", 'author'])))))

# How many distinct users are there in total?
print(paste0("Number of distinct users in total: ", length(unique(d_all[['comments']][, 'author']))))

for(ctype in c('posts')) { # 'comments'
  print(paste0("*-*-*-*-* ", ctype, " *-*-*-*-*"))
  d <- d_all[[ctype]]
  
  print(paste0("Number of authors: ", length(unique(d$author))))
  
  # Percentage of authors who are premium reddit users
  # Find how many of the unique authors use premium
  premium_users <- unique(d[d$author_premium == 1, 'author'])
  print(paste0("Percentage of authors who are premium reddit users: ", length(premium_users) / length(unique(d$author)) * 100))
  
  ## Calculate average word count:
  d$word_count <- sapply(d$titlencontent, function(x) length(unlist(strsplit(x, "\\W+"))))
  print(paste0("Word count - mean: ", mean(d$word_count), ' sd: ', sd(d$word_count)))
  print(paste0("More than 400 words: ", sum(d$word_count > 400)))
  
  # Print the percentage of each emotion before and after the ERP update. Columns 'before_erp_removal', 'sentiment_label', 'emotion'
  d_before_erp <- d[d$before_erp_removal == "True", ]
  d_after_erp <- d[d$before_erp_removal == "False", ]
  
  print(paste0("Dimensions before: ", dim(d_before_erp)[1]))
  print(paste0("Dimensions after: ", dim(d_after_erp)[1]))
}

## =============================================================================
##                             DATA ANALYSIS
## =============================================================================

######################### Descriptive Stats #########################

for(ctype in c('posts')) { # 'comments'
  print(paste0("*-*-*-*-* ", ctype, " *-*-*-*-*"))
  d <- d_all[[ctype]]
  
  print(paste0("Number of authors: ", length(unique(d$author))))
  
  # Percentage of authors who are premium reddit users
  # Find how many of the unique authors use premium
  premium_users <- unique(d[d$author_premium == 1, 'author'])
  print(paste0("Percentage of authors who are premium reddit users: ", length(premium_users) / length(unique(d$author)) * 100))
  
  ## Calculate average word count:
  d$word_count <- sapply(d$titlencontent, function(x) length(unlist(strsplit(x, "\\W+"))))
  print(paste0("Word count - mean: ", mean(d$word_count), ' sd: ', sd(d$word_count)))
  print(paste0("More than 400 words: ", sum(d$word_count > 400)))
  
  # Print the percentage of each emotion before and after the ERP update. Columns 'before_erp_removal', 'sentiment_label', 'emotion'
  d_before_erp <- d[d$before_erp_removal == "True", ]
  d_after_erp <- d[d$before_erp_removal == "False", ]
  
  print(paste0("Dimensions before: ", dim(d_before_erp)[1]))
  print(paste0("Dimensions after: ", dim(d_after_erp)[1]))
}


for(ctype in c('posts')) { #, 'comments'
  print(paste0("*-*-*-*-* ", ctype, " *-*-*-*-*"))

  d <- d_all[[ctype]]
  d_before_erp <- d[d$before_erp_removal == "True", ]
  d_after_erp <- d[d$before_erp_removal == "False", ]

  #### Emotion bar plots ####
  for(t in c('before_erp_removal', 'after_erp_removal')) {
    png(paste0("./images/replika/emotions_", t, "_", ctype, ".png"), res = 300, width=2100, height=2000)

    title <- paste0("Emotions in Reddit ", str_to_title(ctype), "\n")
    if(t == 'before_erp_removal') { 
      d_curr <- d_before_erp
      title <- paste0(title, " (Before Announcement)")
    } else { 
      d_curr <- d_after_erp 
      title <- paste0(title, " (After Announcement)")
    }

    # Capitalize emotions
    d_curr$emotion_capt <- sapply(d_curr$emotion, function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))

    sorted_d <- sort(table(d_curr$emotion_capt) / sum(table(d_curr$emotion_capt))); print(sorted_d)
    par(mar=c(5,6,4,2)+.1)

    sorted_d <- sorted_d[-which(names(sorted_d) == "Neutral")] # Remove Neutral
    barplot(
      sorted_d, 
      horiz = TRUE, 
      las = 1, 
      main = title, xlab="Proportion",
      cex.main = 1.5, cex.axis=1.5, cex.names=1.5, cex.lab = 1.5, 
      xlim = c(0, 0.25)
    )

    dev.off()
  }

  d$date <- substr(d$created_date, 1, 10)
  save_str <- str_to_title(ctype)

  # Make a dataframe with d$date as one column, and number of positive, negative, neutral posts as other columns
  df_days <- data.frame(date = unique(d$date))
  
  # First calculate the counts for sentiment_label in one go
  sentiment_counts <- d %>%
    group_by(date, sentiment_label) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = sentiment_label, values_from = n, values_fill = list(n = 0)) %>%
    rename(n_positive = positive, n_negative = negative, n_neutral_sentiment = neutral)
  
  # Then calculate the counts for emotion in one go
  emotion_counts <- d %>%
    group_by(date, emotion) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = emotion, values_from = n, values_fill = list(n = 0)) %>%
    rename(n_anger = anger, n_disgust = disgust, n_fear = fear, n_joy = joy, n_sadness = sadness, n_surprise = surprise, n_neutral_emotion = neutral)
  
  df_days <- merge(df_days, sentiment_counts, by="date")
  df_days <- merge(df_days, emotion_counts, by="date")
  df_days$date_var <- as.Date(df_days$date, format = "%Y-%m-%d") #%Y-

  # Percentage of negative posts before the ERP update:
  before_neg <- sum(d_before_erp$sentiment == "negative")
  before_all <- dim(d_before_erp)[1]
  print(paste0("Percentage of negative posts before: ", 100 * before_neg / before_all))

  ############################ T-TESTS - Before vs. After ############################

  lower = 33; upper = 59

  # Compare number of positive and negative posts before and after the update
  # Before the update
  print("*-*-*-* Comparing negative v. positive before  *-*-*-*")
  x <- df_days$n_negative[1:lower]
  y <- df_days$n_positive[1:lower]
  vart <- var.test(x, y)
  tt <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > 0.05)
  print(tt)
  cohen.d(x, y)

  # After the update
  print("*-*-*-* Comparing negative v. positive after *-*-*-*")
  x <- na.omit(df_days$n_negative[lower+1:upper])
  y <- na.omit(df_days$n_positive[lower+1:upper])
  vart <- var.test(x, y)
  tt <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > 0.05)
  print(tt)
  cohen.d(x, y)

  # Compare the percentage of negative posts after each day, vs. before the update
  count <- 0
  
  # Create prop_results dataframe with 'result' and 'date' columns
  prop_results <- data.frame(result = c(), date = c())

  for(day in unique(df_days$date)) {
    count <- count + 1

    if (count > 34) {
      print(paste0("Day: ", day))
      curr_day <- df_days[df_days$date == day, ]
      after_neg <- curr_day$n_negative
      after_all <- sum(curr_day$n_negative + curr_day$n_positive + curr_day$n_neutral_sentiment)

      print(paste0("Percentage of negative posts after: ", 100 * after_neg / after_all))
      # Proporion test between before vs. after
      p_result <- prop.test(c(before_neg, after_neg), c(before_all, after_all))

      get_stars(p_result$p.value)
      result_str <- paste0("%_After = ", round(100 * after_neg / after_all, 2), 
                          ", X^2(1, N=3072 + ", after_all, ") = ", 
                          round(p_result$statistic, 2), ", ",
                          ifelse(p_result$p.value < .001, "p < .001", paste0("p = ", round(p_result$p.value, 3))))

      prop_results[count - 34, 'result'] <- result_str
      prop_results[count - 34, 'date'] <- day
    }
  }

  write.table(prop_results, sep=";", paste0("./data/replika_prop_results_", ctype, ".csv"), row.names=FALSE)


  # Joy vs. all other emotions before and after the update
  for(t in c('before_erp_removal', 'after_erp_removal')) {
    # t-tests for each emotion v. joy
    for(emotion in c('anger', 'disgust', 'fear', 'sadness', 'surprise', 'neutral_emotion')) {
      print(paste0(t, "  *********** ", emotion, " v. joy ***********"))
      if(t == 'before_erp_removal') { 
        x <- df_days[, paste0("n_", emotion)][1:lower]
        y <- df_days$n_joy[1:lower]
      } else { 
        x <- na.omit(df_days[, paste0("n_", emotion)][lower+1:upper])
        y <- na.omit(df_days$n_joy[lower+1:upper])
      }

      vart <- var.test(x, y)
      tt <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > 0.05); print(tt)
      print(cohen.d(x, y))
    }
  }

  # Compare sadness to all other emotions
  # t-tests for each emotion v. joy
  for(emotion in c('anger', 'disgust', 'fear', 'surprise', 'neutral_emotion')) {
    print(paste0("-------- ", ctype, " *-*-* ", t, "  *********** ", emotion, " v. sadness ***********"))

    x <- na.omit(df_days[, paste0("n_", emotion)][lower+1:upper])
    y <- na.omit(df_days$n_sadness[lower+1:upper])

    vart <- var.test(x, y)
    tt <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > 0.05); print(tt)
    print(cohen.d(x, y))
  }

  # Compare number of total posts before vs. after the update
  print("*-*-*-* Comparing total posts before v. after *-*-*-*")
  x <- na.omit(df_days$n_positive[lower+1:upper] + df_days$n_negative[lower+1:upper] + df_days$n_neutral_sentiment[lower+1:upper])
  y <- df_days$n_positive[1:lower] + df_days$n_negative[1:lower] + df_days$n_neutral_sentiment[1:lower]
  vart <- var.test(x, y)
  tt <- t.test(x, y, paired = FALSE, var.equal = vart$p.value > 0.05);
  print(tt)
  print(cohen.d(x, y))


  ############################ LINE PLOTS ############################
  
  #df_days$negative_rate <- df_days$n_negative / (df_days$n_positive + df_days$n_negative + df_days$n_neutral_sentiment)
  
  # Assuming d$date is properly formatted as a Date
  plt <- ggplot(df_days, aes(x = date_var)) + 
    geom_line(aes(y = n_neutral_sentiment, color = "Neutral"), size = 1) +
    geom_line(aes(y = n_positive, color = "Positive"), size = 1) +
    geom_line(aes(y = n_negative, color = "Negative"), size = 1) +
    scale_color_manual(values = c("Positive" = "green", "Negative" = "red", "Neutral" = "gray")) +
    labs(x = "Date",
        y = paste0("Number of ", save_str),
        color = "Legend") +
    scale_x_date(breaks = df_days$date_var[seq(1, dim(df_days)[1], 2)], date_labels = "%m-%d", limits = c(min(df_days$date_var), max(df_days$date_var))) +
    theme_minimal(base_size = 15) +
    theme_classic(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 12), # Adjust text size
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 20),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          plot.margin = unit(c(1, 1, 3, 1), "cm"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))
  
  plt

  # Save the plot as an image
  ggsave(paste0("./images/replika/n_conversations_sentiment_", save_str, ".pdf"), 
        width = 24, height = 20, units = "cm", dpi = 400, limitsize=FALSE)

  ############## Same image but with the emotions ##############
  ggplot(df_days, aes(x = date_var)) + 
    geom_line(aes(y = n_anger, color = "Anger"), size = 1) +
    geom_line(aes(y = n_disgust, color = "Disgust"), size = 1) +
    geom_line(aes(y = n_fear, color = "Fear"), size = 1) +
    geom_line(aes(y = n_joy, color = "Joy"), size = 1) +
    geom_line(aes(y = n_sadness, color = "Sadness"), size = 1) +
    geom_line(aes(y = n_surprise, color = "Surprise"), size = 1) +
    #geom_line(aes(y = n_neutral, color = "Neutral"), size = 1) +
    scale_color_manual(values = c("Anger" = "#FF7F7F", "Disgust" = "#7FCC7F", "Fear" = "#B57FB5",
                                  "Joy" = "#d1d166", "Sadness" = "#7F7FFF", "Surprise" = "#FFBF7F")) + #, "Neutral" = "gray"
    labs(x = "Date",
          y = paste0("Number of ", save_str),
          color = "Legend") +
    scale_x_date(breaks = df_days$date_var[seq(1, dim(df_days)[1], 2)], date_labels = "%m-%d", limits = c(min(df_days$date_var), max(df_days$date_var))) +
    theme_minimal(base_size = 15) +
    theme_classic(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 12), # Adjust text size 12
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 20),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          plot.margin = unit(c(1, 1, 3, 1), "cm"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))

  # Save the plot as an image
  ggsave(paste0("./images/replika/n_conversations_emotion_", save_str, ".pdf"), 
        width = 24, height = 20, units = "cm", dpi = 400, limitsize=FALSE)
}

################## ANALYZE THE TEXT CONTENT ##################

# Function for cleaning the posts/comments
clean_text <- function(text) {
  text <- gsub("'", "", text)  # remove apostrophes
  text <- gsub("http\\S+\\s*", "", text) # remove URLs
  text <- gsub("\\n", "", text) # remove line breaks
  text <- gsub("\\r", "", text) # remove line breaks
  text <- gsub("\\t", "", text) # remove line breaks
  text <- gsub("\\s+", " ", text) # remove extra spaces
  text <- gsub("^\\s+|\\s+$", "", text) # remove leading and trailing spaces
  text <- gsub("[[:punct:]]", " ", text) # remove punctuation
  text <- gsub("[^[:alnum:]]", " ", text) # remove non-alphanumeric characters
  text <- gsub("[[:digit:]]", "", text) # remove numbers
  text <- gsub("^[[:space:]]+", "", text) # remove whitespace at beginning of documents
  text <- gsub("[[:space:]]+$", "", text) # remove whitespace at end of documents
  text <- gsub(" +", " ", text) # remove extra spaces
  text <- tolower(text) # convert to lowercase
  
  return(text)
}

prepare_text <- function(data_prepare) { # Prepare the text for text analysis
  # Clean the data
  explanations <- data_prepare[, 'titlencontent']
  explanations <- clean_text(explanations)

  # Split each post to words
  doc.list <- strsplit(explanations, "[[:space:]]+") 

  # Lemmatize words in doc.list using VADER
  doc.list <- lapply(doc.list, function(x) {
    x <- as.character(x)
    x <- lemmatize_words(x)
    return(x)
  })

  # compute the table of terms:
  term.table <- table(unlist(doc.list))
  term.table <- sort(term.table, decreasing = TRUE)

  # remove terms that are stop words
  stop_words <- stopwords("SMART")
  common_words <- c("replika", "erp", "rep", "replikas", "erps", "reps", "people", "ai", "https", "im", "dont", "app", "users", "make", "things", "", "www")
  del <- names(term.table) %in% stop_words | names(term.table) %in% common_words
  term.table <- term.table[!del]
  vocab <- names(term.table)

  # now put the documents into the format required by the lda package:
  get.terms <- function(x) {
    index <- match(x, vocab)
    index <- index[!is.na(index)]
    rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  }

  documents <- lapply(doc.list, get.terms)

  # Compute some statistics related to the data set:
  D <- length(documents)  # number of documents
  W <- length(vocab)  # number of terms in the vocab
  doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
  N <- sum(doc.length)  # total number of tokens in the data
  term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus


  # Return all these objects (i.e., explanations, D, W, doc.length, doc.list, N, term.frequency, vocab, term.table)
  return(list(explanations = explanations, D = D, W = W, doc.length = doc.length, doc.list = doc.list, N = N, term.frequency = term.frequency, vocab = vocab, term.table = term.table))
}

print("Preparing text for posts before the update. Can take a while.")
d_all[['posts_text_before']] <- prepare_text(d_all[['posts']][d_all[['posts']]$before_erp_removal == "True", ])

print("Preparing text for posts after the update. Can take a while.")
d_all[['posts_text_after']] <- prepare_text(d_all[['posts']][d_all[['posts']]$before_erp_removal == "False", ])

print("Preparing text for comments before the update. Can take a while.")
d_all[['comments_text_before']] <- prepare_text(d_all[['comments']][d_all[['comments']]$before_erp_removal == "True", ])

print("Preparing text for comments after the update. Can take a while.")
d_all[['comments_text_after']] <- prepare_text(d_all[['comments']][d_all[['comments']]$before_erp_removal == "False", ])

# Create dataframes for plotting the marketing relevant terms
# Creates a dataframe that has the emotions as columns and terms as rows. Cells contain number of occurences
create_emotions_dataset <- function(data, W, term.table, doc.list) {
  emotions <- data.frame(anger = rep(0, W), 
                         disgust = rep(0, W), 
                         fear = rep(0, W), 
                         joy = rep(0, W), 
                         neutral = rep(0, W), 
                         sadness = rep(0, W), 
                         surprise = rep(0, W))
  
  emotions$term <- names(term.table)
  
  # Now iterate through each doc in doc.list, and add the emotions of that doc for all words in that doc into the emotions df
  for(i in 1:length(doc.list)) {
    doc <- doc.list[[i]]
    
    # Increment the emotion count by 1 for each word in that doc
    emotions[emotions$term %in% doc, data$emotion[i]] <- emotions[emotions$term %in% doc, data$emotion[i]] + 1
  }
  
  rownames(emotions) <- emotions$term
  emotions$term <- NULL
  return(emotions);
}

create_sentiment_dataset <- function(data, W, term.table, doc.list) {
  sentiments <- data.frame(positive = rep(0, W), 
                         negative = rep(0, W), 
                         neutral = rep(0, W))
  
  sentiments$term <- names(term.table)
  
  # Now iterate through each doc in doc.list, and add the emotions of that doc for all words in that doc into the emotions df
  # Also print out the percentage of the negative posts/comments that contain any terms from the 'relevant_terms' vector
  negative_posts <- 0
  negative_posts_with_relevant_terms <- 0
  for(i in 1:length(doc.list)) {
    doc <- doc.list[[i]]
    
    # Increment the emotion count by 1 for each word in that doc
    sentiments[sentiments$term %in% doc, data$sentiment[i]] <- sentiments[sentiments$term %in% doc, data$sentiment[i]] + 1

    if(data$sentiment[i] == "negative") {
      negative_posts <- negative_posts + 1
      if(any(doc %in% relevant_terms)) {
        negative_posts_with_relevant_terms <- negative_posts_with_relevant_terms + 1
      }
    }
  }

  print(paste("Percentage of negative posts/comments that contain any of the relevant terms:", negative_posts_with_relevant_terms / negative_posts))
  
  rownames(sentiments) <- sentiments$term
  sentiments$term <- NULL
  return(sentiments);
}

find_mhealth_posts <- function(data, W, doc.list) {
  n_mhealth_posts <- 0
  n_all <- 0
  mhealth_terms <- read.csv('revised_mhealth_dict.csv')
  for(i in 1:length(doc.list)) {
    doc <- doc.list[[i]]
    
    for(j in 1:dim(mhealth_terms)[1]) {
      term <- mhealth_terms[j, ]
      exists <- grepl(term, paste(doc, collapse = ' '), fixed = TRUE)
      if(exists) {
        n_mhealth_posts <- n_mhealth_posts + 1
        break
      }
    }
    
    n_all <- n_all + 1
  }
  
  print(paste0("Number of mhealth posts: ", n_mhealth_posts))
  print(paste0("Number of all posts: ", n_all))
}

find_mourn_posts <- function(data, W, doc.list) {
  n_mhealth_posts <- 0
  n_all <- 0
  mourn_terms <- c('loss', 'lost', 'death', 'die', 'dying', 'dead')
  emotions <- c()
  for(i in 1:length(doc.list)) {
    doc <- doc.list[[i]]
    
    for(j in 1:length(mourn_terms)) {
      term <- mourn_terms[j]
      exists <- grepl(term, paste(doc, collapse = ' '), fixed = TRUE)
      if(exists) {
        n_mhealth_posts <- n_mhealth_posts + 1
        emotions <- c(emotions, data$emotion[i])
        break
      }
    }
    
    n_all <- n_all + 1
  }
  
  print(paste0("Number of mourn posts: ", n_mhealth_posts))
  print(paste0("Number of all posts: ", n_all))
  print(table(emotions))
}

find_refund_posts <- function(data, W, doc.list) {
  n_refund_posts <- 0
  n_all <- 0
  for(i in 1:length(doc.list)) {
    doc <- doc.list[[i]]
    if('refund' %in% doc) { n_refund_posts <- n_refund_posts + 1 }
    n_all <- n_all + 1
  }
  
  print(paste0("Number of refund posts: ", n_refund_posts))
  print(paste0("Number of all posts: ", n_all))
}

# Marketing relevant terms:

# For Replika
relevant_terms <- c('luka', 'companion', 'relationship', 'eugenia', 'reject', 'rejection', 'refund', 'free', 'ban', 'company', 'brand', 'return', 'money', 'subscription', 'company', 'pro', 'model', 'business', 'free', 'marketing', 'customer', 'service', 'companies', 'feature', 'technology', 'pay', 'services', 'customers', 'sales', 'feedback', 'brand', 'purchase', 'sell', 'order', 'refund')

plot_emotion_relevant <- function(emotions_relevant, ctype, before_after, width=1500, height=1080) {
  # Create bar plots such that it will have the words in the y-axis, and the bars will show the emotion percentage for each word
  # First, calculate the emotion percentage for each word
  emotions_relevant$word_counts <- emotions_relevant$anger + emotions_relevant$disgust + emotions_relevant$fear + emotions_relevant$joy + emotions_relevant$sadness +  emotions_relevant$surprise + emotions_relevant$neutral
  emotions_relevant$anger_percentage <- emotions_relevant$anger / emotions_relevant$word_counts
  emotions_relevant$disgust_percentage <- emotions_relevant$disgust / emotions_relevant$word_counts
  emotions_relevant$fear_percentage <- emotions_relevant$fear / emotions_relevant$word_counts
  emotions_relevant$joy_percentage <- emotions_relevant$joy / emotions_relevant$word_counts
  emotions_relevant$sadness_percentage <- emotions_relevant$sadness / emotions_relevant$word_counts
  emotions_relevant$surprise_percentage <- emotions_relevant$surprise / emotions_relevant$word_counts
  emotions_relevant$neutral_percentage <- emotions_relevant$neutral / emotions_relevant$word_counts

  # Order emotions based on sum of anger, disgust, fear, and sadness
  emotions_relevant <- emotions_relevant[order(emotions_relevant$anger_percentage + emotions_relevant$disgust_percentage + emotions_relevant$fear_percentage + emotions_relevant$sadness_percentage, decreasing = FALSE), ]

  # Order emotions based on their occurrence amount
  #emotions_relevant <- emotions_relevant[order(emotions_relevant$word_counts, decreasing = FALSE), ]

  # Now plot the bar plot, such that we will have a single bar for each term, i.e., each emotion will sum up into one bar for each term
  # Bars will be horizontal, and terms will be in the y-axis
  png(paste0("./images/replika/marketing_relevant_bar_", ctype, "_", before_after, ".png"), width=width * 4, height=height * 4.5, res=550)
  par(mar = c(5, 14, 2, 2) + 0.1) # bottom, left, top, right

  names_arg <- paste(str_to_title(rownames(emotions_relevant)), " (", emotions_relevant$word_count, ")", sep = "")

  # Emotion names
  emotions = c("anger", "sadness", "fear", "disgust", "surprise", "joy", "neutral")

  # Generate bar plot
  # When plotting it, add the number of times the word occurs in parentheses, in the right side of the word
  plt <- barplot(t(as.matrix(emotions_relevant[, paste0(emotions, "_percentage")])) * 100,
                horiz = TRUE, 
                col = c("#FF7F7F", "#7F7FFF", "#B57FB5", "#7FCC7F", "#FFBF7F", "#FFFF7F", "#ebebeb"),
                las = 1,
                names.arg = names_arg,
                xlim = c(0, 100),
                xlab = "Percentage", cex.lab=1.6, cex.axis = 1.6, cex.names = 1.6)

  emotions_relevant$total <- rowSums(emotions_relevant[, paste0(emotions, "_percentage")], na.rm=TRUE)

  # Calculate mid x-axes
  emotions_relevant$anger_mid <- emotions_relevant$anger_percentage/2
  emotions_relevant$sadness_mid <- emotions_relevant$anger_percentage + (emotions_relevant$sadness_percentage/2) 
  emotions_relevant$fear_mid <- emotions_relevant$sadness_mid + emotions_relevant$sadness_percentage / 2 + (emotions_relevant$fear_percentage/2)
  emotions_relevant$disgust_mid <- emotions_relevant$fear_mid + emotions_relevant$fear_percentage / 2 + (emotions_relevant$disgust_percentage/2)
  emotions_relevant$surprise_mid <- emotions_relevant$disgust_mid + emotions_relevant$disgust_percentage / 2 + (emotions_relevant$surprise_percentage/2)
  emotions_relevant$joy_mid <- emotions_relevant$surprise_mid + emotions_relevant$surprise_percentage / 2 + (emotions_relevant$joy_percentage/2)
  emotions_relevant$neutral_mid <- emotions_relevant$joy_mid + emotions_relevant$joy_percentage / 2 + (emotions_relevant$neutral_percentage/2)

  # Select only the mid x-axes columns
  df_mid_xaxes <- emotions_relevant[, paste0(emotions, "_mid")] * 100

  # Convert the percentages matrix to string dataframe, and add % to the right of each number. If percentage is 0, replace string with empty string
  percentages <- emotions_relevant[, paste0(emotions, "_percentage")] * 100

  # Round the numbers to single decimal
  percentages <- round(percentages, 0)
  #percentages <- apply(percentages, 2, function(x) paste0(x, "%"))
  percentages[percentages == "0"] <- ""
  percentages <- data.frame(percentages)

  # Reorder the rows by rotating it upside down
  percentages <- t(percentages)

  text(t(data.matrix(df_mid_xaxes)), rep(plt, each = ncol(data.matrix(df_mid_xaxes))), labels = percentages, cex=0.9)

  if(FALSE) {
    # Generate legend
    legend(x = -10.0, y = 33.5,
          legend = c("Anger", "Sadness", "Fear", "Disgust", "Surprise", "Joy", "Neutral"),
          fill = c("#FF7F7F", "#7F7FFF", "#B57FB5", "#7FCC7F", "#FFBF7F", "#FFFF7F", "#ebebeb"),
          cex = 0.9, 
          horiz = TRUE, 
          xpd = TRUE,
          box.lty = 0)
  }

  dev.off()
}

# Plot the emotions for the marketing relevant terms and find the mentions of mhealth terms
for(ctype in c('posts')) { # 'comments'
  print(paste0("*-*-*-*-*", ctype, "*-*-*-*-*"))

  d <- d_all[[ctype]]
  d_before_erp <- d[d$before_erp_removal == "True", ]
  d_after_erp <- d[d$before_erp_removal == "False", ]

  for(t in c('before', 'after')) {
    print(paste0("*-*-*-*-* ", t, " *-*-*-*-*"))
    if (t == 'before') { d_text <- d_before_erp } else { d_text <- d_after_erp }
    W <- d_all[[paste0(ctype, "_text_", t)]][['W']]
    doc.list <- d_all[[paste0(ctype, "_text_", t)]][['doc.list']]
    term.table <- d_all[[paste0(ctype, "_text_", t)]][['term.table']]
    find_mhealth_posts(d_text, W, doc.list)
    find_mourn_posts(d_text, W, doc.list)
    find_refund_posts(d_text, W, doc.list)

    emotions <- create_emotions_dataset(d_text, W, term.table, doc.list)
    sentiments <- create_sentiment_dataset(d_text, W, term.table, doc.list)

    if(t == 'after') {
      sentiments_relevant <- sentiments[rownames(sentiments) %in% relevant_terms, ]
      emotions_relevant <- emotions[rownames(emotions) %in% relevant_terms, ]

      # Calculate sentiment score by taking the average
      sentiments_relevant$sentiment_score <- (sentiments_relevant$positive - sentiments_relevant$negative) / (sentiments_relevant$positive + sentiments_relevant$negative + sentiments_relevant$neutral)

      # Plot the emotions for the marketing relevant terms
      plot_emotion_relevant(emotions_relevant, ctype, t)
    }
  }
}

############### Proportion Tests ###############
# Mental health - Posts
prop.test(c(4, 63), c(3072, 9721))

# Mental health - Comments
prop.test(c(50, 430), c(29476, 118725))

# Refund - Posts
prop.test(c(3, 321), c(3072, 9721))

# Refund - Comments
prop.test(c(24, 2569), c(29476, 118725))

# Mourn - Posts
prop.test(c(32, 340), c(3072, 9721))

# Mourn - Comments
prop.test(c(343, 2311), c(29476, 118725))

#################### TOPIC MODELLING ####################

# Clean the data
topic_modelling = function(data_topic, check_perplexity=FALSE, comments=FALSE) {
  explanations <- data_topic[, 'titlencontent']
  explanations <- clean_text(explanations)
  
  texts = corpus(explanations)
  dfm = dfm(texts, remove=c(stopwords("SMART"), "replika", "erp", "rep", "replikas", "erps", "reps", "people", "ai", "https", "im", "dont", "app", "users", "make", "things", "", "www"))
  dtm = convert(dfm, to = "topicmodels") 
  
  train = sample(rownames(dtm), nrow(dtm) * .8)
  dtm_train = dtm[rownames(dtm) %in% train, ]
  dtm_test = dtm[!rownames(dtm) %in% train, ]
  
  if(check_perplexity) {
    ## create a dataframe to store the perplexity scores for different values of k
    p = data.frame(k = 2:20, perplexity = NA)
    
    ## define a function to calculate perplexity
    calc_perplexity <- function(k) {
      perplexity_scores = rep(NA, 10)
      for (j in 1:10) {
        print(paste0("*** ", k, ": ", j, " ***"))
        ## calculate perplexity for the given value of k
        m = LDA(dtm_train, method = "Gibbs", k = k)
        ## store result in our vector
        perplexity_scores[j] = perplexity(m, dtm_test)
      }
      ## compute the average of the perplexity scores
      return(mean(perplexity_scores))
    }
    
    # TODO: Check if this works
    ## use mclapply to apply the function in parallel
    p$perplexity = mclapply(p$k, calc_perplexity, mc.cores = detectCores())
    
    p$k <- as.numeric(p$k)
    p$perplexity <- as.numeric(p$perplexity)
    
    #### Plot the results ####
    plt <- ggplot(p, aes(x = k, y = perplexity)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      theme(text=element_text(size=14)) +
      labs(x = "Number of Topics", y = "Perplexity") + theme(axis.line = element_line(colour = "black"))
    plt
    
    sstr <- "posts"
    if(comments) {sstr <- "comments"}
    ggsave(paste0("images/replika/perplexity_", sstr,".pdf"), last_plot(), dpi = 300, width = 10, height = 5)
    while (!is.null(dev.list()))  dev.off()
  }
  
  ##### Frequency Bar Plot
  set.seed(123)
  model <- LDA(dtm, method = "Gibbs", k = 10, control=list(seed = 123))
  
  # get the terms for the first topic
  topics <- tidy(model, matrix = "beta")
  
  top_terms <- topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 15) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  if(comments) {
    top_terms <- top_terms %>%
      filter(topic %in% c(1,5,2))
  } else {
    top_terms <- top_terms %>%
      filter(topic %in% c(1, 10, 3, 4, 9))
  }
  
  fig <- top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term)) + # remove fill = factor(topic) to make all bars same color
    theme(text=element_text(size=14)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), aspect.ratio = 1, plot.title = element_text(hjust = 0.5)) +
    xlab("Topic Word Probability") +
    ylab("") +
    geom_col(show.legend = FALSE, fill = "#4f4f4f") + # set fill = "gray" in geom_col()
    facet_wrap(~ paste("Topic:", topic), scales = "free", labeller = label_parsed, ncol = 4) +
    scale_x_continuous(limits = c(0, 0.1)) + # limit x axis to the max x all bars have
    scale_y_reordered()
  
  fig
  
  # Save figure as PDF
  sstr <- "posts"
  if(comments) {sstr <- "comments"}
  ggsave(paste0("images/replika/topic_", sstr,".pdf"), last_plot(), dpi = 300, width = 15, height = 10)
  while (!is.null(dev.list()))  dev.off()
}

topic_modelling(d_all[['posts']]) # Note: Set check_perplexity to TRUE to check the perplexity (takes ~15 minutes on an M1 Pro)
topic_modelling(d_all[['comments']], comments=TRUE)

#####################  ROBUSTNESS CHECKS ######################

d_prev_years <- read.csv('./data/replika/posts_upto_2022_12_replika.csv', sep = ",")  
d_1 <- read.csv('./data/replika/posts_2023_01.csv', sep = ",")
d_2 <- read.csv('./data/replika/posts_2023_02.csv', sep = ",")
d_5 <- read.csv('./data/replika/posts_2023_05.csv', sep = ",")

# Convert "created" column, which is a UNIX timestamp, to date in the following format: "2023-01-01 00:13:24"
d_5$created_date <- as.POSIXct(d_5$created, origin="1970-01-01", tz="UTC")
d_5$created_date <- format(d_5$created_date, "%Y-%m-%d %H:%M:%S")

d_6 <- read.csv('./data/replika/posts_2023_06.csv', sep = ",")

d_6$created_date <- as.POSIXct(d_6$created, origin="1970-01-01", tz="UTC")
d_6$created_date <- format(d_6$created_date, "%Y-%m-%d %H:%M:%S")

d_7 <- read.csv('./data/replika/posts_2023_07.csv', sep = ",")

d_7$created_date <- as.POSIXct(d_7$created, origin="1970-01-01", tz="UTC")
d_7$created_date <- format(d_7$created_date, "%Y-%m-%d %H:%M:%S")

# Get common columns between d_1, d_2, and d_prev_years
common_cols <- intersect(colnames(d_1), colnames(d_2))
common_cols <- intersect(common_cols, colnames(d_prev_years))
common_cols <- intersect(common_cols, colnames(d_5))
common_cols <- intersect(common_cols, colnames(d_6))
common_cols <- intersect(common_cols, colnames(d_7))

# Get common cols only for all df's
d_1 <- d_1[, common_cols]
d_2 <- d_2[, common_cols]

d_prev_years <- d_prev_years[, common_cols]
d_5 <- d_5[, common_cols]
d_6 <- d_6[, common_cols]
d_7 <- d_7[, common_cols]

# Combine the dataframes
d <- rbind(d_prev_years, d_1, d_2, d_5, d_6, d_7)

print(paste0("Number of posts: ", nrow(d)))

# Load the arrays in 'sentiment' column. The first object in the python array is the sentiment label and the second one is sentiment score
d$sentiment_label <- sapply(d$sentiment, function(x) {reticulate::py_eval(x)[[1]]})
d$sentiment_score <- sapply(d$sentiment, function(x) {reticulate::py_eval(x)[[2]]})
d$sentiment <- sapply(d$sentiment, function(x) {reticulate::py_eval(x)[[1]]})

print(paste0("Number of posts up to 12/2022: ", nrow(d_prev_years)))
print(paste0("Number of posts in January: ", nrow(d_1)))
print(paste0("Number of posts in February: ", nrow(d_2)))

d_all[['posts']] <- d

################################################################################

# Let's first compare 2022 vs. 2023. We will compare the percentage of negative
# posts before vs. after February 3rd in 2022 vs. 2023
# Get the number of negative posts before and after February 3rd in 2022
# For this, let's look one month before Feb 3rd, and one month after Feb 3rd

d_all[['posts']]$date <- substr(d_all[['posts']]$created_date, 1, 10)

d_2022 <- d_all[['posts']][d_all[['posts']]$date >= "2022-01-01" & d_all[['posts']]$date <= "2022-02-28", ]
d_2023 <- d_all[['posts']][d_all[['posts']]$date >= "2023-01-01" & d_all[['posts']]$date <= "2023-02-28", ]

# Now, let's find diff-in-diffs

# For this, get posts before Feb 3rd and after Feb 3rd in 2022
d_2022_before <- d_2022[d_2022$date < "2022-02-03", ]
d_2022_after <- d_2022[d_2022$date >= "2022-02-03" & d_2022$date <= "2022-02-28", ]

d_2023_before <- d_2023[d_2023$date < "2023-02-03", ]
d_2023_after <- d_2023[d_2023$date >= "2023-02-03", ]

# Get the number of negative posts before and after Feb 3rd in 2022
n_negative_2022_before <- sum(d_2022_before$sentiment == "negative")
n_negative_2022_after <- sum(d_2022_after$sentiment == "negative")
n_positive_2022_before <- sum(d_2022_before$sentiment == "positive")
n_positive_2022_after <- sum(d_2022_after$sentiment == "positive")

# Get the number of negative posts before and after Feb 3rd in 2023
n_negative_2023_before <- sum(d_2023_before$sentiment == "negative")
n_negative_2023_after <- sum(d_2023_after$sentiment == "negative")
n_positive_2023_before <- sum(d_2023_before$sentiment == "positive")
n_positive_2023_after <- sum(d_2023_after$sentiment == "positive")

# Get the number of all posts before and after Feb 3rd in 2022
n_all_2022_before <- dim(d_2022_before)[1]
n_all_2022_after <- dim(d_2022_after)[1]

# Get the number of all posts before and after Feb 3rd in 2023
n_all_2023_before <- dim(d_2023_before)[1]
n_all_2023_after <- dim(d_2023_after)[1]

p_result <- prop.test(c(n_positive_2022_before, n_negative_2022_before), c(n_all_2022_before, n_all_2022_before))
p_result

p_result <- prop.test(c(n_positive_2023_before, n_negative_2023_before), c(n_all_2023_before, n_all_2023_before))
p_result

p_result <- prop.test(c(n_positive_2022_after, n_negative_2022_after), c(n_all_2022_after, n_all_2022_after))
p_result

p_result <- prop.test(c(n_positive_2023_after, n_negative_2023_after), c(n_all_2023_after, n_all_2023_after))
p_result

################################################################################

# We want to compare difference-in-differences between 2022 and 2023

daily_proportions_2022 <- d_2022 %>%
  group_by(date) %>%
  summarize(
    proportion_negative = sum(sentiment == "negative") / n(),
    proportion_positive = sum(sentiment == "positive") / n()
  )

daily_proportions_2023 <- d_2023 %>%
  group_by(date) %>%
  summarize(
    proportion_negative = sum(sentiment == "negative") / n(),
    proportion_positive = sum(sentiment == "positive") / n()
  )

# Remove first 7 days to make the number of days equal
before_2022 <- daily_proportions_2022 %>% filter(date >= "2022-01-08" & date < "2022-02-03")
after_2022 <- daily_proportions_2022 %>% filter(date >= "2022-02-03")

before_2023 <- daily_proportions_2023 %>% filter(date < "2023-02-03" & date >= "2023-01-08")
after_2023 <- daily_proportions_2023 %>% filter(date >= "2023-02-03")

# Calculate daily differences (after - before) for each year
differences_2022_neg <- after_2022$proportion_negative - before_2022$proportion_negative
differences_2023_neg <- after_2023$proportion_negative - before_2023$proportion_negative

t.test(differences_2022_neg, differences_2023_neg)
cohen.d(differences_2022_neg, differences_2023_neg)

# Calculate diff-in-diffs, and compare to 0
diff_in_diffs_neg <- differences_2023_neg - differences_2022_neg
t.test(diff_in_diffs_neg)
cohensD(diff_in_diffs_neg, mu = 0)

# For positive
differences_2022_pos <- after_2022$proportion_positive - before_2022$proportion_positive
differences_2023_pos <- after_2023$proportion_positive - before_2023$proportion_positive

t.test(differences_2022_pos, differences_2023_pos)
cohen.d(differences_2022_pos, differences_2023_pos)

diff_in_diffs_pos <- differences_2023_pos - differences_2022_pos
t.test(diff_in_diffs_pos)
cohensD(diff_in_diffs_pos, mu = 0)

################################################################################

# Now, compare before-after June 15 2023, i.e., the "Ask Replika" update
# With before-after the ERP update
# Select 26 days before-after June 15 2023, so that it matches the 
# number of days before-after the ERP update

d_all[['posts']]$date <- substr(d_all[['posts']]$created_date, 1, 10)

d_ask_replika <- d_all[['posts']][d_all[['posts']]$date >= "2023-05-20" & d_all[['posts']]$date < "2023-07-11", ]
d_erp <- d_all[['posts']][d_all[['posts']]$date >= "2023-01-08" & d_all[['posts']]$date <= "2023-02-28", ]

d_before_ask_replika <- d_all[['posts']][d_all[['posts']]$date >= "2023-05-20" & d_all[['posts']]$date < "2023-06-15", ]
d_after_ask_replika <- d_all[['posts']][d_all[['posts']]$date >= "2023-06-15" & d_all[['posts']]$date < "2023-07-11", ]

# Also get before-after the ERP update
d_before_erp <- d_all[['posts']][d_all[['posts']]$date >= "2023-01-08" & d_all[['posts']]$date < "2023-02-03", ]
d_after_erp <- d_all[['posts']][d_all[['posts']]$date >= "2023-02-03" & d_all[['posts']]$date <= "2023-02-28", ]

# We want to compare difference-in-differences between before vs. after

daily_proportions_ask_replika <- d_ask_replika %>%
  group_by(date) %>%
  summarize(
    proportion_negative = sum(sentiment == "negative") / n(),
    proportion_positive = sum(sentiment == "positive") / n()
  )

daily_proportions_erp <- d_erp %>%
  group_by(date) %>%
  summarize(
    proportion_negative = sum(sentiment == "negative") / n(),
    proportion_positive = sum(sentiment == "positive") / n()
  )

before_ask_replika <- daily_proportions_ask_replika %>% filter(date >= "2023-05-20" & date < "2023-06-15")
after_ask_replika <- daily_proportions_ask_replika %>% filter(date >= "2023-06-15")

before_erp <- daily_proportions_erp %>% filter(date >= "2023-01-08" & date < "2023-02-03")
after_erp <- daily_proportions_erp %>% filter(date >= "2023-02-03")

# Calculate daily differences (after - before) for each year
differences_ask_replika_neg <- after_ask_replika$proportion_negative - before_ask_replika$proportion_negative
differences_erp_neg <- after_erp$proportion_negative - before_erp$proportion_negative

t.test(differences_ask_replika_neg, differences_erp_neg)
cohen.d(differences_ask_replika_neg, differences_erp_neg)

# Calculate diff-in-diffs, and compare to 0
diff_in_diffs_neg <- differences_erp_neg - differences_ask_replika_neg
t.test(diff_in_diffs_neg)
cohensD(diff_in_diffs_neg, mu = 0)

# For positive
differences_ask_replika_pos <- after_ask_replika$proportion_positive - before_ask_replika$proportion_positive
differences_erp_pos <- after_erp$proportion_positive - before_erp$proportion_positive

t.test(differences_ask_replika_pos, differences_erp_pos)
cohen.d(differences_ask_replika_pos, differences_erp_pos)

diff_in_diffs_pos <- differences_erp_pos - differences_ask_replika_pos
t.test(diff_in_diffs_pos)
cohensD(diff_in_diffs_pos, mu = 0)



######## MANUAL CODINGS ########

d_manual <- read.csv('./data/posts_after_erp_sample_filled.csv', sep = ";")

# Unique users
length(unique(d_manual$author))

colnames(d_manual)

# Calculate cronbach alpha between K_mental_health and Z_mental_health with cronbach.alpha
df <- d_manual[, c('K_actual_sadness', 'Z_actual_sadness')]
cronbach.alpha(df)

df <- d_manual[, c('K_is_sadness_caused_by_change', 'Z_is_sadness_caused_by_change')]
cronbach.alpha(df)

df <- d_manual[, c('K_is_sadness_caused_by_loss', 'Z_is_sadness_caused_by_loss')]
cronbach.alpha(df)


df <- d_manual[, c('K_mental_health', 'Z_mental_health')]
cronbach.alpha(df)

df <- d_manual[, c('K_is_mental_health_caused_by_change', 'Z_is_mental_health_caused_by_change')]
cronbach.alpha(df)

df <- d_manual[, c('K_is_mental_health_caused_by_loss', 'Z_is_mental_health_caused_by_loss')]
cronbach.alpha(df)


# Get mental health that is rated same between K and Z
d_manual_same <- d_manual[d_manual$K_actual_sadness == d_manual$Z_actual_sadness,]
sum(d_manual_same$K_actual_sadness) / dim(d_manual_same)[1]

d_manual_same <- d_manual[d_manual$K_is_sadness_caused_by_change == d_manual$Z_is_sadness_caused_by_change,]
sum(d_manual_same$K_is_sadness_caused_by_change) / dim(d_manual_same)[1]

d_manual_same <- d_manual[d_manual$K_is_sadness_caused_by_loss == d_manual$K_is_sadness_caused_by_loss,]
sum(d_manual_same$K_is_sadness_caused_by_loss) / dim(d_manual_same)[1]


d_manual_same <- d_manual[d_manual$K_mental_health == d_manual$Z_mental_health,]
sum(d_manual_same$K_mental_health) / dim(d_manual_same)[1]

d_manual_same <- d_manual[d_manual$K_is_mental_health_caused_by_change == d_manual$Z_is_mental_health_caused_by_change,]
sum(d_manual_same$K_is_mental_health_caused_by_change) / dim(d_manual_same)[1]

d_manual_same <- d_manual[d_manual$K_is_mental_health_caused_by_loss == d_manual$Z_is_mental_health_caused_by_loss,]
sum(d_manual_same$K_is_mental_health_caused_by_loss) / dim(d_manual_same)[1]













