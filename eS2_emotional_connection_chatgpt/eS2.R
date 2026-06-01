## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',
               'stats',
               'sjstats',
               'lsr',
               'ggpubr',
               'dplyr',
               'pwr',
               'effsize')

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  this_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile), error = function(e) NULL)
  if (!is.null(this_file) && nzchar(this_file)) {
    setwd(dirname(this_file))
  }
}

d <- read.csv('./data.csv', sep = ",")

# Remove NA rows
d <- d[d$Finished == 1, ]

################################## Exclusions ##################################

print(paste0("Number of participants before attention check: ", nrow(d)))

# Attention check
d <- d[d$att_1 == 2 & d$att_2 == 2,]
print(paste0("Number of participants after attention check: ", nrow(d)))

# Comprehension check
d <- d[d$comp_1 == 2,]
print(paste0("Number of participants after comprehension check: ", nrow(d)))


# Make column names same format
old_names <- c("X1_inclusion", "X2_inclusion", "X3_inclusion", "X4_inclusion", "X5_inclusion", "X6_inclusion", "X7_inclusion", "X8_inclusion")
new_names <- c("X1_inclusion_1", "X2_inclusion_1", "X3_inclusion_1", "X4_inclusion_1", "X5_inclusion_1", "X6_inclusion_1", "X7_inclusion_1", "X8_inclusion_1")

# Change column names
names(d)[names(d) %in% old_names] <- new_names

############################ User characteristics ##############################

## user age
d$age <- as.numeric(as.character(d$age))
mean(d$age, trim = 0, na.rm = TRUE) ## mean age 
sd(d$age, na.rm = TRUE) ## standard deviation 

## user gender
table(d$gender)
100*table(d$gender)[1]/sum(table(d$gender)) ## percentage of males
100*table(d$gender)[2]/sum(table(d$gender)) ## percentage of females
100*table(d$gender)[3]/sum(table(d$gender)) ## percentage of other
100*table(d$gender)[4]/sum(table(d$gender)) ## percentage of other

## user ethnicity
table(d$ethnicity)
table(d$ethnicity)[2]; 100*table(d$ethnicity)[2]/sum(table(d$ethnicity)) ## percentage of asians
table(d$ethnicity)[1]; 100*table(d$ethnicity)[1]/sum(table(d$ethnicity)) ## percentage of blacks
table(d$ethnicity)[7]; 100*table(d$ethnicity)[7]/sum(table(d$ethnicity)) ## percentage of hispanics
table(d$ethnicity)[4]; 100*table(d$ethnicity)[4]/sum(table(d$ethnicity)) ## percentage of whites

sum_mixed <- table(d$ethnicity)[3] + table(d$ethnicity)[5] + table(d$ethnicity)[6]
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

############################ CHATGPT CHARACTERISTICS #############################

# chatgpt relationship status
table(d$is_relationship)
table(d$rep_relation)

# Relationship category codes can be stored as strings (e.g., labels containing "1").
rel_group <- d[d$is_relationship == 1, ]
sum(grepl("1", as.character(rel_group$rep_relation))); 100*sum(grepl("1", as.character(rel_group$rep_relation)))/nrow(rel_group) ## assistant
sum(grepl("2", as.character(rel_group$rep_relation))); 100*sum(grepl("2", as.character(rel_group$rep_relation)))/nrow(rel_group) ## friend
sum(grepl("3", as.character(rel_group$rep_relation))); 100*sum(grepl("3", as.character(rel_group$rep_relation)))/nrow(rel_group) ## romantic partner
sum(grepl("4", as.character(rel_group$rep_relation))); 100*sum(grepl("4", as.character(rel_group$rep_relation)))/nrow(rel_group) ## mentor
sum(grepl("5", as.character(rel_group$rep_relation))); 100*sum(grepl("5", as.character(rel_group$rep_relation)))/nrow(rel_group) ## other


# Subscription Status
table(d$rep_subscribe)
100*table(d$rep_subscribe)[1]/sum(table(d$rep_subscribe)) ## percentage of no subscription
100*table(d$rep_subscribe)[2]/sum(table(d$rep_subscribe)) ## percentage of monthly subscription
100*table(d$rep_subscribe)[3]/sum(table(d$rep_subscribe)) ## percentage of yearly subscription

# Relationship months
d$rep_relation_mos <- as.numeric(as.character(d$rep_relation_mos))
mean(d$rep_relation_mos, trim=0, na.rm=TRUE)
sd(d$rep_relation_mos, na.rm = TRUE) ## standard deviation 

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

dvs <- c("support_old", "support_new", "satisfaction_old", "satisfaction_new", "inclusion")
conditions <- c("stranger", "acquaintance", "brand", "app", "colleague", "friend", "family", "chatgpt")


get_condition_score <- function(data, condition_number, dv) {
  prefix <- paste0("X", condition_number, "_")

  item_suffixes <- switch(
    dv,
    "support_new" = c(1, 9, 10, 11),
    "satisfaction_new" = c(1, 9, 10, 11, 12, 13, 14),
    NULL
  )

  if (is.null(item_suffixes)) {
    col <- paste0(prefix, dv, "_1")
    if (!(col %in% names(data))) {
      stop(paste0("Missing column: ", col))
    }
    return(as.numeric(as.character(data[[col]])))
  }

  cols <- paste0(prefix, dv, "_", item_suffixes)
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste0("Missing columns for ", prefix, dv, ": ", paste(missing_cols, collapse = ", ")))
  }

  df_items <- data[, cols, drop = FALSE]
  df_items <- as.data.frame(lapply(df_items, function(x) as.numeric(as.character(x))))

  return(rowMeans(df_items, na.rm = TRUE))
}


# Let's run repeated measures anova with aov
for(dv in c("support_new", "satisfaction_new", "inclusion")) {
  # Create a data frame for the current DV
  df_anova <- data.frame(
    id = 1:nrow(d),
    stranger = get_condition_score(d, 1, dv),
    acquaintance = get_condition_score(d, 2, dv),
    brand = get_condition_score(d, 3, dv),
    app = get_condition_score(d, 4, dv),
    colleague = get_condition_score(d, 5, dv),
    friend = get_condition_score(d, 6, dv),
    family = get_condition_score(d, 7, dv),
    chatgpt = get_condition_score(d, 8, dv)
  )
  
  # Reshape the data to long format
  df_long <- reshape2::melt(df_anova, id.vars = "id", variable.name = "condition", value.name = "value")
  
  # Run repeated measures ANOVA
  anova_model <- aov(value ~ condition + Error(id/condition), data = df_long)
  anova_summary <- summary(anova_model)
  
  print(paste("Repeated Measures ANOVA for", dv))
  print(anova_summary)
  print(anova_stats(anova_model))
}


# Stars df with 'dv', 'condition', 'value' cols
stars <- data.frame(dv = rep(c("support_new", "satisfaction_new", "inclusion"), 8),
                    condition = rep(conditions, 3),
                    value = rep(0, 24),
                    stringsAsFactors = FALSE)

for (dv in c("support_new", "satisfaction_new", "inclusion")) {
  pvals <- c()   # store raw p-values for this DV
  ttests <- list()
  cds <- list()
  
  for (i in 1:7) {
    x <- get_condition_score(d, i, dv)
    y <- get_condition_score(d, 8, dv)  # chatgpt
    
    tt <- t.test(x, y, paired = TRUE, alternative = "two.sided")
    cd <- effsize::cohen.d(x, y, method = "paired")
    
    pvals <- c(pvals, tt$p.value)
    ttests[[i]] <- tt
    cds[[i]] <- cd
  }
  
  # Apply Bonferroni correction across 7 comparisons
  pvals_adj <- p.adjust(pvals, method = "bonferroni")

  # Print DV
  print(paste0("----- ", dv, " -----"))
  
  # Print mean and SD of chatgpt (y)
  y <- get_condition_score(d, 8, dv)
  mean_y <- mean(y)
  sd_y <- sd(y)
  print(paste0("chatgpt; M = ", round(mean_y, 2), " (", round(sd_y, 2), ")"))
  
  # Save stars and print results
  for (i in 1:7) {
    x <- get_condition_score(d, i, dv)
    
    cond <- conditions[i]
    
    tt <- ttests[[i]]
    cd <- cds[[i]]
    
    # Print APA-style with adjusted p-value
    print(paste0(cond, 
                 "; M = ", round(mean(x), 2), " (", round(sd(x), 2), "), ",
                 "t(", round(tt$parameter, 2), ") = ", round(tt$statistic, 2), 
                 ", p(adj) = ", round(pvals_adj[i], 3),
                 ", p = ", round(pvals[i], 3),
                 ", d = ", round(cd$estimate, 2)))
    
    # Store significance stars
    stars[stars$dv == dv & stars$condition == cond, 'value'] <- get_stars(pvals_adj[i])
  }
}

stars[stars$value == 0, 'value'] <- ""

################################### Correlations: old vs new ###################################


print("\n==================== Correlations: old vs new (pooled across conditions) ====================")
s_old_all <- c(); s_new_all <- c(); sat_old_all <- c(); sat_new_all <- c()
for (cond_num in 1:8) {
  s_old_all <- c(s_old_all, get_condition_score(d, cond_num, "support_old"))
  s_new_all <- c(s_new_all, get_condition_score(d, cond_num, "support_new"))
  sat_old_all <- c(sat_old_all, get_condition_score(d, cond_num, "satisfaction_old"))
  sat_new_all <- c(sat_new_all, get_condition_score(d, cond_num, "satisfaction_new"))
}
ct_support_all <- cor.test(s_old_all, s_new_all, method = "pearson")
ct_satisfaction_all <- cor.test(sat_old_all, sat_new_all, method = "pearson")
print(paste0("support(old vs new): r = ", round(ct_support_all$estimate, 2), ", p = ", round(ct_support_all$p.value, 3)))
print(paste0("satisfaction(old vs new): r = ", round(ct_satisfaction_all$estimate, 2), ", p = ", round(ct_satisfaction_all$p.value, 3)))


################################### EFA on new items (chatgpt) ###################################

fa_cols <- c()

  prefix <- "X8_"
  fa_cols <- c(
    fa_cols,
    paste0(prefix, "support_old_1"),
    paste0(prefix, "satisfaction_old_1"),
    paste0(prefix, "support_new_", c(1, 9, 10, 11)),
    paste0(prefix, "satisfaction_new_", c(1, 9, 10, 11, 12, 13, 14))
  )


missing_fa_cols <- setdiff(fa_cols, names(d))
if (length(missing_fa_cols) > 0) {
  stop(paste0("Missing EFA columns: ", paste(missing_fa_cols, collapse = ", ")))
}

fa_data <- d[, fa_cols, drop = FALSE]
fa_data <- as.data.frame(lapply(fa_data, function(x) as.numeric(as.character(x))))
fa_data <- na.omit(fa_data)

# Make data vertical to have 13 columns for each measure
fa_data_vertical <- data.frame()

  prefix <- "X8_"
  temp_data <- data.frame(
    support_old = fa_data[[paste0(prefix, "support_old_1")]],
    satisfaction_old = fa_data[[paste0(prefix, "satisfaction_old_1")]],
    support_new_1 = fa_data[[paste0(prefix, "support_new_1")]],
    support_new_9 = fa_data[[paste0(prefix, "support_new_9")]],
    support_new_10 = fa_data[[paste0(prefix, "support_new_10")]],
    support_new_11 = fa_data[[paste0(prefix, "support_new_11")]],
    satisfaction_new_1 = fa_data[[paste0(prefix, "satisfaction_new_1")]],
    satisfaction_new_9 = fa_data[[paste0(prefix, "satisfaction_new_9")]],
    satisfaction_new_10 = fa_data[[paste0(prefix, "satisfaction_new_10")]],
    satisfaction_new_11 = fa_data[[paste0(prefix, "satisfaction_new_11")]],
    satisfaction_new_12 = fa_data[[paste0(prefix, "satisfaction_new_12")]],
    satisfaction_new_13 = fa_data[[paste0(prefix, "satisfaction_new_13")]],
    satisfaction_new_14 = fa_data[[paste0(prefix, "satisfaction_new_14")]]
  )
  fa_data_vertical <- rbind(fa_data_vertical, temp_data)


print("\n==================== EFA (chatgpt new items): 3 factors, promax ====================")
efa <- factanal(fa_data_vertical, factors = 2, rotation = "promax")
print(efa)

#################################### Plots ######################################

# Flatten the dataframe to get it ready for plotting. Have columns 'dv', 'condition', and 'participant_number'
dv_plot <- c("b_support_new", "a_satisfaction_new", "c_inclusion")
condition_plot <- c("Stranger", "Acquaintance", "Brand", "App", "Colleague", "Friend", "Family", "ChatGPT")

df_plot <- do.call(
  rbind,
  lapply(1:8, function(cond_num) {
    data.frame(
      dv = rep(dv_plot, each = nrow(d)),
      condition = rep(condition_plot[cond_num], nrow(d) * length(dv_plot)),
      participant_number = rep(1:nrow(d), times = length(dv_plot)),
      value = c(
        get_condition_score(d, cond_num, "support_new"),
        get_condition_score(d, cond_num, "satisfaction_new"),
        get_condition_score(d, cond_num, "inclusion")
      )
    )
  })
)

df_plot[df_plot$dv == "a_satisfaction_new",] %>% group_by(condition) %>% summarise(mean = mean(value))
df_plot[df_plot$dv == "b_support_new",] %>% group_by(condition) %>% summarise(mean = mean(value))
df_plot[df_plot$dv == "c_inclusion",] %>% group_by(condition) %>% summarise(mean = mean(value))

# Multiply inclusion with 100/7
df_plot_orig <- df_plot # Save the original
df_plot[df_plot$dv == "c_inclusion", 'value'] <- df_plot[df_plot$dv == "c_inclusion", 'value'] * 100/7
positions <- c("Stranger", "App", "Brand", "ChatGPT", "Acquaintance", "Colleague", "Friend", "Family")

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
ggsave("./combined_plot.pdf", last_plot(), dpi = 300, width = 12 * 4/5, height = 5 * 4/5)
dev.off()
