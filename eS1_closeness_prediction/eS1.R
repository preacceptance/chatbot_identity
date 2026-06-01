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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

############################ Power Analysis ##############################

d$order_stranger <- as.numeric(d$order_1)
d$order_acquaintance <- as.numeric(d$order_2)
d$order_brand <- as.numeric(d$order_3)
d$order_app <- as.numeric(d$order_4)
d$order_colleague <- as.numeric(d$order_5)
d$order_friend <- as.numeric(d$order_6)
d$order_family <- as.numeric(d$order_7)

d$order_replika <- as.numeric(d$order_8)


mean(d$order_replika, na.rm = TRUE)
sd(d$order_replika, na.rm = TRUE)

# Do a one-sample t-test after deducting 2 from order_replika to test if the mean is significantly different from 0
d$order_replika_adj <- d$order_replika - 2
t_test_result <- t.test(d$order_replika_adj, mu = 0)
print(t_test_result)


# Print order of all, from lowest to highest
order_means <- colMeans(d[, c("order_stranger", "order_acquaintance", "order_brand", "order_app", "order_colleague", "order_friend", "order_family", "order_replika")], na.rm = TRUE)
print(sort(order_means))