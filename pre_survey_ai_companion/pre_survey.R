## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### PILOT #####
d_pilot <- read.csv('./pilot.csv', sep = ",")

d_pilot <- d_pilot[d_pilot$att_1 == "Paul" & d_pilot$att_2 == "Purple", ]

print(dim(d_pilot))

# Remove duplicate IDs
d_pilot <- d_pilot[!duplicated(d_pilot$prolific_id), ]
sum(d_pilot$Q10 == 'yes')

700 / 7723
