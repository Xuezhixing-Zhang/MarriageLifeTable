# library(haven)
library(tidyverse)
library(bayesmlogit)
data <- readRDS("../Marriage_Data.rds") %>%
  select(trans, age, male, nhblack,nhother,hispanic, nativity,hrscohort, c4reg2, c4reg3, c4reg4) %>%
  filter(!is.na(trans))
data <- as.data.frame(data)
out1 <- readRDS("mar_sample1_out1.rds")
out2 <- readRDS("mar_sample1_out2.rds")

trans <- rbind(out1$outwstepwidth,out2$outwstepwidth)
y <- data[,1]
X <- data[,-1]
mlifeTable(y,X,trans =trans,
           groupby = c("male","nhblack","nhother","hispanic"),
           # no_control = c("nativity","hrscohort",
           #          "c4reg2","c4reg3","c4reg4"),
           states=5,
           startages=50,
           age.gap=2,
           nums = 1000,
           file_path=".",
           mlifeTable_plot = T,
           cred = 0.84)






















