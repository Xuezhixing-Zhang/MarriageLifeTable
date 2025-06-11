# library(haven)
library(dplyr)

library(bayesmlogit)
data <- readRDS("../LivingArrangement_Data.rds") %>%
  select(trans, age, male, nhblack,nhother,hispanic, nativity,hrscohort, c4reg2, c4reg3, c4reg4) %>%
  filter(!is.na(trans))

data <- as.data.frame(data)
out1 <- readRDS("liv_out1.rds")
out2 <- readRDS("liv_out2.rds")

trans <- rbind(out1$outwstepwidth,out2$outwstepwidth)
trans
y <- data[,1]
X <- data[,-1]

mlifeTable(y,X,trans =trans,
           states=5,
           groupby = c("male","nhblack","nhother","hispanic"),
           # no_control = c("nativity","hrscohort",
           #          "c4reg2","c4reg3","c4reg4"), 
           startages=50,
           age.gap=2,
           nums = 1000,
           file_path=".",
           mlifeTable_plot = T,
           cred = 0.84,
           compare = T)
View(mlifeTable)
