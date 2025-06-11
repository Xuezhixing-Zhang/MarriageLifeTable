library(bayesmlogit)
library(dplyr)

## Run two regressions simultaneously
k <- 1 # Index for regression

# k <- commandArgs(trailingOnly = TRUE)
# k <- k[1]

data <- readRDS("./Marriage_Data.rds") %>%
  select(trans, age, male, nhblack,nhother,hispanic, nativity,hrscohort, c4reg2, c4reg3, c4reg4) %>%
  filter(!is.na(trans))

y <- data$trans

X <- data[,-1]

out <- bayesmlogit(y=y, X ,samp=2500, burn=1000,verbose=1)

filename <- paste0("./mar_sample1_out",k,".rds")
saveRDS(out, file = filename)


y.1 <- sort(unique(y))
y <- match(y,y.1)
data_f <- as.data.frame(cbind(y,X))
names(data_f)[1] <- 'y'

data_f$y <- as.factor(data_f$y)

J.1 <- nlevels(data_f$y)
X <- stats::model.matrix(y ~ ., data=data_f)
col_names <- colnames(X)
y.all <- stats::model.matrix(~ y - 1, data=data_f)
y <- y.all[,-J.1]
y
