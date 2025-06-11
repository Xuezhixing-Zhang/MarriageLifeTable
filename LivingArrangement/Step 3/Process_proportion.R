# library(bayesmlogit)
library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)

# data <- read.csv("data_1010.csv")
# 
# y <- data[,1]
# X <- data[,-1]


group_FW <- read.table("../lifetable0000.txt", header = T)
group_FH <- read.table("../lifetable0001.txt", header = T)
group_FB <- read.table("../lifetable0100.txt", header = T)

group_MW <- read.table("../lifetable1000.txt", header = T)
group_MH <- read.table("../lifetable1001.txt", header = T)
group_MB <- read.table("../lifetable1100.txt", header = T)

group_FW <- sweep(group_FW[, -5], 1, rowSums(group_FW[, -5]), "/")
group_FH <- sweep(group_FH[, -5], 1, rowSums(group_FH[, -5]), "/")
group_FB <- sweep(group_FB[, -5], 1, rowSums(group_FB[, -5]), "/")
group_MW <- sweep(group_MW[, -5], 1, rowSums(group_MW[, -5]), "/")
group_MH <- sweep(group_MH[, -5], 1, rowSums(group_MH[, -5]), "/")
group_MB <- sweep(group_MB[, -5], 1, rowSums(group_MB[, -5]), "/")


A.mean <- c(mean(group_FW[,1]),mean(group_FH[,1]),mean(group_FB[,1]),
             mean(group_MW[,1]),mean(group_MH[,1]),mean(group_MB[,1]))

A.left <- c(quantile(group_FW[,1],prob=c(0.08)),
             quantile(group_FH[,1],prob=c(0.08)),
             quantile(group_FB[,1],prob=c(0.08)),
             quantile(group_MW[,1],prob=c(0.08)),
             quantile(group_MH[,1],prob=c(0.08)),
             quantile(group_MB[,1],prob=c(0.08)))

A.right <- c(quantile(group_FW[,1],prob=c(0.92)),
              quantile(group_FH[,1],prob=c(0.92)),
              quantile(group_FB[,1],prob=c(0.92)),
              quantile(group_MW[,1],prob=c(0.92)),
              quantile(group_MH[,1],prob=c(0.92)),
              quantile(group_MB[,1],prob=c(0.92)))

S.mean <- c(mean(group_FW[,2]),mean(group_FH[,2]),mean(group_FB[,2]),
            mean(group_MW[,2]),mean(group_MH[,2]),mean(group_MB[,2]))

S.left <- c(quantile(group_FW[,2],prob=c(0.08)),
            quantile(group_FH[,2],prob=c(0.08)),
            quantile(group_FB[,2],prob=c(0.08)),
            quantile(group_MW[,2],prob=c(0.08)),
            quantile(group_MH[,2],prob=c(0.08)),
            quantile(group_MB[,2],prob=c(0.08)))

S.right <- c(quantile(group_FW[,2],prob=c(0.92)),
             quantile(group_FH[,2],prob=c(0.92)),
             quantile(group_FB[,2],prob=c(0.92)),
             quantile(group_MW[,2],prob=c(0.92)),
             quantile(group_MH[,2],prob=c(0.92)),
             quantile(group_MB[,2],prob=c(0.92)))


NS.mean <- c(mean(group_FW[,3]),mean(group_FH[,3]),mean(group_FB[,3]),
            mean(group_MW[,3]),mean(group_MH[,3]),mean(group_MB[,3]))

NS.left <- c(quantile(group_FW[,3],prob=c(0.08)),
            quantile(group_FH[,3],prob=c(0.08)),
            quantile(group_FB[,3],prob=c(0.08)),
            quantile(group_MW[,3],prob=c(0.08)),
            quantile(group_MH[,3],prob=c(0.08)),
            quantile(group_MB[,3],prob=c(0.08)))

NS.right <- c(quantile(group_FW[,3],prob=c(0.92)),
             quantile(group_FH[,3],prob=c(0.92)),
             quantile(group_FB[,3],prob=c(0.92)),
             quantile(group_MW[,3],prob=c(0.92)),
             quantile(group_MH[,3],prob=c(0.92)),
             quantile(group_MB[,3],prob=c(0.92)))

N.mean <- c(mean(group_FW[,4]),mean(group_FH[,4]),mean(group_FB[,4]),
            mean(group_MW[,4]),mean(group_MH[,4]),mean(group_MB[,4]))

N.left <- c(quantile(group_FW[,4],prob=c(0.08)),
            quantile(group_FH[,4],prob=c(0.08)),
            quantile(group_FB[,4],prob=c(0.08)),
            quantile(group_MW[,4],prob=c(0.08)),
            quantile(group_MH[,4],prob=c(0.08)),
            quantile(group_MB[,4],prob=c(0.08)))

N.right <- c(quantile(group_FW[,4],prob=c(0.92)),
             quantile(group_FH[,4],prob=c(0.92)),
             quantile(group_FB[,4],prob=c(0.92)),
             quantile(group_MW[,4],prob=c(0.92)),
             quantile(group_MH[,4],prob=c(0.92)),
             quantile(group_MB[,4],prob=c(0.92)))



Alone <- data.frame(mean = A.mean,
                           left.bound = A.left,
                           right.bound = A.right, 
                           subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"))

Spouse <- data.frame(mean = S.mean,
                      left.bound = S.left,
                      right.bound = S.right, 
                      subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"))

Nonspouse <- data.frame(mean = NS.mean,
                       left.bound = NS.left,
                       right.bound = NS.right, 
                       subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"))

Nursing <- data.frame(mean = N.mean,
                      left.bound = N.left,
                      right.bound = N.right, 
                      subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"))


# write.csv(NeverMarried, file="./general/NeverMarried.csv", row.names = F)
# write.csv(Married, file="./general/Married.csv", row.names = F)
# write.csv(Divorced, file="./general/Divorced.csv", row.names = F)
# write.csv(Widowed, file="./general/Widowed.csv", row.names = F)

# write.csv(Alone, file="./Proportion_Alone.csv", row.names = F)
# write.csv(Spouse, file="./Proportion_Spouse.csv", row.names = F)
# write.csv(Nonspouse, file="./Proportion_Nonspouse.csv", row.names = F)
# write.csv(Nursing, file="./Proportion_Nursing.csv", row.names = F)

A <- ggplot(data = Alone, 
             aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Proportion of Years", y = "Subgroup", 
       title = "Expected duration of time spent as living alone") +
  theme(axis.title = element_text(face='bold'),
        plot.margin = margin(r = 10, l=10)) + ylab('Subgroup')

S <- ggplot(data = Spouse, 
             aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Proportion of Years", y = "Subgroup", 
       title = "Expected duration of time spent as living with spouse") +
  theme(axis.title = element_text(face='bold'),
        plot.margin = margin(r = 10, l=10)) + ylab('Subgroup')

NS <- ggplot(data = Nonspouse, 
            aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Proportion of Years", y = "Subgroup", 
       title = "Expected duration of time spent as living with non-spouse") +
  theme(axis.title = element_text(face='bold'),
        plot.margin = margin(r = 10, l=10)) + ylab('Subgroup')

N <- ggplot(data = Nursing, 
            aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Proportion of Years", y = "Subgroup", 
       title = "Live in Nursing Home") +
  theme(axis.title = element_text(face='bold'),
        plot.margin = margin(r = 10, l=10)) + ylab('Subgroup')

v1 <- grid.arrange(A+labs(title = "Live Alone")+
                     # xlim(0,35)+
                     theme(#panel.border = element_blank(),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5),
                           ),
                   S+labs(title = "Live with Spouse")+
                     # xlim(0,35)+
                     theme(axis.text.y = element_blank(), 
                           axis.ticks.y = element_blank(), 
                           axis.title.y = element_blank(),
                           #panel.border = element_blank(),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   NS+labs(title = "Live with Non-Spouses Only")+
                     # xlim(0,35)+
                     # theme(axis.text.y = element_blank(), 
                     #       axis.ticks.y = element_blank(), 
                     #       axis.title.y = element_blank(),
                     #       #panel.border = element_blank(),
                     #       plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                     theme(plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   N+labs(title = "Nursing Home")+
                     # xlim(0,35)+
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   nrow=2,ncol=2,widths = c(1.1,1))

# write.csv(Alone, file = "../live_Alone.csv",
#           row.names = F)
# write.csv(Spouse, file = "../live_with_Spouse.csv",
#           row.names = F)
# write.csv(Nonspouse, file = "../live_with_Non_Spouse.csv",
#           row.names = F)
# write.csv(Nursing, file = "../live_in_Nursing_Home.csv",
#           row.names = F)

# v2 <- grid.arrange(NM+theme(plot.title = element_text(size=11),
#                             plot.title.position = "plot",
#                             axis.title = element_text(face='bold',size=11)),
#                    M+theme(plot.title = element_text(size=11),
#                            plot.title.position = "plot",
#                            axis.title = element_text(face='bold',size=11),
#                            axis.text.y = element_blank(), 
#                            axis.ticks.y = element_blank(), 
#                            axis.title.y = element_blank()),
#                    D+theme(plot.title = element_text(size=11),
#                            plot.title.position = "plot",
#                            axis.title = element_text(face='bold',size=11)),
#                    W+theme(plot.title = element_text(size=11),
#                            plot.title.position = "plot",
#                            axis.title = element_text(face='bold',size=11),
#                            axis.text.y = element_blank(), 
#                            axis.ticks.y = element_blank(), 
#                            axis.title.y = element_blank()),
#                    nrow=2,ncol=2)

# ggsave(A, filename = "./general/Alone.png",width = 11)
# ggsave(S, filename = "./general/Spouse.png",width = 11)
# ggsave(NS, filename = "./general/Nonspouse.png",width = 11)
# ggsave(N, filename = "./general/Nursing.png",height = 4.4, width = 6)


#Prevalence
# pos.mean <- colMeans(trans)
# length(pos.mean)
# general <-mlifeTable_new(y,X,trans =pos.mean,
#            groupby = NA,
#            vars = c("educ1","educ2","nativity"),
#            states=5,
#            status=0,
#            startages=50,
#            age.gap=2,
#            nums = 1,
#            file_path=".",
#            mlifeTable_plot = F,
#            cred = 0.84)
# 
# 
# general <- data.frame(general[[1]])
# general <- general %>% mutate(age = seq(50,110,by=2))
# colnames(general) <- c("NM","M","D","W","Death","age")
# 
# write.csv(general, file = "./general/prevalence_general.csv",
#           row.names = F)
# 
# subgroups <-mlifeTable_new(y,X,trans =pos.mean,
#                      groupby = c("male","nhblack","nhother","hispanic"),
#                      vars = c("educ1","educ2","nativity"),
#                      states=5,
#                      status=0,
#                      startages=50,
#                      age.gap=2,
#                      nums = 1,
#                      file_path=".",
#                      mlifeTable_plot = F,
#                      cred = 0.84)
# 
# 
# FW.prevalence <- data.frame(subgroups[[1]]) %>% mutate(age = seq(50,110,by=2))
# FH.prevalence <- data.frame(subgroups[[2]]) %>% mutate(age = seq(50,110,by=2))
# FB.prevalence <- data.frame(subgroups[[3]]) %>% mutate(age = seq(50,110,by=2))
# MW.prevalence <- data.frame(subgroups[[4]]) %>% mutate(age = seq(50,110,by=2))
# MH.prevalence <- data.frame(subgroups[[5]]) %>% mutate(age = seq(50,110,by=2))
# MB.prevalence <- data.frame(subgroups[[6]]) %>% mutate(age = seq(50,110,by=2))
# 
# colnames(FW.prevalence) <- c("NM","M","D","W","Death","age")
# colnames(FH.prevalence) <- c("NM","M","D","W","Death","age")
# colnames(FB.prevalence) <- c("NM","M","D","W","Death","age")
# colnames(MW.prevalence) <- c("NM","M","D","W","Death","age")
# colnames(MH.prevalence) <- c("NM","M","D","W","Death","age")
# colnames(MB.prevalence) <- c("NM","M","D","W","Death","age")
# 
# 
# write.csv(FW.prevalence, file = "./general/prevalence_FW.csv",
#           row.names = F)
# write.csv(FH.prevalence, file = "./general/prevalence_FH.csv",
#           row.names = F)
# write.csv(FB.prevalence, file = "./general/prevalence_FB.csv",
#           row.names = F)
# write.csv(MW.prevalence, file = "./general/prevalence_MW.csv",
#           row.names = F)
# write.csv(MH.prevalence, file = "./general/prevalence_MH.csv",
#           row.names = F)
# write.csv(MB.prevalence, file = "./general/prevalence_MB.csv",
#           row.names = F)

Alone$Prop_CI <- sprintf("%.2f [%.2f, %.2f]", 
                         Alone$mean * 100, 
                         Alone$left.bound * 100, 
                         Alone$right.bound * 100)
print(Alone[, c("subgroup", "Prop_CI")])

Spouse$Prop_CI <- sprintf("%.2f [%.2f, %.2f]", 
                          Spouse$mean * 100, 
                          Spouse$left.bound * 100, 
                          Spouse$right.bound * 100)
print(Spouse[, c("subgroup", "Prop_CI")])

Nonspouse$Prop_CI <- sprintf("%.2f [%.2f, %.2f]", 
                            Nonspouse$mean * 100, 
                            Nonspouse$left.bound * 100, 
                            Nonspouse$right.bound * 100)
print(Nonspouse[, c("subgroup", "Prop_CI")])

Nursing$Prop_CI <- sprintf("%.2f [%.2f, %.2f]", 
                           Nursing$mean * 100, 
                           Nursing$left.bound * 100, 
                           Nursing$right.bound * 100)
print(Nursing[, c("subgroup", "Prop_CI")])
