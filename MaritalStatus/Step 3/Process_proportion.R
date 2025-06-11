library(bayesmlogit)
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


NM.mean <- c(mean(group_FW[,1]),mean(group_FH[,1]),mean(group_FB[,1]),
             mean(group_MW[,1]),mean(group_MH[,1]),mean(group_MB[,1]))*100

NM.left <- c(quantile(group_FW[,1],prob=c(0.08)),
             quantile(group_FH[,1],prob=c(0.08)),
             quantile(group_FB[,1],prob=c(0.08)),
             quantile(group_MW[,1],prob=c(0.08)),
             quantile(group_MH[,1],prob=c(0.08)),
             quantile(group_MB[,1],prob=c(0.08)))*100

NM.right <- c(quantile(group_FW[,1],prob=c(0.92)),
              quantile(group_FH[,1],prob=c(0.92)),
              quantile(group_FB[,1],prob=c(0.92)),
              quantile(group_MW[,1],prob=c(0.92)),
              quantile(group_MH[,1],prob=c(0.92)),
              quantile(group_MB[,1],prob=c(0.92)))*100

M.mean <- c(mean(group_FW[,2]),mean(group_FH[,2]),mean(group_FB[,2]),
            mean(group_MW[,2]),mean(group_MH[,2]),mean(group_MB[,2]))*100

M.left <- c(quantile(group_FW[,2],prob=c(0.08)),
            quantile(group_FH[,2],prob=c(0.08)),
            quantile(group_FB[,2],prob=c(0.08)),
            quantile(group_MW[,2],prob=c(0.08)),
            quantile(group_MH[,2],prob=c(0.08)),
            quantile(group_MB[,2],prob=c(0.08)))*100

M.right <- c(quantile(group_FW[,2],prob=c(0.92)),
             quantile(group_FH[,2],prob=c(0.92)),
             quantile(group_FB[,2],prob=c(0.92)),
             quantile(group_MW[,2],prob=c(0.92)),
             quantile(group_MH[,2],prob=c(0.92)),
             quantile(group_MB[,2],prob=c(0.92)))*100


D.mean <- c(mean(group_FW[,3]),mean(group_FH[,3]),mean(group_FB[,3]),
            mean(group_MW[,3]),mean(group_MH[,3]),mean(group_MB[,3]))*100

D.left <- c(quantile(group_FW[,3],prob=c(0.08)),
            quantile(group_FH[,3],prob=c(0.08)),
            quantile(group_FB[,3],prob=c(0.08)),
            quantile(group_MW[,3],prob=c(0.08)),
            quantile(group_MH[,3],prob=c(0.08)),
            quantile(group_MB[,3],prob=c(0.08)))*100

D.right <- c(quantile(group_FW[,3],prob=c(0.92)),
             quantile(group_FH[,3],prob=c(0.92)),
             quantile(group_FB[,3],prob=c(0.92)),
             quantile(group_MW[,3],prob=c(0.92)),
             quantile(group_MH[,3],prob=c(0.92)),
             quantile(group_MB[,3],prob=c(0.92)))*100

W.mean <- c(mean(group_FW[,4]),mean(group_FH[,4]),mean(group_FB[,4]),
            mean(group_MW[,4]),mean(group_MH[,4]),mean(group_MB[,4]))*100

W.left <- c(quantile(group_FW[,4],prob=c(0.08)),
            quantile(group_FH[,4],prob=c(0.08)),
            quantile(group_FB[,4],prob=c(0.08)),
            quantile(group_MW[,4],prob=c(0.08)),
            quantile(group_MH[,4],prob=c(0.08)),
            quantile(group_MB[,4],prob=c(0.08)))*100

W.right <- c(quantile(group_FW[,4],prob=c(0.92)),
             quantile(group_FH[,4],prob=c(0.92)),
             quantile(group_FB[,4],prob=c(0.92)),
             quantile(group_MW[,4],prob=c(0.92)),
             quantile(group_MH[,4],prob=c(0.92)),
             quantile(group_MB[,4],prob=c(0.92)))*100



NeverMarried <- data.frame(mean = NM.mean,
                           left.bound = NM.left,
                           right.bound = NM.right, 
                           subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"))

Married <- data.frame(mean = M.mean,
                      left.bound = M.left,
                      right.bound = M.right, 
                      subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"))

Divorced <- data.frame(mean = D.mean,
                       left.bound = D.left,
                       right.bound = D.right, 
                       subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"))

Widowed <- data.frame(mean = W.mean,
                      left.bound = W.left,
                      right.bound = W.right, 
                      subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"))


# write.csv(NeverMarried, file="./general/NeverMarried.csv", row.names = F)
# write.csv(Married, file="./general/Married.csv", row.names = F)
# write.csv(Divorced, file="./general/Divorced.csv", row.names = F)
# write.csv(Widowed, file="./general/Widowed.csv", row.names = F)

# write.csv(NeverMarried, file="./Proportion_NeverMarried.csv", row.names = F)
# write.csv(Married, file="./Proportion_Married.csv", row.names = F)
# write.csv(Divorced, file="./Proportion_Divorced.csv", row.names = F)
# write.csv(Widowed, file="./Proportion_Widowed.csv", row.names = F)


NM <- ggplot(data = NeverMarried, 
             aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Proportion of Years (%)", y = "Subgroup", 
       title = "Expected duration of time spent as never married") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

M <- ggplot(data = Married, 
            aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Proportion of Years (%)", y = "Subgroup", 
       title = "Expected duration of time spent as married/partnered") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

D <- ggplot(data = Divorced, 
            aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Proportion of Years (%)", y = "Subgroup", 
       title = "Expected duration of time spent as divorced/separated") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

W <- ggplot(data = Widowed, 
            aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Proportion of Years (%)", y = "Subgroup", 
       title = "Widowed") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

v1 <- grid.arrange(NM+labs(title = "Never Married")+
                     # xlim(0,35)+
                     theme(axis.text.y=element_text(size=12),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   M+labs(title = "Married")+
                     # xlim(0,35)+
                     theme(axis.text.y = element_blank(), 
                           axis.ticks.y = element_blank(), 
                           axis.title.y = element_blank(),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   D+labs(title = "Divorced")+
                     # xlim(0,35)+
                     theme(axis.text.y=element_text(size=12),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   # theme(axis.text.y = element_blank(), 
                   #       axis.ticks.y = element_blank(), 
                   #       axis.title.y = element_blank(),
                   #       #panel.border = element_blank(),
                   #       plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   W+labs(title = "Widowed")+
                     # xlim(0,35)+
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   nrow=2,ncol=2,widths = c(1.1,1))

# write.csv(NeverMarried, file = "./NeverMarried.csv",
#           row.names = F)
# write.csv(Married, file = "./Married.csv",
#           row.names = F)
# write.csv(Divorced, file = "./Divorced.csv",
#           row.names = F)
# write.csv(Widowed, file = "./Widowed.csv",
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

ggsave(NM, filename = "./general/NeverMarried.png",width = 11)
ggsave(M, filename = "./general/Married.png",width = 11)
ggsave(D, filename = "./general/Divorced.png",width = 11)
ggsave(W, filename = "./general/Widowed.png",height = 4.4, width = 6)


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

NeverMarried$Prop_CI <- sprintf("%.2f [%.2f, %.2f]", 
                                NeverMarried$mean * 100, 
                                NeverMarried$left.bound * 100, 
                                NeverMarried$right.bound * 100)
print(NeverMarried[, c("subgroup", "Prop_CI")])

Married$Prop_CI <- sprintf("%.2f [%.2f, %.2f]", 
                           Married$mean * 100, 
                           Married$left.bound * 100, 
                           Married$right.bound * 100)
print(Married[, c("subgroup", "Prop_CI")])

Divorced$Prop_CI <- sprintf("%.2f [%.2f, %.2f]", 
                            Divorced$mean * 100, 
                            Divorced$left.bound * 100, 
                            Divorced$right.bound * 100)
print(Divorced[, c("subgroup", "Prop_CI")])

Widowed$Prop_CI <- sprintf("%.2f [%.2f, %.2f]", 
                           Widowed$mean * 100, 
                           Widowed$left.bound * 100, 
                           Widowed$right.bound * 100)
print(Widowed[, c("subgroup", "Prop_CI")])











############################################ By Race
NeverMarried$race <- c(rep(c("White","Hispanic","Black"),2))
NeverMarried$gender <- c(rep("Female",3),rep("Male",3))
NeverMarried$status <- c(rep("Never Married",6))

Married$race <- c(rep(c("White","Hispanic","Black"),2))
Married$gender <- c(rep("Female",3),rep("Male",3))
Married$status <- c(rep("Married",6))

Divorced$race <- c(rep(c("White","Hispanic","Black"),2))
Divorced$gender <- c(rep("Female",3),rep("Male",3))
Divorced$status <- c(rep("Divorced",6))

Widowed$race <- c(rep(c("White","Hispanic","Black"),2))
Widowed$gender <- c(rep("Female",3),rep("Male",3))
Widowed$status <- c(rep("Widowed",6))

merged_data <- rbind(NeverMarried, Married, Divorced, Widowed)



ggplot(data = merged_data, 
       aes(x = mean, 
           y = status, 
           color = gender)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbarh(aes(xmin = left.bound, xmax = right.bound),
                 height = 0,
                 position = position_dodge(width = 0.3)) +
  facet_wrap(~ race) +
  theme_bw() +
  theme(
    panel.background  = element_rect(fill = 'white'),
    plot.background   = element_rect(fill = 'white'),
    axis.title        = element_text(face = 'bold')
  ) +
  labs(
    x     = "Proportion of Years (%)",
    y     = "Marital Status"
  )

