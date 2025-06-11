library(bayesmlogit)
library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)
library(ggpubr)
# data <- read.csv("data_1010.csv")
# 
# y <- data[,1]
# X <- data[,-1]


group_FW_1 <- read.table("./nat1/lifetable0000.txt", header = T)
group_FH_1 <- read.table("./nat1/lifetable0001.txt", header = T)
group_FB_1 <- read.table("./nat1/lifetable0100.txt", header = T)

group_MW_1 <- read.table("./nat1/lifetable1000.txt", header = T)
group_MH_1 <- read.table("./nat1/lifetable1001.txt", header = T)
group_MB_1 <- read.table("./nat1/lifetable1100.txt", header = T)

group_FW_0 <- read.table("./nat0/lifetable0000.txt", header = T)
group_FH_0 <- read.table("./nat0/lifetable0001.txt", header = T)
group_FB_0 <- read.table("./nat0/lifetable0100.txt", header = T)

group_MW_0 <- read.table("./nat0/lifetable1000.txt", header = T)
group_MH_0 <- read.table("./nat0/lifetable1001.txt", header = T)
group_MB_0 <- read.table("./nat0/lifetable1100.txt", header = T)


A.mean <- c(mean(group_FW_1[,1]),mean(group_FH_1[,1]),mean(group_FB_1[,1]),
             mean(group_MW_1[,1]),mean(group_MH_1[,1]),mean(group_MB_1[,1]),
            mean(group_FW_0[,1]),mean(group_FH_0[,1]),mean(group_FB_0[,1]),
            mean(group_MW_0[,1]),mean(group_MH_0[,1]),mean(group_MB_0[,1]))

A.left <- c(quantile(group_FW_1[,1],prob=c(0.08)),
             quantile(group_FH_1[,1],prob=c(0.08)),
             quantile(group_FB_1[,1],prob=c(0.08)),
             quantile(group_MW_1[,1],prob=c(0.08)),
             quantile(group_MH_1[,1],prob=c(0.08)),
             quantile(group_MB_1[,1],prob=c(0.08)),
            quantile(group_FW_0[,1],prob=c(0.08)),
            quantile(group_FH_0[,1],prob=c(0.08)),
            quantile(group_FB_0[,1],prob=c(0.08)),
            quantile(group_MW_0[,1],prob=c(0.08)),
            quantile(group_MH_0[,1],prob=c(0.08)),
            quantile(group_MB_0[,1],prob=c(0.08)))

A.right <- c(quantile(group_FW_1[,1],prob=c(0.92)),
             quantile(group_FH_1[,1],prob=c(0.92)),
             quantile(group_FB_1[,1],prob=c(0.92)),
             quantile(group_MW_1[,1],prob=c(0.92)),
             quantile(group_MH_1[,1],prob=c(0.92)),
             quantile(group_MB_1[,1],prob=c(0.92)),
             quantile(group_FW_0[,1],prob=c(0.92)),
             quantile(group_FH_0[,1],prob=c(0.92)),
             quantile(group_FB_0[,1],prob=c(0.92)),
             quantile(group_MW_0[,1],prob=c(0.92)),
             quantile(group_MH_0[,1],prob=c(0.92)),
             quantile(group_MB_0[,1],prob=c(0.92)))

S.mean <- c(mean(group_FW_1[,2]),mean(group_FH_1[,2]),mean(group_FB_1[,2]),
            mean(group_MW_1[,2]),mean(group_MH_1[,2]),mean(group_MB_1[,2]),
            mean(group_FW_0[,2]),mean(group_FH_0[,2]),mean(group_FB_0[,2]),
            mean(group_MW_0[,2]),mean(group_MH_0[,2]),mean(group_MB_0[,2]))

S.left <- c(quantile(group_FW_1[,2],prob=c(0.08)),
            quantile(group_FH_1[,2],prob=c(0.08)),
            quantile(group_FB_1[,2],prob=c(0.08)),
            quantile(group_MW_1[,2],prob=c(0.08)),
            quantile(group_MH_1[,2],prob=c(0.08)),
            quantile(group_MB_1[,2],prob=c(0.08)),
            quantile(group_FW_0[,2],prob=c(0.08)),
            quantile(group_FH_0[,2],prob=c(0.08)),
            quantile(group_FB_0[,2],prob=c(0.08)),
            quantile(group_MW_0[,2],prob=c(0.08)),
            quantile(group_MH_0[,2],prob=c(0.08)),
            quantile(group_MB_0[,2],prob=c(0.08)))

S.right <- c(quantile(group_FW_1[,2],prob=c(0.92)),
             quantile(group_FH_1[,2],prob=c(0.92)),
             quantile(group_FB_1[,2],prob=c(0.92)),
             quantile(group_MW_1[,2],prob=c(0.92)),
             quantile(group_MH_1[,2],prob=c(0.92)),
             quantile(group_MB_1[,2],prob=c(0.92)),
             quantile(group_FW_0[,2],prob=c(0.92)),
             quantile(group_FH_0[,2],prob=c(0.92)),
             quantile(group_FB_0[,2],prob=c(0.92)),
             quantile(group_MW_0[,2],prob=c(0.92)),
             quantile(group_MH_0[,2],prob=c(0.92)),
             quantile(group_MB_0[,2],prob=c(0.92)))


NS.mean <- c(mean(group_FW_1[,3]),mean(group_FH_1[,3]),mean(group_FB_1[,3]),
             mean(group_MW_1[,3]),mean(group_MH_1[,3]),mean(group_MB_1[,3]),
             mean(group_FW_0[,3]),mean(group_FH_0[,3]),mean(group_FB_0[,3]),
             mean(group_MW_0[,3]),mean(group_MH_0[,3]),mean(group_MB_0[,3]))

NS.left <- c(quantile(group_FW_1[,3],prob=c(0.08)),
             quantile(group_FH_1[,3],prob=c(0.08)),
             quantile(group_FB_1[,3],prob=c(0.08)),
             quantile(group_MW_1[,3],prob=c(0.08)),
             quantile(group_MH_1[,3],prob=c(0.08)),
             quantile(group_MB_1[,3],prob=c(0.08)),
             quantile(group_FW_0[,3],prob=c(0.08)),
             quantile(group_FH_0[,3],prob=c(0.08)),
             quantile(group_FB_0[,3],prob=c(0.08)),
             quantile(group_MW_0[,3],prob=c(0.08)),
             quantile(group_MH_0[,3],prob=c(0.08)),
             quantile(group_MB_0[,3],prob=c(0.08)))

NS.right <- c(quantile(group_FW_1[,3],prob=c(0.92)),
              quantile(group_FH_1[,3],prob=c(0.92)),
              quantile(group_FB_1[,3],prob=c(0.92)),
              quantile(group_MW_1[,3],prob=c(0.92)),
              quantile(group_MH_1[,3],prob=c(0.92)),
              quantile(group_MB_1[,3],prob=c(0.92)),
              quantile(group_FW_0[,3],prob=c(0.92)),
              quantile(group_FH_0[,3],prob=c(0.92)),
              quantile(group_FB_0[,3],prob=c(0.92)),
              quantile(group_MW_0[,3],prob=c(0.92)),
              quantile(group_MH_0[,3],prob=c(0.92)),
              quantile(group_MB_0[,3],prob=c(0.92)))

N.mean <- c(mean(group_FW_1[,4]),mean(group_FH_1[,4]),mean(group_FB_1[,4]),
            mean(group_MW_1[,4]),mean(group_MH_1[,4]),mean(group_MB_1[,4]),
            mean(group_FW_0[,4]),mean(group_FH_0[,4]),mean(group_FB_0[,4]),
            mean(group_MW_0[,4]),mean(group_MH_0[,4]),mean(group_MB_0[,4]))

N.left <- c(quantile(group_FW_1[,4],prob=c(0.08)),
            quantile(group_FH_1[,4],prob=c(0.08)),
            quantile(group_FB_1[,4],prob=c(0.08)),
            quantile(group_MW_1[,4],prob=c(0.08)),
            quantile(group_MH_1[,4],prob=c(0.08)),
            quantile(group_MB_1[,4],prob=c(0.08)),
            quantile(group_FW_0[,4],prob=c(0.08)),
            quantile(group_FH_0[,4],prob=c(0.08)),
            quantile(group_FB_0[,4],prob=c(0.08)),
            quantile(group_MW_0[,4],prob=c(0.08)),
            quantile(group_MH_0[,4],prob=c(0.08)),
            quantile(group_MB_0[,4],prob=c(0.08)))

N.right <- c(quantile(group_FW_1[,4],prob=c(0.92)),
             quantile(group_FH_1[,4],prob=c(0.92)),
             quantile(group_FB_1[,4],prob=c(0.92)),
             quantile(group_MW_1[,4],prob=c(0.92)),
             quantile(group_MH_1[,4],prob=c(0.92)),
             quantile(group_MB_1[,4],prob=c(0.92)),
             quantile(group_FW_0[,4],prob=c(0.92)),
             quantile(group_FH_0[,4],prob=c(0.92)),
             quantile(group_FB_0[,4],prob=c(0.92)),
             quantile(group_MW_0[,4],prob=c(0.92)),
             quantile(group_MH_0[,4],prob=c(0.92)),
             quantile(group_MB_0[,4],prob=c(0.92)))



Alone <- data.frame(mean = A.mean,
                           left.bound = A.left,
                           right.bound = A.right, 
                           subgroup = rep(c("F-W","F-H","F-B","M-W","M-H","M-B"),2),
                    sex =rep(c("F","F","F","M","M","M"),2),
                    nat = c(rep(1,6),rep(0,6)))

Spouse <- data.frame(mean = S.mean,
                      left.bound = S.left,
                      right.bound = S.right, 
                      subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"),
                     sex =c("F","F","F","M","M","M"),
                     nat = c(rep(1,6),rep(0,6)))

Nonspouse <- data.frame(mean = NS.mean,
                       left.bound = NS.left,
                       right.bound = NS.right, 
                       subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"),
                       sex =c("F","F","F","M","M","M"),
                       nat = c(rep(1,6),rep(0,6)))

Nursing <- data.frame(mean = N.mean,
                      left.bound = N.left,
                      right.bound = N.right, 
                      subgroup = c("F-W","F-H","F-B","M-W","M-H","M-B"),
                      sex =c("F","F","F","M","M","M"),
                      nat = c(rep(1,6),rep(0,6)))


write.csv(Alone, file="./general_nat/NeverMarried.csv", row.names = F)
write.csv(Spouse, file="./general_nat/Married.csv", row.names = F)
write.csv(Nonspouse, file="./general_nat/Divorced.csv", row.names = F)
write.csv(Nursing, file="./general_nat/Widowed.csv", row.names = F)


A <- ggplot(data = Alone, 
            aes(x = mean, y = subgroup, xmin = left.bound, xmax = right.bound,color = factor(nat))) +
  geom_point(position = position_dodge(0.3),
             size=2) +
  geom_errorbarh(height = 0,
                 position = position_dodge(0.3),
                 size=1) +
  theme_bw() + 
  scale_color_manual(name = "Nativity status",
                     labels = c("Not born in the U.S.", 
                                "Born in the U.S."),
                     values = c("#3F51B5","#FFC107"))+
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Years", y = "Subgroup", 
       title = "Expected duration of time spent as living alone") +
  theme(axis.title = element_text(face='bold'),
        legend.position="bottom") + ylab('Subgroup')
A
#FFC107 #3F51B5
S <- ggplot(data = Spouse, 
            aes(x = mean, y = subgroup, xmin = left.bound, xmax = right.bound,color = factor(nat))) +
  geom_point(position = position_dodge(0.3),
             size=2) +
  geom_errorbarh(height = 0,
                 position = position_dodge(0.3),
                 size=1) +
  theme_bw() + 
  scale_color_manual(name = "Nativity status",
                     labels = c("Not born in the U.S.", 
                                "Born in the U.S."),
                     values = c("#3F51B5","#FFC107"))+
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Years", y = "Subgroup", 
       title = "Expected duration of time spent as living spouse") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

NS <- ggplot(data = Nonspouse, 
             aes(x = mean, y = subgroup, xmin = left.bound, xmax = right.bound,color = factor(nat))) +
  geom_point(position = position_dodge(0.3),
             size=2) +
  geom_errorbarh(height = 0,
                 position = position_dodge(0.3),
                 size=1) +
  theme_bw() + 
  scale_color_manual(name = "Nativity status",
                     labels = c("Not born in the U.S.", 
                                "Born in the U.S."),
                     values = c("#3F51B5","#FFC107"))+
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Years", y = "Subgroup", 
       title = "Expected duration of time spent as living with non-spouse") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

N <- ggplot(data = Nursing, 
            aes(x = mean, y = subgroup, xmin = left.bound, xmax = right.bound,color = factor(nat))) +
  geom_point(position = position_dodge(0.3),
             size=2) +
  geom_errorbarh(height = 0,
                 position = position_dodge(0.3),
                 size=1) +
  theme_bw() + 
  scale_color_manual(name = "Nativity status",
                     labels = c("Not born in the U.S.", 
                                "Born in the U.S."),
                     values = c("#3F51B5","#FFC107"))+
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Years", y = "Subgroup", 
       title = "Live in Nursing Home") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  }
mylegend<-g_legend(A)

v1 <- grid.arrange(arrangeGrob(A+labs(title = "Live Alone")+
                     # xlim(0,35)+
                     theme(#panel.border = element_blank(),
                       plot.title = element_text(face='bold',size=14,hjust = 0.5),
                       legend.position="none"
                     ),
                   S+labs(title = "Live with Spouse")+
                     # xlim(0,35)+
                     theme(axis.text.y = element_blank(), 
                           axis.ticks.y = element_blank(), 
                           axis.title.y = element_blank(),
                           legend.position="none",
                           #panel.border = element_blank(),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   NS+labs(title = "Live with Non-Spouses Only")+
                     # xlim(0,35)+
                     # theme(axis.text.y = element_blank(), 
                     #       axis.ticks.y = element_blank(), 
                     #       axis.title.y = element_blank(),
                     #       #panel.border = element_blank(),
                     #       plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                     theme(plot.title = element_text(face='bold',size=14,hjust = 0.5),
                           legend.position="none"),
                   N+labs(title = "Nursing Home")+
                     # xlim(0,35)+
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           legend.position="none",
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   nrow=2,widths = c(1.1,1)),
                   mylegend,
                   nrow=2,heights=c(10,1))
