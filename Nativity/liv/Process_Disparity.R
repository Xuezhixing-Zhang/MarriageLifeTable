library(bayesmlogit)
library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)

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

disp_FH_1 <- group_FH_1-group_FW_1
disp_FB_1 <- group_FB_1-group_FW_1
disp_MH_1 <- group_MH_1-group_MW_1
disp_MB_1 <- group_MB_1-group_MW_1

disp_FH_0 <- group_FH_0-group_FW_0
disp_FB_0 <- group_FB_0-group_FW_0
disp_MH_0 <- group_MH_0-group_MW_0
disp_MB_0 <- group_MB_0-group_MW_0

NM.mean <- c(mean(disp_FH_1[,1]),mean(disp_FB_1[,1]),
             mean(disp_MH_1[,1]),mean(disp_MB_1[,1]),
             mean(disp_FH_0[,1]),mean(disp_FB_0[,1]),
             mean(disp_MH_0[,1]),mean(disp_MB_0[,1]))

NM.left <- c(quantile(disp_FH_1[,1],prob=c(0.08)),
             quantile(disp_FB_1[,1],prob=c(0.08)),
             quantile(disp_MH_1[,1],prob=c(0.08)),
             quantile(disp_MB_1[,1],prob=c(0.08)),
             quantile(disp_FH_0[,1],prob=c(0.08)),
             quantile(disp_FB_0[,1],prob=c(0.08)),
             quantile(disp_MH_0[,1],prob=c(0.08)),
             quantile(disp_MB_0[,1],prob=c(0.08)))

NM.right <- c(quantile(disp_FH_1[,1],prob=c(0.92)),
              quantile(disp_FB_1[,1],prob=c(0.92)),
              quantile(disp_MH_1[,1],prob=c(0.92)),
              quantile(disp_MB_1[,1],prob=c(0.92)),
              quantile(disp_FH_0[,1],prob=c(0.92)),
              quantile(disp_FB_0[,1],prob=c(0.92)),
              quantile(disp_MH_0[,1],prob=c(0.92)),
              quantile(disp_MB_0[,1],prob=c(0.92)))

M.mean <- c(mean(disp_FH_1[,2]),mean(disp_FB_1[,2]),
            mean(disp_MH_1[,2]),mean(disp_MB_1[,2]),
            mean(disp_FH_0[,2]),mean(disp_FB_0[,2]),
            mean(disp_MH_0[,2]),mean(disp_MB_0[,2]))

M.left <- c(quantile(disp_FH_1[,2],prob=c(0.08)),
            quantile(disp_FB_1[,2],prob=c(0.08)),
            quantile(disp_MH_1[,2],prob=c(0.08)),
            quantile(disp_MB_1[,2],prob=c(0.08)),
            quantile(disp_FH_0[,2],prob=c(0.08)),
            quantile(disp_FB_0[,2],prob=c(0.08)),
            quantile(disp_MH_0[,2],prob=c(0.08)),
            quantile(disp_MB_0[,2],prob=c(0.08)))

M.right <- c(quantile(disp_FH_1[,2],prob=c(0.92)),
             quantile(disp_FB_1[,2],prob=c(0.92)),
             quantile(disp_MH_1[,2],prob=c(0.92)),
             quantile(disp_MB_1[,2],prob=c(0.92)),
             quantile(disp_FH_0[,2],prob=c(0.92)),
             quantile(disp_FB_0[,2],prob=c(0.92)),
             quantile(disp_MH_0[,2],prob=c(0.92)),
             quantile(disp_MB_0[,2],prob=c(0.92)))


D.mean <- c(mean(disp_FH_1[,3]),mean(disp_FB_1[,3]),
            mean(disp_MH_1[,3]),mean(disp_MB_1[,3]),
            mean(disp_FH_0[,3]),mean(disp_FB_0[,3]),
            mean(disp_MH_0[,3]),mean(disp_MB_0[,3]))

D.left <- c(quantile(disp_FH_1[,3],prob=c(0.08)),
            quantile(disp_FB_1[,3],prob=c(0.08)),
            quantile(disp_MH_1[,3],prob=c(0.08)),
            quantile(disp_MB_1[,3],prob=c(0.08)),
            quantile(disp_FH_0[,3],prob=c(0.08)),
            quantile(disp_FB_0[,3],prob=c(0.08)),
            quantile(disp_MH_0[,3],prob=c(0.08)),
            quantile(disp_MB_0[,3],prob=c(0.08)))

D.right <- c(quantile(disp_FH_1[,3],prob=c(0.92)),
             quantile(disp_FB_1[,3],prob=c(0.92)),
             quantile(disp_MH_1[,3],prob=c(0.92)),
             quantile(disp_MB_1[,3],prob=c(0.92)),
             quantile(disp_FH_0[,3],prob=c(0.92)),
             quantile(disp_FB_0[,3],prob=c(0.92)),
             quantile(disp_MH_0[,3],prob=c(0.92)),
             quantile(disp_MB_0[,3],prob=c(0.92)))

W.mean <- c(mean(disp_FH_1[,4]),mean(disp_FB_1[,4]),
            mean(disp_MH_1[,4]),mean(disp_MB_1[,4]),
            mean(disp_FH_0[,4]),mean(disp_FB_0[,4]),
            mean(disp_MH_0[,4]),mean(disp_MB_0[,4]))

W.left <- c(quantile(disp_FH_1[,4],prob=c(0.08)),
            quantile(disp_FB_1[,4],prob=c(0.08)),
            quantile(disp_MH_1[,4],prob=c(0.08)),
            quantile(disp_MB_1[,4],prob=c(0.08)),
            quantile(disp_FH_0[,4],prob=c(0.08)),
            quantile(disp_FB_0[,4],prob=c(0.08)),
            quantile(disp_MH_0[,4],prob=c(0.08)),
            quantile(disp_MB_0[,4],prob=c(0.08)))

W.right <- c(quantile(disp_FH_1[,4],prob=c(0.92)),
             quantile(disp_FB_1[,4],prob=c(0.92)),
             quantile(disp_MH_1[,4],prob=c(0.92)),
             quantile(disp_MB_1[,4],prob=c(0.92)),
             quantile(disp_FH_0[,4],prob=c(0.92)),
             quantile(disp_FB_0[,4],prob=c(0.92)),
             quantile(disp_MH_0[,4],prob=c(0.92)),
             quantile(disp_MB_0[,4],prob=c(0.92)))



NeverMarried <- data.frame(mean = NM.mean,
                           left.bound = NM.left,
                           right.bound = NM.right, 
                           subgroup = rep(c("F-H","F-B","M-H","M-B"),2),
                           sex =rep(c("F","F","M","M"),2),
                           nat = c(rep(1,4),rep(0,4)))

Married <- data.frame(mean = M.mean,
                      left.bound = M.left,
                      right.bound = M.right, 
                      subgroup = rep(c("F-H","F-B","M-H","M-B"),2),
                      sex =rep(c("F","F","M","M"),2),
                      nat = c(rep(1,4),rep(0,4)))

Divorced <- data.frame(mean = D.mean,
                       left.bound = D.left,
                       right.bound = D.right, 
                       subgroup = rep(c("F-H","F-B","M-H","M-B"),2),
                       sex =rep(c("F","F","M","M"),2),
                       nat = c(rep(1,4),rep(0,4)))

Widowed <- data.frame(mean = W.mean,
                      left.bound = W.left,
                      right.bound = W.right, 
                      subgroup = rep(c("F-H","F-B","M-H","M-B"),2),
                      sex =rep(c("F","F","M","M"),2),
                      nat = c(rep(1,4),rep(0,4)))

NM <- ggplot(data = NeverMarried, 
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

M <- ggplot(data = Married, 
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

D <- ggplot(data = Divorced, 
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

W <- ggplot(data = Widowed, 
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

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
mylegend<-g_legend(NM)


v1 <- grid.arrange(arrangeGrob(NM+labs(title = "Live Alone")+
                                 # xlim(0,35)+
                                 theme(#panel.border = element_blank(),
                                   plot.title = element_text(face='bold',size=14,hjust = 0.5),
                                   legend.position="none"
                                 ),
                               M+labs(title = "Live with Spouse")+
                                 # xlim(0,35)+
                                 theme(axis.text.y = element_blank(), 
                                       axis.ticks.y = element_blank(), 
                                       axis.title.y = element_blank(),
                                       legend.position="none",
                                       #panel.border = element_blank(),
                                       plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                               D+labs(title = "Live with Non-Spouse Only")+
                                 # xlim(0,35)+
                                 # theme(axis.text.y = element_blank(), 
                                 #       axis.ticks.y = element_blank(), 
                                 #       axis.title.y = element_blank(),
                                 #       #panel.border = element_blank(),
                                 #       plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                                 theme(plot.title = element_text(face='bold',size=14,hjust = 0.5),
                                       legend.position="none"),
                               W+labs(title = "Nursing Home")+
                                 # xlim(0,35)+
                                 theme(axis.text.y = element_blank(),
                                       axis.ticks.y = element_blank(),
                                       axis.title.y = element_blank(),
                                       legend.position="none",
                                       plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                               nrow=2,widths = c(1.1,1)),
                   mylegend,
                   nrow=2,heights=c(10,1))

# 
write.csv(NeverMarried, file = "./disparity_nat/Disparity_Live_Alone.csv",
          row.names = F)
write.csv(Married, file = "./disparity_nat/Disparity_Live_with_Spouse.csv",
          row.names = F)
write.csv(Divorced, file = "./disparity_nat/Disparity_Live_with_Non_Spouse.csv",
          row.names = F)
write.csv(Widowed, file = "./disparity_nat/Disparity_Live_in_Nursing_Home.csv",
          row.names = F)
