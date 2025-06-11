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

disp_FH <- -(group_FW-group_FH)
disp_FB <- -(group_FW-group_FB)
disp_MH <- -(group_MW-group_MH)
disp_MB <- -(group_MW-group_MB)

NM.mean <- c(mean(disp_FH[,1]),mean(disp_FB[,1]),mean(disp_MH[,1]),
             mean(disp_MB[,1]))

NM.left <- c(quantile(disp_FH[,1],prob=c(0.08)),
             quantile(disp_FB[,1],prob=c(0.08)),
             quantile(disp_MH[,1],prob=c(0.08)),
             quantile(disp_MB[,1],prob=c(0.08)))

NM.right <- c(quantile(disp_FH[,1],prob=c(0.92)),
              quantile(disp_FB[,1],prob=c(0.92)),
              quantile(disp_MH[,1],prob=c(0.92)),
              quantile(disp_MB[,1],prob=c(0.92)))

M.mean <- c(mean(disp_FH[,2]),mean(disp_FB[,2]),mean(disp_MH[,2]),
            mean(disp_MB[,2]))

M.left <- c(quantile(disp_FH[,2],prob=c(0.08)),
            quantile(disp_FB[,2],prob=c(0.08)),
            quantile(disp_MH[,2],prob=c(0.08)),
            quantile(disp_MB[,2],prob=c(0.08)))

M.right <- c(quantile(disp_FH[,2],prob=c(0.92)),
             quantile(disp_FB[,2],prob=c(0.92)),
             quantile(disp_MH[,2],prob=c(0.92)),
             quantile(disp_MB[,2],prob=c(0.92)))


D.mean <- c(mean(disp_FH[,3]),mean(disp_FB[,3]),mean(disp_MH[,3]),
            mean(disp_MB[,3]))

D.left <- c(quantile(disp_FH[,3],prob=c(0.08)),
            quantile(disp_FB[,3],prob=c(0.08)),
            quantile(disp_MH[,3],prob=c(0.08)),
            quantile(disp_MB[,3],prob=c(0.08)))

D.right <- c(quantile(disp_FH[,3],prob=c(0.92)),
             quantile(disp_FB[,3],prob=c(0.92)),
             quantile(disp_MH[,3],prob=c(0.92)),
             quantile(disp_MB[,3],prob=c(0.92)))

W.mean <- c(mean(disp_FH[,4]),mean(disp_FB[,4]),mean(disp_MH[,4]),
            mean(disp_MB[,4]))

W.left <- c(quantile(disp_FH[,4],prob=c(0.08)),
            quantile(disp_FB[,4],prob=c(0.08)),
            quantile(disp_MH[,4],prob=c(0.08)),
            quantile(disp_MB[,4],prob=c(0.08)))

W.right <- c(quantile(disp_FH[,4],prob=c(0.92)),
             quantile(disp_FB[,4],prob=c(0.92)),
             quantile(disp_MH[,4],prob=c(0.92)),
             quantile(disp_MB[,4],prob=c(0.92)))



NeverMarried <- data.frame(mean = NM.mean,
                           left.bound = NM.left,
                           right.bound = NM.right, 
                           subgroup = c("F-H","F-B","M-H","M-B"))

Married <- data.frame(mean = M.mean,
                      left.bound = M.left,
                      right.bound = M.right, 
                      subgroup = c("F-H","F-B","M-H","M-B"))

Divorced <- data.frame(mean = D.mean,
                       left.bound = D.left,
                       right.bound = D.right, 
                       subgroup = c("F-H","F-B","M-H","M-B"))

Widowed <- data.frame(mean = W.mean,
                      left.bound = W.left,
                      right.bound = W.right, 
                      subgroup = c("F-H","F-B","M-H","M-B"))

NM <- ggplot(data = NeverMarried, 
             aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Years", y = "Subgroup", 
       title = "Expected duration of time spent as never married") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

M <- ggplot(data = Married, 
            aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Years", y = "Subgroup", 
       title = "Expected duration of time spent as married/partnered") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

D <- ggplot(data = Divorced, 
            aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Years", y = "Subgroup", 
       title = "Expected duration of time spent as divorced/separated") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

W <- ggplot(data = Widowed, 
            aes_string(x = 'mean', y = 'subgroup', xmin = 'left.bound', xmax = 'right.bound')) +
  geom_point() +
  geom_errorbarh(height = 0) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'))+
  labs(x = "Years", y = "Subgroup", 
       title = "Expected duration of time spent as widowed") +
  theme(axis.title = element_text(face='bold')) + ylab('Subgroup')

v1 <- grid.arrange(NM+labs(title = "Live Alone")+
                     # xlim(-10,10)+
                     theme(axis.text.y=element_text(size=12),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   M+labs(title = "Live with Spouse")+
                     # xlim(-10,10)+
                     theme(axis.text.y = element_blank(), 
                           axis.ticks.y = element_blank(), 
                           axis.title.y = element_blank(),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   D+labs(title = "Live with Non-Spouses Only")+
                     # xlim(-10,10)+
                     theme(axis.text.y=element_text(size=12),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                     # theme(axis.text.y = element_blank(), 
                     #       axis.ticks.y = element_blank(), 
                     #       axis.title.y = element_blank(),
                     #       #panel.border = element_blank(),
                     #       plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   W+labs(title = "Live in Nursing Home")+
                     # xlim(-10,10)+
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank(),
                           plot.title = element_text(face='bold',size=14,hjust = 0.5)),
                   nrow=2,ncol=2,widths= c(1.1,1))

write.csv(NeverMarried, file = "./Disparity_Live_Alone.csv",
          row.names = F)
write.csv(Married, file = "./Disparity_Live_with_Spouse.csv",
          row.names = F)
write.csv(Divorced, file = "./Disparity_Live_with_Non_Spouse.csv",
          row.names = F)
write.csv(Widowed, file = "./Disparity_Live_in_Nursing_Home.csv",
          row.names = F)

