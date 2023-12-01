rm(list = ls())
library(reshape2)
library(ggplot2)
library(tidyverse)
library(grid)

args<-commandArgs(T)

#> 旧版颜色
mycols <- c("#FF0000FF", "#FF9900FF", "#FFCC00FF", "#00FF00FF", "#6699FFFF", 
            "#CC33FFFF", "#99991EFF", "#999999FF", "#FF00CCFF", "#CC0000FF", 
            "#FFCCCCFF", "#FFFF00FF", "#CCFF00FF", "#358000FF", "#0000CCFF", 
            "#99CCFFFF", "#00FFFFFF", "#CCFFFFFF", "#9900CCFF", "#CC99FFFF", 
            "#996600FF", "#666600FF", "#666666FF", "#CCCCCCFF", "#79CC3DFF", 
            "#CCCC99FF", "#63B8FF", "#EECBAD", "#CD2626", "#7CFC00", "#99CCFFFF")

#> 丰度表
df <- read.table(args[1], sep="\t", header=T, check.names=F, quote="")
df$Taxonomy <- factor(df$Taxonomy, levels=df$Taxonomy)
df <- melt(df)
colnames(df) <- c("tax", "sample", "abundance")
for (i in 1: nrow(df)) df[i, 3] <- df[i, 3] * 100

#> 样本-分组文件
mapp <- read.table(args[2], sep="\t", header=T, check.names=F, comment.char="")[c("#SampleID", "Group")]
colnames(mapp) <- c("sample", "group")
mapp$group <- factor(mapp$group, levels=unique(mapp$group))
mapp_count <- mapp %>% count(group) %>% mutate(x_axis=1)

p <- ggplot(df, aes(x=sample, y=abundance, fill=tax)) + 
  geom_bar(stat="identity", position="stack", width=0.8) + 
  theme_classic() + 
  labs(title=args[3], 
       y="Relative abundance (%)") + 
  theme(legend.title=element_blank(), 
        axis.text.x=element_text(color="black", angle=90, hjust=1, vjust=0.4), 
        axis.title.x=element_blank(), 
        axis.text.y=element_text(color="black"), 
        axis.title.y=element_text(face="bold", size=12),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom", 
        legend.box.spacing=unit(1.5, "cm"), 
        plot.title=element_text(face="bold", size=13, hjust=.5)) +
  scale_fill_manual(values=mycols) +
  scale_y_continuous(expand=c(0, 0.01)) +
  coord_cartesian(clip="off")

#> 加底部注释
group_col <- c("#FDC187", "#00B037", "#4892F9")
for (i in 1: nrow(mapp_count)){
  p <- p + annotation_custom(grob=rectGrob(gp=gpar(fill=group_col[i], col=group_col[i])), 
                             xmin=i*8-7.5, xmax=i*8+.5, ymin=-7, ymax=-7.5) +
    annotation_custom(grob=textGrob(mapp_count[i, 1],), 
                      xmin=i*8-7.5, xmax=i*8+.5, ymin=-10, ymax=-10.5)
}

pdf(args[4], height=8, width=9)
p
dev.off()

