rm(list = ls())

library(ggplot2)
library(reshape2)
library(readxl)
library(ggpubr)

# 列样本，行物种
phylum <- read_excel("", sheet="")
mapping <- read.table("", header=T, comment.char="", check.names=F)
mapping <- mapping[, c("Group", "#SampleID")]
colnames(mapping) <- c("group", "sample")

p1 <- phylum[1,]
p1 <- melt(p1)
colnames(p1) <- c("tax", "sample", "abd")
p1 <- merge(p1, mapping, sort=F)
p1$group <- factor(p1$group, levels=unique(mapping$group))

fig <- ggplot(p1, aes(x=group, y=abd, shape=group, color=group)) + 
  geom_point(position=position_jitter(), size=5)+
  stat_summary(fun=mean, geom="bar", position=position_dodge(0.9), alpha=0, lwd=1.5) + 
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(0.9), width=0.5, lwd=1.5) +
  labs(title=phylum[1, 1], y="Relative abundance") +
  theme_classic() + 
  theme(panel.grid=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_text(face="bold", size=18, color="black"),
        axis.text.x=element_text(face="bold", size=15, angle=45, vjust=0.95, hjust=0.9, color="black"),
        axis.text.y=element_text(face="bold", size=13, color="black"),
        axis.line=element_line(linewidth=1),
        plot.title=element_text(hjust=0.5, size=22, face="bold", color="black"), 
        legend.position="None") +
  scale_color_manual(values=c("red", "gold", "blue")) + 
  scale_y_continuous(expand=c(0, 0), limits=c(0, 1.1)) +
  # 配对统计，CON组为ref组
  stat_compare_means(label="p.signif", method="t.test", ref.group="CON", 
                     symnum.args=list(cutpoints=c(0, 0.001, 0.01, 0.05, Inf), 
                                      symbols=c("***", "**", "*", "")), 
                    label.y=1.1, size=12)

pdf('test.pdf')
print(fig)
dev.off()
