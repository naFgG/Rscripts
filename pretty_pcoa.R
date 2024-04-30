rm(list=ls())
library(vegan)
library(ape)
library(ggplot2)

df <- read.table("feature.xls", sep="\t", header=T, row.name=1, check.names=F, quote="")
mapp <- read.table("mapping.txt", sep="\t", header=T, check.names=F, quote="")
mapp$Group <- factor(mapp$Group, levels=unique(mapp$Group))

dis <- vegdist(t(df), method="bray")
adn <-  adonis2(dis ~ Group, mapp)

df.pcoa <- pcoa(dis, correction="cailliez")
plotData <- as.data.frame(df.pcoa$vectors)
xlabel <- paste("PCoA1 ", round(df.pcoa$values$Relative_eig[1] * 100, 2), 
                "%", sep="")
ylabel <- paste("PCoA2 ", round(df.pcoa$values$Relative_eig[2] * 100, 2), 
                "%", sep="")

plotData$Sample <- rownames(plotData)
plotData <- merge(plotData, mapp, by="Sample")
rownames(plotData) <- plotData$Sample
plotData <- plotData[, -1]
pval <- adn[1, 5]

#> 原始带执行椭圆的
p1 <- ggplot(plotData, aes(x=Axis.1, y=Axis.2, color=Group)) +
  geom_point(cex=3.5) +
  labs(x=xlabel, y=ylabel) +
  theme_bw()+
  theme(panel.grid=element_blank(), 
        axis.title.x=element_text(face="bold"), 
        axis.text.x=element_text(face="bold")) +
  geom_vline(xintercept=0, lty=2, color="darkgrey") +
  geom_hline(yintercept=0, lty=2, color="darkgrey") +
  stat_ellipse(level=0.95, type="norm", show.legend=F)


#> 不同组的外围顶点连线
group_list <- list()
for (g in unique(mapp$Group)){
  grp <- plotData[plotData$Group==g, c(1, 2, ncol(plotData))]
  group_list[[g]] <- grp[chull(grp), ]
}
p2 <- ggplot(plotData, aes(x=Axis.1, y=Axis.2, color=Group)) +
  geom_point(cex=3.5)
for (l in seq_along(group_list)){
  p2 <- p2 + geom_polygon(data=group_list[[l]], fill=NA, show.legend=F)
  print(p2)
}
p2 <- p2 + 
  labs(x=xlabel, y=ylabel) +
  theme_bw()+
  theme(panel.grid=element_blank(), 
        axis.title.x=element_text(face="bold"), 
        axis.text.x=element_text(face="bold")) +
  geom_vline(xintercept=0, lty=2, color="darkgrey") +
  geom_hline(yintercept=0, lty=2, color="darkgrey")


#> 质心连线
library(doBy)
#> 以均值为质心
data_2axe <- plotData[, c(1, 2, ncol(plotData))]
data_mean <- summaryBy(Axis.1 + Axis.2 ~ Group, data_2axe, FUN=mean)
data_plot <- merge(data_2axe, data_mean, by="Group", sort=F)
p3 <- ggplot(data_plot, aes(x=Axis.1, y=Axis.2, color=Group)) +
  geom_point(cex=3.5) + 
  geom_segment(aes(x=Axis.1.mean, y=Axis.2.mean, 
                   xend=Axis.1, yend=Axis.2), 
               show.legend=F, alpha=0.6) +
  labs(x=xlabel, y=ylabel) +
  theme_bw()+
  theme(panel.grid=element_blank(), 
        axis.title.x=element_text(face="bold"), 
        axis.text.x=element_text(face="bold")) +
  geom_vline(xintercept=0, lty=2, color="darkgrey") +
  geom_hline(yintercept=0, lty=2, color="darkgrey")
             

library(patchwork)
p <- p1 / p2 / p3 + plot_layout(guides="collect") + plot_annotation(tag_levels='I')
pdf("test.pdf", height=12)
p
dev.off()
