rm(list = ls())
library(vegan)
library(ggplot2)
library(ape)
library(reshape2)
library(rstatix)
library(pairwiseAdonis)

df <- read.table("绘图数据/传统方法各位点鱼类和捕获数.xls", sep="\t", row.name=1, header=TRUE, check.names=F)
mapp <- read.table("东西.txt", sep="\t", header=TRUE)

dis <- vegdist(df, method="bray")
# PERMANOVA
adn <- adonis2(dis ~ group, mapp)
a <- as.data.frame(adn)
a <- cbind(rownames(a), a)
colnames(a)[1] <- " "
write.table(a, file="传统方法东西PERMANOVA检验结果.xls", sep="\t", row.names=F)
# pairwies adonis
pair.adonis <- pairwise.adonis(dis, mapp$group)
write.table(pair.adonis, file="传统方法东西成对PERMANOVA检验结果.xls", sep="\t", row.names=F)

df.pcoa <- pcoa(dis, correction="cailliez")
plotData <- as.data.frame(df.pcoa$vectors)
xlabel <- paste("PCoA1 ", round(df.pcoa$values$Rel_corr_eig[1] * 100, 2), 
                "%", sep="")
ylabel <- paste("PCoA2 ", round(df.pcoa$values$Rel_corr_eig[2] * 100, 2), 
                "%", sep="")

plotData$site <- rownames(plotData)
plotData <- merge(plotData, mapp, by="site")
rownames(plotData) <- plotData$site
plotData <- plotData[, -1]
pval <- adn[1, 5]

p <- ggplot(plotData, aes(x=Axis.1, y=Axis.2, color=group))+
  geom_point(cex=3.5)+
  theme_bw()+
  theme(panel.grid=element_blank(), 
        plot.title=element_text(hjust=0.5, size=11, face="bold"),
        legend.title=element_blank(),
        legend.text=element_text(size=13))+
  geom_vline(xintercept=0, lty=2, color="darkgrey")+
  geom_hline(yintercept=0, lty=2, color="darkgrey")+
  xlab(xlabel)+
  ylab(ylabel)+
  labs(title=paste0("PERMANOVA ", "P-value: ", pval))+
  stat_ellipse(level=0.95, type="norm", show.legend=F)+
  geom_text(aes(x=Axis.1, y=Axis.2, label=rownames(plotData)), size=6, 
            hjust=-0.1, vjust=1.25, alpha=0.5, show.legend=F)
pdf("传统方法东西poca.pdf")
print(p)
dev.off()
