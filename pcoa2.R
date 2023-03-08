rm(list = ls())
setwd("D:/rLearning")
library(vegan)
library(ape)
# library(phyloseq) # calculate un/weight unifrac
library(ggplot2)

df <- read.table("otu_test.xls", row.name=1, header=TRUE)
mapp <- read.table("mapping.txt", header=TRUE)

# calculate distance matrix
dis <- vegdist(t(df), method="bray")
# write.table(as.matrix(dis), file="bray_curtis_distance.xls", sep="\t")
# PERMANOVA
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

# ggplot(plotData, aes(x=Axis.1, y=Axis.2, color=Group, shape=Group))+
ggplot(plotData, aes(x=Axis.1, y=Axis.2, color=Group))+
  geom_point(cex=3.5)+
  theme_bw()+
  theme(panel.grid=element_blank(), 
        axis.title.x=element_text(face="bold"), 
        axis.text.x=element_text(face="bold", size=25, family="serif"),
        plot.title=element_text(hjust=0.5, size=20, face="bold"))+
  geom_vline(xintercept=0, lty=2, color="darkgrey")+
  geom_hline(yintercept=0, lty=2, color="darkgrey")+
  xlab(xlabel)+
  ylab(ylabel)+
  labs(title=paste0("PERMANOVA ", "Pvalue: ", pval))+
  stat_ellipse(level=0.95, type="norm", show.legend=F)+
  geom_text(aes(x=Axis.1, y=Axis.2, label=rownames(plotData)), size=6, 
            hjust=-0.1, vjust=1.25, alpha=0.5, show.legend=F)