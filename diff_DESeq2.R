rm(list=ls())
library(DESeq2)
library(ggplot2)
library(ggrepel)
library(tidyverse)

# counts file
countdata <- read.table("", header=T, row.names=1, check.names=F)
# metadata
coldata <- read.table("", header=T, row.names=1, check.names=F)
countdata <- countdata[, rownames(coldata)]
coldata$Group <- factor(coldata$Group, levels=unique(coldata$Group))
# if counts file are not int, run this for-loop
for (i in rownames(countdata)){
  for (j in colnames(countdata)){
    countdata[i, j] <- round(countdata[i, j], digits=0)
  }
}

dds <- DESeqDataSetFromMatrix(countData=countdata, colData=coldata, design=~ Group)
# One click differential analysis
dds2 <- DESeq(dds)
# res <- results(dds2, contrast=c("Group", levels(coldata$Group)[1], levels(coldata$Group)[2]))
# default: Treat vs. Control
res <- results(dds2)
openxlsx::write.xlsx(as.data.frame(res), ".xlsx", rowNames=T)

# Vocalno plot
df <- as.data.frame(res[, c("log2FoldChange", "pvalue", "padj")])
for (i in rownames(df)){
  if (is.na(df[i, "padj"])){
    df[i, "padj"] <- df[i, "pvalue"]
  }
  if (df[i, "pvalue"] < 0.05 & df[i, "padj"] < 0.05){
    df[i, "pinfo"] <- "padj"
  } else if (df[i, "pvalue"] < 0.05 & df[i, "padj"] >= 0.05){
    df[i, "pinfo"] <- "p"
  } else if (df[i, "pvalue"] >= 0.05){
    df[i, "pinfo"] <- "non"
  }
}
df$pinfo <- factor(df$pinfo, levels=c("padj", "p", "non"))

p <- ggplot(df, aes(x=log2FoldChange, y=-log10(pvalue), color=pinfo, shape=pinfo, fill=pinfo)) +
  geom_point()+
  labs(x=expression(Log[2]*" fold change"),
       y=expression(-Log[10]*italic(P))) +
  geom_vline(xintercept=0, color="black", linetype="dotted", linewidth=.6) +
  geom_hline(yintercept=-log10(0.05), color="black", linetype="dotted", linewidth=.6) +
  theme_classic() +
  theme(legend.title=element_blank(), 
        legend.text.align=0,
        axis.text=element_text(color="black", size=12), 
        axis.ticks.length=unit(0.2, "cm")) +
  scale_color_manual(values=c("red", "red", "black"), 
                     labels=c(expression("Adjusted "*italic(P)*" < 0.05"), expression(italic(P)*" < 0.05"), '')) +
  scale_shape_manual(values=c(16, 1, 21), 
                     labels=c(expression("Adjusted "*italic(P)*" < 0.05"), expression(italic(P)*" < 0.05"), '')) +
  scale_fill_manual(values=c("red", NA, "grey"), 
                    labels=c(expression("Adjusted "*italic(P)*" < 0.05"), expression(italic(P)*" < 0.05"), '')) +
  geom_text_repel(data=df2, aes(label=rownames(df2)), 
                  color="black", size=3, nudge_y=0.01) + 
  guides(color=guide_legend(override.aes=list(shape=c(16, 1, NA), size=3)))

pdf(".pdf")
p
dev.off()
