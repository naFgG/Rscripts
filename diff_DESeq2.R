rm(list=ls())
library(DESeq2)
library(ggplot2)
library(ggrepel)
library(tidyverse)

countdata <- read.table("genus/genus.xls", header=T, row.names=1, check.names=F)
# Abundance top 30
# top30 <- countdata %>% rowSums() %>% sort(decreasing=T) %>% head(30) %>% names()
# countdata <- countdata[top30, ]

coldata <- read.table("mapping.txt", header=T, row.names=1, check.names=F)
countdata <- countdata[, rownames(coldata)]
coldata$Group <- factor(coldata$Group, levels=unique(coldata$Group))

for (i in rownames(countdata)){
  for (j in colnames(countdata)){
    countdata[i, j] <- round(countdata[i, j], digits=0)
  }
}

dds <- DESeqDataSetFromMatrix(countData=countdata, colData=coldata, design=~ Group)
# 一键式差异分析
dds2 <- DESeq(dds)
# res <- results(dds2, contrast=c("Group", levels(coldata$Group)[1], levels(coldata$Group)[2]))
# 默认 对照 vs. 控制
res <- results(dds2)
# openxlsx::write.xlsx(as.data.frame(res), "genus/diff_genus_top30.xlsx", rowNames=T)
openxlsx::write.xlsx(as.data.frame(res), "genus/diff_genus.xlsx", rowNames=T)

# 画图
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

# padj < 0.05 min30
df2 <- df %>% arrange(padj) %>% head(15)
# all p
# df2 <- subset(df, pvalue < 0.05)

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
 # scale_y_continuous(breaks=seq(0, 25, by=5), expand=c(0.01, 0.05)) +
  guides(color=guide_legend(override.aes=list(shape=c(16, 1, NA), size=3)))

# pdf("genus/volcano_top30.pdf")
pdf("genus/volcano_pmin15.pdf")
p
dev.off()
