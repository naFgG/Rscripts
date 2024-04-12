rm(list=ls())
source("utilities.R")
library(reshape2)
library(ggh4x)

args <- commandArgs(T)
# args <- c("species.others.top15.xls", "mapping.txt", "species.others.top15.pdf")

otu <- read.table(args[1], header=T, sep="\t", check.names=F, quote="")
barcol <- colorRampPalette(mycol)(nrow(otu))
otu <- melt(otu)
colnames(otu) <- c("tax", "Sample", "abundance")
otu$tax <- factor(otu$tax, levels=unique(otu$tax))
meta <- read.table(args[2], header=T, sep="\t", check.names=F, quote="")
otu$Sample <- factor(otu$Sample, levels=meta$Sample)
df <- merge(otu, meta, by="Sample", sort=F)
df$Group <- factor(df$Group, levels=unique(meta$Group))

p <- ggplot(df, aes(x=abundance, y=weave_factors(Sample, Group), fill=tax)) + 
  geom_vline(xintercept=c(0, 0.2, 0.4, 0.6, 0.8), color="black")+
  geom_bar(stat="identity", position="fill", color="black", linewidth=0.5, width=0.6) + 
  labs(x="Relative abundance (%)", 
       y="") +
  theme_classic() + 
  theme(axis.text.y=element_text(color="black", size=14), 
        ggh4x.axis.nesttext.y=element_text(size=17),
        axis.text.x.top=element_text(color="black", size=14, vjust=0.5), 
        axis.title.x.top=element_text(color="black", size=18, vjust=1), 
        axis.ticks=element_blank(),
        axis.line.x=element_blank(),
        ggh4x.axis.nestline.y=element_line(linewidth=1.2),
        axis.line.y=element_line(linewidth=1.1), 
        legend.key.spacing.y=unit(0.1, "cm"), 
        legend.text=element_text(size=14)) +
  scale_fill_manual(values=barcol, name=NULL) + 
  scale_x_continuous(expand=expansion(mult=c(0, 0)), 
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                     labels=c("0%", "20%", "40%", "60%", "80%", "100%"), 
                     position="top") + 
  scale_y_discrete(expand=expansion(mult=c(0.05, 0.05))) + 
  guides(y="axis_nested", 
         y.sec=guide_axis_manual(labels=NULL), 
         fill=guide_legend(ncol=2))

w <- ((max(nchar(meta$Sample)) + max(nchar(meta$Group))) %/% 4) + 11
h <- nrow(meta) / 2 + 1
pdf(args[3], width=w, height=h)
p
dev.off()
