rm(list = ls())
library(ggplot2)
library(reshape2)
library(RColorBrewer)

otu <- read.table("传统方法鱼类Top10.xls", header=T, check.names=FALSE, sep="\t")
mycol <- colorRampPalette(brewer.pal(6, "Set3"))(11)

mdata <- melt(otu)
mdata$site <- factor(mdata$site, levels=paste0("st", 1:24))

p <- ggplot(mdata, aes(x=site, y=value, fill=variable)) +
    geom_bar(stat="identity") +
    xlab("") +
    ylab("Relative abundance (%)") +
    scale_fill_manual(values=rev(mycol)) +
    theme_classic() +
    theme(
        legend.title=element_blank(),
        axis.text.x=element_text(size=12, angle=45, hjust=1, color="black"),
        axis.text.y=element_text(size=12, color="black"),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=12, face="italic", color="black")
    ) +
    scale_y_continuous(expand=expansion(mult=c(0.01, 0.01)))

ggsave(filename="传统捕捞Top10堆叠图.pdf", p, height=9, width=12, limitsize=FALSE)
