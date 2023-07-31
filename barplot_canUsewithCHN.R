rm(list = ls())
library(ggplot2)
library(reshape2)
library(showtext)
library(sysfonts)

otu <- read.table("Archaea/phylum4group.xls", header=T, check.names=FALSE, sep="\t")
mycol <- c("#A3A3A6", "#64221D", "#B72435", "#E46D55", "#F2B587", "#FCD3DF", "#F6E9D9", "#EAF0F2", "#72ACD3", "#795FF9", "#275A81", "#103B86")
mycol <- c("#A3A3A6", "#64221D", "#B72435", "#E46D55", "#F2B587", "#FCD3DF", "#EAF0F2", "#72ACD3", "#795FF9", "#275A81", "#103B86")


mdata <- melt(otu)
# 使用Windows字体
font_add("Hei", "simhei.ttf")
font_add("HeiBold", "msyhbd.ttc")
font_add("TimesIta", "timesi.ttf")
showtext_auto()

p <- ggplot(mdata, aes(x=variable, y=value * 100, fill=Taxonomy)) +
  geom_bar(stat="identity", color="black") +
  xlab("") +
  ylab("中文") +
  labs(title="中文") +
  scale_fill_manual(values=rev(mycol)) +
  theme_classic() +
  theme(
    legend.title=element_blank(),
    axis.text.x=element_text(size=16, color="black", family="Hei"),
    axis.text.y=element_text(size=14, color="black"),
    axis.title.y=element_text(size=18, family="Hei"),
    legend.text=element_text(size=12, color="black", family="TimesIta"),
    plot.title=element_text(size=24,color="#0C00AA", hjust=0.5, face="bold", family="HeiBold")
  ) +
  scale_y_continuous(expand=expansion(mult=c(0, 0)))

pdf("Archaea/phylum4group.pdf")
# showtext_begin()
print(p)
dev.off()

# ggsave(filename="phylum_new.pdf", p, height=8, width=6, limitsize=FALSE)
