rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)

edna <- read_excel("绘图数据/eDNA优势度.xlsx")
edna["method"] <- rep("eDNA Based", nrow(edna))
fish <- read_excel("绘图数据/传统方法优势度.xlsx")
fish["method"] <- rep("Fishing-net Based", nrow(fish))
bined <- edna %>% bind_rows(fish)

p <- ggplot(bined, aes(x=method, y=Species, size=Y, color=method)) + 
  geom_point() + 
  theme_bw() +
  theme(axis.title.y=element_blank(), 
        axis.text.y=element_text(face="italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=12, angle=45, hjust=1),
        ) +
  scale_color_manual(values=c("#386CB0", "#FDC086")) + 
  scale_size_continuous(name="Dominance index value") +
  guides(color="none")
pdf("优势度气泡图.pdf", height=15, width=9)
print(p)
dev.off()
