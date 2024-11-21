rm(list=ls())
library(ggplot2)
library(dplyr)
library(openxlsx)
library(reshape2)
library(ggalluvial)

# OTU表
data <- read.xlsx("XLSX")
data <- data %>%
  group_by(Taxon) %>% 
  summarise(across(everything(), ~sum(.x))) %>% 
  mutate(across(where(is.numeric), ~ .x / sum(.x))) %>% 
  slice_max(rowSums(across(where(is.numeric))), n=nrow(data))
data$Taxon <- factor(data$Taxon, levels=rev(data$Taxon))
mycolor <- "颜色选择器"

data <- melt(data)
data$value <- data$value * 100

p <- ggplot(data, aes(x=variable, y=value, fill=Taxon, stratum=Taxon, alluvium=Taxon))+
  geom_bar(stat="identity", width=0.7) +
  xlab("") +
  ylab("Relative abundance(%)") +
  scale_fill_manual(values=rev(mycolor)) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(size=13, color="black"),
        axis.text.y = element_text(size=13, color = "black"),
        axis.title.y = element_text(size=15)) +
  guides(fill=guide_legend(ncol=1)) +
  geom_alluvium(width=0.7, alpha=0.5, curve_type="linear") + 
  scale_y_continuous(expand=expansion(mult=c(0.01, 0.01)))

pdf("X.pdf")
print(p)
dev.off()
