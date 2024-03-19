rm(list=ls())
library(ggplot2)
library(dplyr)

args <- commandArgs(T)

mycol <- c("#F0A3FF", "#0075DC", "#993F00", "#4C005C", "#2BCE48", "#FFCC99", 
           "#808080", "#94FFB5", "#8F7C00", "#9DCC00", "#C20088", "#003380", 
           "#FFA405", "#FFA8BB", "#426600", "#FF0010", "#5EF1F2", "#00998F", 
           "#740AFF", "#990000", "#FFFF00", "#CD2990", "#9F79EE", "#CD5C5C", 
           "#63B8FF", "#EECBAD", "#CD2626", "#7CFC00", "#FF7F00", "#C6E2FF")

df <- read.table(args[1], header=T, check.names=F, quote="", stringsAsFactors=F)
df$Sum <- df[, 2: ncol(df), drop=F] %>% rowSums()
top15 <- df %>% arrange(desc(Sum)) %>% head(15)
top15 <- top15[, -ncol(top15)]
others <- df %>% arrange(desc(Sum)) %>% tail(nrow(df)-15)
top15["Others", ] <- c("Others", others[, 2: (ncol(df)-1), drop=F] %>% colSums())
colnames(top15) <- c("id", "abundance")
top15$foo <- "foo"
top15$id <- factor(top15$id, levels=top15$id)

pdf(args[2])
ggplot(top15, aes(x=foo, y=as.numeric(abundance)/sum(df$Sum)*100, fill=id)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta="y", start=0) +
  labs(x="", y="") +
  theme_minimal() +
  theme(panel.grid=element_blank(), 
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        legend.title=element_blank()) +
  scale_fill_manual(values=mycol)
dev.off()

