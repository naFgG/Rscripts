rm(list=ls())
source("common_color.R")
library(ggplot2)
library(dplyr)
library(openxlsx)
library(reshape2)

data <- read.xlsx("alpha多样性/index.xlsx")
rownames(data) <- data$Sample
group <- read.xlsx("alpha多样性/group.xlsx")
data <- data[group$Sample, ]
group$Group <- factor(group$Group, levels=unique(group$Group))
mycolor <- deal_color("micro", length(unique(group$Group)), levels(group$Group))

out <- merge(data, group, by="Sample", sort=F)
out <- melt(out)
colnames(out)[3] <- c("index")

facet_max <- out %>%
  group_by(index) %>%
  summarise(y_max = max(value, na.rm = TRUE))

kw_labels <- out %>%
  group_by(index) %>%
  summarise(
    p_value = kruskal.test(value ~ Group)$p.value
  ) %>%
  mutate(significance=case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01  ~ "**",
    p_value <= 0.05  ~ "*",
    TRUE             ~ " "))  %>%
  left_join(facet_max, by="index") %>%
  mutate(label = paste0("p = ", signif(p_value, 3), "\n     ", significance))
  
p <- ggplot(out, aes(x=Group, y=value, fill=Group, color=Group)) +
  geom_boxplot(alpha=0.5, width=0.8) +
  geom_jitter(alpha=0.8, size=1) +
  facet_wrap(~index, scales="free") +
  theme_bw() +
  scale_color_manual(values=mycolor) + 
  scale_fill_manual(values=mycolor) + 
  theme(axis.title=element_blank(),
        strip.text.x=element_text(face="bold")) +
  geom_text(data=kw_labels, aes(x=1, y=y_max, label=label), inherit.aes=F, hjust=0, size=3) + 
  scale_y_continuous(expand=expansion(mult=c(0.1, 0.1)))

pdf("alpha多样性/index.pdf", width=8, height=8)
print(p)
dev.off()

