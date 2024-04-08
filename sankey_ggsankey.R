rm(list=ls())
source("utilities.R")
library(ggsankey)
library(ggplot2)
library(dplyr)

args <- commandArgs(T)
args <- c("L6.xls", "sankey_ggsankey.pdf")

genus <- read.table(args[1], sep="\t", header=T, quote="", check.names=F)
genus <- filter(genus, !grepl("Other|unclassified|no_rank", genus)) %>% 
  tibble::column_to_rownames(var="genus") %>%
  mutate(rowsum=rowSums(.)) %>% 
  arrange(desc(rowsum)) %>%
  head(30) %>% 
  select(-last_col()) %>%
  tibble::rownames_to_column(var="genus")

taxon <- tidyr::separate(genus, genus, 
                         into=c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus"), 
                         sep=";") %>%
  select(c("Phylum", "Family", "Genus"))
for (c in colnames(taxon)) taxon[[c]] <- factor(taxon[[c]], levels=unique(taxon[[c]]))
taxon <- taxon %>% make_long(Phylum, Family, Genus)

p <- ggplot(taxon, aes(x=x, next_x=next_x, node=node, next_node=next_node, fill=node)) +
  geom_sankey(node.color="black", flow.alpha=0.5) +
  geom_sankey_text(aes(label=paste0("      ", node)), # 加了6个空格使标签在右边
                   size=2, hjust=0, color="black") +
  scale_fill_manual(values=mycol3[1: length(unique(taxon$node))]) +
  theme_sankey() +
  labs(x=NULL) +
  theme(legend.position="none", 
        axis.text.x=element_text(color="black"))

pdf(args[2])
p
dev.off()
