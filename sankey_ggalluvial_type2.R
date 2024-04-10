rm(list=ls())
source("utilities.R")
library(reshape2)
library(ggalluvial)
library(ggrepel)

args <- commandArgs(T)
args <- c("L6.xls", "sankey_ggalluvial_type2.pdf")

genus <- read.table(args[1], sep="\t", header=T, quote="", check.names=F)
genus <- filter(genus, !grepl("Other|unclassified|no_rank", genus)) %>% 
  tibble::column_to_rownames(var="genus") %>%
  mutate(rowsum=rowSums(.)) %>% 
  arrange(desc(rowsum)) %>%
  head(30) %>% 
  select(rowsum) %>%
  tibble::rownames_to_column(var="genus")
taxon <- tidyr::separate(genus, genus, 
                         into=c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus"), 
                         sep=";") %>%
  select(c("Phylum", "Family", "Genus", "rowsum"))

p <- ggplot(taxon, aes(axis1=Phylum, axis2=Family, axis3=Genus, y=rowsum)) +
  geom_alluvium(aes(fill=Genus)) + 
  geom_stratum() +
  geom_text(aes(label=after_stat(stratum)), stat="stratum", size=3) +
  labs(x="", y="") + 
  theme_minimal() +
  theme(panel.grid=element_blank(), 
        axis.text=element_blank(), 
        legend.position="none") +
  scale_fill_manual(values=mycol3[1: length(taxon$Genus)])

pdf(args[2])
p
dev.off()

