rm(list=ls())
source("utilities.R")
library(networkD3)
library(dplyr)
library(htmlwidgets)

args <- commandArgs(T)
args <- c("L6.xls", "sankey_networkD3.html")

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

family_genus <- taxon[c("Family", 'Genus', 'rowsum')]
names(family_genus) <- c('source', 'target', 'abundance')
#> 分组求和
phylum_family <- aggregate(taxon$rowsum, by=list(taxon$Phylum, taxon$Family), FUN=sum)
names(phylum_family) <- c('source', 'target', 'abundance')

links <- rbind(phylum_family, family_genus)
#> 根据nodes的物种顺序，给links物种确定ID
nodes <- reshape2::melt(taxon, id="rowsum")
nodes <- nodes[!duplicated(nodes$value), ] %>% select(-rowsum)
links$sourceID <- match(links$source, nodes$value) - 1 
links$targetID <- match(links$target, nodes$value) - 1

p <- sankeyNetwork(Links=links, Nodes=nodes, 
                   Source="sourceID", Target="targetID", 
                   Value="abundance", NodeID="value", 
                   NodeGroup="variable", 
                   fontSize=12, sinksRight=F)
saveWidget(p, args[2])
