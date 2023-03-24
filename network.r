rm(list = ls())
library(igraph)
library(psych)
library(dplyr)

genus <- read.table("genus2.xls", header = TRUE, row.names = 1)
# filter out rows contain 'uncultured'
genus <- filter(genus, !grepl('uncultured', rownames(genus)))
genus <- t(genus)

corr <- corr.test(genus, use = "pairwise", 
                  method = "spearman", adjust = "fdr", alpha = .05) 

# set colors
mycol <- c("#D096CE", "#6BBF07", "#00A9F3", "#5A534A", "#FE9900", "#FD5180",
           "#00A689", "#E6C3BC", "#0048B2", "#FEED02",
           "#999999", "#A6CEE3","#FB9A99","#E31A1C","#E6AB02","#6A3D9A",
           "#FC8D62","#A6D854","#FFD92F","#BEBADA","#FB8072","#80B1D3",
           "#8DA0CB","#4DAF4A","#984EA3","#c6c386","#66C2A5","#B2DF8A",
           "#7FC97F","#BEAED4","#FDC086","#386CB0","#F0027F","#BF5B17",
           "#666666","#1B9E77","#7570B3","#66A61E", "#FF7F00","#A6761D")

corr.r <- corr$r
corr.r.write <- cbind(rownames(corr.r), corr.r)
write.table(corr.r.write, file = "correlate_value.xls", sep = "\t",
            row.names = FALSE)
# use upper matrix
corr.r[!upper.tri(corr.r)] <- 0

corr.p <- corr$p
corr.p.write <- cbind(rownames(corr.p), corr.p)
write.table(corr.p.write, file = "p_value.xls", sep = "\t",
            row.names = FALSE)
corr.p[!upper.tri(corr.p)] <- 0

# corr.r[corr.p[upper.tri(corr.p)] > 0.01 | abs(corr.r[upper.tri(corr.r)]) < 0.8] <- 0
corr.r[corr.p[upper.tri(corr.p)] > 0.05] <- 0
net <- graph_from_adjacency_matrix(corr.r,mode = "undirected",
                                      weighted=TRUE,
                                      diag=FALSE)

# zeroEdge <- V(net)[degree(net) == 0]
# net <- delete.vertices(net, zeroEdge)
net.weight <- E(net)$weight
# view vertex attributes
# vertex_attr(net)
# E(net)$weight <- NA

feature <- read.table("feature_L6.txt", header = TRUE)
# filter out rows contain 'Other' & 'uncultured'
feature <- filter(feature, !grepl('Other', Taxon))
feature <- filter(feature, !grepl('uncultured', Taxon))

# calculate total abundance
k <- c()
g <- c()
for (i in strsplit(feature[, 1], ";")){
  k <- c(k, unlist(strsplit(i[2], "p__"))[2])
  g <- c(g, unlist(strsplit(i[6], "g__"))[2])
}
feature <- cbind(k, g, feature[, -1])
feature$abundance <- rowSums(feature[, c(-1, -2)])
feature <- feature[, c(1, 2, ncol(feature))]
feature <- feature[which(feature$abundance > 0), ]

v2f <- c()
for (i in V(net)$name) v2f <- c(v2f, which(feature$g == i))
# select and order vertex's tax and abundance
kga <- feature[v2f, ]

# sort
kga <- arrange(kga, kga$k)
kga$klevel <- factor(kga$k, labels = 1: length(unique(kga$k)))

# set node colors
vcol <- c()
for (i in 1:length(kga$g)){vcol <- c(vcol, kga[which(kga$g == V(net)$name[i]), 4])}
V(net)$color <- mycol[vcol]
# set node size
vsize <- c()
for (i in 1:length(kga$g)){vsize <- c(vsize, which(kga$g == V(net)$name[i]))}
V(net)$size <- log(kga$abundance[vsize] * 1e6)
# color the edges of the graph based on their source node color
# get the starting node for each edge with the ends()
edge.start <- ends(net, es = E(net), names = F)[, 1]
edge.col <- V(net)$color[edge.start]

set.seed(123)
pdf("Bacteria.pdf", width = 10, height = 12)
plot(net, vertex.frame.color = 'gray', vertex.label = NA, 
     edge.lty = 1, edge.color = edge.col, edge.width = 2,
     edge.curved = TRUE)
legend(x = -.5, y = -1.2,
       unique(kga$k), pch = 22, pt.bg = unique(mycol[kga$klevel]),
       pt.cex = 2, cex = 1,
       bty = "n", ncol = 3)
dev.off()
