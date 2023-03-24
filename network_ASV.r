rm(list = ls())
library(igraph)
library(psych)
library(dplyr)
library(RColorBrewer)

asv <- read.table("ASV.xls", header = TRUE, row.names = 1)
asv <- t(asv)

corr <- corr.test(asv, use = "pairwise", 
                  method = "spearman", adjust = "fdr", alpha = .05) 

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

#####
#corr.r <- read.table("correlate_value.xls", header = TRUE, row.names = 1)
#corr.p <- read.table("p_value.xls", header = TRUE, row.names = 1)
#corr.r <- as.matrix(corr.r)
#corr.p <- as.matrix(corr.p)
#corr.r[!upper.tri(corr.r)] <- 0
#corr.p[!upper.tri(corr.p)] <- 0
#####

corr.r[corr.p[upper.tri(corr.p)] > 0.01 | abs(corr.r[upper.tri(corr.r)]) < 0.8] <- 0
# corr.r[corr.p[upper.tri(corr.p)] > 0.05] <- 0
net <- graph_from_adjacency_matrix(corr.r,mode = "undirected",
                                   weighted=TRUE,
                                   diag=FALSE)

# filter out zero degree
zeroEdge <- V(net)[degree(net) == 0]
net <- delete.vertices(net, zeroEdge)
net.weight <- E(net)$weight
# view vertex attributes
# vertex_attr(net)
# edge_attr(net)
# E(net)$weight <- NA

feature <- read.table("taxonomy.xls", header = TRUE)
# select non-zero vertex's row from feature
# order in g is equal to order in V(net)
g <- c()
for (i in 1: length(V(net)$name)){
  g <- c(g, which(feature[, 1] == V(net)$name[i]))
}
asv_g <- feature[g, c(1, 7)]

# transpose back to calculate abundance
t_asv <- as.data.frame(t(asv))
t_asv <- t_asv[g, ]
t_asv$abundance <- rowSums(t_asv)
a <- c()
for (i in rownames(t_asv)){
  a <- c(a, which(asv_g[, 1] == i))
}
abd <- t_asv[a, "abundance"]
aga <- cbind(asv_g, abd)

# set node size
V(net)$size <- log(aga$abd * 1e1)
# set node colors
mycol <- colorRampPalette(brewer.pal(12,'Set3'))(length(unique(aga$genus)))
aga$col <- rep(0, nrow(aga))
for (i in 1: length(unique(aga$genus))){
  aga$col[aga$genus == unique(aga$genus)[i]] <- i
}
V(net)$color <- mycol[aga$col]
# color the edges of the graph based on their source node color
# get the starting node for each edge with the ends()
edge.start <- ends(net, es = E(net), names = F)[, 1]
edge.col <- V(net)$color[edge.start]


# calculate proportion for legend display
total_abundance <- sum(aga$abd)
legend_abundance <- c()
for (i in unique(aga$genus)){
  sub_abundance <- round(sum(filter(aga, grepl(i, genus))$abd) / total_abundance,
                         digits=4) * 100
  legend_abundance <- c(legend_abundance, 
                        paste(i, " (", sub_abundance, "%) ", sep=""))
}

set.seed(12345)
pdf("Bacteria.pdf", width = 210, height = 330, family = "Times")
plot(net, vertex.frame.color = 'gray', vertex.label = NA, 
     edge.lty = 1, edge.color = edge.col, edge.width = 8,
     edge.curved = TRUE)
legend(x = -.8, y = -1.1,
       legend_abundance, pch = 22, pt.bg = mycol, col = 'white',
       pt.cex = 30, cex = 15,
       bty = "n", ncol = 3)
dev.off()
