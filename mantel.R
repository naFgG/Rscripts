rm(list=ls())
library(linkET)
library(dplyr)
library(ggplot2)

args <- commandArgs(T)
# args <- c("genus.Wilcoxon.diff.heatmap.xls", "分组_表型.xls", "mantel_test.pdf")

genus <- read.table(args[1], header=T, row.names=1, sep="\t", quote="", check.names=F)
env <- read.table(args[2], header=T, row.names=1, sep="\t", quote="", check.names=F)
env <- env[colnames(genus), ]
genus <- t(genus)

#> 每个物种一列的列表，物种index为所在第几列
genus_ls <- list()
for (i in 1:ncol(genus)) genus_ls[colnames(genus)[i]] <- i

mantel <- mantel_test(genus, env, spec_select=genus_ls) %>% 
  mutate(rd=cut(r, breaks=c(-Inf, 0.25, 0.5, Inf), 
                labels = c("<= 0.25", "0.25 < x <= 0.5", "< 0.5")),
         pd=cut(p, breaks=c(-Inf, 0.001, 0.01, 0.05, Inf), 
                labels=c("<= 0.001", "0.001 < x <= 0.01", "0.01 < x <= 0.05", "> 0.05")))

#> 需要对理化因子做一次相关性，画热图
p <- qcorrplot(correlate(env, method="spearman"), type="upper", diag=F) +
  geom_square() +
  geom_couple(mantel, aes(color=pd, size=rd), 
              curvature=nice_curvature(),
              node.shape=c(17, 16), 
              node.color="black") +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values=c(0.3, 0.5, 0.8)) +
  scale_color_manual(values=color_pal(4)) +
  guides(size=guide_legend(title="Mantel's r", order=2), 
         colour=guide_legend(title="Mantel's P-value", order=1),
         fill=guide_colorbar(title="Spearman's r", order=3))

pdf(args[3], width=11)
p
dev.off()
