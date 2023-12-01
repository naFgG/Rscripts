rm(list=ls())
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(circlize))

args <- commandArgs(T)
#> 样本分组文件
mapping <- read.table(args[1], sep="\t", check.names=F, header=T, comment.char="")
mapping <- mapping[c("#SampleID", "Group")]
mapping$Group <- factor(mapping$Group, levels=unique(mapping$Group))
#> 丰度文件
df <- read.table(args[2], sep="\t", check.names=F, row.names=1, header=T, comment.char="")
df <- t(scale(t(df)))

col_fun <- colorRamp2(c(min(df), 0, max(df)), c("firebrick", "white", "navy"), 
                      space="RGB")
group_col <- c("#FDC187", "#00B037", "#4892F9")
names(group_col) <- unique(mapping$Group)
col_anno <- HeatmapAnnotation(group=mapping$Group, 
                              col=list(group=group_col), 
                              show_annotation_name=F, 
                              show_legend=F)

hm <- Heatmap(df, col=col_fun, 
              heatmap_legend_param=list(title=NULL, legend_height=unit(4, "cm")),
              cluster_rows=T, cluster_columns=F, 
              column_split=mapping$Group, column_gap=unit(0, "mm"),
              top_annotation=col_anno, 
              show_column_names=F, 
              row_names_max_width=unit(12, "cm"),)
pdf(args[3], width=12)
hm
dev.off()
