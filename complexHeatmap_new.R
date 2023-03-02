library(ComplexHeatmap) 
library(circlize)

data<-read.table("KEGG_L3_sorted.xls", header=T, sep='\t', row.names=1, check.names=F)
for (i in (1:nrow(data))){
  rownames(data)[i] <- sub(",s", "'s", rownames(data)[i], fixed = T)
}
data<-data[which(rowSums(data) > 0),]

map <-read.table("mapping.txt", header=T, sep="\t", row.names=1,
                 check.names=F, comment.char ="")
anno <-read.table("anno.xls", header=T, sep="\t", row.names=1, 
                  check.names=F, comment.char ="", quote="")

map$Group <- factor(map$Group,levels=unique(map$Group))
anno_columns<-map[,'Group',drop=F]
mycol <- c("#7FC97F","#BEAED4","#FDC086","#386CB0","#F0027F","#BF5B17",
           "#666666","#1B9E77","#7570B3","#66A61E", "#E6AB02","#A6761D",
           "#A6CEE3","#B2DF8A","#FB9A99","#E31A1C","#FF7F00","#6A3D9A",
           "#8DA0CB","#4DAF4A","#984EA3","#c6c386","#999999","#66C2A5",
           "#FC8D62","#A6D854","#FFD92F","#BEBADA","#FB8072","#80B1D3",
           "#FDB462","#BC80BD","#B3B3B3","#33A02C","#B3DE69","#4038b0",
           "#ee7576","#e94749","#E78AC3","#ff0000","#A65628","#d80172",
           "#F781BF","#D95F02","#E7298A","#1F78B4","#FDBF6F","#CAB2D6",
           "#B15928","#FBB4AE", "#B3CDE3",'#0173b2','#de8f05','#029e73',
           '#d55e00','#cc78bc','#ca9161','#fbafe4','#949494','#ece133',
           '#56b4e9',"#00AFBB","#E7B800","#FC4E07","#FFDB6D","#C4961A",
           "#F4EDCA","#D16103","#C3D7A4","#52854C","#4E84C4","#293352")

col_fun <- colorRamp2(c(-4, 0, 4), c("#24649B", "#FFFFFF", "#98262D"))
col_ha <- HeatmapAnnotation(Group = anno_block(
  gp = gpar(fill = mycol[1: length(unique(map$Group))], col = "white"), 
  labels = anno_columns$Group, height = unit(1, "cm")))
row_ha <- rowAnnotation(Level1 = anno$Level1, 
                        Level2 = anno$Level2, 
                        gap = unit(4, "points"),
                        show_annotation_name = FALSE)

data <- t(scale(t(data), center = TRUE, scale = TRUE))

pdf("KEGG_L3.heatmap.pdf", height = 58, width = 20)
hm <- Heatmap(data, heatmap_legend_param = list(at = c(-4, -2, 0, 2, 4), title = " ", legend_height = unit(6, "cm")),
        rect_gp = gpar(col = "white", lwd = 0.5), 
        cluster_rows = FALSE, cluster_columns = FALSE, 
        top_annotation = col_ha, column_split = 1:length(unique(map$Group)), column_title = NULL,
        left_annotation = row_ha, show_column_names = FALSE, row_names_max_width = unit(12, "cm"),
        width = ncol(data)*unit(120, "pt"), height = nrow(data)*unit(15, "pt")) 
draw(hm, legend_grouping = "original")
dev.off()
