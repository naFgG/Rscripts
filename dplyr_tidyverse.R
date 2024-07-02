rm(list=ls())
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(circlize))
library(dplyr)
library(tidyverse)

l3.ave <- read.table("KEGG_L3_ave.xls", sep="\t", header=T, row.names=1, check.names=F, quote="")
l3.ave["sums"] <- rowSums(l3.ave)
top30 <- l3.ave %>% arrange(desc(sums)) %>% head(30) %>% rownames()

l3 <- read.table("KEGG_L3.xls", sep="\t", header=T, row.names=1, check.names=F, quote="")

meta <- read.table("mapping.txt", sep="\t", header=T, check.names=F, quote="")
meta$Growth <- factor(meta$Growth, levels=unique(meta$Growth))
meta$Type <- factor(meta$Type, levels=c("Bulk Soil", "Rhizosphere", "Root"))
# 组去除S
meta$Nitrogen <- gsub("S", "", meta$Nitrogen)
meta <- meta %>% arrange(Nitrogen)

l3 <- l3[top30, meta$Sample]
write.table(cbind("Pathways"=rownames(l3), l3), file="KEGG_L3_Top30.xls", sep="\t", quote=F, row.names=F)
l3 <- t(l3) %>% as.data.frame()
l3$Sample <- rownames(l3)
merged_data <- merge(l3, meta, by="Sample") %>% column_to_rownames("Sample")
grouped_data <- merged_data %>% 
  group_by(Type, Growth, Nitrogen) %>%
  summarise(across(everything(), ~ mean(.x, na.rm=T))) 
# 这里的 across(everything(), ~ mean(.x, na.rm = TRUE)) 表示对所有列（除了分组列）计算平均值，忽略缺失值

col_anno <- grouped_data[, 1:3]
df <- t(grouped_data) %>% as.data.frame()
df <- df[4: nrow(df), ]
colnames(df) <- seq(ncol(df))

kegg <- read.table("predicted_metagenomes.L3.txt", sep="\t", header=T, 
                   row.names=1, check.names=F, quote="", comment.char="")
kegg <- kegg[, "KEGG_Pathways", drop=F]
kegg <- kegg %>% separate(KEGG_Pathways, into=c("Level1", "Level2"), sep="; ",
                          extra="warn", fill="right")
kegg <- kegg[rownames(df), ]
df <- df %>% mutate_all(as.numeric)
df <- t(scale(t(df)))


col_fun <- colorRamp2(c(min(df), max(df)), c("darkblue", "gold"), space="RGB")
lv1_color <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")
names(lv1_color) <- unique(kegg$Level1)
lv2_color <- c("#F8766D", "#E18A00", "#BE9C00", "#8CAB00", "#24B700", "#00BE70", 
               "#00C1AB", "#00BBDA", "#00ACFC", "#8B93FF", "#D575FE", "#F962DD", 
               "#FF65AC")
names(lv2_color) <- unique(kegg$Level2)
row_ha <- rowAnnotation(`Level 1`=kegg$Level1, `Level 2`= kegg$Level2, 
                        col=list(`Level 1`=lv1_color, 
                                 `Level 2`=lv2_color),
                        annotation_name_rot=270, show_annotation_name=T, 
                        gap=unit(4, "points"))
graphics <- list(
  "N0"=function(x, y, w, h){ grid.points(x, y, gp=gpar(col="#F8766D"), pch=16) },
  "N180"=function(x, y, w, h){ grid.points(x, y, gp=gpar(col="#00BA38"), pch=16) },
  "N300"=function(x, y, w, h){ grid.points(x, y, gp=gpar(col="#619CFF"), pch=16) }
)
graphics2 <- list(
  "Bulk Soil"=function(x, y, w, h){ grid.rect(x, y, w, h*0.2, gp=gpar(fill="black", col="black")) },
  "Rhizosphere"=function(x, y, w, h){ grid.rect(x, y, w, h*0.2, gp = gpar(fill="black", col="black")) },
  "Root"=function(x, y, w, h){ grid.rect(x, y, w, h*0.2, gp = gpar(fill="black", col="black")) }
)

col_ha <- HeatmapAnnotation(`Growth Stage`=col_anno$Growth, 
                            Nitrogen=anno_customize(col_anno$Nitrogen, graphics=graphics, border=F),
                            Type=anno_customize(col_anno$Type, graphics=graphics2, border=F),
                            col=list(`Growth Stage`=c("Flowering Stage"="#F8766D", "Maturity Stage"="#00BFC4")), 
                            show_annotation_name=c(T, T, F),
                            annotation_height=unit(c(2, 2, 1), "mm"),
                            gap=unit(4, "points"), 
                            annotation_name_gp=list(fontsize=10))

lgd = Legend(title="Nitrogen", at=names(graphics), graphics=graphics)

hm <- Heatmap(df, col=col_fun, cluster_rows=T, cluster_columns=F, show_column_names=F,
              heatmap_legend_param=list(title="Row Z scores", legend_height=unit(4, "cm")),
              left_annotation=row_ha, bottom_annotation=col_ha, 
              column_split=col_anno$Type, column_gap=unit(0, "mm"),
              column_title_side="bottom", row_names_max_width=unit(12, "cm"))

pdf("KEGG_L3_Top30.pdf", width=18, height=10)        
draw(hm, annotation_legend_list=lgd, legend_grouping="original")
dev.off()
