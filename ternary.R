rm(list=ls())
library(ggtern)
library(reshape2)
library(multcomp)
library(tidyverse)
library(magrittr)

args <- commandArgs(T)
args <- c("feature.xls", "mapping.txt")

asv_df <- read.table(args[1], sep="\t", header=T, quote="", check.names=F, row.names=1)
rownames(asv_df) <- paste0("ASV_", rownames(asv_df))
asv_df$ASV <- rownames(asv_df)
# 这个mapping只有 Sample + Group 两列
meta <- read.table(args[2], sep="\t", header=T, quote="", check.names=F)

asv_long <- melt(asv_df, id="ASV")
colnames(asv_long) <- c("ASV", "Sample", "Abundance")
asv_long <- merge(asv_long, meta, by="Sample", sort=F)

# 初始化结果数据框
results <- data.frame(ASV=rownames(asv_df), Group="-", stringsAsFactors=F)

# 进行 ANOVA 和 Tukey HSD 检验
for (asv in rownames(asv_df)) {
  asv_data <- asv_long[asv_long$ASV == asv, ]
  # ANOVA
  anova_res <- aov(Abundance ~ Group, data=asv_data)
  # 检查 ANOVA 检验的显著性
  if (summary(anova_res)[[1]][["Pr(>F)"]][1] < 0.05) {
    # Tukey HSD
    tukey_res <- TukeyHSD(anova_res)
    # 提取显著性结果
    tukey_df <- as.data.frame(tukey_res$Group)
    colnames(tukey_df)[4] <- "padj"
    # 提取均值差异显著的组
    sig_groups <- rownames(tukey_df)[tukey_df$padj < 0.05]
    if (length(sig_groups) > 0) {
      # 比较组均值，均值最大的组为富集组
      group_means <- aggregate(Abundance ~ Group, data=asv_data, mean)
      # 提取最大值所在行
      enriched_group <- group_means %>% filter(Abundance == max(Abundance))
      enriched_group %<>% .[1, "Group"]

      results[results$ASV == asv, "Group"] <- enriched_group
    }
  }
}

# 按组求平均ASV表
result_asv <- dcast(asv_long[2:4], ASV ~ Group, mean, value.var="Abundance")
result_asv <- result_asv[, c("ASV", unique(meta$Group))]

plot_data <- merge(results, result_asv, by="ASV", sort=F)
write.table(plot_data, file="Ternary_ASV.xls", sep="\t", row.names=F, quote=F)

plot_data$average <- apply(plot_data[3: 5], 1 , mean)
plot_data$Group <- factor(plot_data$Group, levels=c(unique(meta$Group), "-"))

summy <- plot_data %>% count(Group) %>% column_to_rownames("Group")

pdf("Ternary_ASV.pdf", width=8, height=5)
ggtern(plot_data, aes(plot_data[,3], plot_data[, 4], plot_data[, 5], size=average)) +
  geom_point(alpha=0.8, aes(color=Group)) +
  geom_mask() + 
  labs(x=colnames(plot_data)[3],
       y=colnames(plot_data)[4],
       z=colnames(plot_data)[5]) +
  scale_colour_manual(values=c("green3", "black", "red", "gray"), 
                      labels=c(paste0(levels(plot_data$Group)[1], "_ASVs (", summy[levels(plot_data$Group)[1], 1], ")"), 
                               paste0(levels(plot_data$Group)[2], "_ASVs (", summy[levels(plot_data$Group)[2], 1], ")"), 
                               paste0(levels(plot_data$Group)[3], "_ASVs (", summy[levels(plot_data$Group)[3], 1], ")"), 
                               " ")) + 
  theme_bw(base_size=15) +
  guides(size="none",
         color=guide_legend(title="", 
                            override.aes=list(alpha=c(1, 1, 1, 0), size=3)))
dev.off()
