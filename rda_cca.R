rm(list = ls())
library(vegan)
library(ggplot2)

mycol <- c("#7FC97F","#BEAED4","#FDC086","#386CB0","#F0027F","#BF5B17",
           "#666666","#1B9E77","#7570B3","#66A61E", "#E6AB02","#A6761D")

# 列为样本，行为物种
genus <- read.table('genus.top10.xls', header=TRUE, row.names=1, sep="\t")
mapping <- read.table('mapping.txt', header=TRUE, sep="\t")
genus <- t(genus)
# 列为环境因子，行为样本
env <- read.table('pc.xls', row.names=1, sep='\t', header=TRUE,
                  stringsAsFactors=FALSE, check.names=FALSE)
# 调整环境因子中样品的顺序
env <- env[match(row.names(genus), row.names(env)),]

# 把decorana的结果输出到文件，再读取回来判断Axis lengths范围
# 根据Axis lengths选择是使用常规RDA还是CCA
sink("decorana.txt")
decorana(genus)
sink()
decor <- read.csv("decorana.txt")
axis.length <- as.numeric(strsplit(decor[nrow(decor), 1], " ")[[1]][11])

if (axis.length <= 4){
  rda <- rda(genus ~ ., env, scale=FALSE)
  rda.scaling <- summary(rda, scaling=2)
  # R2校正
  r2 <- RsquareAdj(rda)
  if (is.na(r2$adj.r.squared)){
    rda1 <- paste("RDA1 (", 
                  round(rda.scaling$concont$importance[2, 1], 4) * 100, 
                  "%)", sep="")
    rda2 <- paste("RDA2 (", 
                  round(rda.scaling$concont$importance[2, 2], 4) * 100, 
                  "%)", sep="")
  } else{
    rda1 <- paste("RDA1 (", 
                  round(r2$adj.r.squared * rda.scaling$concont$importance[2, 1], 4) * 100, 
                  "%)", sep="")
    rda2 <- paste("RDA2 (", 
                  round(r2$adj.r.squared * rda.scaling$concont$importance[2, 2], 4) * 100, 
                  "%)", sep="")
  }
  # 约束轴的置换检验及p值校正
  rda_test <- anova(rda, permutations=999)
  sink("rda_annova.txt")
  print(rda_test)
  sink()
  rda_test_axis <- anova(rda, by='axis', permutations=999)
  # p值校正
  rda_test_axis$p_adj <- p.adjust(rda_test_axis$`Pr(>F)`, method='bonferroni')
  sink("rda_axis_annova.txt")
  print(rda_test_axis)
  sink()
  # 环境因子的显著性检验
  rda_ef <- envfit(rda ~ ., data=env, perm=999, choices=c(1, 2), display='sites')
  sink("env_fit.txt")
  print(rda_ef)
  sink()
  # 作图
  samples <- data.frame(rda.scaling$sites)[1:2]
  samples$Sample <- rownames(samples)
  samples <- merge(samples, mapping, by="Sample", sort=FALSE)
  rownames(samples) <- samples[, 1]
  samples <- samples[-1]
  species <- data.frame(rda.scaling$species)[1:2]
  enviro <- data.frame(rda.scaling$biplot)[1:2]
  # 样本与环境因子
  p1 <- ggplot(samples) + 
    scale_color_manual(values=mycol) +
    geom_point(aes(x=RDA1*1.2, y=RDA2*1.2, color=Group), size=3) +
    geom_text(data=samples, 
              aes(x=RDA1*1.4, y=RDA2*1.4, label=rownames(samples), color=Group), 
              size=3) +
    #
    geom_segment(data=enviro, 
                 aes(x=0, xend=RDA1, y=0, yend=RDA2), 
                 arrow=arrow(length=unit(0.3, "cm")), linewidth=1.1, 
                 color=mycol[11], alpha=0.8) +
    geom_text(data=enviro, 
              aes(x=RDA1*1.05, y=RDA2*1.05, label=rownames(enviro)), 
              size=4, colour=mycol[11], check_overlap=TRUE) +
    geom_vline(xintercept=0, lty=2, color="darkgrey")+
    geom_hline(yintercept=0, lty=2, color="darkgrey")+ 
    xlab(rda1) + 
    ylab(rda2) +
    theme_bw() + 
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  ggsave("samples_env_RDA.pdf", p1, width=8, heigh=8)
  ggsave("samples_env_RDA.png", p1, width=8, heigh=8)
  # 样本、环境因子、物种
  p2 <- ggplot(samples) + 
    scale_color_manual(values=mycol) +
    #
    geom_point(aes(x=RDA1*1.2, y=RDA2*1.2, color=Group), size=3) +
    geom_text(data=samples, 
              aes(x=RDA1*1.4, y=RDA2*1.4, label=rownames(samples), color=Group), 
              size=3) +
    #
    geom_segment(data=enviro, 
                 aes(x=0, xend=RDA1, y=0, yend=RDA2), 
                 arrow=arrow(length=unit(0.3, "cm")), linewidth=1.1, 
                 color=mycol[11], alpha=0.8) +
    geom_text(data=enviro, 
              aes(x=RDA1*1.05, y=RDA2*1.05, label=rownames(enviro)), 
              size=4, colour=mycol[11], check_overlap=TRUE) +
    #
    geom_segment(data=species, 
                 #aes(x=0, xend=RDA1*1.1, y=0, yend=RDA2*1.1), 
                 aes(x=0, xend=RDA1*2.6, y=0, yend=RDA2*2.6), 
                 arrow=arrow(length=unit(0.3, "cm")), linewidth=1.1, 
                 color="purple", alpha=0.8) +
    geom_text(data=species, 
              #aes(x=RDA1*1.2, y=RDA2*1.2, label=rownames(species)), 
              aes(x=RDA1*2.7, y=RDA2*2.7, label=rownames(species)), 
              size=3, colour="purple", check_overlap=TRUE) +
    geom_vline(xintercept=0, lty=2, color="darkgrey")+
    geom_hline(yintercept=0, lty=2, color="darkgrey")+ 
    xlab(rda1) + 
    ylab(rda2) +
    theme_bw() + 
    theme(legend.title=element_text(size = 20),
          legend.text=element_text(size = 18),
          axis.title.x=element_text(size = 20),
          axis.title.y=element_text(size = 20),
          axis.text.x=element_text(size = 18),
          axis.text.y=element_text(size = 18),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  ggsave("samples_env_genus_RDA.pdf", p2, width=10, heigh=10)
  ggsave("samples_env_genus_RDA.png", p2, width=10, heigh=10)
  
} else{
  cca_data <- cca(genus ~ ., env, scale = FALSE)
  cca.scaling <- summary(cca_data, scaling=2)
  # R2校正
  r2 <- RsquareAdj(cca_data)
  if (is.na(r2$adj.r.squared)){
    cca1 <- paste("CCA1 (", 
                  round(cca.scaling$concont$importance[2, 1], 4) * 100, 
                  "%)", sep="")
    cca2 <- paste("CCA2 (", 
                  round(cca.scaling$concont$importance[2, 2], 4) * 100, 
                  "%)", sep="")
  } else{
    cca1 <- paste("CCA1 (", 
                  round(r2$adj.r.squared * cca.scaling$concont$importance[2, 1], 4) * 100, 
                  "%)", sep="")
    cca2 <- paste("CCA2 (", 
                  round(r2$adj.r.squared * cca.scaling$concont$importance[2, 2], 4) * 100, 
                  "%)", sep="")
  }
  # 约束轴的置换检验及p值校正
  cca_test <- anova(cca_data, permutations=999)
  sink("cca_annova.txt")
  print(cca_test)
  sink()
  cca_test_axis <- anova(cca_data, by='axis', permutations=999)
  # p值校正
  cca_test_axis$p_adj <- p.adjust(cca_test_axis$`Pr(>F)`, method='bonferroni')
  sink("cca_axis_annova.txt")
  print(cca_test_axis)
  sink()
  # 环境因子的显著性检验
  cca_ef <- envfit(cca_data ~ ., data=env, perm=999, choices=c(1, 2), display='sites')
  sink("env_fit.txt")
  print(cca_ef)
  sink()
  # 作图
  samples <- data.frame(cca.scaling$sites)[1:2]
  samples$Sample <- rownames(samples)
  samples <- merge(samples, mapping, by="Sample", sort=FALSE)
  rownames(samples) <- samples[, 1]
  samples <- samples[-1]
  species <- data.frame(cca.scaling$species)[1:2]
  enviro <- data.frame(cca.scaling$biplot)[1:2]
  # 样本与环境因子
  p1 <- ggplot(samples) + 
    scale_color_manual(values=mycol) +
    geom_point(aes(x=CCA1*1.2, y=CCA2*1.2, color=Group), size=3) +
    geom_text(data=samples, 
              aes(x=CCA1*1.4, y=CCA2*1.4, label=rownames(samples), color=Group), 
              size=3) +
    #
    geom_segment(data=enviro, 
                 aes(x=0, xend=CCA1, y=0, yend=CCA2), 
                 arrow=arrow(length=unit(0.3, "cm")), linewidth=1.1, 
                 color=mycol[11], alpha=0.8) +
    geom_text(data=enviro, 
              aes(x=CCA1*1.05, y=CCA2*1.05, label=rownames(enviro)), 
              size=4, colour=mycol[11], check_overlap=TRUE) +
    geom_vline(xintercept=0, lty=2, color="darkgrey")+
    geom_hline(yintercept=0, lty=2, color="darkgrey")+ 
    xlab(cca1) + 
    ylab(cca2) +
    theme_bw() + 
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  ggsave("samples_env_CCA.pdf", p1, width=8, heigh=8)
  ggsave("samples_env_CCA.png", p1, width=8, heigh=8)
  # 样本、环境因子、物种
  p2 <- ggplot(samples) + 
    scale_color_manual(values=mycol) +
    #
    geom_point(aes(x=CCA1*1.2, y=CCA2*1.2, color=Group), size=3) +
    geom_text(data=samples, 
              aes(x=CCA1*1.4, y=CCA2*1.4, label=rownames(samples), color=Group), 
              size=3) +
    #
    geom_segment(data=enviro, 
                 aes(x=0, xend=CCA1, y=0, yend=CCA2), 
                 arrow=arrow(length=unit(0.3, "cm")), linewidth=1.1, 
                 color=mycol[11], alpha=0.8) +
    geom_text(data=enviro, 
              aes(x=CCA1*1.05, y=CCA2*1.05, label=rownames(enviro)), 
              size=4, colour=mycol[11], check_overlap=TRUE) +
    #
    geom_segment(data=species, 
                 aes(x=0, xend=CCA1*1.1, y=0, yend=CCA2*1.1), 
                 arrow=arrow(length=unit(0.3, "cm")), linewidth=1.1, 
                 color="purple", alpha=0.8) +
    geom_text(data=species, 
              aes(x=CCA1*1.2, y=CCA2*1.2, label=rownames(species)), 
              size=3, colour="purple", check_overlap=TRUE) +
    geom_vline(xintercept=0, lty=2, color="darkgrey")+
    geom_hline(yintercept=0, lty=2, color="darkgrey")+ 
    xlab(cca1) + 
    ylab(cca2) +
    theme_bw() + 
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  ggsave("samples_env_genus_CCA.pdf", p2, width=10, heigh=10)
  ggsave("samples_env_genus_CCA.png", p2, width=10, heigh=10)
}
