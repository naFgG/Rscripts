rm(list = ls())
library(picante)
library(treeio)
library(tidyverse)
library(reshape2)
library(openxlsx)

mapping <- read.table("mapping.txt", sep="\t", check.names=F, header=T, quote="")
colnames(mapping) <- c("sample", "group")
mapping$group <- factor(mapping$group, levels=unique(mapping$group))
#> import tree data
tree <- read.tree("rep_phylo.tre")
#> import asv data
asv <- read.table("feature_54samples_filtered_16S.xls", sep="\t", header=T, row.names=1, check.names=F)
#> rownames are sample names
t_asv <- t(asv)
#> prune tree to match community data
prune_tree <- prune.sample(t_asv, tree)
#> Match taxa in phylogeny and data
match_phylo_asv <- match.phylo.data(prune_tree, asv)

### 第一种方法, 得到的数据是对称矩阵 ####
#> calculate betaNTI
set.seed(1234567)
#> comdistnt: Calculates inter-community mean nearest taxon distance
#> cophenetic: Cophenetic Distances for a Hierarchical Clustering
#> calculate empirical betaMNTD
beta.mntd.weighted <- as.matrix(comdistnt(t(match_phylo_asv$data), 
                                          cophenetic(match_phylo_asv$phy), 
                                          abundance.weighted=T))

if (identical(colnames(match_phylo_asv$data), colnames(beta.mntd.weighted))) {
  #> calculate random betaMNTD
  #> 用于for循环的阵列
  rand.weighted.bNMTD.comp <- array(c(-999), dim=c(ncol(match_phylo_asv$data), ncol(match_phylo_asv$data), 999))
  for (rep in 1: 999){
    rand.weighted.bNMTD.comp[, , rep] <- as.matrix(comdistnt(t(match_phylo_asv$data),
                                                             taxaShuffle(cophenetic(match_phylo_asv$phy)), 
                                                             abundance.weighted=T,
                                                             exclude.conspecifics=F))
    print(c(date(), rep))
  }
  #> 建立空矩阵
  weighted.bNTI <- matrix(c(NA), nrow=ncol(match_phylo_asv$data), 
                          ncol=ncol(match_phylo_asv$data))
  for (columns in 1: (ncol(match_phylo_asv$data) - 1)){
    for (rows in (columns + 1): ncol(match_phylo_asv$data)){
      rand.vals <- rand.weighted.bNMTD.comp[rows, columns, ]
      weighted.bNTI[rows, columns] <- (beta.mntd.weighted[rows, columns] - mean(rand.vals)) / sd(rand.vals)
      rm("rand.vals")
    }
  }
  rownames(weighted.bNTI) <- colnames(match_phylo_asv$data)
  colnames(weighted.bNTI) <- colnames(match_phylo_asv$data)
  
  betaNTI <- weighted.bNTI %>% as_tibble(rownames="sample") %>% 
    melt(id.vars="sample") %>% 
    dplyr::filter(value!="NA") %>% 
    mutate(process=if_else(abs(value) > 2, "deterministic","stochastic"))
}


### 第二种方法, 直接得到每个样本的NTI，且有p值 ####
#> 得到遗传距离矩阵
pd_dist <- cophenetic(match_phylo_asv$phy)
#> 计算MNTD obs
pd_mntd <- mntd(t(match_phylo_asv$data), 
                pd_dist, 
                abundance.weighted=T)
#> 每个样本的MNTD obs, 不需要保存
cbind(pd_mntd, rownames(t(match_phylo_asv$data))) %>% 
  as_tibble() %>% 
  mutate(pd_mntd=as.numeric(pd_mntd))
#> 对数据随机化, 产生null model, 再计算n次MNTD random
z_mntd <- ses.mntd(t(match_phylo_asv$data),
                   pd_dist,
                   null.model="taxa.labels", # 这个自主选择
                   abundance.weighted=T,
                   runs=999,
                   iterations=1000)
write.xlsx(z_mntd %>% mutate(NTI=-mntd.obs.z), 'betaNTI.xlsx', rowNames=T)
#> z_mntd中的mntd.obs.z = -NTI, 所以取NTI值要取反
#> 取作图数据
#> p值小于0.05
#nti <- z_mntd %>% mutate(NTI=-mntd.obs.z) %>% select(mntd.obs.p, NTI) %>%
#  rownames_to_column(var="sample") %>% left_join(mapping, by="sample") %>% 
#  filter(mntd.obs.p < 0.05)
#> 所有的
nti <- z_mntd %>% mutate(NTI=-mntd.obs.z) %>% select(mntd.obs.p, NTI) %>%
rownames_to_column(var="sample") %>% left_join(mapping, by="sample")

#> 带趋势线的箱线图
library(ggplot2)
mycol <- c("#7FC97F", "#BEAED4", "#FDC086", "#386CB0", "#F0027F", "#BF5B17")
pdf('betaNTI.pdf', width=6, height=5)
ggplot(nti, aes(x=group, y=NTI, color=group)) + 
  geom_boxplot(size=1) + 
  geom_jitter(size=2) + 
  geom_hline(yintercept=c(-2, 2), linetype=2, color="grey50") +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        axis.text.x=element_text(color="black", size=11, angle=45, vjust=1, hjust=1),
        axis.text.y=element_text(color="black", size=11), 
        axis.title.y=element_text(color="black", size=13)) +
  scale_y_continuous(limits=c(-3, 5)) + 
  scale_color_manual(values=mycol) +
  guides(color="none") + 
  labs(x="", y=expression(beta*NTI)) +
  geom_smooth(method="loess", se=T, aes(group=1), color="#C86B8C", fill="#EEC6CF")
dev.off()

