rm(list=ls())
library("reshape2")
library("ggplot2")
library("ggpubr")
library("rstatix")
library("dplyr")

index <- read.table("两种方法四季香农指数.xls", sep="\t", header=TRUE, check.names=FALSE)
mIndex <- melt(index, id="method")
colnames(mIndex)[2: 3] <- c("season", "index")
mIndex$method <- factor(mIndex$method)

tTest <- mIndex %>% t_test(index ~ method) %>% 
  adjust_pvalue(method="bonferroni") %>% 
  add_significance(p.col='p', cutpoints=c(0, 0.001, 0.01, 0.05, 1), symbols=c('***', '**', '*', 'ns'))
write.table(tTest, '香农指数t检验值.txt', sep="\t", quote = F, row.names=F)

wilTest <- mIndex %>% wilcox_test(index ~ method) %>% 
  adjust_pvalue(method="bonferroni") %>% 
  add_significance(p.col='p', cutpoints=c(0, 0.001, 0.01, 0.05, 1), symbols=c('***', '**', '*', 'ns'))
write.table(wilTest, '香农指数wilcoxon秩和检验.txt', sep="\t", quote = F, row.names=F)

for (t in c("T test", "Wilcoxon test")){
  bxp <- ggplot(mIndex, aes(x=method, y=index, color=method))+
    geom_boxplot(outlier.shape=NA, lwd=1)+
    geom_point(position=position_jitterdodge(), alpha=0.5, size=2)+
    labs(x="", y="", title=t)+
    theme_bw()+
    theme(panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(),
          axis.text.x=element_blank(), 
          legend.title=element_blank(), 
          plot.title=element_text(hjust=.5), 
          legend.text=element_text(size=12), 
          legend.key.size=unit(20, "point"), 
          legend.spacing=unit(1, "cm"))
  if (t == "T test"){
    # T test
    sigPos <- tTest %>% add_xy_position(x="method", dodge=0.8)
    bxp <- bxp + stat_pvalue_manual(sigPos, label="p.signif", tip.length=0.01, hide.ns=TRUE)
    pdf("香农指数t检验箱线图.pdf", height=5, width=6)
    print(bxp)
    dev.off()
  } else {
    # wilcoxon test
    sigPos <- wilTest %>% add_xy_position(x="method", dodge=0.8)
    bxp <- bxp + stat_pvalue_manual(sigPos, label="p.signif", tip.length=0.01, hide.ns=TRUE)
    pdf("香农指数wilcoxon秩和检验箱线图.pdf", height=5, width=6)
    print(bxp)
    dev.off()
  }
}
