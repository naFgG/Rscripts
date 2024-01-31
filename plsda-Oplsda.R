rm(list=ls())
library(ropls)
library(ggplot2)
library(tools)

args <- commandArgs(T)

mycol <- c("#7FC97F", "#BEAED4", "#FDC086", "#386CB0", "#F0027F", "#BF5B17",
           "#666666", "#1B9E77", "#7570B3", "#66A61E", "#E6AB02", "#A6761D")
path <- file_path_sans_ext(args[1])

df <- read.table(args[1], header=T, row.names=1, check.names=F, quote="")
df <- t(df)
meta <- read.table(args[2], header=T, check.names=F, quote="")
rownames(meta) <- meta$Sample
meta$Group <- factor(meta$Group, levels=unique(meta$Group))
df <- df[rownames(meta), ]
group.nums <- length(unique(meta$Group))

if (group.nums == 2) {
  # oplsda
  oplsda <- opls(df, meta$Group, predI=1, orthoI=1)
  dev.off()
  write.table(oplsda@summaryDF, file=sprintf("%s.OPLS-DA.summary.xls", path), 
              quote=F, sep="\t", row.names=F, eol="\n")
  
  vip <- as.data.frame(oplsda@vipVn)
  vip <- cbind(rownames(vip), vip)
  colnames(vip) <- c("", "VIP")
  write.table(vip, file=sprintf("%s.OPLS-DA.vip.xls", path), quote=F, sep="\t", row.names=F, eol="\n")
  
  data1 <- as.data.frame(oplsda@scoreMN)
  data1 <- cbind(data1, oplsda@orthoScoreMN)
  write.table(cbind(Sample=rownames(data1), data1), file=sprintf("%s.OPLS-DA.score.xls", path), quote=F, sep="\t", row.names=F, eol="\n")
  data1$Sample <- rownames(data1)
  data1 <- merge(data1, meta, by="Sample", sort=F)
  
  p1 <- ggplot(data1, aes(x=p1, y=o1, color=Group, fill=Group)) + 
    geom_point(cex=2.5) + 
    stat_ellipse(geom="polygon", level=0.95, type="norm", show.legend=F, alpha=.3, color=NA) +
    labs(x=paste0("p1 (", oplsda@modelDF["p1", 1]*100, "%)")) +
    theme_bw() +
    theme(panel.grid=element_blank(), 
          legend.title=element_blank()) + 
    scale_color_manual(values=mycol[1: 2]) + 
    scale_fill_manual(values=mycol[1: 2])
  pdf(sprintf("%s.OPLS-DA.pdf", path), width=6, height=5)
  print(p1)
  dev.off()
  
  # plsda
  plsda <- opls(df, meta$Group, predI=2, orthoI=0)
  dev.off()
  write.table(plsda@summaryDF, file=sprintf("%s.PLS-DA.summary.xls", path), 
              quote=F, sep="\t", row.names=F, eol="\n")
  
  vip2 <- as.data.frame(plsda@vipVn)
  vip2 <- cbind(rownames(vip2), vip2)
  colnames(vip2) <- c("", "VIP")
  write.table(vip2, file=sprintf("%s.PLS-DA.vip.xls", path), quote=F, sep="\t", row.names=F, eol="\n")
  
  data2 <- as.data.frame(plsda@scoreMN[, c(1, 2)])
  write.table(cbind(Sample=rownames(data2), data2), file=sprintf("%s.PLS-DA.score.xls", path), quote=F, sep="\t", row.names=F, eol="\n")
  data2$Sample <- rownames(data2)
  data2 <- merge(data2, meta, by="Sample", sort=F)
  
  p2 <- ggplot(data2, aes(x=p1, y=p2, color=Group, fill=Group)) + 
    geom_point(cex=2.5) + 
    stat_ellipse(geom="polygon", level=0.95, type="norm", show.legend=F, alpha=.3, color=NA) +
    labs(x=paste0("p1 (", plsda@modelDF["p1", 1]*100, "%)"), 
         y=paste0("p2 (", plsda@modelDF["p2", 1]*100, "%)")) +
    theme_bw() +
    theme(panel.grid=element_blank(), 
          legend.title=element_blank()) + 
    scale_color_manual(values=mycol[1: 2]) + 
    scale_fill_manual(values=mycol[1: 2])
  pdf(sprintf("%s.PLS-DA.pdf", path), width=6, height=5)
  print(p2)
  dev.off()
} else {
  # plsda
  plsda <- opls(df, meta$Group, predI=2, orthoI=0)
  graphics.off()
  write.table(plsda@summaryDF, file=sprintf("%s.PLS-DA.summary.xls", path), 
              quote=F, sep="\t", row.names=F, eol="\n")
  
  vip2 <- as.data.frame(plsda@vipVn)
  vip2 <- cbind(rownames(vip2), vip2)
  colnames(vip2) <- c("", "VIP")
  write.table(vip2, file=sprintf("%s.PLS-DA.vip.xls", path), quote=F, sep="\t", row.names=F, eol="\n")
  
  data2 <- as.data.frame(plsda@scoreMN[, c(1, 2)])
  write.table(cbind(Sample=rownames(data2), data2), file=sprintf("%s.PLS-DA.score.xls", path), quote=F, sep="\t", row.names=F, eol="\n")
  data2$Sample <- rownames(data2)
  data2 <- merge(data2, meta, by="Sample", sort=F)

  p <- ggplot(data2, aes(x=p1, y=p2, color=Group, fill=Group)) + 
    geom_point(cex=2.5) + 
    stat_ellipse(geom="polygon", level=0.95, type="norm", show.legend=F, alpha=.3, color=NA) +
    labs(x=paste0("p1 (", plsda@modelDF["p1", 1]*100, "%)"), 
         y=paste0("p2 (", plsda@modelDF["p2", 1]*100, "%)")) +
    theme_bw() +
    theme(panel.grid=element_blank(), 
          legend.title=element_blank()) + 
    scale_color_manual(values=mycol[1: group.nums]) + 
    scale_fill_manual(values=mycol[1: group.nums])
  pdf(sprintf("%s.PLS-DA.pdf", path), width=6, height=5)
  print(p)
  dev.off()
}

