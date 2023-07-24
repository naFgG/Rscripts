rm(list = ls())
library(microDecon)
library(readxl)
library(tidyverse)

excel <- read_excel('台确认单.xlsx', sheet='样本信息')
mapp <- read.table('mapping.org.txt', header=TRUE, comment.char="", row.names=1)
feature <- read.table('feature.xls', header=TRUE, check.names=FALSE)

# reorder
negative <- excel[grep("^阴性-.+$|^阴-.+$", excel$"样本下机名称"), ]$'样本分析名称'
real <- rownames(mapp[!rownames(mapp) %in% negative, ])
feature <- feature[, c('ASV_ID', negative, real)]

# calculate samples number for each group
nBlank <- length(negative)
nSample <- mapp[!rownames(mapp) %in% negative, ] %>% count(Group)
nSample <- nSample[, 2]

# decontaminating
result <- decon(data=feature, numb.blanks=nBlank, numb.ind=nSample, taxa=FALSE)
feature.new <- result$decon.table
feature.new <- feature.new[, -2] %>% arrange(feature.new, ASV_ID)
feature.new <- feature.new[apply(feature.new[, -1], 1, sum) > 0, ]
write.table(feature.new, file="feature.new.xls", sep="\t", row.names=F, quote=F)
