rm(list = ls())
library(circlize)

# 随机生成基因组BED格式数据 ####
set.seed(999)
bed = generateRandomBed()
