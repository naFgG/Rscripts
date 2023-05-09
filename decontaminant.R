rm(list = ls())
library(phyloseq)
library(decontam)
library(readxl)

args <- commandArgs(T)
if (length(args) != 4){
  cat('Usage: Rscript decontaminant.R 云平台确认单.xlsx mapping.org.txt feature.org.xls feature_tax.org.xls\n')
  q(status=1)
}

# import data
excel <- read_excel(args[1], sheet="样本信息")
mapp <- read.table(args[2], sep="\t", row.names=1, header=T, comment.char="")
otu <- as.matrix(read.table(args[3], sep="\t", row.name=1, header=T, check.names=F))
tax <- as.matrix(read.table(args[4], sep="\t", row.names=1, header=T, check.names=F))

# transform to phyloseq format
otu <- otu_table(otu, taxa_are_rows=T)
tax <- tax_table(tax)
mapp <- sample_data(mapp)
ps <- phyloseq(otu, mapp, tax)
## mark negative samples
sample_data(ps)$is.neg <- 
  rownames(sample_data(ps)) %in% excel[grep("阴性-", excel$"样本下机名称"), ]$"样本分析名称"

# identify contaminants with "prevalence" method and default threshold
id.contam <- isContaminant(ps, method="prevalence", neg="is.neg")

# keep non-contaminant asv
keep <- prune_taxa(!id.contam$contaminant, ps)
## rewrite file
otu.new <- as.data.frame(otu_table(keep))
otu.new <- otu.new[, !colnames(otu.new) %in% excel[grep("阴性-", excel$"样本下机名称"), ]$"样本分析名称"]
otu.new <- otu.new[apply(otu.new, 1, sum) > 0, ]
otu.new <- cbind(rownames(otu.new), otu.new)
colnames(otu.new)[1] <- "ASV_ID"
write.table(otu.new, file="feature.xls", sep="\t", row.names=F, quote=F)

tax.new <- as.data.frame(tax_table(keep))
tax.new <- tax.new[, !colnames(tax.new) %in% excel[grep("阴性-", excel$"样本下机名称"), ]$"样本分析名称"]
tax.new <- tax.new[otu.new[, 1], ]
tax.new <- cbind(rownames(tax.new), tax.new)
colnames(tax.new)[1] <- "ASV_ID"
write.table(tax.new, file="feature_tax.xls", sep="\t", row.names=F, quote=F)

mapp.new <- mapp[!rownames(mapp) %in% excel[grep("阴性-", excel$"样本下机名称"), ]$"样本分析名称", ]
mapp.new <- cbind(rownames(mapp.new), mapp.new)
colnames(mapp.new)[1] <- "#SampleID"
write.table(mapp.new, file="mapping.txt", sep="\t", row.names=F, quote=F)
