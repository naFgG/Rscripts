rm(list = ls())
library(limma)
library(tidyverse)

# Gene Express Matrix
data <- read.table('', header=T, sep="\t", quote="", check.names=F, row.names=1)
# Samples to Group
mapp <- read.table('', sep="\t", header=T, check.names=F)
data <- data[mapp$Sample]

### construct design matrix ####
samples.freq <- count(mapp, Group)
rownames(samples.freq) <- samples.freq$Group
samples.freq <- samples.freq[unique(mapp$Group), ]
samples <- factor(c(rep(samples.freq[1, 1], samples.freq[1, 2]), 
                    rep(samples.freq[2, 1], samples.freq[2, 2])))
#> 0: No intercept
design.mat <- model.matrix(~ samples + 0)
colnames(design.mat) <- levels(samples)

### construct contrast matrix ####
contrast.mat <- makeContrasts(diff=paste0(colnames(design.mat)[1], " - ", colnames(design.mat)[2]), 
                              levels=design.mat)
colnames(contrast.mat) <- "Diff"

### Fit linear model to estimate T, N for each gene ####
fit <- lmFit(data, design.mat)
### Fit linear model to estimate a set of contrast####
fit2 <- contrasts.fit(fit, contrast.mat)
### empirical Bayes moderation ####
fit3 <- eBayes(fit2)

res <- topTable(fit3, coef="Diff", p.value=.05, adjust.method="fdr", fc=2, number=nrow(data))

res.write <- cbind(Gene_id=rownames(res), res)
write.table(res.write, "all_limma_out.xls", sep="\t", quote=F, row.names=F)
diff.sig <- res[res$P.Value < 0.05, ]
write.table(cbind(Gene_id=rownames(diff.sig), diff.sig), "sig_limma_out.xls", sep="\t", quote=F, row.names=F)
