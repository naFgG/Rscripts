rm(list = ls())
library(UpSetR)
library(hash)

df <- read_excel('species.xlsx', sheet="C-femoral_triangle")

# data transform
val <- c()
for (i in 1: length(names(df))){
  val <- c(val, unlist(na.omit(df[, i])))
}
val <- unique(val)

zero_matrx <- matrix(rep(0, length(names(df)) * length(val)), 
                     ncol=length(names(df)), nrow=length(val))
zero_matrx <- as.data.frame(zero_matrx)
colnames(zero_matrx) <- names(df)
rownames(zero_matrx) <- val

for (i in names(df)){
  for (j in na.omit(df[i])){
    zero_matrx[j, i] <- 1
  }
}

pdf("species_C-femoral_triangle.pdf", width=6.5, height=5)
upset(zero_matrx, nsets=length(names(zero_matrx)), 
      mb.ratio = c(0.65, 0.35),
      sets=names(zero_matrx), keep.order=TRUE,
      point.size=3, line.size=1, 
      #order.by="degree", decreasing=FALSE,
      text.scale=c(1.2, 1.2, 1, 1.1, 1.1, 1.2))
dev.off()
