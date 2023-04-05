library(vegan)

accumresult <- function(x, y="", factor="", level, scale="",
                        method="exact", permutations=100, conditioned=T,
                        gamma="boot", ...) {
  op <- options()
  options(warn=-1)
  subs <- c(1:nrow(x))
  if (class(y) == "data.frame" && factor != "") {
    subs <- y[, factor] == level
    for (q in 1:length(subs)) {
      if (is.na(subs[q])) {
        subs[q] <- F
      }
    }
    x <- x[subs, , drop=F]
    freq <- apply(x, 2, sum)
    subs2 <- freq > 0
    x <- x[, subs2, drop=F]
  }
  if (dim(as.matrix(x))[1] == 0) {
    result <- list(
      call=match.call(), method=method, sites=0,
      richness=NA, sd=NA, perm=NA
    )
    return(result)
  }
  result <- specaccum(
    x,
    method=method, permutations=permutations,
    conditioned=conditioned, gamma=gamma, ...
  )
  if (scale != "") {
    y <- y[subs, , drop=F]
    tot <- mean(y[, scale])
    result$sites <- result$sites * tot
  }
  options(op)
  return(result)
}

data <- read.table("绘图数据/传统方法各位点鱼类和捕获数.xls", header=T, sep="\t", row.names=1, check.names=FALSE)

Accum <- accumresult(
  data,
  method="random", permutations=100
)
sp1 <- list(
  method="exact",
  sites=Accum$sites, richness=Accum$richness, sd=Accum$sd
)
sp2 <- Accum

summary(sp2)
pdf(file="传统方法物种累积曲线.pdf")
vegan:::plot.specaccum(
  sp1,
  ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
  xlab="Number of sites sampled", ylab="Species detected"
)
boxplot(sp2, col="yellow", add=TRUE, pch="+")
dev.off()
