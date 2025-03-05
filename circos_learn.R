rm(list=ls())
suppressPackageStartupMessages(library(circlize))

set.seed(999)
n <- 1000

df <- data.frame(
  sectors=sample(letters[1:8], n, replace=T),
  x=rnorm(n),
  y=runif(n)
)

# 全局参数
circos.par("track.height"=0.1)
# 扇区初始化，需要分区和x轴数据
circos.initialize(df$sectors, x=df$x)
# 所有轨道都必须先有circos.track创建，并且只需要y轴数据
circos.track(df$sectors, y=df$y, 
             panel.fun=function(x, y) {
               circos.text(CELL_META$xcenter, 
                           CELL_META$cell.ylim[2] + mm_y(5),
                           CELL_META$sector.index)
               circos.axis(labels.cex=0.6)
             })
col <- rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(df$sectors, df$x, df$y, col=col, pch=16, cex=0.5)
circos.text(-1, 0.5, "text", sector.index="a", track.index=1)
# trackHist()省去了circos.track()这一步
bgcol <- rep(c("#EFEFEF", "#CCCCCC"), 4)
circos.trackHist(df$sectors, df$x, bin.size=0.2, bg.col=bgcol, col=NA)
# circos.track的x和y会根据sectors划分子集，并传递给panel.fun，所以panel.fun中的x，y是当前cell中的值
circos.track(df$sectors, x=df$x, y=df$y, 
             panel.fun=function(x, y) {
               ind=sample(length(x), 10)
               x2=x[ind]
               y2=y[ind]
               od=order(x2)
               circos.lines(x2[od], y2[od])
             })
# 更新第二个轨道的扇区d的cell，调用update函数后当前cell即重定向为指定cell，然后可以使用低级图形函数绘图
circos.update(sector.index="d", track.index=2, bg.col="#FF8080", bg.border="black")
circos.points(x=-2:2, y=rep(0.5, 5), col="white")
circos.text(CELL_META$xcenter, CELL_META$ycenter, "updated", col="white")
# 继续创建新轨道时，仍然是最新的轨道之后创建
# 因为热图不需要知道x，y确切的值，只是从左到右、从上到下填充，所以这里关于x，y确切的值都没有用
circos.track(ylim=c(0, 1), panel.fun=function(x, y){
  xlim=CELL_META$xlim
  ylim=CELL_META$ylim
  breaks=seq(xlim[1], xlim[2], by=0.1)
  n_breaks=length(breaks)
  circos.rect(breaks[-n_breaks], rep(ylim[1], n_breaks-1),
              breaks[-1], rep(ylim[2], n_breaks-1), 
              col=rand_color(n_breaks), border=NA)
})
# 连线
circos.link("a", 0, "b", 0, h=0.4)
circos.link("c", c(-0.5, 0.5), "d", c(-0.5, 0.5), col="red", border="blue", h=0.2)
circos.link("e", 0, "g", c(-1, 1), col="green", border="black", lwd=2, lty=2)
# 重置图形参数和内部变量，必须
circos.clear()
