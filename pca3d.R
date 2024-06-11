rm(list=ls())
library(rgl)

args <- commandArgs(T)
# args
# 1: 样本的x, y, z的坐标，最后两行是 eigvals	和 % variation explained
# 2：样本-分组表
# 3：输出图形名，只能是png格式

df <- read.table(args[1], sep="\t", header=T, quote="", check.names=F, row.names=1)
coordinate <- df[1: (nrow(df)-2), 1: 3]
explained <- df[nrow(df), 1: 3]
metadata <- read.table(args[2], sep="\t", header=T, quote="", check.names=F, 
                       row.names=1, comment.char="")["Group", drop=F]
coordinate <- coordinate[rownames(metadata), ]
colnames(coordinate)[1: 3] <- c("x", "y", "z")
coordinate <- cbind(coordinate, metadata)

x <- coordinate$x
y <- coordinate$y
z <- coordinate$z

#' @param new.device a logical value. If TRUE, creates a new device
#' @param bg the background color of the device
#' @param width the width of the device
# 初始化RGL设备函数
rgl_init <- function(new.device=F, bg="white", width=960) { 
  if( new.device | rgl.cur() == 0 ) { # rgl.cur()：返回活动设备 ID
    open3d()  # open3d(): 打开一个新设备
    par3d(windowRect=50 + c( 0, 0, width, width )) # par3d(windowRect): 设置窗口大小
    bg3d(color=bg)
  }
  clear3d(type=c("shapes", "bboxdeco")) # clear3d(type): 从指定堆栈中清除场景，即清除当前设备
  view3d(theta=15, phi=20, zoom=0.7) # view3d(theta, phi, fov, zoom): 设置视点。参数theta和phi是极坐标
}

# 分组颜色
# mycol <- c("#7FC97F", "#BEAED4", "#FDC086", "#386CB0", "#F0027F", "#BF5B17")
mycol <- c("#94B2D6","#BF4F4C","#C2D59A","#7F63A1","#4AABC5","#F69545","#2C4C74")
coordinate$Group <- factor(coordinate$Group, levels=unique(coordinate$Group))
group_colors <- mycol[as.numeric(coordinate$Group)]

rgl_init()
# 添加球体
spheres3d(x, y, z, r=0.02, color=group_colors, alpha=0.7)
# 添加坐标轴，相交于最小值
segments3d(c(min(x), max(x)+0.5), c(min(y), min(y)), c(min(z), min(z)))
segments3d(c(min(x), min(x)), c(min(y), max(y)+0.5), c(min(z), min(z)))
segments3d(c(min(x), min(x)), c(min(y), min(y)), c(min(z), max(z)+0.5))
# 添加坐标轴标签
text3d(x=c(max(x)/3, min(x)*1.3, min(x)*1.8), 
       y=c(min(y), max(y)/3, min(y)*1.4), 
       z=c(min(z)*1.4, min(z), min(z)), 
       text=c(paste0("PC1 ", round(explained[1, 1], 2), "%"), 
              paste0("PC2 ", round(explained[1, 2], 2), "%"), 
              paste0("PC3 ", round(explained[1, 3], 2), "%")), 
       size=2)
# 添加刻度线
axis3d('x', at=c(0, round(max(x), 1), round(max(x), 1)+(round(max(x), 1)-0)), pos=c(min(x), min(y), min(z)), nticks=5)
axis3d('y', at=c(0, round(max(y), 1), round(max(y), 1)+(round(max(y), 1)-0)), pos=c(min(x), min(y), min(z)), nticks=5)
axis3d('z', at=c(0, round(max(z), 1), round(max(z), 1)+(round(max(z), 1)-0)), pos=c(min(x), min(y), min(z)), nticks=5)
# 添加置信椭圆
for (i in 1: length(levels(coordinate$Group))) {
  selected <- coordinate$Group == levels(coordinate$Group)[i]
  xx <- x[selected]; yy <- y[selected]; zz <- z[selected]
  # 需要协方差阵
  ellips <- ellipse3d(cov(cbind(xx, yy, zz)), centre=c(mean(xx), mean(yy), mean(zz)), 
                      level=0.95)
  shade3d(ellips, col=mycol[i], alpha=0.05, lit=T)
  wire3d(ellips, col=mycol[i], lit=F)
  # 组名
  texts3d(mean(xx)*2, mean(yy)*2, mean(zz), 
          text=levels(coordinate$Group)[i], col=mycol[i], cex=2)
}
rgl.snapshot(args[3])
