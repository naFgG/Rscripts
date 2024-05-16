rm(list=ls())
library(ggplot2)
library(ggbreak)

df <- data.frame(x=1:20,
                y=c(seq(10000, 5000, length.out=10), seq(500, 100, length.out=10)))

p1 <- ggplot(df, aes(x=x, y=y)) + 
  geom_col() + # 原始图
  scale_y_break(c(600, 8000)) + # 选择范围，这两个数还是会显示在图形上的
  xlab(NULL) + # 截断后还可以接ggplot其他语法
  ylab(NULL) + 
  theme_minimal()


p1 + scale_y_break(c(8500, 9000), # 多个断点
                   scales=2, # scales放大9000以后区域的倍数
                   space=0.5) # 增加空隙

p2 <- ggplot(df, aes(x=x, y=y)) + 
  geom_col() +
  scale_wrap(n=2) # 图变两行