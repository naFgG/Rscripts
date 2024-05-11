rm(list=ls())
library(ggrain)

data(iris)

# 第一张，最简单的雨云图 ####
ggplot(iris, aes(x=1, y=Sepal.Width)) +
  geom_rain() +
  theme_classic() +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())


# 第二张，根据物种填充 ####
ggplot(iris, aes(x=1, y=Sepal.Width, fill=Species)) +
  geom_rain(alpha=.5) +
  theme_classic() +
  scale_fill_brewer(palette="Dark2")


#> color调整点，用boxplot.args调整箱线图
ggplot(iris, aes(x=1, y=Sepal.Width, fill=Species, color=Species)) +
  geom_rain(alpha=.6,
            boxplot.args=list(color="black", outlier.shape=NA)) +
  theme_classic() +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2")

#> rain.side调整云的位置，boxplot.args.pos调整箱线图不重叠
ggplot(iris, aes(x=1, y=Sepal.Width, fill=Species, color=Species)) +
  geom_rain(alpha=.5, 
            rain.side="l",
            boxplot.args=list(color="black", outlier.shape=NA),
            boxplot.args.pos=list(
              position=ggpp::position_dodgenudge(x=.1, width=0.1), width=0.1
            )) +
  theme_classic() +
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  guides(fill="none", color="none")


# 第三张, 把每个雨云根据物种分开 ####
ggplot(iris, aes(x=Species, y=Sepal.Width, fill=Species)) +
  geom_rain(alpha=.5) +
  theme_classic() +
  scale_fill_brewer(palette="Dark2") +
  guides(fill="none", color="none")

#> 翻转
ggplot(iris, aes(x=Species, y=Sepal.Width, fill=Species)) +
  geom_rain(alpha=.5) +
  theme_classic() +
  scale_fill_brewer(palette="Dark2") +
  guides(fill="none", color="none") +
  coord_flip()

#> 增加小提琴图、箱线图、点的间距
ggplot(iris, aes(x=Species, y=Sepal.Width, fill=Species)) +
  geom_rain(alpha=.5, 
            boxplot.args.pos=list(
              width=0.05, position=position_nudge(x=0.13)),
            violin.args.pos = list(
              side="r",
              width=0.7, position=position_nudge(x=0.2))) +
  theme_classic() +
  scale_fill_brewer(palette="Dark2") +
  guides(fill="none", color="none") +
  coord_flip()

#> 增加协变量Sepal.Length，使点图的颜色对应Sepal.Length
ggplot(iris, aes(x=Species, y=Sepal.Width, fill=Species)) +
  geom_rain(alpha=.6,
            cov="Sepal.Length") +
  theme_classic() +
  scale_fill_brewer(palette="Dark2") +
  guides(fill="none", color="none") +
  scale_color_viridis_c(option="A", direction= -1)



set.seed(42) # the magic number
iris_subset <- iris[iris$Species %in% c('versicolor', 'virginica'),]
iris.long <- cbind(rbind(iris_subset, iris_subset, iris_subset), 
                   data.frame(time = c(rep("t1", dim(iris_subset)[1]), rep("t2", dim(iris_subset)[1]), rep("t3", dim(iris_subset)[1])),
                              id = c(rep(1:dim(iris_subset)[1]), rep(1:dim(iris_subset)[1]), rep(1:dim(iris_subset)[1]))))
# adding .5 and some noise to the versicolor species in t2
iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"] <- iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"] + .5 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"]), sd = .2)
# adding .8 and some noise to the versicolor species in t3
iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"] <- iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"] + .8 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"]), sd = .2)
# now we subtract -.2 and some noise to the virginica species
iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"] <- iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"] - .2 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"]), sd = .2)
# now we subtract -.4 and some noise to the virginica species
iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"] <- iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"] - .4 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"]), sd = .2)
iris.long$Sepal.Width <- round(iris.long$Sepal.Width, 1) # rounding Sepal.Width so t2 data is on the same resolution
iris.long$time <- factor(iris.long$time, levels = c('t1', 't2', 't3'))


# 第四张，相对及连线 ####
#> 相对
ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5, rain.side = 'f2x2') +
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none')

#> 根据id列，使相同id连线

ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5, rain.side = 'f2x2', id.long.var = "id") +
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none')

#> 按物种颜色连线，依然为color参数控制
ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species, color = Species)) +
  geom_rain(alpha = .5, rain.side = 'f2x2', id.long.var = "id",
            violin.args = list(color = NA, alpha = .7)) +
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  scale_color_manual(values=c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none')


# 第五张，添加显著性标志 ####
ggplot(iris.long[iris.long$Species == 'versicolor' & iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5, rain.side = 'f1x1') +
  ggsignif::geom_signif(
    comparisons = list(c("t1", "t2")),
    map_signif_level = TRUE) +
  scale_fill_manual(values=c("darkorange", "darkorange")) +
  theme_classic()
