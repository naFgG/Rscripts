rm(list = ls())
library(ggplot2)
library(ggh4x)

# 示例 ####
#> Setting up some random data 
n <- 200
df <- data.frame(
  x = c(rpois(n, 25),
        rnbinom(n, 5, 0.2),
        rgamma(n, 30, 1.5),
        rchisq(n, 15)),
  distribution = rep(c("Poisson", "Negative Binomial", 
                       "Gamma", "Chi-squared"), each = n),
  type = rep(c("Discrete", "Continuous"), each = 2 * n)
)

ggplot(df, aes(x, y = after_stat(count),
               fill = distribution, colour = distribution)) +
  geom_histogram(position = "identity", binwidth = 1, 
                 alpha = 0.3, colour = NA) +
  #> 离散分布用阶梯表示
  stat_theodensity(data = ggsubset(type == "Discrete"),
                   distri = "nbinom", geom = "step",
                   position = position_nudge(x = -0.5)) +
  #> 另一种类型用线条表示
  stat_theodensity(data = ggsubset(type == "Continuous"),
                   distri = "gamma") +
  scale_colour_discrete(aesthetics = c("colour", "fill"), guide = "none") +
  #> 具有跨类别的镶嵌条
  facet_nested(~ type + distribution, scales = "free_x") +
  #> 精确控制面板的纵横比
  force_panelsizes(rows = 1.618, cols = 1, respect = TRUE) +
  # 调整单个面板的比例
  facetted_pos_scales(list(
    scale_x_continuous(labels = scales::number_format(0.1)),
    #> 给第二个面板x轴标定义出现的频率
    scale_x_continuous(guide = guide_axis_minor()),
    scale_x_continuous(limits = c(0, 40), oob = scales::oob_keep),
    scale_x_continuous(expand = c(1, 0))
  ))



# 刻面 ####
#> 标准画图
p <- ggplot(mpg, aes(displ, hwy, colour = as.factor(cyl))) + geom_point() +
  labs(x = "Engine displacement", y = "Highway miles per gallon") +
  guides(colour = "none")

p + facet_wrap2(vars(class))

#> 区别在于，即使scales = "fixed"（默认情况下），
#> 您也可以使用参数在（部分或全部）内部面绘制轴 axes。
#> axes="all" ==> 所有分面图都有x轴y轴标签
#> 此外，您可以选择省略轴标签，但通过设置参数保留内部面的轴刻度 remove_labels。
#> remove_labels="x" ==> 一列中只有最下面的分面图显示x轴标签，其余只显示轴刻度
p + facet_wrap2(vars(class), axes = "all", remove_labels = "x")

#> 另一个好处是您可以强制指定精确的行数和列数。
#> trim_blank=FALSE ==> 整幅图大小强制是4行4列的大小，不满的会留空白
p + facet_wrap2(vars(class), nrow = 4, ncol = 4, trim_blank = FALSE)


# 网格 ####
p + facet_grid2(vars(year), vars(drv), axes = "all", remove_labels = "y")
#> 此外，facet_grid2()还支持该软件包所谓的“独立”尺度。
#> 这缓解了比例只能在布局的行和列之间自由的约束 ggplot2::facet_grid()
#> 并且允许比例在布局的行和列内自由。这保留了网格布局，但保留了包裹面中尺度的灵活性。
#> 请注意，在下图中，每个面板的 x 轴都是独立的。
p + facet_grid2(vars(year), vars(drv), scales = "free_x", independent = "x")
#> 为独立尺度而必须做出的一项牺牲是: 
#> facet_grid2()不能有独立尺度而必须有space = "free"独立维度。
#> 然而，可以在不同的维度上组合它们。
p + facet_grid2(vars(year), vars(drv), scales = "free", independent = "y", space = "free_x")
#> render_empty参数可用于在行和列都没有绘制信息时，不绘制该子图。
p + facet_grid2(vars(drv), vars(cyl), render_empty = FALSE)


# 嵌套面 ####
#> 如果外条带属于同一类别，则它们可以跨越内条带。
#> 在下面的示例中，我们将根据叶子的长短对鸢尾花进行分类。
new_iris <- transform(iris, Nester = ifelse(Species == "setosa", "Short Leaves", "Long Leaves"))
iris_plot <- ggplot(new_iris, aes(Sepal.Width, Sepal.Length)) + geom_point()
iris_plot + facet_nested(~ Nester + Species)


# 嵌套线 ####
#> 如果希望条带具有空白背景，仍然可以通过设置来指示分层性质nest_line = elment_line()。
#> 线条的外观继承自主题元素ggh4x.facet.nestline。
#> 在下面的示例中，嵌套线是虚线，因为给定元素具有linetype = 2
#> 也可以在theme中设置线的颜色。
iris_plot +
  facet_nested(~ Nester + Species, nest_line = element_line(linetype = 2)) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "blue"))
#> 请注意，“ShortLeaves”/“setosa”条带不是由嵌套线分隔的。
#> 可以使用facet_nested(..., solo_line = TRUE) 参数打开此类条带之间的嵌套线
iris_plot +
facet_nested(~ Nester + Species, nest_line = element_line(linetype = 2), solo_line = TRUE) +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "blue"))

# 变体 ####
#> 每一列都有分面
#> dir = "v"， 分面垂直排序
p + 
  facet_nested_wrap(
    vars(cyl, drv), dir = "v", strip.position = "left",
    axes = "all", remove_labels = "x"
  ) +
  theme(strip.placement = "outside")


# 手动刻面 ####
#> design为图形布局
design <- matrix(c(1,2,3,2), 2, 2)
p + facet_manual(vars(factor(drv)), design = design)
#> 还可以用字符串表示布局
design <- "
  A##
  AB#
  #BC
  ##C
"
p + facet_manual(vars(drv), design = design)



# 条带 ####
p2 <- p +
  theme(strip.background = element_rect(colour = "black", linewidth = 2),
        axis.line.y = element_line(colour = "black", linewidth = 2))
#> clip=on, 轴线与分面线对不齐
p2 + facet_wrap2(vars(year), strip = strip_vanilla(clip = "on"))
p2 + facet_wrap2(vars(year), strip = strip_vanilla(clip = "off"))

df <- data.frame(
  long = paste("A long title that is going to make the\n",
               "smaller title take up too much space"),
  short = LETTERS[1:3],
  x = 1:3, y = 1:3
)
#> size参数
#> constant 会使图的布局变得奇怪，分面标题会很长
p2 <- ggplot(df, aes(x, y)) +
  geom_point() +
  theme(strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside",
        plot.title.position = "plot")
p2 + facet_grid2(long + short ~ ., switch = "y",
                 strip = strip_vanilla(size = "constant")) +
  ggtitle('size = "constant"')
#> variable 会缩小每层条带之间的过多间距
p2 + facet_grid2(long + short ~ ., switch = "y",
                  strip = strip_vanilla(size = "variable")) +
  ggtitle('size = "variable"')
#> 还可以自定义strip格式
ridiculous_strips <- strip_themed(
  # Horizontal strips
  background_x = elem_list_rect(fill = c("limegreen", "dodgerblue")),
  text_x = elem_list_text(colour = c("dodgerblue", "limegreen"),
                          face = c("bold", "bold")),
  # 按层分颜色
  by_layer_x = TRUE,
  # Vertical strips
  background_y = elem_list_rect(
    fill = c("gold", "tomato", "deepskyblue")
  ),
  text_y = elem_list_text(angle = c(0, 90)),
  by_layer_y = FALSE
)

p + facet_grid2(class ~ drv + year, strip = ridiculous_strips)
#> 还可以用list来指定
p + facet_grid2(
  . ~ drv + year,
  strip = strip_themed(
    background_x = list(NULL, element_rect(colour = "black"), element_blank(),
                        element_rect(fill = "black")),
    text_x = list(NULL, NULL, NULL, element_text(colour = "white"))
  )
)

# 嵌套条带 ####
#> bleed=False，顶层分组不同时即使下一层分组相同也不合并
p + facet_wrap2(
  vars(cyl, drv), ncol = 4,
  strip = strip_nested(bleed = FALSE)
) +
  ggtitle('bleed = FALSE')
p + facet_wrap2(
  vars(cyl, drv), ncol = 4,
  strip = strip_nested(bleed = TRUE)
) +
  ggtitle("bleed = TRUE")

# 设置面板尺寸 ####
p + force_panelsizes(rows = unit(2, "cm"), cols = unit(2, "in"))

lvls <- factor(c("Points", "Density"), c("Points", "Density"))
g <- ggplot(faithful) +
  geom_point(aes(waiting, eruptions),
             data = ~ cbind(.x, facet = lvls[1])) +
  geom_density(aes(y = eruptions),
               data = ~ cbind(faithful, facet = lvls[2])) +
  facet_grid(~ facet, scales = "free_x")
#> 使用相对单位
g + force_panelsizes(cols = c(1, 0.3), rows = c(0.5), respect = TRUE)
#> 使用绝对单位
g <- g + force_panelsizes(
  cols = c(1, 0.3), total_width = unit(6, "cm"), 
  total_height = unit(4, "cm")
)
g + scale_x_facet(facet == "Density", breaks = c(0, 0.2, 0.4))


# 轴嵌套 ####
#> 可以画成样本下方线+组名的效果
df <- data.frame(
  item = c("Coffee", "Tea", "Apple", "Pear", "Car"),
  type = c("Drink", "Drink", "Fruit", "Fruit", ""),
  amount = c(5, 1, 2, 3, 1),
  stringsAsFactors = FALSE
)
#> 顺序会乱
ggplot(df, aes(interaction(item, type), amount)) +
  geom_col() +
  guides(x = "axis_nested")
#> 保持因子水平顺序
ggplot(df, aes(weave_factors(item, type), amount)) +
  geom_col() +
  guides(x = "axis_nested")
#> 更改颜色
ggplot(df, aes(weave_factors(item, type), amount)) +
  geom_col() +
  guides(x = "axis_nested") +
  theme(
    ggh4x.axis.nestline.x = element_line(colour = "red", size = 2),
    ggh4x.axis.nesttext.x = element_text(colour = "blue")
  )
#> 多重堆叠
df$type2 <- c(rep("Consumables", 4), "Vehicle")
df$appletea <- c("", rep("Ingredient of apple tea", 2), rep(NA, 2))
ggplot(df, aes(weave_factors(item, type, appletea, type2), amount)) +
  geom_col() +
  guides(x = "axis_nested")