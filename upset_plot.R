rm(list=ls())
library(ggplot2)
library(ComplexUpset)
library(patchwork)

movies <- as.data.frame(ggplot2movies::movies)
genres <- colnames(movies)[18:24]
# [1] "Action"  "Animation"  "Comedy"  "Drama"  "Documentary"  "Romance"  "Short"
#> 使电影类型是布尔值
movies[genres] <- movies[genres] == 1
movies[movies$mpaa == "", "mpaa"] = NA
movies <- na.omit(movies)

#> 基本用法
upset(movies, # 数据框，有组变量和协变量
      genres, # 指示组成员的列名
      name="genre", # 最下方x轴名
      width_ratio=0.1 # 左下方条形图占比，数字越大占比越大
      )


# 选择交集 ####
upset(movies, genres, name="genre", width_ratio=0.1, 
      min_size=10, # 选择至少有10个成员的交集
      wrap=T, # 画title时方便，就是把图往中间缩进
      set_sizes=F # 不显示左下方条形图
      ) +
  ggtitle("Without empty groups (Short dropped)") +
upset(movies, genres, name="genre", width_ratio=0.1, 
      min_size=10, 
      wrap=T, 
      set_sizes=F,
      keep_empty_groups=T # 保留空交集的组
      ) + 
  ggtitle("With empty groups")

upset(movies, 
      genres, 
      width_ratio=0.1,
      min_degree=3, # 最少交集为3才需要显示
)


# 增加注释 ####
upset(
  movies,
  genres,
  annotations=list(
    # 第一种方法，通过列表
    "Length"=list(
      aes=aes(x=intersection, y=length),
      geom=geom_boxplot(na.rm=TRUE)
    ),
    # 第二种，用ggplot
    "Rating"=(
      # x 自动继承为intersectionnote
      ggplot(mapping=aes(y=rating))
      + geom_jitter(aes(color=log10(votes)), na.rm=TRUE)
      + geom_violin(alpha=0.5, na.rm=TRUE)
    ),
    # 第三种，用upset_annotate
    "Budget"=upset_annotate("budget", geom_boxplot(na.rm=TRUE))
  ),
  min_size=10,
  width_ratio=0.1
)

# 改变注释的模式
upset(
  movies,
  genres,
  mode="inclusive_intersection",
  annotations=list(
    # 如果不指定，则用upset声明的mode
    "Length (inclusive intersection)"=(
      ggplot(mapping=aes(y=length))
      + geom_jitter(alpha=0.2, na.rm=TRUE)
    ),
    "Length (exclusive intersection)"=(
      ggplot(mapping=aes(y=length))
      + geom_jitter(alpha=0.2, na.rm=TRUE)
      + upset_mode("exclusive_intersection")
    ),
    "Length (inclusive union)"=(
      ggplot(mapping=aes(y=length))
      + geom_jitter(alpha=0.2, na.rm=TRUE)
      + upset_mode("inclusive_union")
    )
  ),
  min_size=10,
  width_ratio=0.1
)


# 调整交集大小####
upset(
  movies,
  genres,
  base_annotations=list(
    "Intersection size"=intersection_size(counts=FALSE) # 取消显示条形图上的数字
  ),
  min_size=10,
  width_ratio=0.1
)

upset(
  movies,
  genres,
  base_annotations=list(
    "Intersection size"=intersection_size(
      text_colors=c(
        on_background="brown", on_bar="yellow" # 调整数字颜色
      ), 
      text=list( # 调整数字角度
        vjust=-0.1,
        hjust=-0.1,
        angle=45
      )
    )
    + annotate( # 在右上角写注释
      geom="text", x=Inf, y=Inf,
      label=paste("Total:", nrow(movies)),
      vjust=1, hjust=1
    )
    + ylab("Intersection size")
  ),
  min_size=10,
  width_ratio=0.1
)

# 填充条形图
upset(
  movies,
  genres,
  base_annotations=list(
    "Intersection size"=intersection_size(
      counts=FALSE,
      mapping=aes(fill=mpaa)  # 与ggplot语法一样
    ) + scale_fill_manual(values=c(
      "R"="#E41A1C", "PG"="#377EB8",
      "PG-13"="#4DAF4A", "NC-17"="#FF7F00"
    ))
  ),
  width_ratio=0.1
)

# 调整条形图高度
upset(
  movies,
  genres,
  height_ratio=1, # 点图与条形图的比例，值越大，点图越高
  width_ratio=0.1
)

# 不要条形图
upset(
  movies,
  genres,
  base_annotations=list(),
  min_size=10,
  width_ratio=0.1
)


# 调整集合大小 ####
upset(
  movies, 
  genres,
  min_size=10,
  width_ratio=0.3,
  encode_sets=F,  # 在annotate()能直接选择集合名字
  set_sizes=(
    upset_set_size()
    # 接入ggplot语法
    + geom_text(aes(label=..count..), hjust=1.1, stat="count")
    + annotate(geom="text", label="@", x="Drama", y=850, color="white", size=3) # Drama集合条上加注释
    + expand_limits(y=1100)
    + theme(axis.text.x=element_text(angle=90), # x轴旋转
            axis.ticks.x=element_line()) # 显示x轴轴须
  )
)

# 改变strip颜色
upset(
  movies,
  genres,
  min_size=10,
  width_ratio=0.2,
  stripes=upset_stripes(
    geom=geom_segment(size=5),
    colors=c("cornsilk1", "deepskyblue1", "grey90")
  )
)

# 高亮 ####
upset(
  movies, genres, name="genre", width_ratio=0.1, min_size=10,
  annotations = list(
    "Length"=list(
      aes=aes(x=intersection, y=length),
      geom=geom_boxplot(na.rm=TRUE)
    )
  ),
  queries=list(
    upset_query(
      intersect=c("Drama", "Comedy"),  # 仅在此两个交集操作
      color="red", # 点图用
      fill="red",  # 条形图用
      only_components=c("intersections_matrix", "Intersection size") # 仅在此两个区域操作
    ),
    upset_query(
      set="Drama",  
      fill="blue"  # Drama的完整集合填充为蓝色，所以仅填充左下方条形图
    ),
    upset_query(
      intersect=c("Romance", "Comedy"),
      fill="yellow",
      only_components=c("Length") # 表明填充的图形区域
    )
  )
)