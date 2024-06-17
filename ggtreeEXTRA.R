rm(list=ls())
library(ggtreeExtra)
library(ggtree)
library(ggstar)
library(ggnewscale)
library(treeio)
library(ggplot2)

# The path of tree file.
trfile <- system.file("extdata", "tree.nwk", package="ggtreeExtra")
# The path of file to plot tip point.
tippoint1 <- system.file("extdata", "tree_tippoint_bar.csv", package="ggtreeExtra")
# The path of first layer outside of tree.
ring1 <- system.file("extdata", "first_ring_discrete.csv", package="ggtreeExtra")
# The path of second layer outside of tree.
ring2 <- system.file("extdata", "second_ring_continuous.csv", package="ggtreeExtra")

# The tree file was imported using read.tree. If you have other tree format files, you can use corresponding functions from treeio package to read it.
tree <- read.tree(trfile)

# This dataset will to be plotted point and bar.
dat1 <- read.csv(tippoint1)
# This dataset will to be plotted heatmap
dat2 <- read.csv(ring1)
# This dataset will to be plotted heatmap
dat3 <- read.csv(ring2)

# The format of the datasets is the long shape for ggplot2. If you have short shape dataset,
# you can use `melt` of `reshape2` or `pivot_longer` of `tidyr` to convert it.

# We use ggtree to create fan layout tree. 
p <- ggtree(tree, layout="fan", open.angle=10, size=0.5)
p

# You should specify the column contained tip labels to y axis of `mapping`, here is `y=ID`.
# the `position` parameter was set to `identity` to add the points on the tip nodes.
p2 <- p + 
  geom_fruit(
    data=dat1,
    geom=geom_star,
    mapping=aes(y=ID, fill=Location, size=Length, starshape=Group),
    position="identity",
    starstroke=0.2
  ) + 
  scale_size_continuous(
    range=c(1, 3), # the range of size.
    guide=guide_legend(
      keywidth=0.5, 
      keyheight=0.5,
      override.aes=list(starshape=15),
      order=2
    )
  ) +
  scale_fill_manual(
    values=c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7"),
    guide="none" 
  ) + 
  scale_starshape_manual(
    values=c(1, 15),
    guide=guide_legend(
      keywidth=0.5,
      keyheight=0.5,
      order=1
    )
  )
p2

# Next, we will add a heatmap layer on the outer ring of p2 using `geom_tile` of `ggplot2`.
# Since we want to map some variable of dataset to the `fill` aesthetics of `geom_tile`, but 
# the `fill` of p2 had been mapped. So I need use `new_scale_fill` of `ggnewscale` package to initialize it.
p3 <- p2 + 
  new_scale_fill() + 
  geom_fruit(
    data=dat2,
    geom=geom_tile,
    mapping=aes(y=ID, x=Pos, fill=Type),
    offset=0.08,   # The distance between external layers, default is 0.03 times of x range of tree.
    pwidth=0.25 # width of the external layer, default is 0.2 times of x range of tree.
  ) + 
  scale_fill_manual(
    values=c("#339933", "#dfac03"),
    guide=guide_legend(keywidth=0.5, keyheight=0.5, order=3)
  ) 
p3

# You can also add heatmap layer for continuous values.
p4 <- p3 + 
  new_scale_fill() +
  geom_fruit(
    data=dat3,
    geom=geom_tile,
    mapping=aes(y=ID, x=Type2, alpha=Alpha, fill=Type2),
    pwidth=0.15,
    axis.params=list(
      axis="x", # add axis text of the layer.
      text.angle=-45, # the text angle of x-axis.
      hjust=0  # adjust the horizontal position of text of axis.
    )
  ) +
  scale_fill_manual(
    values=c("#b22222", "#005500", "#0000be", "#9f1f9f"),
    guide=guide_legend(keywidth=0.5, keyheight=0.5, order=4)
  ) +
  scale_alpha_continuous(
    range=c(0, 0.4), # the range of alpha
    guide=guide_legend(keywidth=0.5, keyheight=0.5, order=5)
  ) 


# Then add a bar layer outside of the tree.
p5 <- p4 + 
  new_scale_fill() +
  geom_fruit(
    data=dat1,
    geom=geom_col,
    mapping=aes(y=ID, x=Abundance, fill=Location),  #The 'Abundance' of 'dat1' will be mapped to x
    pwidth=0.4,
    axis.params=list(
      axis="x", # add axis text of the layer.
      text.angle=-45, # the text size of axis.
      hjust=0  # adjust the horizontal position of text of axis.
    ),
    grid.params=list() # add the grid line of the external bar plot.
  ) + 
  scale_fill_manual(
    values=c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7"),
    guide=guide_legend(keywidth=0.5, keyheight=0.5, order=6)
  ) +
  theme(legend.background=element_rect(fill=NA), # the background of legend.
    legend.title=element_text(size=7), # the title size of legend.
    legend.text=element_text(size=6), # the text size of legend.
    legend.spacing.y=unit(0.02, "cm")  # the distance of legends (y orientation).
  ) 
p5


# add multiple layers on the same position ####
# To reproduce.
set.seed(1024)
# generate a tree contained 100 tip labels.
tr <- rtree(100)
# generate three datasets, which are the same except the third column name.
dt <- data.frame(id=tr$tip.label, value=abs(rnorm(100)), group=c(rep("A",50),rep("B",50)))
df <- dt
dtf <- dt
colnames(df)[[3]] <- "group2"
colnames(dtf)[[3]] <- "group3"
# plot tree 
p <- ggtree(tr, layout="fan", open.angle=0)
p
# the first ring.
p1 <- p + 
  geom_fruit(
    data = dt,
    geom = geom_col,
    mapping = aes(y=id, x=value, fill=group),
  ) + 
  new_scale_fill()
p1
# the second ring
# geom_fruit_list is a list, which first element must be layer of geom_fruit.
p2 <- p1 + 
  geom_fruit_list(
    geom_fruit(
      data = df,
      geom = geom_col,
      mapping = aes(y=id, x=value, fill=group2),
    ),
    scale_fill_manual(values=c("blue", "red")), # To map group2
    new_scale_fill(), # To initialize fill scale.
    geom_fruit(
      data = dt,
      geom = geom_star,
      mapping = aes(y=id, x=value, fill=group),
      size = 2.5,
      color = NA,
      starstroke = 0
    )
  ) + 
  new_scale_fill()
p2
# The third ring
p3 <- p2 + 
  geom_fruit(
    data = dtf,
    geom = geom_col,
    mapping = aes(y=id, x=value, fill=group3),
    stat = "identity"
  ) +
  scale_fill_manual(values=c("#00AED7", "#009E73"))
#> Warning in geom_col(data = structure(list(label = c("t1", "t10", "t100", :
#> Ignoring unknown parameters: `stat`
p3

