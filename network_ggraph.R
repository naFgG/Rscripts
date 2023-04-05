rm(list = ls())
library(igraph)
library(tidygraph)
library(ggraph)
library(tidyverse)

# This sets the default style to the graph style
set_graph_style()

# create an igraph network
G <- erdos.renyi.game(50, .4)
# convert it to a tidygraph network
G <- as_tbl_graph(G)


# 此处不用上面创建的G网络，新建一个
# create_notable()创建一个名叫zachary的对象
# 此函数直接返回的是tbl_graph网络对象
create_notable('zachary') %>%
# 使用此对象中的nodes table
  activate(nodes) %>%
# 使用mutate创建degree变量
# mutate 将=号右边的新值赋予左边的值
# centrality_degree() 计算每个节点的重要性(每个节点有多少边?)
  mutate(degree = centrality_degree())


# 实际上网络已经被储存成数据框，可以被导出
# 创建边属性，储存到变量bw
# bw 是储存的是叫边介数的值
# 用ggplot画出来
create_notable('zachary') %>%
  activate(edges) %>%
  mutate(bw = centrality_edge_betweenness()) %>%
  as_tibble() %>%
  ggplot() +
  geom_histogram(aes(x=bw)) +
  theme_minimal()

# ase()中设置美学参数
create_notable('zachary') %>%
  activate(nodes) %>% 
  # Creates a `group` variable based on the infomap algorithm
  mutate(group = as.factor(group_infomap())) %>% 
  ggraph(layout = 'stress') +
  # 将所有边设为 .2宽 浅蓝色，fan是曲线
  geom_edge_fan(width = .2, color = 'lightblue') + 
  # 根据组以不同颜色填充node
  geom_node_point(aes(color = group)) + 
  coord_fixed() + 
  theme_graph()


# 又是新的数据集
nodes <- read_csv('https://raw.githubusercontent.com/jdfoote/Communication-and-Social-Networks/spring-2021/resources/school_graph_nodes.csv')
edges <- read_csv('https://raw.githubusercontent.com/jdfoote/Communication-and-Social-Networks/spring-2021/resources/school_graph_edges.csv')
G <- graph_from_data_frame(v=nodes, d=edges) %>% as_tbl_graph()

# 以alcohol_use作为颜色变量填充node
# scale_color_viridis()，使用viridis pallett
G %>%
  ggraph(layout = 'stress') +
  geom_edge_fan(width = .5, color = 'gray') +
  geom_node_point(aes(color=alcohol_use), size = 3) +
  scale_color_viridis()

# 边的颜色将根据edges type的类型不同赋予不同的颜色
G %>%
  ggraph(layout = 'stress') +
  geom_edge_fan(aes(color=type),width = .5) +
  geom_node_point(aes(color=alcohol_use), size = 3) +
  scale_color_viridis()

# 指定分组颜色
# 节点的颜色用 scale_color_manual()
# 边的颜色用 scale_edge_color_manual()
# 指定边
G %>%
  ggraph(layout = 'stress') +
  geom_edge_fan(aes(color=type),width = .5) +
  geom_node_point(aes(color=alcohol_use), size = 3) +
  scale_color_viridis() +
  scale_edge_color_manual(values = c('friendship' = '#ceb888', 'primary_school' = 'lightgray'))
  # 两种方法指定都可以
  # scale_edge_color_manual(values = c('#ceb888', 'lightgray'))


# 改变节点的大小和边的宽度
# 根据delinquency改变节点大小，delinquency中有NA值的话节点会消失
G %>%
  ggraph(layout = 'stress') +
  geom_edge_fan(aes(color=type), width = .5) +
  geom_node_point(aes(color=alcohol_use, size = delinquency)) +
  scale_color_viridis() +
  scale_edge_color_manual(values = c('friendship' = '#ceb888', 'primary_school' = 'gray'))
# 加上改变边大小
# 此网络中没有权重参数，另写
# 有权重参数可以直接用网络中的权重参数
a <- c(3, 3, 3, rep(c(1, 2), 100))
# 1
G %>%
  ggraph(layout = 'stress') +
  # 在aes中直接添加width
  # 需要在网络图中就存在权重这一列
  geom_edge_fan(aes(color=type, width = weight)) +
  geom_node_point(aes(color=alcohol_use, size = delinquency)) +
  scale_color_viridis() +
  scale_edge_color_manual(values = c('friendship' = '#ceb888', 'primary_school' = 'gray'))
# 2
G %>%
  ggraph(layout = 'stress') +
  geom_edge_fan(aes(color=type)) +
  geom_node_point(aes(color=alcohol_use, size = delinquency)) +
  scale_color_viridis() +
  scale_edge_color_manual(values = c('friendship' = '#ceb888', 'primary_school' = 'gray')) + 
  # 使用如下函数
  scale_edge_width(range = c(1, 6))
# 可以尝试edge_width
geom_edge_fan(aes(color=type), edge_width)
edge_alpha
edge_linetype


# 以某个节点为中心，显示其他节点到此节点的距离
# 此例中心节点为3
create_notable('zachary') %>%
  mutate(d = distances(.G(), to=3)) %>%
  ggraph(layout = 'focus', focus = 3) +
  geom_edge_fan() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = r), data.frame(r = 1:3), colour = 'grey') + 
  geom_node_point(aes(color = as.factor(d)), size = 3) +
  coord_fixed() + 
  scale_color_viridis_d() +
  labs(color='Distance from Node 3')


# 有向边
G %>%
  ggraph(layout = 'stress') +
  geom_edge_fan(arrow = arrow(length = unit(3, 'mm')),
                # 决定how far away from the node center to start and end the arrows
                end_cap = circle(1,'mm'),
                start_cap = circle(1, 'mm')
  ) + 
  geom_node_point()

# 根据边的渐变色代表方向
G %>%
  ggraph(layout='stress') +
  # 开启渐变色
  geom_edge_fan(aes(alpha = stat(index))) + 
  geom_node_point(color = '#ceb888', size = 5) + 
  # 增加图例"Edge direction" 
  scale_edge_alpha('Edge direction', guide = 'edge_direction')


# Facets
# 根据性别把网络图分成两幅
G %>%
  ggraph(layout = 'stress') +
  geom_edge_fan(aes(color=type), width = .5) +
  geom_node_point(aes(color=alcohol_use, size = delinquency)) +
  scale_color_viridis() +
  scale_edge_color_manual(values = c('friendship' = '#ceb888', 'primary_school' = 'gray')) +
  # The (~) can be read as “by”
  facet_nodes(~sex)


# 数据过滤
G %>%
  # Remember to activate whatever you want to filter
  activate(edges) %>% 
  # 只保留type等于friendship的行
  filter(type == 'friendship') %>%
  ggraph(layout = 'stress') + 
  geom_edge_fan(color = 'gray', width = .5) +
  geom_node_point(aes(color=alcohol_use), size = 3) + 
  scale_color_viridis()


# .N() lets us access the nodes table from the edge table
# and from and to refer to the node that an edge is coming from and the node it is going to, respectively
G %>% 
  activate(edges) %>%
  filter(type == 'friendship') %>%
  mutate(drinking_diff = abs(.N()$alcohol_use[from] - .N()$alcohol_use[to])) %>%
  ggraph(layout = 'stress') + 
  geom_edge_fan(aes(color = drinking_diff), width = .5) +
  geom_node_point(aes(color=alcohol_use), size = 3) + 
  scale_color_viridis() + 
  scale_edge_color_viridis()


# 边一定要比点先画


# 改变图例标题
# 一般是使用scale*()函数的name参数
G %>%
  ggraph(layout = 'stress') +
  geom_edge_fan(aes(color=type), width = .5) +
  geom_node_point(aes(color=alcohol_use, size = delinquency)) +
  scale_color_viridis(name = 'Alcohol Use') +
  scale_edge_color_manual(values = c('friendship' = '#ceb888', 'primary_school' = 'lightgray'), name = 'Edge Type') +
  scale_size(name = 'Delinquency') + 
  # 加了整幅图的标题
  labs(title = 'The relationship between delinquency and alcohol use')


# 确定layout，固定节点的位置
l <- create_layout(G, 'stress') %>% select(x,y) %>% as.matrix()
G %>%
  activate(edges) %>%
  ggraph(layout = l) + 
  geom_edge_fan() + 
  geom_node_point()
# 过滤后节点位置不变
G %>%
  activate(edges) %>%
  filter(type == 'friendship') %>%
  ggraph(layout = l) + 
  geom_edge_fan() + 
  geom_node_point()
