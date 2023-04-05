rm(list = ls())
library(circlize)

col_fun = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))

# circos.heatmap() ####
## heatmap() ####
set.seed(123)
mat1 = rbind(cbind(matrix(rnorm(50*5, mean = 1), nr = 50), 
                   matrix(rnorm(50*5, mean = -1), nr = 50)),
             cbind(matrix(rnorm(50*5, mean = -1), nr = 50), 
                   matrix(rnorm(50*5, mean = 1), nr = 50))
)
rownames(mat1) = paste0("R", 1:100)
colnames(mat1) = paste0("C", 1:10)
mat1 = mat1[sample(100, 100), ]
split = sample(letters[1:5], 100, replace = TRUE)
split = factor(split, levels = letters[1:5])
Heatmap(mat1, row_split = split)
## convert to circos heatmap ####
col_fun1 = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
circos.heatmap(mat1, split = split, col = col_fun1)
circos.clear()

# zoom ####
## right half original while left half zoomed ####
set.seed(123)
df <-  data.frame(
  sectors = sample(letters[1:6], 400, replace = TRUE),
  x = rnorm(400),
  y = rnorm(400),
  stringsAsFactors = FALSE)
zoom_df_a = df[df$sectors == "a", ]
zoom_df_b = df[df$sectors == "b", ]
zoom_df_b = zoom_df_b[order(zoom_df_b[, 2])[1:10], ]
zoom_df = rbind(zoom_df_a, zoom_df_b)
zoom_df$sectors = paste0("zoom_", zoom_df$sectors)
df2 = rbind(df, zoom_df)  
xrange = tapply(df2$x, df2$sectors, function(x) max(x) - min(x))
normal_sector_index = unique(df$sectors)
zoomed_sector_index = unique(zoom_df$sectors)
sector.width = c(xrange[normal_sector_index] / sum(xrange[normal_sector_index]), 
                 xrange[zoomed_sector_index] / sum(xrange[zoomed_sector_index]))
circos.par(start.degree = 90, points.overflow.warning = FALSE)
circos.initialize(df2$sectors, x = df2$x, sector.width = sector.width)
circos.track(df2$sectors, x = df2$x, y = df2$y, 
             panel.fun = function(x, y) {
               circos.points(x, y, col = "red", pch = 16, cex = 0.5)
               circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + mm_y(2), 
                           CELL_META$sector.index, niceFacing = TRUE)
             })
circos.link("a", get.cell.meta.data("cell.xlim", sector.index = "a"),
            "zoom_a", get.cell.meta.data("cell.xlim", sector.index = "zoom_a"),
            border = NA, col = "#00000020")
circos.link("b", c(zoom_df_b[1, 2], zoom_df_b[10, 2]),
            "zoom_b", get.cell.meta.data("cell.xlim", sector.index = "zoom_b"),
            rou1 = get.cell.meta.data("cell.top.radius", sector.index = "b"),
            border = NA, col = "#00000020")
circos.clear()