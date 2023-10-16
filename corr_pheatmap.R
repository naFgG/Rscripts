rm(list = ls())
library(pheatmap)
library(psych)
library(openxlsx)

args <- commandArgs(T)
if (args[1] == "-h" | args[1] == "--help"){
  print("Rscript corr_pheatmap df1.xls df2.xls outpath method[pearson|spearman]")
}

names <- paste(tools::file_path_sans_ext(basename(args[1])), 
               tools::file_path_sans_ext(basename(args[2])), 
               sep="-")

df1 <- read.table(args[1], header=T, row.names=1, sep="\t", quote="", check.names=F)
df2 <- read.table(args[2], header=T, row.names=1, sep="\t", quote="", check.names=F)
df2 <- df2[, colnames(df1)]
df1 <- t(df1); df2 <- t(df2)

corr <- corr.test(df1, df2, method=args[4], adjust="fdr")
cor.r <- corr$r
cor.p <- corr$p
sheets <- list("Correlative_Value"=cor.r, "P_Value"=cor.p)
if (args[4] == "pearson") {
  openxlsx::write.xlsx(sheets, paste0(args[3], "/", names, "_Pearson.xlsx"), rowNames=T)
} else if (args[4] == "spearman") {
  openxlsx::write.xlsx(sheets, paste0(args[3], "/", names, "_Spearman.xlsx"), rowNames=T)
}

cor.p[cor.p < 0.001] <- "***"
cor.p[cor.p >= 0.001 & cor.p < 0.01] <- "**"
cor.p[cor.p >= 0.01 & cor.p < 0.05] <- "*"
cor.p[cor.p >= 0.05] <- ""

colors <- c("#0000FF", "#0202FF", "#0404FF", "#0606FF", "#0808FF", "#0A0AFF", "#0C0CFF", 
            "#0E0EFF", "#1010FF", "#1212FF", "#1414FF", "#1616FF", "#1818FF", "#1A1AFF", 
            "#1C1CFF", "#1E1EFF", "#2020FF", "#2222FF", "#2424FF", "#2626FF", "#2828FF", 
            "#2A2AFF", "#2C2CFF", "#2E2EFF", "#3030FF", "#3232FF", "#3434FF", "#3636FF", 
            "#3838FF", "#3A3AFF", "#3C3CFF", "#3E3EFF", "#4040FF", "#4141FF", "#4444FF", 
            "#4646FF", "#4848FF", "#4949FF", "#4C4CFF", "#4E4EFF", "#5050FF", "#5151FF", 
            "#5454FF", "#5656FF", "#5858FF", "#5959FF", "#5C5CFF", "#5E5EFF", "#6060FF", 
            "#6161FF", "#6464FF", "#6666FF", "#6868FF", "#6969FF", "#6C6CFF", "#6E6EFF", 
            "#7070FF", "#7171FF", "#7474FF", "#7676FF", "#7878FF", "#7979FF", "#7C7CFF", 
            "#7E7EFF", "#8080FF", "#8282FF", "#8383FF", "#8686FF", "#8888FF", "#8A8AFF", 
            "#8C8CFF", "#8E8EFF", "#9090FF", "#9292FF", "#9393FF", "#9696FF", "#9898FF", 
            "#9A9AFF", "#9C9CFF", "#9E9EFF", "#A0A0FF", "#A2A2FF", "#A3A3FF", "#A6A6FF", 
            "#A8A8FF", "#AAAAFF", "#ACACFF", "#AEAEFF", "#B0B0FF", "#B2B2FF", "#B3B3FF", 
            "#B6B6FF", "#B8B8FF", "#BABAFF", "#BCBCFF", "#BEBEFF", "#C0C0FF", "#C2C2FF", 
            "#C3C3FF", "#C6C6FF", "#C8C8FF", "#CACAFF", "#CCCCFF", "#CECEFF", "#D0D0FF", 
            "#D2D2FF", "#D3D3FF", "#D6D6FF", "#D8D8FF", "#DADAFF", "#DCDCFF", "#DEDEFF", 
            "#E0E0FF", "#E2E2FF", "#E3E3FF", "#E6E6FF", "#E8E8FF", "#EAEAFF", "#ECECFF", 
            "#EEEEFF", "#F0F0FF", "#F2F2FF", "#F3F3FF", "#F6F6FF", "#F8F8FF", "#FAFAFF", 
            "#FCFCFF", "#FEFEFF", "#FFFEFE", "#FFFCFC", "#FFFAFA", "#FFF8F8", "#FFF6F6", 
            "#FFF3F3", "#FFF2F2", "#FFF0F0", "#FFEEEE", "#FFECEC", "#FFEAEA", "#FFE8E8", 
            "#FFE5E5", "#FFE3E3", "#FFE2E2", "#FFE0E0", "#FFDEDE", "#FFDCDC", "#FFDADA", 
            "#FFD8D8", "#FFD6D6", "#FFD3D3", "#FFD2D2", "#FFD0D0", "#FFCECE", "#FFCCCC", 
            "#FFCACA", "#FFC8C8", "#FFC5C5", "#FFC3C3", "#FFC2C2", "#FFC0C0", "#FFBEBE", 
            "#FFBCBC", "#FFBABA", "#FFB8B8", "#FFB6B6", "#FFB3B3", "#FFB2B2", "#FFB0B0", 
            "#FFAEAE", "#FFACAC", "#FFAAAA", "#FFA8A8", "#FFA5A5", "#FFA3A3", "#FFA2A2", 
            "#FFA0A0", "#FF9E9E", "#FF9C9C", "#FF9A9A", "#FF9898", "#FF9696", "#FF9393", 
            "#FF9292", "#FF9090", "#FF8E8E", "#FF8C8C", "#FF8A8A", "#FF8888", "#FF8585", 
            "#FF8383", "#FF8282", "#FF8080", "#FF7E7E", "#FF7C7C", "#FF7A7A", "#FF7878", 
            "#FF7676", "#FF7373", "#FF7171", "#FF7070", "#FF6E6E", "#FF6C6C", "#FF6A6A", 
            "#FF6868", "#FF6565", "#FF6363", "#FF6161", "#FF6060", "#FF5E5E", "#FF5C5C", 
            "#FF5A5A", "#FF5858", "#FF5656", "#FF5353", "#FF5151", "#FF5050", "#FF4E4E", 
            "#FF4C4C", "#FF4A4A", "#FF4848", "#FF4545", "#FF4343", "#FF4141", "#FF4040", 
            "#FF3E3E", "#FF3C3C", "#FF3A3A", "#FF3838", "#FF3636", "#FF3333", "#FF3131", 
            "#FF3030", "#FF2E2E", "#FF2C2C", "#FF2A2A", "#FF2828", "#FF2525", "#FF2323", 
            "#FF2121", "#FF2020", "#FF1E1E", "#FF1C1C", "#FF1A1A", "#FF1818", "#FF1616", 
            "#FF1313", "#FF1111", "#FF1010", "#FF0E0E", "#FF0C0C", "#FF0A0A", "#FF0808", 
            "#FF0505", "#FF0303", "#FF0101", "#FF0000")

if (args[4] == "pearson") {
  pdf(paste0(args[3], "/", names, "_Pearson_heatmap.pdf"))
} else if (args[4] == "spearman") {
  pdf(paste0(args[3], "/", names, "_Spearman_heatmap.pdf"))
}
pheatmap(cor.r, display_numbers=cor.p, number_color="black", color=colors)
dev.off()
