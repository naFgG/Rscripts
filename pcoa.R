library(vegan)
library(ggplot2)
library(ape)
# PCoA无分组，无置信椭圆
data("varespec")
df <- varespec
rownames(df) <- paste0("site",1:24)
bray_dist <- vegdist(df,method = "bray")
df.pcoa <- pcoa(bray_dist)

df.plot <- data.frame(df.pcoa$vectors)
x_label <- round(df.pcoa$values$Rel_corr_eig[1]*100,2)
y_label <- round(df.pcoa$values$Rel_corr_eig[2]*100,2)
ggplot(data=df.plot,aes(x=Axis.1,y=Axis.2))+
  geom_point()+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  labs(x=paste0("PCoA1 ",x_label,"%"),
       y=paste0("PCoA2 ",y_label,"%"))

# PCoA有分组，有置信椭圆
df.plot$group<-ifelse(df.plot$Axis.1<0,"AAA","BBB")
ggplot(data=df.plot,aes(x=Axis.1,y=Axis.2,
                        color=group,shape=group))+
  geom_point(size=5)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  labs(x=paste0("PCoA1 ",x_label,"%"),
       y=paste0("PCoA2 ",y_label,"%"))+
  stat_ellipse(data=df.plot,
               geom = "polygon",
               aes(fill=group),
               alpha=0.3)+
  scale_fill_manual(values = c("#e31a1c","#1f78b4"))

# p <- p + scale_color_manual(values = mycol) +
  # labs(title = paste0("PERMANOVA ", "Pvalue: ", pval)) +
  # theme(
    # plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 18),
    # axis.title.x = element_text(size = 20),
    # axis.title.y = element_text(size = 20),
    # axis.text.x = element_text(size = 18),
    # axis.text.y = element_text(size = 18),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
  # )