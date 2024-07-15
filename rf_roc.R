rm(list=ls())
library(pROC)
library(ggplot2)
library(caret)

args <- commandArgs(T)
outpath <- dirname(args[1])

# 样本在列，物种在行
df <- read.table(args[1],sep="\t",header=T,row.names=1)
df <- data.frame(t(df))

# 样本-分组表
mapping <- read.table(args[2],sep="\t",header=T,row.names=1,comment.char="", na.strings = "")
group <- mapping$Group
group <- data.frame(group)
# 二分类问题
group$group <- as.factor(group$group)
df_group <- cbind(df, group)

# 设置十折交叉验证的控制参数
cv_control <- trainControl(method="cv", number=10, savePredictions="final", 
                           classProbs=T, summaryFunction=twoClassSummary)
# 训练随机森林模型并执行十折交叉验证
set.seed(123456)
rf_model_cv <- train(group ~ ., data=df_group, 
                     method="rf", trControl=cv_control, metric="ROC", importance=T)
# 提取最终训练的随机森林模型
final_rf_model <- rf_model_cv$finalModel
# 查看特征重要性
importance_rf <- data.frame(importance(final_rf_model))
importance_rf <- importance_rf[order(importance_rf$MeanDecreaseGini, decreasing=T), ]
rownames(importance_rf) <- gsub("X.Eubacterium.", "[Eubacterium]", rownames(importance_rf))
rownames(importance_rf) <- gsub(".Eubacterium.", "[Eubacterium]", rownames(importance_rf))
importance_rf <- cbind(" "=rownames(importance_rf), importance_rf)
# 混淆矩阵
confusion_df <- cbind(" "=rownames(final_rf_model$confusion), data.frame(final_rf_model$confusion))

write.table(importance_rf, file=paste0(outpath, "/importance.xls"), sep="\t", quote=F, row.names=F)
write.table(confusion_df, file=paste0(outpath, "/confusion.xls"), sep="\t", quote=F, row.names=F)

# 提取交叉验证的预测概率和真实标签
pred <- rf_model_cv$pred
# 计算ROC曲线
tmp <- levels(group$group)[2]
# roc()的第二个参数是阴性类别
# 默认设置的方向是将"control"类别作为阴性类别（负类，通常表示没有事件的情况），而将"case"类别作为阳性类别（正类，通常表示有事件的情况）。
roc_obj <- roc(pred$obs, pred[, tmp], levels=levels(group$group))
roc_data <- data.frame(FPR=rev(roc_obj$specificities),
                       TPR=rev(roc_obj$sensitivities))
# 绘制ROC曲线
p <- ggroc(roc_obj, legacy.axes=T, color="blue", linewidth=0.8)+
  geom_abline(slope=1, intercept=0, linetype="dashed", color="red") +
  labs(x="1-Specificity", y="Sensitivity")+
  theme_bw()+
  theme(panel.grid=element_blank())
pdf(paste0(outpath, "/ROC.pdf"), width=5, height=4)
p
dev.off()

