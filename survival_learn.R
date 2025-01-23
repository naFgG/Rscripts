#! /usr/bin/env Rscript
rm(list=ls())
suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(survminer))
suppressPackageStartupMessages(library(ggsurvfit))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(gtsummary))

## 这个数据的持续时间是以日为单位的
survival_data <- lung
# 修改删失状态 ####
## 0=删失，1=事件，必须是这样编码 ####
survival_data %<>% 
  mutate(status=recode(status, `1`=0, `2`=1))
## 如果数据中没有生存时间，需要先计算
date_ex <- 
  tibble(
    sx_date = c("2007-06-22", "2004-02-13", "2010-10-27"), # 手术日期
    last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31") # 最后一次随访日期
  )
## 转换为日期格式
date_ex %<>% 
  mutate(sx_date=ymd(sx_date),
         last_fup_date=ymd(last_fup_date))
## 计算持续时间，以年为单位
date_ex %>% 
  mutate(
    os_yrs = as.duration(sx_date %--% last_fup_date) / dyears(1)
  )

# 创建生存对象 ####
## 对象如果删失，则后跟+
Surv(survival_data$time, survival_data$status)[1:10]

# Kaplan-Meier ###
## Surv(), 第一个参数是持续时间，第二个参数是事件状态。对象如果删失，则后跟+
## ~1, 无分组的生存模型，不考虑任何分组变量，仅计算总体的生存曲线
## survfit2是ggsurvfit包的，只是为了更好的画图
s1 <- survfit2(Surv(time, status) ~ 1, data=survival_data)
## 还是用survminer好一点

p <- s1 %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()

p + add_risktable()

## 估计n年生存概率 ####
### 这个数据集是天，所以要转换
summary(s1, times=365.25) # 41%左右
### 生成表格
s1 %>% 
  tbl_survfit(times=365.25,
              label_header="**1-year survival (95% CI)**")
### 中位生存时间就是s1对象的median

# log-rank检验，组间生存时间是否存在差异 ####
## 性别差异
survdiff(Surv(time, status) ~ sex, data=survival_data)


# Cox回归 ####
coxph(Surv(time, status) ~  , survival_data)
coxph(Surv(time, status) ~ sex + age, survival_data) %>% 
  tbl_regression(exp = TRUE) 
# df$treatment <- relevel(df$treatment, ref = 'placebo')
# 森林图
s3 <- coxph(Surv(time, status) ~ sex + age, survival_data)
ggforest(s3, data=survival_data)
