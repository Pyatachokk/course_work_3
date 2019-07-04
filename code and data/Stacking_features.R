library(rio)
library(timeSeries)
library(rlist)
library(foreign)
library(ggplot2)
library(dplyr)
library(forecast)
library(TSPred)
library(forecTheta)
library(NHMSAR)
library(tsDyn)
library(TSA)
library(gridExtra)

# Loading sample features
norm_features = read.csv('train_features/train_normed.csv', row.names = 'X')
centered_features = read.csv('train_features/train_centered.csv', row.names = 'X')
means = read.csv('train_features/means.csv', row.names = 'X')

norm_features$nperiods = NULL
centered_features$nperiods = NULL

labels = norm_features$label
norm_features$label = NULL
centered_features$label = NULL

# Loading generated features

feat_gen_yearly = read.csv('generated/features/Generated_centered_features_yearly.csv', row.names = 'X')
feat_gen_quarterly = read.csv('generated/features/Generated_centered_features_quarterly.csv', row.names = 'X')
feat_gen_monthly = read.csv('generated/features/Generated_centered_features_monthly.csv', row.names = 'X')
feat_gen_weekly = read.csv('generated/features/Generated_centered_features_weekly.csv', row.names = 'X')
feat_gen_daily = read.csv('generated/features/Generated_centered_features_daily.csv', row.names = 'X')

names(feat_gen_yearly) = names(norm_features)
names(feat_gen_quarterly) = names(norm_features)
names(feat_gen_monthly) = names(norm_features)
names(feat_gen_weekly) = names(norm_features)
names(feat_gen_daily) = names(norm_features)

# Splitting sample features and stacking it with generated features

feat_yearly = rbind(centered_features %>% slice(1: 1999), feat_gen_yearly)
feat_quarterly = rbind(centered_features %>% slice(2000: 3999), feat_gen_quarterly)
feat_monthly = rbind(centered_features %>% slice(4000: 5999), feat_gen_monthly)
feat_weekly = rbind(centered_features %>% slice(6000: 6285), feat_gen_weekly)
feat_daily = rbind(centered_features %>% slice(6286: 8285), feat_gen_daily)

# Stacking all features
feat_all = rbind(feat_yearly, feat_quarterly, feat_monthly, feat_weekly, feat_daily)


# Loading metrics and generating leabels vector
metrics_yearly = read.csv('metrics/Metrics_yearly.csv', row.names = 'X')
metrics_quarterly = read.csv('metrics/Metrics_quarterly.csv', row.names = 'X')
metrics_monthly = read.csv('metrics/Metrics_monthly.csv', row.names = 'X')
metrics_weekly = read.csv('metrics/Metrics_weekly.csv', row.names = 'X')
metrics_daily = read.csv('metrics/Metrics_daily.csv', row.names = 'X')

metrics = rbind(metrics_yearly, metrics_quarterly, metrics_monthly, metrics_weekly, metrics_daily)
metrics_best = data.frame(apply(metrics, 1, function(x) (sort(x))[1]), apply(metrics, 1, function(x) names(sort(x)[1])))
names(metrics_best) = c('value', 'label')


feat_all$label = metrics_best$label

# Writing the final dataset for classification

write.csv(feat_all, sep = ",", dec = ".", file = 'final_dataset.csv',row.names = T, col.names = T)





