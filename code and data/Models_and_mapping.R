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
library(skimr)

# Loading raw data
data_raw= read.csv('sample/Monthly_sample.csv', row.names = 'X')
data_raw_gen = read.csv('generated/raw/Generated_monthly.csv', row.names = 'X')

# quarterly = read.csv('sample/Quarterly_sample.csv', row.names = 'X')
# monthly = read.csv('sample/Monthly_sample.csv', row.names = 'X')
# weekly = read.csv('sample/Weekly_sample.csv', row.names = 'X')
# daily = read.csv('sample/Daily_sample.csv', row.names = 'X')
# hourly = read.csv('sample/Hourly_sample.csv', row.names = 'X')

# Stacking sample data and generated data, defining parameters
data = cbind(data_raw, data_raw_gen)
freq = 12
h = 16
origin = 140

# Defining empty matrix for results
results = matrix(ncol = 9, nrow = length(data))

for (i in seq(1, length(data))){
  tser = ts(data[[i]], frequency = freq)
  

  train = ts(tser[1: 140], frequency = freq)
  test = ts(tser[141:156], frequency = freq)
  
  
  
  # Naive
  
  naive_forecast = as.vector(naive(train, h = h)$mean)
  naive_error = sMAPE(naive_forecast, test)
  
  # Seasonal naive
  snaive_forecast = as.vector(snaive(train, h=h)$mean)
  snaive_error = sMAPE(snaive_forecast, test)
  
  
  # Random walk with drift
  rwd_forecast = as.vector(rwf(train, h = h, drift = TRUE)$mean)
  rwd_error = nnet_error = sMAPE(rwd_forecast, test)
  
  # Neural net
  nnet_forecast = as.vector((nnetar(train) %>% forecast(h = h))$mean)
  nnet_error = sMAPE(nnet_forecast, test)
  
  # TBATS
  tbats_forecast = as.vector((tbats(train, use.parallel = F) %>% forecast(h = h))$mean)
  tbats_error = sMAPE(tbats_forecast, test)
  
  
  
  # ARIMA
  arima_forecast = (auto.arima(train, stepwise=FALSE, approximation=FALSE) %>% forecast(h = h))$mean
  arima_error = sMAPE(as.vector(arima_forecast), test)
  
  
  
  #Simple exponential smoothing
  fc = ses(train, h=h)
  ses_forecast = data.frame(fc)$Point.Forecast
  ses_esrror = sMAPE(ses_forecast, test)
  
  
  
  # ETS
  ets_forecast = (ets(train) %>% forecast(h = h))$mean
  ets_error = sMAPE(as.vector(ets_forecast), test)
  
  ` `
  #Theta
  theta_forecast = thetaf(train, h = h)$mean
  theta_error = sMAPE(as.vector(theta_forecast), test)
  
  resid_vec = c(naive_error, snaive_error, rwd_error, arima_error, ses_esrror, ets_error, theta_error, tbats_error, nnet_error)
  results[i,] = resid_vec
  print(i)
  
}

metrics = data.frame(results)
names(metrics) = c('Naive', 'Seasonal_Naive', 'RW_with_drift', 'ARIMA', 'SES', 'ETS', 'Theta', 'TBATS', 'Neural_net')

# Saving the results
write.csv(metrics, sep = ",", dec = ".", file = 'metrics/Metrics_monthly.csv',row.names = T, col.names = T)



# Plots

norm_features = read.csv('train_features/train_normed.csv', row.names = 'X')
centered_features = read.csv('train_features/train_centered.csv', row.names = 'X')
means = read.csv('train_features/means.csv', row.names = 'X')

norm_features$nperiods = NULL
centered_features$nperiods = NULL

labels = norm_features$label
norm_features$label = NULL
centered_features$label = NULL
pca = prcomp(norm_features, center = T, scale. = F)
pca$x = data.frame(pca$x)[,1:2]
names(pca$x) = c('PC_1', 'PC_2')

# Loading sample PCA data

pca_yearly = data.frame(pca$x) %>% slice(1: 1999)
pca_quarterly = data.frame(pca$x) %>% slice(2000: 3999)
pca_monthly = data.frame(pca$x) %>% slice(4000: 5999)
pca_weekly = data.frame(pca$x) %>% slice(6000: 6285)
pca_daily = data.frame(pca$x) %>% slice(6286: 8285)
pca_hourly = data.frame(pca$x) %>% slice(8286:8530)

# Loading generated PCA data
pca_gen_yearly = read.csv('generated/pca/Generated_yearly_pca.csv', row.names = 'X')
pca_gen_quarterly = read.csv('generated/pca/Generated_quarterly_pca.csv', row.names = 'X')
pca_gen_monthly = read.csv('generated/pca/Generated_monthly_pca.csv', row.names = 'X')
pca_gen_weekly = read.csv('generated/pca/Generated_weekly_pca.csv', row.names = 'X')
pca_gen_daily = read.csv('generated/pca/Generated_daily_pca.csv', row.names = 'X')


# Stacking sample adn generated pca data
pca_yearly = rbind(data.frame(pca$x) %>% slice(1: 1999), pca_gen_yearly)
pca_quarterly = rbind(data.frame(pca$x) %>% slice(2000: 3999), pca_gen_quarterly)
pca_monthly = rbind(data.frame(pca$x) %>% slice(4000: 5999), pca_gen_monthly)
pca_weekly = rbind(data.frame(pca$x) %>% slice(6000: 6285), pca_gen_weekly)
pca_daily = rbind(data.frame(pca$x) %>% slice(6286: 8285), pca_gen_daily)


# Loading metrics
metrics_yearly = read.csv('metrics/Metrics_yearly.csv', row.names = 'X')
metrics_quarterly = read.csv('metrics/Metrics_quarterly.csv', row.names = 'X')
metrics_monthly = read.csv('metrics/Metrics_monthly.csv', row.names = 'X')
metrics_weekly = read.csv('metrics/Metrics_weekly.csv', row.names = 'X')
metrics_daily = read.csv('metrics/Metrics_daily.csv', row.names = 'X')

# Stacking metrics
metrics = rbind(metrics_yearly, metrics_quarterly, metrics_monthly, metrics_weekly, metrics_daily)

# Defining function,  calculating mean and sd for best models
num_best = function(data){
  a = data.frame(apply(data, 1, function(x) (sort(x))[1]), apply(data, 1, function(x) names(sort(x)[1])))
  names(a) = c('value', 'label')
  
  return(a %>% group_by(label) %>% summarize(mean = mean(value), sd = sd(value)))
}



pca_all = rbind(pca_yearly, pca_quarterly, pca_monthly, pca_weekly, pca_daily)

# Choosing best models for each time series

metrics_best_yearly = data.frame(apply(metrics_yearly, 1, function(x) (sort(x))[1]), apply(metrics_yearly, 1, function(x) names(sort(x)[1])))
metrics_best_quarterly = data.frame(apply(metrics_quarterly, 1, function(x) (sort(x))[1]), apply(metrics_quarterly, 1, function(x) names(sort(x)[1])))
metrics_best_monthly = data.frame(apply(metrics_monthly, 1, function(x) (sort(x))[1]), apply(metrics_monthly, 1, function(x) names(sort(x)[1])))
metrics_best_weekly = data.frame(apply(metrics_weekly, 1, function(x) (sort(x))[1]), apply(metrics_weekly, 1, function(x) names(sort(x)[1])))
metrics_best_daily = data.frame(apply(metrics_daily, 1, function(x) (sort(x))[1]), apply(metrics_daily, 1, function(x) names(sort(x)[1])))
metrics_best = data.frame(apply(metrics, 1, function(x) (sort(x))[1]), apply(metrics, 1, function(x) names(sort(x)[1])))



names(metrics_best_yearly) = c('value', 'label')
names(metrics_best_quarterly) = c('value', 'label')
names(metrics_best_monthly) = c('value', 'label')
names(metrics_best_weekly) = c('value', 'label')
names(metrics_best_daily) = c('value', 'label')
names(metrics_best) = c('value', 'label')

# Plotting best SMAPE for each cluster

metrics_all =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics_best$value), show.legend = T ) +
  labs(title="All series") +
  scale_color_gradient(low='green', high='black') + labs(x = NULL, y = 'PC2')
metrics_all$labels$colour <- "SMAPE"
plot(metrics_all)

m_plot_yearly = ggplot(pca_yearly, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics_best_yearly$value), show.legend = T ) +
  labs(title="Yearly series") +
  scale_color_gradient(low='green', high='black') + labs(x = NULL, y = NULL)
m_plot_yearly$labels$colour <- "SMAPE"
plot(m_plot_yearly)

m_plot_quarterly = ggplot(pca_quarterly, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics_best_quarterly$value), show.legend = T ) +
  labs(title="Quarterly series") +
  scale_color_gradient(low='green', high='black')+ labs(x = NULL, y = 'PC2')
m_plot_quarterly$labels$colour <- "SMAPE"
plot(m_plot_quarterly)

m_plot_monthly = ggplot(pca_monthly, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics_best_monthly$value), show.legend = T ) +
  labs(title="Monthly series") +
  scale_color_gradient(low='green', high='black') + labs(x = NULL, y = NULL)
m_plot_monthly$labels$colour <- "SMAPE"
plot(m_plot_monthly)

m_plot_weekly = ggplot(pca_weekly, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics_best_weekly$value), show.legend = T ) +
  labs(title="Weekly series") +
  scale_color_gradient(low='green', high='black') + labs(x = "PC1", y = 'PC2')
m_plot_weekly$labels$colour <- "SMAPE"
plot(m_plot_weekly)

m_plot_daily = ggplot(pca_daily, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics_best_daily$value), show.legend = T ) +
  labs(title="Daily series") +
  scale_color_gradient(low='green', high='black') + ylim(-0.15, 0.25) + labs(x = "PC1", y = NULL)
m_plot_daily$labels$colour <- "SMAPE"
plot(m_plot_daily)

grid.arrange(metrics_all, m_plot_yearly, m_plot_weekly, m_plot_daily, nrow = 2)



# Ploting SMAPE fore each model
metrics_naive =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics$Naive), show.legend = T ) +
  labs(title="Naive") +
  scale_color_gradient(low='orange', high='black') + labs(x = 'PC2', y = 'PC2')
metrics_naive$labels$colour <- "SMAPE"
plot(metrics_naive)
ggsave('image1.png', width = 10, height = 8, dpi = 500)

metrics_snaive =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics$Seasonal_Naive), show.legend = T ) +
  labs(title="Seasonal Naive") +
  scale_color_gradient(low='orange', high='black') + labs(x = 'PC2', y = 'PC2')
metrics_snaive$labels$colour <- "SMAPE"
plot(metrics_snaive)
ggsave('image2.png', width = 10, height = 8, dpi = 500)

metrics_rwd =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics$RW_with_drift), show.legend = T ) +
  labs(title="Random walk with drift") +
  scale_color_gradient(low='orange', high='black') + labs(x = 'PC2', y = 'PC2')
metrics_rwd$labels$colour <- "SMAPE"
plot(metrics_rwd)
ggsave('image3.png', width = 10, height = 8, dpi = 500)

metrics_ses =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics$SES), show.legend = T ) +
  labs(title="Simple exponential smoothing") +
  scale_color_gradient(low='orange', high='black') + labs(x = 'PC2', y = 'PC2')
metrics_ses$labels$colour <- "SMAPE"
plot(metrics_ses)
ggsave('image4.png', width = 10, height = 8, dpi = 500)

metrics_ets =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics$ETS), show.legend = T ) +
  labs(title="ETS") +
  scale_color_gradient(low='orange', high='black') + labs(x = 'PC2', y = 'PC2')
metrics_ets$labels$colour <- "SMAPE"
plot(metrics_ets)
ggsave('image5.png', width = 10, height = 8, dpi = 500)

metrics_arima =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics$ARIMA), show.legend = T ) +
  labs(title="ARIMA") +
  scale_color_gradient(low='orange', high='black') + labs(x = 'PC2', y = 'PC2')
metrics_arima$labels$colour <- "SMAPE"
plot(metrics_arima)
ggsave('image6.png', width = 10, height = 8, dpi = 500)

metrics_theta =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics$Theta), show.legend = T ) +
  labs(title="Theta") +
  scale_color_gradient(low='orange', high='black') + labs(x = 'PC2', y = 'PC2')
metrics_theta$labels$colour <- "SMAPE"
plot(metrics_theta)
ggsave('image7.png', width = 10, height = 8, dpi = 500)

metrics_tbats =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics$TBATS), show.legend = T ) +
  labs(title="TBATS") +
  scale_color_gradient(low='orange', high='black') + labs(x = 'PC2', y = 'PC2')
metrics_tbats$labels$colour <- "SMAPE"
plot(metrics_tbats)
ggsave('image8.png', width = 10, height = 8, dpi = 500)

metrics_nnet =  ggplot(pca_all, aes(x=PC_1, y=PC_2)) + 
  geom_point(aes(col= metrics$Neural_net), show.legend = T ) +
  labs(title="Neural Net") +
  scale_color_gradient(low='orange', high='black') + labs(x = 'PC2', y = 'PC2')
metrics_nnet$labels$colour <- "SMAPE"
plot(metrics_nnet)
ggsave('image9.png', width = 10, height = 8, dpi = 500)


