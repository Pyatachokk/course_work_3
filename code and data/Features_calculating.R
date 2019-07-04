library(rio)
library(timeSeries)
library(rlist)
library(foreign)
library(ggplot2)
library(dplyr)
library(ggbiplot)
library(gridExtra)
library(GA)
library(ggalt)
library(pracma)
library(tidyverse)
library(sp)
library(random)
library(SearchTrees)
library(Metrics)
library(TSdist)
library(car)
library(readxl)


library(nonlinearTseries)
library(moments) #skewness, kurtosis
library(tseries) #Nonlinearity test
library(Chaos01) #Chaoticness measure
library(ForeCA) #Spectral entropy
library(arfima) # Self-similarity
library(tsfeatures) #Many features
library(ismev) #Extreme dist shape parameter
library(anomalousACM) #Anomalous features

library(umap)


# Loading sampled data


yearly = read.csv('sample/Yearly_sample.csv', row.names = 'X')
quarterly = read.csv('sample/Quarterly_sample.csv', row.names = 'X')
monthly = read.csv('sample/Monthly_sample.csv', row.names = 'X')
weekly = read.csv('sample/Weekly_sample.csv', row.names = 'X')
daily = read.csv('sample/Daily_sample.csv', row.names = 'X')
hourly = read.csv('sample/Hourly_sample.csv', row.names = 'X')




data = list(Yearly = yearly, Quarterly = quarterly, Monthly = monthly,
            Weekly = weekly, Daily = daily, Hourly = hourly)




# for (set in data) {
#   kurtosis = colKurtosis(set)
#   skewness = colSkewness(set)
# 
# }



# Fixing the dataset

set=data$Yearly

# setting the length of training part
train_length = 27


feat = function(set){
  
  # Generatin empty arrays for one datset features
  lyap = c() 
  kurtosis = c()
  skewness = c()
  chaos = c()
  extreme_dist_shape = c()
  lshift = c()
  fspots = c()
  max_kl = c()
  seasonal_strength = c()
  trend = c()
  spike = c()
  linearity = c()
  curvature = c()
  
  # Extracting part of features
  features = tsfeatures(set,scale = F, parallel = T, features = c('acf_features', 'pacf_features', 
                                                                  'entropy', 'lumpiness', 'stability',
                                                                  'crossing_points', 'hurst', 'unitroot_kpss', 'nonlinearity'))
  # Extracting other features
  for (i in c(1: length(set))) {
    tser = ts(set[[i]])[1: train_length]
    print(i)
    
    # ml = maxLyapunov(tser, radius = 1,min.embedding.dim = 10, time.lag = 5, do.plot = F)
    # lyap[i] = estimate(ml)
    kurtosis[i] = kurtosis(tser) #Excess kurtosis = kurtosis - 3
    skewness[i] = skewness(tser)
    chaos[i] = testChaos01(tser)
    extreme_dist_shape[i] = gev.fit(tser, show = F)$mle[3]
    measures = tsmeasures(tser, width = 5)
    lshift[i] = measures[4]
    fspots[i] = measures[7]
    max_kl[i] = max_kl_shift(tser, width = 5)
    stl_f = stl_features(tser)
    trend[i] = stl_f[3]
    spike[i] = stl_f[4]
    linearity[i] = stl_f[5]
    curvature[i] = stl_f[6]
    seasonal_strength[i] = 0
  }
  
  # Adding second part of features to the first
  
  features$kurtosis = kurtosis
  features$skewness = skewness
  features$chaos = chaos
  features$lshift = lshift
  features$fspots = fspots
  features$max_kl = max_kl
  features$extreme_dist_shape = extreme_dist_shape
  features$seasonal_strength = seasonal_strength
  features$trend = trend
  features$spike = spike
  features$linearity = linearity
  features$curvature = curvature
  
  # Deleting excess features
  
  
  features$seasonal_period = NULL
  features$x_acf10 = NULL
  features$diff1_acf10 = NULL 
  features$diff2_acf10 = NULL
  features$diff1x_pacf5 = NULL
  features$diff2x_pacf5 = NULL
  features$e_acf1 = NULL
  features$e_acf10 = NULL
  
  return(features)
  
}

# Applying function and writing features to csv
features = feat(set)

# write.csv(features, sep = ",", dec = ".", file = 'Yearly_features.csv',row.names = T, col.names = T)

