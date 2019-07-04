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
library(ggplot2)
library(ggfortify)


# Loading raw data
yearly = read.csv('sample/Yearly_sample.csv', row.names = 'X')
quarterly = read.csv('sample/Quarterly_sample.csv', row.names = 'X')
monthly = read.csv('sample/Monthly_sample.csv', row.names = 'X')
weekly = read.csv('sample/Weekly_sample.csv', row.names = 'X')
daily = read.csv('sample/Daily_sample.csv', row.names = 'X')
hourly = read.csv('sample/Hourly_sample.csv', row.names = 'X')

# Choosing dataset and setting parameters
raw_data = monthly
row_length = 156
sample_size = 300
n_neighbors = 10
frequency = 12

# Loading features and parameters

norm_features = read.csv('train_features/train_normed.csv', row.names = 'X')
centered_features = read.csv('train_features/train_centered.csv', row.names = 'X')
means = read.csv('train_features/means.csv', row.names = 'X')

norm_features$nperiods = NULL
centered_features$nperiods = NULL
# write.csv(norm_features, 'train_normed.csv')

# Separating labels

labels = norm_features$label
norm_features$label = NULL
centered_features$label = NULL

pca = prcomp(norm_features, center = T, scale. = F)


# Splitting PCA data. Need to be careful with indeces.

pca_yearly = data.frame(pca$x) %>% slice(1: 1999)
pca_quarterly = data.frame(pca$x) %>% slice(2000: 3999)
pca_monthly = data.frame(pca$x) %>% slice(4000: 5999)
pca_weekly = data.frame(pca$x) %>% slice(6000: 6285)
pca_daily = data.frame(pca$x) %>% slice(6286: 8285)
pca_hourly = data.frame(pca$x) %>% slice(8286:8530)


# Choosing cluster
pca_data = pca_monthly
pca_data = data.frame(pca_data$PC1, pca_data$PC2)
names(pca_data) = c('PC1', 'PC2')

# Setting bounds for grid generation depending on the cluster parameters

pc1_upper = max(pca_data$PC1) + 0.25
pc1_lower = min(pca_data$PC1) - 0.25

pc2_upper = max(pca_data$PC2) + 0.25
pc2_lower = min(pca_data$PC2) - 0.25

# Defining function for grid generation

make_grid = function(x_seq, y_seq){
  a = max(c(length(x_seq), length(y_seq)))
  PC_1 = c()
  PC_2 = c()
  for (i in seq(1, length(x_seq))){
    for (j in seq(1,  length(y_seq))){
      PC_1[ a * (i - 1) + j] =  x_seq[i]
      PC_2[ a * (i - 1) + j] =  y_seq[j]
    }
  }
  return(data.frame(PC_1, PC_2))
}

# Defining parameters for grid generation. 0.015 is a step for each axis.

x_seq = seq(pc1_lower, pc1_upper, 0.015)
y_seq = seq(pc2_lower,  pc2_upper, 0.015)

# Generating of the polygon around cluster

get_polygon = function(data){
  poly = chull(data$PC1, data$PC2)
  data = data.frame((data %>% slice(poly))$PC1, (data %>% slice(poly))$PC2)
  names(data)= c('PC_1', 'PC_2')
  return(data)
}

# Filtering the generated grid to leave just points inside polygon



get_polygon(pca_data)
is_in_polygon = point.in.polygon(make_grid(x_seq, y_seq)$PC_1, make_grid(x_seq, y_seq)$PC_2, get_polygon(pca_data)$PC_1, get_polygon(pca_data)$PC_2)

in_polygon_indeces = which(is_in_polygon == 1, arr.ind = TRUE)
in_polygon_grid = make_grid(x_seq, y_seq) %>% slice(in_polygon_indeces)
plot(in_polygon_grid)

# Plotting inside-polygon grid

g_prob = ggplot() +

  geom_point(data = pca_data, aes(x = PC1, y= PC2), col = 'green') +
  geom_point(data = in_polygon_grid, aes(x = PC_1, y = PC_2), alpha = 0.1)+
  labs(title='Time series clusters') + labs(x = "PC1", y = NULL) +
  geom_encircle(data = pca_data,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1)
plot(g_prob)





# Searching nearest neighbours for every point inside in-polygon grid

A <- SpatialPoints(cbind(x=pca_data$PC1, y=pca_data$PC2))
B <- SpatialPoints(cbind(x=in_polygon_grid$PC_1, y=in_polygon_grid$PC_2))

tree <- createTree(coordinates(A))
inds_t <- knnLookup(tree, newdat=coordinates(B), k=1)
inds_t



# FIltering in-polygon grid to leave just points, which neighbours not very close
min_dist = 0.02

indeces = c()

for (i in seq(nrow(in_polygon_grid))){
  if(EuclideanDistance(as.vector(t(in_polygon_grid[i,])), as.vector(t(pca_data[inds_t[i],]))) > min_dist){
    indeces = c(indeces, i)
  }
  
}

# Choosing final target points
targets = in_polygon_grid %>% slice(indeces)
targets = targets  %>% slice(sample(seq(1, nrow(targets)), sample_size))


# Plotting targets
g_prob = ggplot() +

  geom_point(data = pca_data, aes(x = PC1, y= PC2), col = 'green') +
  geom_point(data = targets, aes(x = PC_1, y = PC_2),col = 'blue')+
  labs(title='Time series clusters') + labs(x = "PC1", y = NULL)+
  geom_encircle(data = pca_data,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1)+
  geom_point(data = targets, aes(x = PC_1, y = PC_2),col = 'red')
# geom_point(data = pca_year %>% slice(inds[1,]), aes(x = PC1, y = PC2), col = 'black')+
# geom_point(data = pca_year %>% slice(inds[2,]), aes(x = PC1, y = PC2), col = 'black')+
# geom_point(data = pca_year %>% slice(inds[3,]), aes(x = PC1, y = PC2), col = 'black')

plot(g_prob)


# Searching nearest neighbours for each target point
set.seed(2)

A <- SpatialPoints(cbind(x=pca_data$PC1, y=pca_data$PC2))
B <- SpatialPoints(cbind(x=targets$PC_1, y=targets$PC_2))


tree <- createTree(coordinates(A))
inds_p <- knnLookup(tree, newdat=coordinates(B), k=n_neighbors)

# Plotting examples of targets and neighbours

g_prob = ggplot() +

  geom_point(data = pca_data, aes(x = PC1, y= PC2), col = 'green') +
  # geom_point(data = targets, aes(x = PC_1, y = PC_2),col = 'blue')+
  labs(title='Time series clusters') + labs(x = "PC1", y = NULL) +
  geom_encircle(data = pca_data,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1) +
  geom_point(data = targets %>% slice(1:3), aes(x = PC_1, y = PC_2),col = 'red')+
  geom_point(data = pca_data %>% slice(inds_p[1,]), aes(x = PC1, y = PC2), col = 'black')+
  geom_point(data = pca_data %>% slice(inds_p[2,]), aes(x = PC1, y = PC2), col = 'black')+
  geom_point(data = pca_data %>% slice(inds_p[3,]), aes(x = PC1, y = PC2), col = 'black')

plot(g_prob)




# Generation part


# Defining projection weights

pc_1_weights = as.vector(t(pca$rotation[,1]))
pc_2_weights = as.vector(t(pca$rotation[,2]))

# Defining emty matrices for writng results
results_pca = matrix(ncol=2, nrow = sample_size)
results_raw = matrix(ncol = sample_size, nrow = row_length)
results_features = matrix(ncol = 23, nrow = sample_size)

# Loading params
params = read.csv('train_features/means.csv', row.names = 'X')
names(params) = c('mean', 'max', 'min')


# Defining function for features calculation. After calculation features are normed and centered with params
features = function(tser){
  tser = ts(tser, frequency = frequency)
  acf = acf_features(tser)
  pacf = pacf_features(tser)
  measures = tsmeasures(tser, width = frequency)
  stl_f = stl_features(tser)
  
  
  features = c(acf[1], acf[3], acf[6], pacf[1], entropy(tser), lumpiness(tser),
               stability(tser), crossing_points(tser), hurst(tser), unitroot_kpss(tser),
               nonlinearity(tser), kurtosis(tser), skewness(tser), testChaos01(tser),
               measures[4], measures[7], max_kl_shift(tser, width = frequency)[1],gev.fit(tser, show = F)$mle[3],
               stl_f[9], stl_f[3], stl_f[4], stl_f[5], stl_f[6])
  
  normed_fvec = (as.vector(t(features)) - as.vector(t(params$min))) / (as.vector(t(params$max)) - as.vector(t(params$min)))
  centered_fvec = normed_fvec - as.vector(t(params$mean))
  return(centered_fvec)
}

# Defining the fitness function (minimize)

f = function(predicted){
  feat = features(predicted)
  pc_predicted = c(feat %*% pc_1_weights, feat %*% pc_2_weights)
  return(EuclideanDistance(target, pc_predicted))
}



# Generation loop

for (i in seq(sample_size)){
  
  # Choosing target and his initial population
  target = as.vector(t(targets[i,]))
  init_pop = t(raw_data[inds_p[i,]])
  
  # Defining fitness function (maximize)
  fitness = function(x) -f(x)
  
  
  suggestedSol <- init_pop
  GA1 <- ga(type = "real-valued", 
            fitness =  fitness,
            lower = rep(-5, row_length), upper = rep(5, row_length), 
            suggestions = suggestedSol,
            popSize = n_neighbors, maxiter = 100,
            pcrossover = 0.8, pmutation = 0.2, parallel = 2, maxFitness = -0.001)
  
  # Writing results
  feat_sol = features(ts(t(GA1@solution), frequency = frequency))
  pc_predicted = c(feat_sol %*% pc_1_weights, feat_sol %*% pc_2_weights)
  results_pca[i, ] = as.vector(pc_predicted)
  results_raw[, i] = as.vector(t(GA1@solution))
  results_features[i, ] = feat_sol
  print(i)
  
}

# Plot optimization process example
plot(GA1)

# Generated data

gen_data_raw = data.frame(results_raw)
gen_data_pca = data.frame(results_pca)
gen_data_feat = data.frame(results_features)
names(gen_data_pca) = c('PC_1', 'PC_2')


# Sorting points to leave just inside polygon

plot(get_polygon(pca_data))
is_in_poly_gen = point.in.polygon(gen_data_pca$PC_1, gen_data_pca$PC_2, get_polygon(pca_data)$PC_1, get_polygon(pca_data)$PC_2)

in_polygon_gen_pca_inds = which(is_in_poly_gen == 1, arr.ind = TRUE)

in_polygon_gen_raw = gen_data_raw[in_polygon_gen_pca_inds]
in_polygon_gen_pca = gen_data_pca %>% slice(in_polygon_gen_pca_inds)
in_polygon_gen_feat = gen_data_feat %>% slice(in_polygon_gen_pca_inds)


# Saving the results


write.csv(in_polygon_gen_raw, sep = ",", dec = ".", file = 'Generated_monthly.csv',row.names = T, col.names = T)
write.csv(in_polygon_gen_pca, sep = ",", dec = ".", file = 'Generated_monthly_pca.csv',row.names = T, col.names = T)
write.csv(in_polygon_gen_feat, sep = ",", dec = ".", file = 'Generated_centered_features_monthly.csv',row.names = T, col.names = T)


# Plotting last generated series and its neighbours

a = autoplot(ts(init_pop[1,]))
b = autoplot(ts(init_pop[2,]))
c = autoplot(ts(init_pop[3,]))
d = autoplot(ts(init_pop[4,]))
e = autoplot(ts(t(GA1@solution)))

grid.arrange(a, b, c, d, e, nrow = 5)

plot(GA1 + labs(title='Time series clusters'))

# Plotting old data plus new


g_prob = ggplot() +

    geom_point(data = pca_data, aes(x = PC1, y= PC2), col = 'green') +
    # geom_point(data = targets, aes(x = PC_1, y = PC_2),col = 'blue')+
    labs(title='Time series clusters') + labs(x = "PC1", y = NULL) +
    geom_encircle(data = pca_data,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1) +
    geom_point(data = targets, aes(x = PC_1, y = PC_2),col = 'red')+
    geom_point(data = gen_data_pca, aes(x = PC_1, y = PC_2), col = 'black')

  plot(g_prob)

  g_prob = ggplot() +
    
    geom_point(data = pca_data, aes(x = PC1, y= PC2), col = 'green') +
    # geom_point(data = targets, aes(x = PC_1, y = PC_2),col = 'blue')+
    labs(title='Time series clusters') + labs(x = "PC1", y = NULL) +
    geom_encircle(data = pca_data,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1) +
    geom_point(data = targets %>% slice(1:3), aes(x = PC_1, y = PC_2),col = 'red')+
    geom_point(data = in_polygon_gen_pca %>% slice(1,), aes(x = PC_1, y = PC_2), col = 'black')+
    geom_point(data = in_polygon_gen_pca %>% slice(2,), aes(x = PC_1, y = PC_2), col = 'black')+
    geom_point(data = in_polygon_gen_pca %>% slice(3,), aes(x = PC_1, y = PC_2), col = 'black')
  
  plot(g_prob)


  
# Plotting old data plus new data for each cluster
  
pca_gen_yearly = read.csv('generated/pca/Generated_yearly_pca.csv', row.names = 'X')
pca_gen_quarterly = read.csv('generated/pca/Generated_quarterly_pca.csv', row.names = 'X')
pca_gen_monthly = read.csv('generated/pca/Generated_monthly_pca.csv', row.names = 'X')
pca_gen_weekly = read.csv('generated/pca/Generated_weekly_pca.csv', row.names = 'X')
pca_gen_daily = read.csv('generated/pca/Generated_daily_pca.csv', row.names = 'X')

gen_yearly_plot = ggplot() +
  
  geom_point(data = pca_yearly, aes(x = PC1, y= PC2), col = 'green') +
  labs(title='Generated yearly series') + labs(x = NULL, y = 'PC2') + 
  geom_encircle(data = pca_yearly,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1) +
  
  geom_point(data = pca_gen_yearly, aes(x = PC_1, y = PC_2), col = 'purple')

plot(gen_yearly_plot)


gen_quarterly_plot = ggplot() +
  
  geom_point(data = pca_quarterly, aes(x = PC1, y= PC2), col = 'green') +
  labs(title='Generated montly series') + labs(x = NULL, y = NULL) + 
  geom_encircle(data = pca_quarterly,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1) +
  
  geom_point(data = pca_gen_quarterly, aes(x = PC_1, y = PC_2), col = 'purple')

plot(gen_quarterly_plot)

gen_monthly_plot = ggplot() +
  
  geom_point(data = pca_monthly, aes(x = PC1, y= PC2), col = 'green') +
  labs(title='Generated monthly series') + labs(x = NULL, y = 'PC2') + 
  geom_encircle(data = pca_monthly,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1) +
  
  geom_point(data = pca_gen_monthly, aes(x = PC_1, y = PC_2), col = 'purple')

plot(gen_monthly_plot)

gen_weekly_plot = ggplot() +
  
  geom_point(data = pca_weekly, aes(x = PC1, y= PC2), col = 'green') +
  labs(title='Generated weekly series') + labs(x = NULL, y = NULL) + 
  geom_encircle(data = pca_weekly,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1) +
  
  geom_point(data = pca_gen_weekly, aes(x = PC_1, y = PC_2), col = 'purple')

plot(gen_weekly_plot)

gen_daily_plot = ggplot() +
  
  geom_point(data = pca_daily, aes(x = PC1, y= PC2), col = 'green') +
  labs(title='Generated daily series') + labs(x = "PC1", y = 'PC2') + 
  geom_encircle(data = pca_daily,aes(x = PC1, y= PC2) , expand = 0, s_shape = 1) +
  geom_point(data = pca_gen_daily, aes(x = PC_1, y = PC_2), col = 'purple') +
  ylim(-0.15, 0.25)

plot(gen_daily_plot)

pca_gen = rbind(pca_gen_yearly, pca_gen_quarterly, pca_gen_monthly, pca_gen_weekly, pca_gen_daily)


gen_all_plot = ggplot() +
  
  geom_point(data = pca$x, aes(x = PC1, y= PC2), col = 'green') +
  labs(title='All generated series') + labs(x = "PC1", y = NULL) + 
  geom_point(data = pca_gen, aes(x = PC_1, y = PC_2), col = 'purple')

plot(gen_all_plot)

grid.arrange(gen_yearly_plot, gen_quarterly_plot, gen_daily_plot, gen_all_plot, nrow = 2)


