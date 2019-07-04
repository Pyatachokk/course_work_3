
library(ggplot2)
library(ggcorrplot)


# Loading normed features, centered features and parameters of standartization
norm_features = read.csv('train_features/train_normed.csv', row.names = 'X')
centered_features = read.csv('train_features/train_centered.csv', row.names = 'X')
means = read.csv('train_features/means.csv', row.names = 'X')

norm_features$nperiods = NULL
centered_features$nperiods = NULL


# Separating labels from dataset
labels = norm_features$label
norm_features$label = NULL
centered_features$label = NULL
# write.csv(norm_features, 'train_normed.csv')


# Correlation matrix

corr <- round(cor(norm_features), 1)

# Plotting correlation matrix
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Коррелограмма признаков", 
           ggtheme=theme_bw)

plot(hist(norm_features$max_kl))



#Creating custom umap settings instead default
custom.settings = umap.defaults
custom.settings$n_neighbors = 20
custom.settings$min_dist = 0.5

# Generating 2-dimensional embeddings
embeddings = umap(norm_features, method = 'naive', config = custom.settings)


embed = data.frame(embeddings$layout)

# Plotting embeddings
gg <- ggplot(embed, aes(x=X1, y=X2)) + 
  geom_point(aes(col=labels))  + labs(title = 'UMAP embeddings') 


plot(gg)



# Performing PCA
pca = prcomp(norm_features, center = T, scale. = F)
pca$x = data.frame(pca$x)

pca_yearly = data.frame(pca$x) %>% slice(1: 1999)
pca_quarterly = data.frame(pca$x) %>% slice(2000: 3999)
pca_monthly = data.frame(pca$x) %>% slice(4000: 5999)
pca_weekly = data.frame(pca$x) %>% slice(6000: 6285)
pca_daily = data.frame(pca$x) %>% slice(6286: 8285)
pca_hourly = data.frame(pca$x) %>% slice(8286:8530)

# Plotting biplot, which shows strength of features
gg_1 <- ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = labels, varname.size = 5)  +
  scale_color_discrete(name = '') + 
  theme(legend.direction = 'horizontal', legend.position = 'top')  + labs(title = 'Linear PCA of normed features') 
print(gg_1)


# Plotting strength of features through normed features


g_1 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$x_acf1), show.legend = T ) +
  labs(title="First-order ACF") +
  scale_color_gradient(low="blue", high="red") + labs(x = NULL, y ='PC2')
g_1$labels$colour <- "Value"

g_2 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$x_pacf5), show.legend = T ) +
  labs(title="5-th order PACF") +
  scale_color_gradient(low="blue", high="red") + labs(x = NULL, y = NULL)
g_2$labels$colour <- "Value"

g_3 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$entropy ), show.legend = T) +
  labs(title="Entropy") +
  scale_color_gradient(low="blue", high="red")
g_3$labels$colour <- "Value"

g_4 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$lumpiness ), show.legend = T ) +
  labs(title="Lumpiness") +
  scale_color_gradient(low="blue", high="red") + labs(x = "PC1", y = NULL)
g_4$labels$colour <- "Value"

g_5 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$stability ), show.legend = T ) +
  labs(title="Stability") +
  scale_color_gradient(low="blue", high="red")+ labs(x = NULL, y ='PC2')
g_5$labels$colour <- "Value"

g_6 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$crossing_points ), show.legend = T ) +
  labs(title="Crossing points") +
  scale_color_gradient(low="blue", high="red") + labs(x = NULL, y = NULL)
g_6$labels$colour <- "Value"

g_7 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$hurst ), show.legend = T ) +
  labs(title="Hurst") +
  scale_color_gradient(low="blue", high="red") 
g_7$labels$colour <- "Value"

g_8 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$unitroot_kpss ), show.legend = T ) +
  labs(title="Unitroot KPSS") +
  scale_color_gradient(low="blue", high="red") + labs(x = "PC1", y = NULL)

g_8$labels$colour <- "Value"

g_9 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$nonlinearity ), show.legend = T ) +
  labs(title="Nonlinearity") +
  scale_color_gradient(low="blue", high="red")+ labs(x = NULL, y ='PC2')
g_9$labels$colour <- "Value"

g_10 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$kurtosis), show.legend = T ) +
  labs(title="Kurtosis") +
  scale_color_gradient(low="blue", high="red") + labs(x = NULL, y = NULL)
g_10$labels$colour <- "Value"

g_11 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$skewness ), show.legend = T ) +
  labs(title="Skewness") +
  scale_color_gradient(low="blue", high="red")
g_11$labels$colour <- "Value"

g_12 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$chaos ), show.legend = T ) +
  labs(title="Chaos") +
  scale_color_gradient(low="blue", high="red") + labs(x = "PC1", y = NULL)
g_12$labels$colour <- "Value"


g_13 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$lshift), show.legend = T ) +
  labs(title="Level shift") +
  scale_color_gradient(low="blue", high="red") + labs(x = NULL, y ='PC2')
g_13$labels$colour <- "Value"

g_14 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$fspots), show.legend = T ) +
  labs(title="Flat spots") +
  scale_color_gradient(low="blue", high="red") + labs(x = NULL, y = NULL)
g_14$labels$colour <- "Value"

g_15 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$max_kl), show.legend = T ) +
  labs(title="Maximum KL-divergence") +
  scale_color_gradient(low="blue", high="red")
g_15$labels$colour <- "Value"

g_16 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$extreme_dist_shape), show.legend = T ) +
  labs(title="Extreme distribution shape parameter") +
  scale_color_gradient(low="blue", high="red") + labs(x = "PC1", y = NULL)
g_16$labels$colour <- "Value"


g_17 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$seasonal_strength), show.legend = T) +
  labs(title="Seasonality strength") +
  scale_color_gradient(low="blue", high="red") + labs(x = NULL, y ='PC2')
g_17$labels$colour <- "Value"

g_18 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$trend), show.legend = T ) +
  labs(title="Trend") +
  scale_color_gradient(low="blue", high="red") + labs(x = NULL, y = NULL)
g_18$labels$colour <- "Value"

g_19 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$linearity), show.legend = T ) +
  labs(title="Linearity") +
  scale_color_gradient(low="blue", high="red")
g_19$labels$colour <- "Value"

g_20 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$spike), show.legend = T ) +
  labs(title="Spike") +
  scale_color_gradient(low="blue", high="red") + labs(x = "PC1", y = NULL)
g_20$labels$colour <- "Value"

g_21 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$curvature), show.legend = T ) +
  labs(title="Curvature") +
  scale_color_gradient(low="blue", high="red")+ labs(x = NULL, y ='PC2')
g_21$labels$colour <- "Value"

g_22 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$diff1_acf1), show.legend = T ) +
  labs(title="First difference ACF") +
  scale_color_gradient(low="blue", high="red") + labs(x = NULL, y = NULL)
g_22$labels$colour <- "Value"


g_23 = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col=norm_features$diff2_acf1), show.legend = T ) +
  labs(title="Second difference ACF") +
  scale_color_gradient(low="blue", high="red")
g_23$labels$colour <- "Value"


# Creating a grid of four plots

grid.arrange(g_8, g_12, g_3, g_19, ncol = 2, heights = c(30, 30, 1, 1))

grid.arrange(g_1, g_2, g_3, g_4, ncol = 2, heights = c(30, 30, 1, 1))

grid.arrange(g_5, g_6, g_7, g_8, ncol = 2, heights = c(30, 30, 1, 1))


grid.arrange(g_9, g_10, g_11, g_12, ncol = 2, heights = c(30, 30, 1, 1))


grid.arrange(g_13, g_14, g_15, g_16, ncol = 2, heights = c(30, 30, 1, 1))

grid.arrange(g_17, g_18, g_19, g_20, ncol = 2, heights = c(30, 30, 1, 1))


grid.arrange(g_21, g_22, g_23, ncol = 2, heights = c(30, 30, 1, 1))



# Simple PCA plot vith periodicity labels
g_prob = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col = labels), show.legend = T ) +
  labs(title='Time series clusters') + labs(x = "PC1", y = NULL) + 
  xlim(-1, 1.5) + 
  ylim(-1.5, 0.75)

plot(g_prob)

# Encircling clusters


g_pca = ggplot(data.frame(pca$x), aes(x=PC1, y=PC2)) + 
  geom_point(aes(col = labels), show.legend = T ) +
  labs(title='Time series clusters') + labs(x = "PC1", y = NULL) +
  geom_encircle(aes(x=PC1, y=PC2,group = labels, col = labels, fill = labels), expand = 0, spread = 0.001, s_shape = 1, alpha = 0.2)


plot(g_pca)
