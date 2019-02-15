## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE
)


## ------------------------------------------------------------------------
library(tSpace)
data("ts")


## ----fig.height=4, fig.width=8-------------------------------------------

visualization <- ts$ts_file
library(ggplot2)

ggplot(visualization, aes(tPC1, tPC2, color = Cell))+
  geom_point()+
  ggtitle('Visualization of the tSpace analysis of T cell development in tPC1 & tPC2')+
  scale_color_manual(values =  c('gray85', 'red', 'orange', 'blue', 'limegreen', 'skyblue', '#88fcd1', '#ee00a4', 'purple', 'black', 'pink', 'gold', 'firebrick', 'green', 'slateblue'))+
  theme_classic()

ggplot(visualization, aes(tPC1, tPC3, color = Cell))+
  geom_point()+
   ggtitle('Visualization of the tSpace analysis of T cell development in tPC1 & tPC3')+
  scale_color_manual(values =  c('gray85', 'red', 'orange', 'blue', 'limegreen', 'skyblue', '#88fcd1', '#ee00a4', 'purple', 'black', 'pink', 'gold', 'firebrick', 'green', 'slateblue'))+
  theme_classic()

## ----fig.height=6, fig.width=8-------------------------------------------
library(plotly)

p3d <- plot_ly(visualization, x = visualization$tPC1, y = visualization$tPC2, z = visualization$tPC3, color = visualization$Cell, colors = c('gray85', 'red', 'orange', 'blue', 'limegreen', 'skyblue', '#88fcd1', '#ee00a4', 'purple', 'black', 'pink', 'gold', 'firebrick', 'green'), marker = list(size = I(4)), type = 'scatter3d', text = ~paste("Pop: ", visualization$Cell, "<br>Index: ", visualization$Index) ) %>%  
      layout(paper_bgcolor='transparent')

p3d

## ----fig.height=4, fig.width=8-------------------------------------------
ggplot(visualization, aes(tPC1, tPC3, color = Cell))+
  geom_point()+
  scale_color_manual(values =  c('gray85', 'red', 'orange', 'blue', 'limegreen', 'skyblue', '#88fcd1', '#ee00a4', 'purple', 'black', 'pink', 'gold', 'firebrick', 'green', 'slateblue'))+
  ggtitle('Showing the filtering tresholds for isolation of DN3 population')+
  geom_vline(xintercept = 0.01)+
  geom_hline(yintercept = 0.027)+
  theme_classic()

## ------------------------------------------------------------------------

dn3.trajectories <- ts$tspace_matrix[,which(colnames(ts$tspace_matrix) %in% paste0('T_', visualization[which(visualization$tPC3 > 0.027 & visualization$tPC1 < 0.01), 'Index']))]

## ----fig.height=4, fig.width=8-------------------------------------------

ggplot(visualization, aes(tPC1, tPC3, color = dn3.trajectories[,1]))+
  geom_point()+
  ggtitle('Heatmap of distances from trajectory 1')+
  scale_color_gradientn(colours = c('magenta', 'gold', 'black'))+
  theme_classic()

ggplot(visualization, aes(tPC1, tPC3, color = dn3.trajectories[,2]))+
  geom_point()+
  ggtitle('Heatmap of distances from trajectory 2')+
  scale_color_gradientn(colours = c('magenta', 'gold', 'black'))+
  theme_classic()

ggplot(visualization, aes(tPC1, tPC3, color = dn3.trajectories[,3]))+
  geom_point()+
  ggtitle('Heatmap of distances from trajectory 3')+
  scale_color_gradientn(colours = c('magenta', 'gold', 'black'))+
  theme_classic()


## ------------------------------------------------------------------------

visualization$trajectory_dist <- rowMeans(dn3.trajectories)

## ----fig.height=4, fig.width=8-------------------------------------------
ggplot(visualization, aes(tPC1, tPC3, color = Cell))+
  geom_point()+
  scale_color_manual(values =  c('gray85', 'red', 'orange', 'blue', 'limegreen', 'skyblue', '#88fcd1', '#ee00a4', 'purple', 'black', 'pink', 'gold', 'firebrick', 'green'))+
  ggtitle('Showing the filtering tresholds for isolation of DN3 to CD4 branch')+
  geom_vline(xintercept = -0.001)+
  geom_hline(yintercept = -0.008)+
  theme_classic()

t.dn3.cd4 <- visualization[which(visualization$tPC1 > -0.001 & visualization$tPC3 > -0.008), ]

ggplot(visualization, aes(tPC1, tPC3, color = 'Rest'))+
  geom_point()+
  ggtitle('Examination of the isolation of DN3 to CD4 branch')+
  geom_point(data = t.dn3.cd4, mapping = aes(tPC1, tPC3, color = 'Isolated trajectory'))+
  theme_classic()


## ---- warning = FALSE, fig.height=4, fig.width=4-------------------------

smooth.df <- bin.trajectory(x = t.dn3.cd4[,22:34], trajectory = t.dn3.cd4$trajectory_dist, n = 250, trim=T, stat = 'median')

clean.df <- smooth.df[!is.na(smooth.df[,1]),]

heatmap(as.matrix(t(clean.df[,1:12])), Rowv = NA, Colv = NA, scale = 'none', col = cm.colors(12))


## ----fig.height=4, fig.width=8-------------------------------------------
library(umap)


umap.conf <- umap.defaults
umap.conf$n_neighbors <- 7
umap.conf$metric <- 'pearson'
umap.conf$min_dist <- 0.3

set.seed(1111)

umap.ts <- umap(ts$tspace_matrix, config = umap.conf)

visualization <- cbind(visualization, umap.ts$layout)
colnames(visualization)[36:37] <- c('umap1', 'umap2')


ggplot(visualization, aes(umap1, umap2, color = Cell))+
  geom_point()+
  scale_color_manual(values =  c('gray85', 'red', 'orange', 'blue', 'limegreen', 'skyblue', '#88fcd1', '#ee00a4', 'purple', 'black', 'pink', 'gold', 'firebrick', 'green'))+
  theme_classic()



