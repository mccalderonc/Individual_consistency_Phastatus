library(fpc)
library(dbscan)
library(tidyverse)

load(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/data/bocas_foragingnoisland.RData")

######### February data ###########
# filter February data
foraging_bocas_feb <- bats.cave_clean %>%
  filter(date>="2021-01-01"& date<="2022-02-28" & tag_local_identifier!="0C506E35_G")

# split February data by ID
foraging_bocasfeb_ls <- split(foraging_bocas_feb, foraging_bocas_feb$ID)

# find the appropiate eps to calculate cluster using dbscan
lapply(foraging_bocasfeb_ls, function(x){
  dbscan::kNNdistplot(as.matrix(x[,c("x","y")]), k =  3, minPts=5)
  abline(h = 50, lty = 2)
})

# set seed
set.seed(123)

# compute DBSCAN using fpc package
db <- fpc::dbscan(as.matrix(foraging_bocasfeb_ls[[2]][,c("x","y")]), eps = 15, MinPts = 5, method = "hybrid")# Plot DBSCAN results

# plot results of dbscan
plot(db, foraging_bocasfeb_ls[[2]][,c("x","y")], main = "DBSCAN", frame = FALSE)

# compute DBSCAN using fpc package for multiple individuals
db_feb <- lapply(foraging_bocasfeb_ls, function(id){
  db <- fpc::dbscan(as.matrix(id[,c("x","y")]), eps = 50, MinPts = 3, method = "hybrid")
  return(db)
})

# plot cluster of dbscane using lat and long in UTM
pdf(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/results/clustering/clusterdbscn_feb.pdf")
lapply(1:length(db_feb), function(i){
  idname <- names(db_feb[i])
  plot(db_feb[[idname]], foraging_bocasfeb_ls[[idname]][,c("x","y")], main = idname, frame = FALSE)
})
dev.off()

# plot cluster of dbscan using different function
library("factoextra")
pdf(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/results/clustering/clusterplot_feb.pdf")
lapply(1:length(db_feb), function(i){
  idname <- names(db_feb[i])
  fviz_cluster(db_feb[[idname]], foraging_bocasfeb_ls[[idname]][,c("x","y")], geom = "point", main = idname )
})
dev.off()

# include clustering column to the data frame
foraging_bocasfeb_ls <- lapply(1:length(db_feb), function(i){
  idname <- names(db_feb[i])
  foraging_bocasfeb_ls[[idname]]$cluster <- db_feb[[idname]]$cluster
  return(foraging_bocasfeb_ls[[idname]])
})


names(foraging_bocasfeb_ls) <-  names(db_feb)

mapview::mapView(foraging_bocasfeb_ls[[49]], xcol="location_long", ycol="location_lat", zcol="cluster", legend=F, crs="EPSG:4326")

#check 15
#check 19
#check 34
#check 38
#check 44
#check 49
#check 52 2E500235_G_2022-02-03
#check 54 2E500235_G_2022-02-05
#check 59
#check 74
#check 88
#foraging_bocasfeb_ls[[96]]

foraging_bocasfeb_ls <- lapply(foraging_bocasfeb_ls, function(x){
  data <-  x[x$cluster!=0,]
  return(data)
})

########## March data #######

# filter only march data
foraging_bocas_march <- bats.cave_clean %>%
  filter(date>="2022-02-28" & date <="2022-03-31" & year_cave== "2022_ajcave")

# create a list with splitting by ID
foraging_bocasmarch_ls <- split(foraging_bocas_march, foraging_bocas_march$ID)

# filter data with complete commutes
# foraging_bocasmarch_ls <- foraging_bocasmarch_ls[names(foraging_bocasmarch_ls) %in% c("PH_TS_011_2022-03-08","PH_TS_011_2022-03-12", "PH_TS_011_2022-03-15", "PH_TS_011_2022-03-17", "PH_TS_014_2022-03-07", "PH_TS_014_2022-03-08", "PH_TS_014_2022-03-09", "PH_TS_014_2022-03-10", "PH_TS_016_2022-03-09","PH_TS_016_2022-03-10", "PH_TS_074_2022-03-08", "PH_TS_074_2022-03-10", "PH_TS_074_2022-03-12", "PH_TS_079_2022-03-07",  "PH_TS_079_2022-03-16", "PH_TS_080_2022-03-09", "PH_TS_080_2022-03-10", "PH_TS_080_2022-03-11", "PH_TS_080_2022-03-14", "PH_TS_080_2022-03-15", "PH_TS_080_2022-03-16", "PH_TS_080_2022-03-17", "PH_TS_080_2022-03-18", "PH_TS_080_2022-03-19", "PH_TS_080_2022-03-20", "PH_TS_080_2022-03-21","PH_TS_100_2022-03-07", "PH_TS_100_2022-03-10", "PH_TS_100_2022-03-11", "PH_TS_100_2022-03-16", "PH_TS_100_2022-03-17", "PH_TS_100_2022-03-18", "PH_TS_112_2022-03-08", "PH_TS_112_2022-03-11", "PH_TS_120_2022-03-08", "PH_TS_120_2022-03-11") == TRUE]


foraging_bocasmarch_ls <- foraging_bocasmarch_ls[names(foraging_bocasmarch_ls) %in%  c("PH_TS_011_2022-03-07", "PH_TS_011_2022-03-08", "PH_TS_011_2022-03-09", "PH_TS_011_2022-03-10", "PH_TS_011_2022-03-11", "PH_TS_011_2022-03-12", "PH_TS_011_2022-03-13", "PH_TS_011_2022-03-14", "PH_TS_011_2022-03-15", "PH_TS_011_2022-03-16", "PH_TS_011_2022-03-17", "PH_TS_014_2022-03-07", "PH_TS_014_2022-03-08", "PH_TS_014_2022-03-09", "PH_TS_014_2022-03-10", "PH_TS_014_2022-03-11", "PH_TS_014_2022-03-12", "PH_TS_014_2022-03-13", "PH_TS_014_2022-03-14", "PH_TS_016_2022-03-09", "PH_TS_016_2022-03-10", "PH_TS_016_2022-03-11", "PH_TS_016_2022-03-16", "PH_TS_016_2022-03-17", "PH_TS_018_2022-03-08", "PH_TS_018_2022-03-09", "PH_TS_018_2022-03-10", "PH_TS_018_2022-03-11", "PH_TS_074_2022-03-08", "PH_TS_074_2022-03-09", "PH_TS_074_2022-03-10", "PH_TS_074_2022-03-11", "PH_TS_074_2022-03-12", "PH_TS_079_2022-03-10", "PH_TS_079_2022-03-11", "PH_TS_079_2022-03-12", "PH_TS_079_2022-03-13", "PH_TS_079_2022-03-14", "PH_TS_079_2022-03-15", "PH_TS_079_2022-03-18", "PH_TS_080_2022-03-08", "PH_TS_080_2022-03-09", "PH_TS_080_2022-03-10", "PH_TS_080_2022-03-11", "PH_TS_080_2022-03-12", "PH_TS_080_2022-03-14", "PH_TS_080_2022-03-16", "PH_TS_080_2022-03-17", "PH_TS_080_2022-03-19", "PH_TS_080_2022-03-20", "PH_TS_080_2022-03-21",  "PH_TS_083_2022-03-08", "PH_TS_083_2022-03-09", "PH_TS_083_2022-03-10", "PH_TS_083_2022-03-11", "PH_TS_083_2022-03-13", "PH_TS_098_2022-03-07", "PH_TS_098_2022-03-08", "PH_TS_098_2022-03-09",  "PH_TS_112_2022-03-09", "PH_TS_112_2022-03-11", "PH_TS_112_2022-03-12", "PH_TS_113_2022-03-08", "PH_TS_113_2022-03-10", "PH_TS_113_2022-03-12", "PH_TS_120_2022-03-08", "PH_TS_120_2022-03-09", "PH_TS_121_2022-03-08", "PH_TS_121_2022-03-10", "PH_TS_121_2022-03-11", "PH_TS_121_2022-03-14") == TRUE]

#removed this ids
#PH_TS_049 PH_TS_052 PH_TS_056 PH_TS_062 PH_TS_072 PH_TS_096


# find the appropiate eps to calculate cluster using dbscan
lapply(foraging_bocasmarch_ls, function(x){
  dbscan::kNNdistplot(as.matrix(x[,c("x","y")]), k =  3)
  abline(h = 50, lty = 2)
})

set.seed(123)

# compute DBSCAN using fpc package for multiple individuals
db_march <- lapply(foraging_bocasmarch_ls, function(id){
  db <- fpc::dbscan(as.matrix(id[,c("x","y")]), eps = 50, MinPts = 3, method = "hybrid")
  return(db)
})

# plot cluster of dbscan using lat and long in UTM
pdf(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/results/clustering/clusterdbscn_march.pdf")
lapply(1:length(db_march), function(i){
  idname <- names(db_march[i])
  plot(db_march[[idname]], foraging_bocasmarch_ls[[idname]][,c("x","y")], main = idname, frame = FALSE)
})
dev.off()

# plot cluster of dbscan using different function
pdf(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/results/clustering/clusterplot_march.pdf")
lapply(1:length(db_march), function(i){
  idname <- names(db_march[i])
  fviz_cluster(db_march[[idname]], foraging_bocasmarch_ls[[idname]][,c("x","y")], geom = "point", main = idname )
})
dev.off()

#include clustering column to the data frame
foraging_bocasmarch_ls <- lapply(1:length(db_march), function(i){
  idname <- names(db_march[i])
  foraging_bocasmarch_ls[[idname]]$cluster <- db_march[[idname]]$cluster
  return(foraging_bocasmarch_ls[[idname]])
})


names(foraging_bocasmarch_ls) <-  names(db_march)

mapview::mapView(foraging_bocasmarch_ls[[35]], xcol="location_long", ycol="location_lat", zcol="cluster", legend=F, crs="EPSG:4326")

foraging_bocasmarch_ls <- lapply(foraging_bocasmarch_ls, function(x){
  data <-  x[x$cluster!=0,]
  return(data)
})

#foraging_bocasmarch_ls[[33]] 3 cluster maybe non

########## Wet season 2023 data #######

# filter only march data
foraging_bocas_wet <- bats.cave_clean %>%
  filter(date >="2022-03-31")

# create a list with splitting by ID
foraging_bocaswet_ls <- split(foraging_bocas_wet, foraging_bocas_wet$ID)

foraging_bocaswet_ls <- foraging_bocaswet_ls[names(foraging_bocaswet_ls) %in% c("PHYL11_2023-08-15", "PHYL11_2023-08-16", "PHYL11_2023-08-17", "PHYL11_2023-08-18", "PHYL16_2023-08-14", "PHYL16_2023-08-15",  "PHYL25_2023-08-14", "PHYL25_2023-08-15", "PHYL25_2023-08-16", "PHYL25_2023-08-17", "PHYL25_2023-08-18", "PHYL28_2023-08-17", "PHYL28_2023-08-15", "PHYL7_2023-08-16", "PHYL7_2023-08-17", "PHYL7_2023-08-18", "PHYL9_2023-08-14", "PHYL9_2023-08-15", "PHYL9_2023-08-16", "PHYL9_2023-08-17", "PHYL9_2023-08-18") == TRUE]


# find the appropiate eps to calculate cluster using dbscan
lapply(foraging_bocaswet_ls, function(x){
  dbscan::kNNdistplot(as.matrix(x[,c("x","y")]), k =  3)
  abline(h = 50, lty = 2)
})

set.seed(123)

# compute DBSCAN using fpc package for multiple individuals
db_wet <- lapply(foraging_bocaswet_ls, function(id){
  db <- fpc::dbscan(as.matrix(id[,c("x","y")]), eps = 50, MinPts = 3, method = "hybrid")
  return(db)
})

# plot cluster of dbscane using lat and long in UTM
pdf(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/results/clustering/clusterdbscn_wet.pdf")
lapply(1:length(db_wet), function(i){
  idname <- names(db_wet[i])
  plot(db_wet[[idname]], foraging_bocaswet_ls[[idname]][,c("x","y")], main = idname, frame = FALSE)
})
dev.off()

# plot cluster of dbscan using different function
pdf(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/results/clustering/clusterplot_wet.pdf")
lapply(1:length(db_wet), function(i){
  idname <- names(db_wet[i])
  fviz_cluster(db_wet[[idname]], foraging_bocaswet_ls[[idname]][,c("x","y")], geom = "point", main = idname)
})
dev.off()

# include clustering column to the data frame
foraging_bocaswet_ls <- lapply(1:length(db_wet), function(i){
  idname <- names(db_wet[i])
  foraging_bocaswet_ls[[idname]]$cluster <- db_wet[[idname]]$cluster
  return(foraging_bocaswet_ls[[idname]])
})


names(foraging_bocaswet_ls) <-  names(db_wet)

#foraging_bocaswet_ls <- foraging_bocaswet_ls[c(-15, -21)]

mapview::mapView(foraging_bocaswet_ls[[19]], xcol="location_long", ycol="location_lat", zcol="cluster", legend=F, crs="EPSG:4326")

# remove cluster which are zero
foraging_bocaswet_ls <- lapply(foraging_bocaswet_ls, function(x){
  data <-  x[x$cluster!=0,]
  return(data)
})

#foraging_bocaswet_ls[[6]] second cluster belongs to the 1st
#foraging_bocaswet_ls[[12]] check
#remove foraging_bocaswet_ls[[15]]
#remove foraging_bocaswet_ls[[21]]

############## Merge clustering lists ###########

# convert to dataframe
cluster_feb <- do.call(rbind,foraging_bocasfeb_ls)
cluster_march <- do.call(rbind,foraging_bocasmarch_ls)
cluster_wet <- do.call(rbind,foraging_bocaswet_ls)

# merge dataframes
cluster_all <- rbind(cluster_feb, cluster_march, cluster_wet)
cluster_all$year_cave_f<- factor(cluster_all$year_cave, levels=c("2022_lagruta_Feb", "2022_ajcave",    "2023_ajcave"), labels=c("dry 2022-1", "dry 2022-2",  "wet 2023"))

write.csv(cluster_all, file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/data/clusters_all.csv")

summary_clusters <- cluster_all %>%
  group_by(year_cave_f, tag_local_identifier, date)%>%
  dplyr::summarise(mean_cluster=mean(cluster), sd_cluster=sd(cluster))

write.csv(summary_clusters, file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/data/summary_clusters.csv")

summary_clusters_days <- summary_clusters %>%
  group_by(tag_local_identifier)%>%
  dplyr::summarise(mean_cluster=mean(mean_cluster), count = n())


# plot cluster
cluster_plot <- ggplot(aes(x= year_cave_f,y=cluster, fill=year_cave_f), data=cluster_all) +
  geom_boxplot(alpha = 0.5) +
  #geom_point(position = position_jitter(seed = 1, width = 0.2)) +
  scale_fill_manual(values=c("#756BB1", "#21918c","#F0E442"))+
  #geom_jitter(aes(y=cluster), position = position_jitter(width = 0.2, height = 0.5))+
  stat_summary(fun = mean, color = "black") +
  stat_summary(
    fun.min = function(x) mean(x) - sd(x), 
    fun.max = function(x) mean(x) + sd(x), 
    geom = "errorbar",
    color = "black",
    width = .3
  )+
  ylab("No. of foraging \n patches")+
  xlab("Period")+
  guides(fill="none")+
  theme_classic()+
  theme(legend.position = "none", axis.title = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20), axis.text.y = element_text(size=14), axis.text.x = element_text(size=15))

cluster_plot 

ggsave(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/results/clustering/foraging_patches_all.pdf")

### Figure of overlap and clusters
(within_between_all / cluster_plot) + plot_layout(axes = "collect_x") + plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = 20)  # Customize tag size and appearance
  )

ggsave(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/figures/overlap_patches.pdf", width = 10, height = 9)


cluster_mean <- cluster_all%>%
  group_by(year_cave_f, ID)%>%
  dplyr::summarise(no_cluster=n_distinct(cluster), meanpatches=mean(cluster), sdpatches=sd(cluster))
  

# plot cluster
cluster_id <- ggplot(aes(x=mean_cluster,y=tag_local_identifier, color=year_cave_f), data=summary_clusters) +
  #geom_boxplot(alpha = 0.5) +
  geom_point(aes(alpha=year_cave_f)) +
  scale_color_manual(values=c("#756BB1", "#21918c","#F0E442"), name="Period", labels=c("Dry 2022-1", "Dry 2022-2","Wet 2023"))+
  scale_alpha_manual(values = c(0.3, 0.3,0.3))+
  stat_summary(aes(color=year_cave_f),fun = mean) +
  stat_summary(aes(color=year_cave_f),
    fun.min = function(x) mean(x) - sd(x), 
    fun.max = function(x) mean(x) + sd(x), 
    geom = "errorbar",
    width = .3
  )+
  ylab("Individuals")+
  xlab("No. of foraging patches")+
  theme_classic()+
  guides(alpha="none")+
  theme(legend.position = "bottom", axis.title = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20), axis.text.y = element_text(size=14), axis.text.x = element_text(size=14))

cluster_id


# overlap and foraging patches
library(patchwork)
ind_overlap + cluster_id + plot_layout(axes = "collect_y", guides = "collect") +plot_annotation(tag_levels = "A") &theme(legend.position = "bottom", legend.text = element_text(size=14), legend.title = element_text(size = 16), plot.tag = element_text(size = 18))

ggsave(file="~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/results/clustering/foraging_patches_byid.pdf", width = 10, height = 10)

### check all cluster by id and day
lapply(split(cluster_all, cluster_all$tag_local_identifier), function(x){
  ggplot(aes(x= location_long,y=location_lat, color=as.factor(date), shape=as.factor(cluster)), data=x) +
    geom_point()+
    ggtitle(unique(x$tag_local_identifier))
})

  
#### Test if the number of foraging patches change between periods

# fit a Poisson mixed model
model <- glm(no_cluster ~ year_cave_f , 
               data = cluster_mean, 
               family = "poisson")

# summarize the model
summary(model)

plot(model)

# calculate residual deviance and degrees of freedom
deviance_value <- sum(residuals(model, type = "pearson")^2)
df_residual <- df.residual(model)

# dispersion ratio
dispersion_ratio <- deviance_value / df_residual
dispersion_ratio

library(MASS)

# fit a Negative Binomial mixed model
model_nb <- glmer.nb(no_cluster ~ year_cave_f + (1 | ID), 
                     data = cluster_mean)
car::Anova(model_nb)

plot(model_nb)

# post-hoc pairwise comparisons
emmeans(model_nb, pairwise ~ year_cave_f)
