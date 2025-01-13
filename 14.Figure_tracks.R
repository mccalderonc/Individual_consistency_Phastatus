library(ggplot2)
library(ggnewscale) 
library(ggspatial)
library(magick)
library(ggmap)
library(ggsn)
library(patchwork) 
library(gridExtra)
library(tidyverse)
library(lubridate)
library(dplyr)
library(scales)
library(ggh4x)

allpaths <- read.csv(file = "/Users/ccalderon/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/data/allpairwise_out_inbound_paths.csv")


bocas_filtered <- bocas %>%
  filter(tag_local_identifier!="PH_TS_004" , tag_local_identifier!="PH_TS_024", tag_local_identifier!="PH_TS_085" , tag_local_identifier!="PH_TS_103")%>%
  filter(year_cave=="2022_ajcave"|year_cave=="2022_lagruta_Feb"| year_cave=="2023_ajcave")

# plot using stamen map from Bocas
register_stadiamaps("") 

# create a bounding box
e <- make_bbox(location_long, location_lat, data = bocas_filtered)

# make a data frame with the locations of the colonies
colonies <- data.frame(colonies=c("lagruta", "ajcave"), location_lat=c( 9.396448,9.440312), location_long=c(-82.271541, -82.274955))

#order year-cave
bocas_filtered$year_cave_f <- factor(bocas_filtered$year_cave, levels = c("2022_lagruta_Feb","2022_ajcave","2023_ajcave"))

dat1 <- bocas_filtered %>% filter(year_cave_f == "2022_lagruta_Feb")
dat2 <- bocas_filtered %>% filter(year_cave_f == "2022_ajcave")
dat3 <- bocas_filtered %>% filter(year_cave_f == "2023_ajcave")


# Necessary to put RH% into the facet labels
period_names <- as_labeller(
  c(`2022_lagruta_Feb` = "Dry 2022-1", `2022_ajcave` = "Dry 2022-2",`2023_ajcave` = "Wet 2023"))
# plot
plot_map <- get_stadiamap(e, zoom = 12, maptype = "stamen_toner_lite") %>% ggmap()+
  coord_sf(crs = "+proj=lonlat", expand = FALSE)+
  #scale_x_continuous(breaks = c(-82.50, -82.40, -82.30, -82.35))+
  geom_path(data=dat1, aes(x=location_long, y=location_lat, col=tag_local_identifier, group=tag_local_identifier))+
  scale_colour_brewer(palette="Greens")+
  guides(shape="none", size="none", fill="none", color="none")+
  #scale_colour_gradientn(colors = c("#A6D854", high="#4DAF4A")) +
  #scale_color_manual(labels=c("Dry 2022-1"))+
  new_scale_color() +
  geom_path(data=dat2, aes(x=location_long, y=location_lat, col=tag_local_identifier, group=tag_local_identifier))+
  scale_colour_brewer(palette="Purples")+
  guides(shape="none", size="none", fill="none", color="none")+
  #scale_colour_gradientn(colors = c("#756BB1", "#54278F")) +
  #scale_color_manual(labels=c("Dry 2022-2"))+
  new_scale_color() +
  geom_path(data=dat3, aes(x=location_long, y=location_lat, col=tag_local_identifier, group=tag_local_identifier))+
  scale_colour_brewer(palette="Oranges")+
  #annotate(geom = "text", x = -82.2, y = 9.45, label = "N=34", color = "black", size = 6) +
  guides(shape="none", size="none", fill="none", color="none")+
  #scale_colour_gradientn(colors = c("#E6AB02", high="#D95F02")) +
  #scale_color_manual(labels=c("Wet 2023"))+
  #scale_color_manual(name="Period", labels=c("Dry 2022-1", "Dry 2022-2", "Wet 2023"))+#values=c("#4DAF4A", "#54278F","#E6AB02"),
  #scale_color_discrete(name="Period", labels=c("Dry 2022-1", "Dry 2022-2", "Wet 2023"))+
  new_scale_color() +
  geom_point(data=colonies, aes(x=location_long, y=location_lat, shape=colonies, size=5, fill=colonies, color=colonies))+
  scale_shape_manual(values = c( 23, 21))+
  scale_color_manual(values = c("black", "black"))+
  scale_fill_manual(values = alpha(c( "black","black"), c(0.6,0.6)))+
  annotation_scale(location = "bl", line_width = 1, height = unit(0.5, "cm"), pad_x= unit(0.5, "cm"), pad_y= unit(0.5, "cm"), text_cex = 2)+
  theme_classic()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        plot.title= element_text(size = 20),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        strip.text.x = element_text(size = 16, color="black"))+
  guides(shape="none", size="none", fill="none", color="none")+
  facet_wrap2(.~year_cave_f, nrow=2, ncol=2, labeller = period_names)+
  xlab("Longitude")+
  ylab("Latitude")

plot_map

ggsave(file = "~/ownCloud/PhDLife/P.hastatus/Thesis/Paper3/analysis/figures/alltracks.pdf", width = 15, height = 10)

