#===============================================================================================================#
#===============================================================================================================#
#   Spatiotemporal variability in climatic sensitivity of dominant temperate tree species in Central Europe  ####
#                                       Jiří Mašek, Jan Tumajer, Václav Treml
#                                               Graphic part of script
#===============================================================================================================#
#===============================================================================================================#

# Necessary packages

library(ggplot2) # plotting of figures
library(ggh4x) # facets for ggplot with all axes
library(cowplot) # grouping of figures
library(ggtext) # text labels to graphs
library(tidyverse)
library(sf)
library(sfheaders) # shapefile to data frame
library(RCzechia) # shapefile of Czechia
library(viridis) # color gradients
library(ggnewscale) # adding another color scale
library(ggpubr)

setwd("C:/Users/Jiri/Desktop/TDC_daily_cor/")

#==========================================================================================================#
#####                                   Fig. 1 (Map, clima and areals)                                 ####
#==========================================================================================================#

# Map
CWB_mean<-read.table("Data/Temporary_data/CWB_mean.txt", dec = ".", header = T)

# Loading of list of sites
Loc_list<-read.table("Data/Loc_list.txt", dec = ".", header = T)
Loc_list$TREE_TYPE<- ifelse(Loc_list$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

# Loading of Czechia polygon
CR_map  <-  republika()
CR_map_df<- sf_to_df(CR_map, fill = FALSE, unlist = NULL)

Map<- ggplot()+
  geom_tile(data = CWB_mean, aes(x=LON, y=LAT, fill=CWB))+
  scale_fill_viridis(option="magma", direction=-1, na.value=NA)+
  labs(fill = "CWB (mm)")+
  theme(legend.title=element_text(size=14))+
  geom_polygon(data = CR_map_df, aes(x=x, y=y), col = "black", linewidth = 1, fill = NA)+

  new_scale_fill()+
  geom_point(data = Loc_list, aes(x=LON, y=LAT, fill=SPECIES, shape=TREE_TYPE), size=4)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values = c("#004ca8", "#7ab5f5", "#38a800","#ffff00", "#f56767"))+
  
  scale_x_continuous(breaks = seq(12, 19, 1))+
  
  labs(x= "Longitude (°)", y= "Latitude (°)", shape = "")+
  guides(fill = "none")+
  theme(axis.ticks = element_line(color = "black"))+
  theme(strip.text  = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12, color="black"))+
  theme(axis.text.y = element_text(size = 12, color="black"))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(legend.text = element_text(size = 15))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.margin=margin(t=-5))+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))

# Clima
Clima_gradient<-read.table("Data/Temporary_data/Clima_gradient.txt", dec = ".", header = T)

Clima_graf<-ggplot()+geom_point(data= Clima_gradient, aes(x=TEMP_mean, y=PREC_mean, fill=SPECIES, shape=TREE_TYPE), size=4)+
  labs(x= "Mean annual temperature (°C)", y= "Mean annual precipitation (mm)")+
  scale_fill_manual(values = c("#004ca8", "#7ab5f5", "#38a800","#ffff00", "#f56767"))+
  scale_shape_manual(values=c(21, 24))+
  
  guides(fill = "none")+
  scale_x_continuous(breaks = seq(1, 9, 2))+
  theme(strip.text  = element_text(size = 16))+
  theme(axis.text.x = element_text(size = 16, color="black"))+
  theme(axis.text.y = element_text(size = 16, color="black"))+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(legend.text = element_text(size = 16))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))+
  theme(axis.ticks = element_line(color = "black"))+
  theme(legend.position="bottom", legend.margin=margin(r = 10))

# Areals
Path_shp<- list.files(path = "C:/Users/Jiri/Desktop/TDC_daily_cor/Data/Species_areals", full.names = T, pattern = ".shp$")

shp<- Path_shp %>% map(read_sf)
shp_full<- bind_rows(shp)
shp_full$SPECIES<- c("ABAL", "FASY", "PCAB", "PISY", "QUsp")
shp_full$TREE_TYPE<- ifelse(shp_full$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")
shp_full$TREE_TYPE_order<- factor(shp_full$TREE_TYPE, levels = c("Conifer", "Broadleaf"))

World<-map_data("world")

CR_map  <-  republika("low")
CR_map <- sf_to_df(CR_map) 

Areals<- ggplot()+
  geom_polygon(data = World, aes(x=long, y=lat, group=group), fill= "#696a6b")+
  geom_sf(data = shp_full, aes(fill=SPECIES), color=NA)+
  geom_polygon(data = CR_map, aes(x=x, y=y, group = polygon_id), col = "black", linewidth = 1, fill = NA)+
  coord_sf(xlim = c(-10, 30), y = c(30, 70))+
  scale_fill_manual(values = c("#004ca8", "#7ab5f5", "#38a800","#ffff00", "#f56767"))+
  facet_nested_wrap(~TREE_TYPE_order+SPECIES, axes = "all", nrow = 1)+
  
  
  labs(x= "Longitude (°)", y= "Latitude (°)")+
  theme(strip.text  = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 12, color="black"))+
  theme(axis.text.y = element_text(size = 12, color="black"))+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(legend.text = element_text(size = 12))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))+
  theme(axis.ticks = element_line(color = "black"))+
  theme(legend.position="none", legend.box="vertical", legend.margin=margin(r = 10))

upper_row<-plot_grid(Map, Clima_graf, rel_widths = c(1, 0.7), labels = c("A", "B"), label_size = 21)

plot_grid(upper_row, Areals, nrow = 2, rel_heights = c(1,0.8), labels = c("A","C"), label_size = 21)

ggsave("Graphs/Fig. 1 (Map, clima, areals).tiff", height = 300, width = 400, units = "mm", dpi = 300)

#==========================================================================================================#
#####                               Fig. 2 (Median and categorization)                                  ####
#==========================================================================================================#

Grid_median_all<-read.table("Data/Temporary_data/Grid_median.txt", dec = ".", header = T)
Grid_median<- Grid_median_all[,c(2:3)]
Grid_median[, c(3:12)]<- Grid_median_all %>% dplyr::select(contains("Var"))
Grid_median<- gather(Grid_median, "CODE", "COR", 3:12)
Grid_median$SPECIES<- lapply(strsplit(as.character(Grid_median$CODE), "\\_"), "[",2)
Grid_median$CLIM<- lapply(strsplit(as.character(Grid_median$CODE), "\\_"), "[",3)
Grid_median <- as.data.frame(lapply(Grid_median, unlist))
Grid_median$TREE_TYPE<- ifelse(Grid_median$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")
Grid_median<- na.omit(Grid_median)
Grid_median$CLIM2<- ifelse(Grid_median$CLIM=="TEMP", "Temperature", "Precipitation")

# Loading of list of sites
Loc_list<-read.table("Data/Loc_list.txt", dec = ".", header = T)
Loc_list$TREE_TYPE<- ifelse(Loc_list$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

Solstice_data<- aggregate(Grid_median$CWB, list(Grid_median$SPECIES), max)
Solstice_data$MIN<- aggregate(Grid_median$CWB, list(Grid_median$SPECIES), min)[,2]
colnames(Solstice_data)[c(1,2)]<- c("SPECIES", "MAX")
Solstice_data<- rbind(Solstice_data,Solstice_data)
Solstice_data[c(1:5), "CLIM"]<- "TEMP"
Solstice_data[c(6:10), "CLIM"]<- "PREC"
Solstice_data$TREE_TYPE<- ifelse(Solstice_data$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

break_positions <- c(2, 101, 200, 299, 366, 465, 564, 663)
labels <- c(1, 100, 200, 300, 1, 100, 200, 300)

Fig_2_A<- ggplot()+geom_tile(data=Grid_median, aes(x=DOY, y=CWB, fill=COR, alpha = COR>0.2 | COR < c(-0.2)))+
  geom_segment(data = Loc_list, aes(y=CWB, yend = CWB, x=0, xend = 5))+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=172, xend = 172), linetype = "dashed", linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=365, xend = 365), linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=537, xend = 537), linetype = "dashed", linewidth = 1)+
  
  facet_nested(TREE_TYPE~SPECIES~CLIM2, switch = "y", labeller = labeller(supp = supp_labs))+
  scale_fill_viridis(option="turbo", name = "Correlation")+
  labs(x= "Day of year", y= "Climatic water balance (mm)")+
  scale_alpha_discrete(range = c(0.5, 1))+
  scale_x_continuous(breaks = break_positions, labels = labels)+

  guides(alpha = "none")+
  theme(axis.ticks = element_line(color = "black"))+
  theme(strip.text  = element_text(size = 17))+
  theme(axis.text.x = element_text(size = 17, color="black"))+
  theme(axis.text.y = element_text(size = 17, color="black"))+
  theme(axis.title.x = element_text(size = 17))+
  theme(axis.title.y = element_text(size = 17))+
  theme(legend.text = element_text(size = 12))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "none")+
  #theme(legend.position = "right")+
  theme(legend.key.width= unit(8, 'mm'))+
  theme(legend.direction='vertical')+
  theme(strip.text.y = element_blank())+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))

# Legend extraction, it is necessary to switch in on in the figure
# Legend<- get_legend(Fig_2_A)
# as_ggplot(Legend)
# ggsave("Graphs/Accompanying_figures/Legend.tiff", height = 40, width = 20, units = "mm", dpi = 300)

Categorization<-read.table("Data/Temporary_data/Categorization.txt", dec = ".", header = T)
Categorization<- gather(Categorization, "SPECIES", "CAT", 2:6)
Categorization$DOY<- lapply(strsplit(as.character(Categorization$COOR), "\\_"), "[",1)
Categorization$CWB<- lapply(strsplit(as.character(Categorization$COOR), "\\_"), "[",2)
Categorization <- as.data.frame(lapply(Categorization, unlist))
Categorization$TREE_TYPE<- ifelse(Categorization$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")
Categorization$CLIMA_RESPONSE<- "Climate response category"
Categorization<- na.omit(Categorization)

Fig_2_B<- ggplot()+geom_tile(data = Categorization, aes(x=as.numeric(DOY), y=as.numeric(CWB), fill=CAT))+
  facet_nested(cols = vars(CLIMA_RESPONSE),rows = vars(TREE_TYPE, SPECIES))+
  scale_fill_manual(values = c("#1c1b18", "#38afb5", "#4fab38", "#ffcd45", "#8b0bb3","#f56767", "#f4fc05", "#004ca8", "#cbd0d4"))+
  geom_segment(data = Loc_list, aes(y=CWB, yend = CWB, x=0, xend = 5))+
  #geom_segment(data = Month_DOYs, aes(y=-15, yend = -2, x=DOY, xend = DOY),linetype="dashed")+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=172, xend = 172), linetype = "dashed", linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=365, xend = 365), linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=537, xend = 537), linetype = "dashed", linewidth = 1)+
  
  guides(alpha = "none")+ 
  labs(x= "Day of year", y= "")+
  scale_x_continuous(breaks = break_positions, labels = labels)+
  theme(axis.line.y = element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks = element_line(color = "black"))+
  theme(strip.text  = element_text(size = 17))+
  theme(axis.text.x = element_text(size = 17, color="black"))+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_text(size = 17))+
  theme(axis.title.y = element_text(size = 17))+
  theme(legend.text = element_text(size = 17))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "none")+
  theme(legend.key.width= unit(8, 'mm'))+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))

plot_grid(Fig_2_A, Fig_2_B, nrow = 1, rel_widths =  c(1.7, 1))
ggsave("Graphs/Accompanying_figures/Fig. 2 (Median and categorization).tiff", height = 500, width = 400, units = "mm", dpi = 300)

### Clima legend for Fig_2_B
Clima_square<- as.data.frame(matrix(ncol = 5, nrow = 9)); colnames(Clima_square)<- c("T_c", "P_c", "Temp","Prec", "Color")

Clima_square$T_c<- rep(c(1:3),3)
Clima_square$P_c<- rep(c(1:3), each=3)
Clima_square$Temp<- c("–", "0", "+", "–", "0", "+", "–", "0", "+")
Clima_square$Prec<- c("–", "–", "–", "0", "0", "0", "+", "+", "+")
Clima_square$Color<- c("#1c1b18", "#f4fc05", "#ffcd45", "#4fab38", "#cbd0d4","#f56767", "#38afb5", "#004ca8", "#8b0bb3")


ggplot(Clima_square)+geom_tile(aes(x = T_c, y = P_c, fill = Color))+
  scale_fill_identity() +
  geom_text(aes(T_c-0.1, P_c, label = Temp), size = 8, color = "red", fontface="bold")+
  geom_text(aes(T_c+0.1, P_c, label = Prec), size = 8, color = "blue", fontface="bold")+
  
  labs(x= "Temprature", y= "Precipitation")+
  theme(axis.ticks = element_line(color = "black"))+
  theme(strip.text  = element_text(size = 17))+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_text(size = 17))+
  theme(axis.title.y = element_text(size = 17))+
  theme(legend.text = element_text(size = 17))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "none", legend.margin=margin(t=-5))+
  theme(legend.key.width= unit(8, 'mm'))

ggsave("Graphs/Accompanying_figures/Clima_square.tiff", height = 100, width = 100, units = "mm", dpi = 300)

#==========================================================================================================#
#####                                 Fig. S1 (Species gradients)                                      ####
#==========================================================================================================#

# Fig S2, panel B
Loc_list<-read.table("Data/Loc_list.txt", dec = ".", header = T)
Loc_list$TREE_TYPE_order<- factor(Loc_list$TREE_TYPE, levels = c("Conifer", "Broadleaf"))

ggplot(Loc_list, aes(x=CWB, fill=SPECIES))+geom_histogram(colour = "black")+
  facet_nested_wrap(~TREE_TYPE_order+SPECIES, axes="all", nrow = 2)+
  scale_alpha_discrete(range = c(0.3, 1))+
  
  labs(x= "Climatic water balance (mm)", y= "Number of sites")+
  scale_fill_manual(values = c("#004ca8", "#7ab5f5", "#38a800","#ffff00", "#f56767"), name="Species")+
  theme(axis.ticks = element_line(color = "black"))+
  theme(strip.text  = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12, color="black"))+
  theme(axis.text.y = element_text(size = 12, color="black"))+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(legend.text = element_text(size = 12))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "none", legend.margin=margin(t=-5))+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))

ggsave("Graphs/Fig. S1 (Species gradients).tiff", height = 130, width = 230, units = "mm", dpi = 300)

#==========================================================================================================#
#####                                 Fig. S2 (Tree-ring chronologies)                                  ####
#==========================================================================================================#

Tree_data<-read.table("Data/Temporary_data/Tree_data.txt", dec = ".", header = T)

# Loading of list of sites
Loc_list<-read.table("Data/Loc_list.txt", dec = ".", header = T)
Loc_list$TREE_TYPE<- ifelse(Loc_list$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

Tree_data$YEAR<- as.numeric(rownames(Tree_data))
Tree_gg<- gather(Tree_data, "SITE", "VAL", 1:725)
Tree_gg<- merge(Tree_gg, Loc_list[,c(1,5,14,15)], by.x="SITE", by.y="SITE", all.x=T)

Tree_gg$TREE_TYPE_order<- factor(Tree_gg$TREE_TYPE, levels = c("Conifer", "Broadleaf"))

Tree_gg<-Tree_gg[!(Tree_gg$SITE %in% c("P800034PCAB","P000246PCAB", "P800036PCAB")),]

ggplot(Tree_gg, aes(x=YEAR, y=VAL, group=SITE, colour = CWB))+geom_line(linewidth = 0.2)+
  facet_nested_wrap(~TREE_TYPE_order+SPECIES, axes = "all", nrow = 2)+
  scale_color_viridis(option="magma", direction=-1, na.value=NA, limit = c(-18,76))+
  
  labs(x ="", y= "Tree-ring index", color = "CWB (mm)")+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(strip.text  = element_text(size = 12, color = "black"))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(panel.spacing = unit(1.5, "lines"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.title.y = element_text(size = 12, color = "black"))+
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 15))

ggsave("Graphs/Fig. S2 (Tree_rings).tiff", height = 200, width = 350, units = "mm", dpi = 300)

#==========================================================================================================#
#####                                   Fig. S3 (IDW interpolation)                                     ####
#==========================================================================================================#

Grid_IDW_all<-read.table("Data/Temporary_data/Grid_IDW.txt", dec = ".", header = T)
Grid_IDW<- Grid_IDW_all[,c(2:3)]
Grid_IDW[, c(3:12)]<- Grid_IDW_all %>% dplyr::select(contains("Var"))

Grid_IDW<- gather(Grid_IDW, "CODE", "COR", 3:12)
Grid_IDW$SPECIES<- lapply(strsplit(as.character(Grid_IDW$CODE), "\\_"), "[",2)
Grid_IDW$CLIM<- lapply(strsplit(as.character(Grid_IDW$CODE), "\\_"), "[",3)
Grid_IDW <- as.data.frame(lapply(Grid_IDW, unlist))
Grid_IDW$TREE_TYPE<- ifelse(Grid_IDW$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")
Grid_IDW<- na.omit(Grid_IDW)
Grid_IDW$CLIM2<- ifelse(Grid_IDW$CLIM=="TEMP", "Temperature", "Precipitation")

break_positions <- c(2, 101, 200, 299, 366, 465, 564, 663)
labels <- c(1, 100, 200, 300, 1, 100, 200, 300)

# Loading of list of sites
Loc_list<-read.table("Data/Loc_list.txt", dec = ".", header = T)
Loc_list$TREE_TYPE<- ifelse(Loc_list$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

Solstice_data<- aggregate(Grid_IDW$CWB, list(Grid_IDW$SPECIES), max)
Solstice_data$MIN<- aggregate(Grid_IDW$CWB, list(Grid_IDW$SPECIES), min)[,2]
colnames(Solstice_data)[c(1,2)]<- c("SPECIES", "MAX")
Solstice_data<- rbind(Solstice_data,Solstice_data)
Solstice_data[c(1:5), "CLIM"]<- "TEMP"
Solstice_data[c(6:10), "CLIM"]<- "PREC"
Solstice_data$TREE_TYPE<- ifelse(Solstice_data$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

ggplot()+geom_tile(data=Grid_IDW, aes(x=DOY, y=CWB, fill=COR, alpha = COR>0.2 | COR < c(-0.2)))+
  geom_segment(data = Loc_list, aes(y=CWB, yend = CWB, x=0, xend = 5))+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=172, xend = 172), linetype = "dashed", linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=365, xend = 365), linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=537, xend = 537), linetype = "dashed", linewidth = 1)+
  
  facet_nested(TREE_TYPE~SPECIES~CLIM2)+
  scale_fill_viridis(option="turbo", name = "Correlation")+
  labs(x= "Day of year", y= "Climatic water balance (mm)")+
  scale_alpha_discrete(range = c(0.5, 1))+
  
  scale_x_continuous(breaks = break_positions, labels = labels)+
  guides(alpha = "none")+
  theme(axis.ticks = element_line(color = "black"))+
  theme(strip.text  = element_text(size = 17))+
  theme(axis.text.x = element_text(size = 17, color="black"))+
  theme(axis.text.y = element_text(size = 17, color="black"))+
  theme(axis.title.x = element_text(size = 17))+
  theme(axis.title.y = element_text(size = 17))+
  theme(legend.text = element_text(size = 17))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.margin=margin(t=-5))+
  theme(legend.key.width= unit(12, 'mm'))+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))

ggsave("Graphs/Fig. S3 (IDW interpolation).tiff", height = 500, width = 350, units = "mm", dpi = 300)

#==========================================================================================================#
#####                                Fig. S4 (Short and long windows)                                   ####
#==========================================================================================================#

Grid_IDW_all<-read.table("Data/Temporary_data/Grid_IDW.txt", dec = ".", header = T)
Grid_IDW<- Grid_IDW_all[,c(2:3)]
Grid_IDW[, c(3:22)]<- Grid_IDW_all %>% dplyr::select(contains("Fix"))

Grid_IDW<- gather(Grid_IDW, "CODE", "COR", 3:22)
Grid_IDW$TYPE<- lapply(strsplit(as.character(Grid_IDW$CODE), "\\_"), "[",2)
Grid_IDW$SPECIES<- lapply(strsplit(as.character(Grid_IDW$CODE), "\\_"), "[",3)
Grid_IDW$CLIM<- lapply(strsplit(as.character(Grid_IDW$CODE), "\\_"), "[",4)
Grid_IDW <- as.data.frame(lapply(Grid_IDW, unlist))
Grid_IDW$TREE_TYPE<- ifelse(Grid_IDW$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")
Grid_IDW<- na.omit(Grid_IDW)
Grid_IDW$CLIM2<- ifelse(Grid_IDW$CLIM=="TEMP", "Temperature", "Precipitation")

# Loading of list of sites
Loc_list<-read.table("Data/Loc_list.txt", dec = ".", header = T)
Loc_list$TREE_TYPE<- ifelse(Loc_list$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

Solstice_data<- aggregate(Grid_IDW$CWB, list(Grid_IDW$SPECIES), max)
Solstice_data$MIN<- aggregate(Grid_IDW$CWB, list(Grid_IDW$SPECIES), min)[,2]
colnames(Solstice_data)[c(1,2)]<- c("SPECIES", "MAX")
Solstice_data<- rbind(Solstice_data,Solstice_data)
Solstice_data[c(1:5), "CLIM"]<- "TEMP"
Solstice_data[c(6:10), "CLIM"]<- "PREC"
Solstice_data$TREE_TYPE<- ifelse(Solstice_data$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

break_positions <- c(2, 101, 200, 299, 366, 465, 564, 663)
labels <- c(1, 100, 200, 300, 1, 100, 200, 300)

Short<- ggplot()+geom_tile(data=Grid_IDW[Grid_IDW$TYPE=="20",], aes(x=DOY, y=CWB, fill=COR, alpha = COR>0.2 | COR < c(-0.2)))+
  geom_segment(data = Loc_list, aes(y=CWB, yend = CWB, x=0, xend = 5))+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=172, xend = 172), linetype = "dashed", linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=365, xend = 365), linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=537, xend = 537), linetype = "dashed", linewidth = 1)+
  
  facet_nested(TREE_TYPE~SPECIES~CLIM2)+
  scale_fill_viridis(option="turbo", name = "Correlation")+
  labs(x= "Day of year", y= "Climatic water balance (mm)")+
  scale_alpha_discrete(range = c(0.5, 1))+
  scale_x_continuous(breaks = break_positions, labels = labels)+
  
  guides(alpha = "none")+
  theme(axis.ticks = element_line(color = "black"))+
  theme(strip.text  = element_text(size = 17))+
  theme(axis.text.x = element_text(size = 17, color="black"))+
  theme(axis.text.y = element_text(size = 17, color="black"))+
  theme(axis.title.x = element_text(size = 17))+
  theme(axis.title.y = element_text(size = 17))+
  theme(legend.text = element_text(size = 17))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.margin=margin(t=-5))+
  theme(legend.key.width= unit(12, 'mm'))+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))

Long<- ggplot()+geom_tile(data=Grid_IDW[Grid_IDW$TYPE=="90",], aes(x=DOY, y=CWB, fill=COR, alpha = COR>0.2 | COR < c(-0.2)))+
  geom_segment(data = Loc_list, aes(y=CWB, yend = CWB, x=0, xend = 5))+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=172, xend = 172), linetype = "dashed", linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=365, xend = 365), linewidth = 1)+
  geom_segment(data = Solstice_data, aes(y=MIN, yend = MAX, x=537, xend = 537), linetype = "dashed", linewidth = 1)+
  
  facet_nested(TREE_TYPE~SPECIES~CLIM2)+
  scale_fill_viridis(option="turbo", name = "Correlation")+
  labs(x= "Day of year", y= "Climatic water balance (mm)")+
  scale_alpha_discrete(range = c(0.5, 1))+
  scale_x_continuous(breaks = break_positions, labels = labels)+
  
  guides(alpha = "none")+
  theme(axis.ticks = element_line(color = "black"))+
  theme(strip.text  = element_text(size = 17))+
  theme(axis.text.x = element_text(size = 17, color="black"))+
  theme(axis.text.y = element_text(size = 17, color="black"))+
  theme(axis.title.x = element_text(size = 17))+
  theme(axis.title.y = element_text(size = 17))+
  theme(legend.text = element_text(size = 17))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.margin=margin(t=-5))+
  theme(legend.key.width= unit(12, 'mm'))+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))

plot_grid(Short, Long, ncol = 2, labels = c("A","B"), label_size = 21)
ggsave("Graphs/Fig. S4 (Short and long windows).tiff", height = 500, width = 500, units = "mm", dpi = 300)

#==========================================================================================================#
#####                                     Fig. S5 (Effect of window)                                     ####
#==========================================================================================================#

Daily_cor_PER<- read.table("Data/Temporary_data/Daily_cor_PER.txt", dec = ".", header = T)
Periods<- Daily_cor_PER[Daily_cor_PER$TYPE == "Var", c(1:6)]
Periods<- Periods %>% distinct(SITE, PER, .keep_all = TRUE) # deleting duplicated rows

Periods$TREE_TYPE<- ifelse(Periods$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")
Periods<- na.omit(Periods)
Periods$CODE<- paste(Periods$SPECIES, Periods$CLIM, sep="_")
Periods$CLIM2<- ifelse(Periods$CLIM=="TEMP", "Temperature", "Precipitation")

Levels<- as.data.frame(unique(Periods$CODE)); colnames(Levels)<- "CODE"

for (i in c(1:nrow(Levels))) {
  
  Set<- Periods[Periods$CODE==Levels[i, "CODE"],]
  
  model<- lm(Set$PER~Set$CWB)
  model_sum<- summary(model)
  Levels[i, "P_val"]<- model_sum$coefficients[2,4]
  Levels[i, "R2"]<- model_sum$r.squared
  Levels[i, "Slope"]<- model_sum$coefficients[2,1]
  
}

Levels$SPECIES<- lapply(strsplit(as.character(Levels$CODE), "\\_"), "[",1)
Levels$CLIM<- lapply(strsplit(as.character(Levels$CODE), "\\_"), "[",2)
Levels <- as.data.frame(lapply(Levels, unlist))
Levels$TREE_TYPE<- ifelse(Levels$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")
Levels$CLIM2<- ifelse(Levels$CLIM=="TEMP", "Temperature", "Precipitation")

Levels$x<- 55
Levels$y<- 60

ggplot(Periods, aes(x=PER, y=CWB, fill=SPECIES, shape=TREE_TYPE))+geom_point()+
  facet_nested(TREE_TYPE~SPECIES~CLIM2, axes="all")+geom_smooth(method = lm, aes(x=PER, y=CWB), color="black")+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values = c("#004ca8", "#7ab5f5", "#38a800","#ffff00", "#f56767"))+
  geom_richtext(data = Levels, mapping = aes(x = x, y = y, 
                label = paste0("p = ", sprintf("%0.3f",round(P_val, 3)), "; ", "R<sup>2</sup> = " , sprintf("%0.3f",round(R2, 3)), "; ", "slope = " , sprintf("%0.3f",round(Slope, 3)))), 
                label.color = NA, fill="#bdbebf", size=4)+
  labs(y= "Climatic water balance (mm)", x= "Window length (days)")+
  
  theme(axis.ticks = element_line(color = "black"))+
  theme(strip.text  = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12, color="black"))+
  theme(axis.text.y = element_text(size = 12, color="black"))+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(legend.text = element_text(size = 12))+
  theme(panel.background = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "none", legend.margin=margin(t=-5))+
  theme(legend.key = element_rect(colour = "transparent", fill = "white"))

ggsave("Graphs/Fig. S5 (Efect of window).tiff", height = 250, width = 200, units = "mm", dpi = 300)

#==========================================================================================================#
#####                                   Fig. S6 (Clima within year)                                     ####
#==========================================================================================================#

# Loading of list of sites
Loc_list<-read.table("Data/Loc_list.txt", dec = ".", header = T)
Loc_list$TREE_TYPE<- ifelse(Loc_list$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

Temp_data<-read.table("Data/Temporary_data/Temp_data.txt", dec = ".", header = T)
Prec_data<-read.table("Data/Temporary_data/Prec_data.txt", dec = ".", header = T)

Temp_data$DOY<- rownames(Temp_data)
Temp_gg<- gather(Temp_data, "SITE", "VAL", 1:725)
Temp_gg<- merge(Temp_gg, Loc_list[,c(1,5,14,15)], by.x="SITE", by.y="SITE", all.x=T)
Temp_gg$CLIM<- "Temperature (°C)"

Prec_data$DOY<- rownames(Prec_data)
Prec_gg<- gather(Prec_data, "SITE", "VAL", 1:725)
Prec_gg<- merge(Prec_gg, Loc_list[,c(1,5,14,15)], by.x="SITE", by.y="SITE", all.x=T)
Prec_gg$CLIM<- "Precipitation (mm)"
Prec_gg[Prec_gg$VAL>10, "VAL"]<- Prec_gg[Prec_gg$VAL>10, "VAL"]-5

Clim_gg<- rbind(Temp_gg, Prec_gg)

Temp_line<- as.data.frame(unique(Clim_gg$SPECIES)); colnames(Temp_line)<- "SPECIES"
Temp_line$CLIM<- "Temperature (°C)"
Temp_line$VAL<- 5
Temp_line$DOY_5<- c(102, 88, 100, 92, 108)
Temp_line$TREE_TYPE<- c("Broadleaf", "Broadleaf", "Conifer", "Conifer", "Conifer")

ggplot()+geom_line(data=Clim_gg, aes(x=as.numeric(DOY), y=VAL, group=SITE, color=CWB), linewidth = 0.2)+
  facet_nested(CLIM~TREE_TYPE+SPECIES, scales = "free_y", axes = "all", switch = "y")+
  scale_color_viridis(option="magma", direction=-1, na.value=NA, limit = c(-18,76))+
  geom_vline(xintercept=172, color = "black", linetype="dashed")+
  geom_hline(yintercept=0, color = "black", linetype="dotted")+
  labs(color = "CWB (mm)")+
  theme(legend.title=element_text(size=14))+
  
  new_scale_color()+
  
  geom_segment(data=Temp_line, aes(y=VAL, yend = VAL, x=0, xend = DOY_5), color="#e30215",linetype="dashed")+
  geom_segment(data=Temp_line, aes(y=-8, yend = 5, x=DOY_5, xend = DOY_5), color="#e30215",linetype="dashed")+
  geom_smooth(data=Clim_gg, aes(x=as.numeric(DOY), y=VAL, colour = CLIM))+
  scale_color_manual(values = c("#1851cc", "#e30215"))+
  guides(color="none")+
  
  labs(x ="Day of year", y= "", color="")+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(strip.text  = element_text(size = 12, color = "black"))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(panel.spacing = unit(1.5, "lines"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.title.y = element_text(size = 12, color = "black"))+
  theme(legend.position = "bottom")+
  theme(strip.placement = "outside")+
  theme(legend.text = element_text(size = 15))

ggsave("Graphs/Fig. S6 (Clima within year).tiff", height = 180, width = 400, units = "mm", dpi = 300)
