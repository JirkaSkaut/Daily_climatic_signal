#===============================================================================================================#
#===============================================================================================================#
#   Spatiotemporal variability in climatic sensitivity of dominant temperate tree species in Central Europe  ####
#                                       Jiří Mašek, Jan Tumajer, Václav Treml
#                                               Graphic part of script
#===============================================================================================================#
#===============================================================================================================#

# Necessary packages

library(ncdf4) # package for netcdf manipulation
library(dplR) # package for tree-ring data treatment
library(lubridate) # package for DOY adding
library(dendroTools) # package for tree-ring daily correlations
library(reshape2) # package for data structure changing
library(tidyr) # reshape to long format, drop_na
library(raster) # creating raster from XYZ
library(terra) # changing format of raster
library(dplyr) # selecting and mutate
#library(dendRolAB) # PCGA function

# Set of working directory
#setwd("D:/Masek/")
setwd("C:/Users/Jiri/Desktop/TDC_daily_cor/")

#==========================================================================================================#
#####                     1. Daily correlations calculation and selection                               ####
#==========================================================================================================#

# Loading of list of sites
Loc_list<-read.table("Data/Loc_list.txt", dec = ".", header = T)
Loc_list$TREE_TYPE<- ifelse(Loc_list$SPECIES %in% c("PISY", "PCAB", "ABAL"), "Conifer", "Broadleaf")

# Loading of climatic grids
temp <- nc_open("Data/Clima_grids/temperature_wgs_daily.nc")
prec <- nc_open("Data/Clima_grids/precipitation_wgs_daily.nc")

# Loading of PER and DOY selection functions
source("Skripts/Accompanying/PER_DOY_selection.R")

# Empty data-sets for tree-ring and climate data
Temp_data<- as.data.frame(matrix(ncol = nrow(Loc_list), nrow = 365)); colnames(Temp_data)<- Loc_list$SITE
Prec_data<- as.data.frame(matrix(ncol = nrow(Loc_list), nrow = 365)); colnames(Prec_data)<- Loc_list$SITE
Tree_data<- as.data.frame(matrix(ncol = nrow(Loc_list), nrow = 50)); colnames(Tree_data)<- Loc_list$SITE; rownames(Tree_data)<- c(1961:2010)

# Empty data-sets for daily correlations
Daily_cor_PER<- data.frame(SPECIES=NA, SITE=NA, CWB=NA, CLIM=NA, TYPE=NA, PER=NA, DOY=NA, COR=NA, Test=NA)
Daily_cor_DOY<- data.frame(SPECIES=NA, SITE=NA, CWB=NA, CLIM=NA, PER=NA, DOY=NA, COR=NA, Test=NA)

start<- now()

# For cycle for each site
for (i in 1:c(nrow(Loc_list))) {
  
  # Loading of tree-ring data and chronology building
  #serie <- read.rwl(paste("D:/Masek/TDC_2/", Loc_list[i, "RWL"], sep = ""))
  serie <- read.rwl(paste("C:/Users/Jiri/Desktop/TDC_daily_cor/Data/TreeDataClim/TDC_2/", Loc_list[i, "RWL"], sep = ""))
  
  detrend_serie <- detrend(serie, method = "Spline", nyrs = 40)
  chronology <- chron(detrend_serie, biweight = T)
  chronology$samp.depth<- NULL
  chronology<- subset(chronology, subset=rownames(chronology)<=2010 & rownames(chronology)>=1961)
  
  # Saving the chronology for Figure S
  Tree_data[,i]<- chronology$std
  
  # Extraction of daily climatic data from grids
  STORAGE_T <- data.frame(TIMESTAMP = seq(from = as.Date("19610101", tryFormats = c("%Y%m%d")), to = as.Date("20201231", tryFormats = c("%Y%m%d")), by = "day"))
  STORAGE_P <- data.frame(TIMESTAMP = seq(from = as.Date("19610101", tryFormats = c("%Y%m%d")), to = as.Date("20201231", tryFormats = c("%Y%m%d")), by = "day"))
  
  site1x <- Loc_list[i, "LON"]; site1y <- Loc_list[i, "LAT"]
  
  Order.X <- data.frame(ORDER = c(1:temp$dim$x$len), GRID = ncvar_get(nc = temp, varid = "x"))
  Order.Y <- data.frame(ORDER = c(1:temp$dim$y$len), GRID = ncvar_get(nc = temp, varid = "y"))
  Order.X$DIFFERENCE <- abs(Order.X$GRID - site1x)
  Order.Y$DIFFERENCE <- abs(Order.Y$GRID - site1y)
  site1x.order <- Order.X[Order.X$DIFFERENCE == min(Order.X$DIFFERENCE),"ORDER"]
  site1y.order <- Order.Y[Order.Y$DIFFERENCE == min(Order.Y$DIFFERENCE),"ORDER"]
  
  STORAGE_T[,"Temperature"] <- ncvar_get(nc = temp, varid = "T", start = c(site1x.order, site1y.order, 1), count = c(1,1,-1)) 
  STORAGE_P[,"Precipitation"] <- ncvar_get(nc = prec, varid = "P", start = c(site1x.order, site1y.order, 1), count = c(1,1,-1)) 
  
  # Changing structure of temperature data
  STORAGE_T$Year<- lapply(strsplit(as.character(STORAGE_T$TIMESTAMP), "\\-"), "[",1)
  STORAGE_T <- as.data.frame(lapply(STORAGE_T, unlist))
  STORAGE_T$DOY<- yday(STORAGE_T$TIMESTAMP)
  STORAGE_T$TIMESTAMP<- NULL
  STORAGE_T<- STORAGE_T[, c(2,3,1)]
  
  Loc_list[i, "TEMP_mean"]<- mean(STORAGE_T[STORAGE_T$Year<= 2010, "Temperature"])

  # Calculating mean temperature through year
  for (j in c(1:365)) {
    
    Set<- STORAGE_T[STORAGE_T$Year<= 2010,]
    Set<- Set[Set$DOY== j,]

    Temp_data[j,i]<-mean(Set$Temperature)
    
  }
  
  # Changing structure of precipitation data
  STORAGE_P$Year<- lapply(strsplit(as.character(STORAGE_P$TIMESTAMP), "\\-"), "[",1)
  STORAGE_P <- as.data.frame(lapply(STORAGE_P, unlist))
  STORAGE_P$DOY<- yday(STORAGE_P$TIMESTAMP)
  STORAGE_P$TIMESTAMP<- NULL
  STORAGE_P<- STORAGE_P[, c(2,3,1)]
  
  STORAGE_P1<-STORAGE_P[STORAGE_P$Year<= 2010,]
  STORAGE_P1[STORAGE_P1$Precipitation<0,]<- 0
  
  Prec_year<- aggregate(STORAGE_P1$Precipitation, list(STORAGE_P1$Year), sum)
  Loc_list[i, "PREC_mean"]<- mean(Prec_year$x)
  
  # Calculating mean precipitation through year
  for (j in c(1:365)) {
    
    Set<- STORAGE_P1[STORAGE_P1$Year<= 2010,]
    Set<- Set[Set$DOY== j,]
    
    Prec_data[j,i]<-mean(Set$Precipitation)
    
  }
  
  # Calculation of daily correlations
  COR_t <- daily_response(chronology, env_data = STORAGE_T, previous_year = T, tidy_env_data = T, method = "cor", aggregate_function = "mean", reference_window = "middle", remove_insignificant = F, lower_limit = 20, upper_limit = 90, row_names_subset = TRUE)
  COR_t_d<- melt(COR_t$calculations)
  colnames(COR_t_d)<- c("PER", "DOY", "COR")
  COR_t_d$Test<- (abs(COR_t_d$COR)/sqrt(1-COR_t_d$COR^2))*sqrt(50-2)
  
  COR_p <- daily_response(chronology, env_data = STORAGE_P, previous_year = T, tidy_env_data = T, method = "cor", aggregate_function = "sum", reference_window = "middle", remove_insignificant = F, lower_limit = 20, upper_limit = 90, row_names_subset = TRUE)
  COR_p_d<- melt(COR_p$calculations)
  colnames(COR_p_d)<- c("PER", "DOY", "COR")
  COR_p_d$Test<- (abs(COR_p_d$COR)/sqrt(1-COR_p_d$COR^2))*sqrt(50-2)
  
  # Selection of extreme Period 
  COR_t_PER<- Period_select(COR_t_d)
  COR_t_PER$CLIM<- "TEMP"
  COR_p_PER<- Period_select(COR_p_d)
  COR_p_PER$CLIM<- "PREC"
  
  COR_PER_s<- rbind(COR_t_PER, COR_p_PER)
  COR_PER_s$TYPE<- "Var"
  
  # Selection of short (20) and long (90) period
  COR_t_f<- COR_t_d[COR_t_d$PER %in% c(20,90),]
  COR_t_f$CLIM<- "TEMP"
  COR_p_f<- COR_p_d[COR_p_d$PER %in% c(20,90),]
  COR_p_f$CLIM<- "PREC"
  
  COR_PER_s_f<- rbind(COR_t_f, COR_p_f)
  COR_PER_s_f$TYPE<- paste("Fix", COR_PER_s_f$PER, sep = "_")
  
  COR_PER_s_vf<- rbind(COR_PER_s[,-5], COR_PER_s_f)
  
  COR_PER_s_vf$SPECIES<- Loc_list[i, "SPECIES"]
  COR_PER_s_vf$SITE<- Loc_list[i, "SITE"]
  COR_PER_s_vf$CWB<- Loc_list[i, "CWB"]
  
  # To combine all sites together
  Daily_cor_PER<- rbind(Daily_cor_PER, COR_PER_s_vf)
  
  # Selection of extreme DOY
  COR_t_DOY<- DOY_select(COR_t_d)
  COR_t_DOY$CLIM<- "TEMP"
  COR_p_DOY<- DOY_select(COR_p_d)
  COR_p_DOY$CLIM<- "PREC"
  
  COR_DOY_s<- rbind(COR_t_DOY, COR_p_DOY)
  
  COR_DOY_s$SPECIES<- Loc_list[i, "SPECIES"]
  COR_DOY_s$SITE<- Loc_list[i, "SITE"]
  COR_DOY_s$CWB<- Loc_list[i, "CWB"]
  
  # To combine all sites together
  Daily_cor_DOY<- rbind(Daily_cor_DOY, COR_DOY_s[,-5])
  
  print(i)
  
}

end<- now()

write.table(paste(print(duration<- end-start), "hours"), "Temporary_data/Time.txt", row.names = F, col.names = F, sep = "\t")

# Saving of results of tree-ring data as text files
write.table(Tree_data, "Temporary_data/Tree_data.txt", row.names = T, col.names = T, sep = "\t")

# Saving of results of climatic data as text files
Clima_gradient<- Loc_list[,c(1,3,4,5,14,16,17)]
write.table(Clima_gradient, "Temporary_data/Clima_gradient.txt", row.names = F, col.names = T, sep = "\t")
write.table(Temp_data, "Temporary_data/Temp_data.txt", row.names = T, col.names = T, sep = "\t")
write.table(Prec_data, "Temporary_data/Prec_data.txt", row.names = T, col.names = T, sep = "\t")

# Saving of results of daily correlations as text files
Daily_cor_PER<- Daily_cor_PER[-1,]
write.table(Daily_cor_PER, "Temporary_data/Daily_cor_PER.txt", row.names = F, col.names = T, sep = "\t")
Daily_cor_DOY<- Daily_cor_DOY[-1,]
write.table(Daily_cor_DOY, "Temporary_data/Daily_cor_DOY.txt", row.names = F, col.names = T, sep = "\t")


#==========================================================================================================#
#####                           2. Interpolation of climatic signal                              ####
#==========================================================================================================#

Daily_cor_PER<-read.table("Data/Temporary_data/Daily_cor_PER.txt", dec = ".", header = T)
Daily_cor_PER<-Daily_cor_PER[!is.na(Daily_cor_PER$DOY),]

# For sake of interpolation, each site has to have unique value of CWB,
# therefore here we are adjusting the values by very small numbers

# Since 12 sites have no significant correlation surrounded by 80 % of other significant correlations 
# in window 11 days and 11 periods, we are now working with 713 sites

SITE_CWB<-unique(Daily_cor_PER[,c(2,3)])
Rand<- sample(0:1000,nrow(SITE_CWB), replace=F)/100000
SITE_CWB$CWB_n<- SITE_CWB$CWB+Rand
Daily_cor_PER<- merge(Daily_cor_PER, SITE_CWB[,c(1,3)], by.x="SITE", by.y="SITE", all.x=T)

# Creating code for each graph
Daily_cor_PER$CODE<- paste(Daily_cor_PER$TYPE, Daily_cor_PER$SPECIES, Daily_cor_PER$CLIM, sep = "_")

# Creating grid for the whole dataset
DOY<- c(1:730)
CWB<- seq(round(min(Daily_cor_PER$CWB_n)), round(max(Daily_cor_PER$CWB_n)), 1)
Grid_whole<- as.data.frame(as.matrix(expand.grid(CWB,DOY))); colnames(Grid_whole)<- c("CWB", "DOY")

# Scaling the grid to 0-1
Grid_whole$DOY_s<- (Grid_whole$DOY-min(Grid_whole$DOY))/(max(Grid_whole$DOY)-min(Grid_whole$DOY))
Grid_whole$CWB_s<- (Grid_whole$CWB-min(Grid_whole$CWB))/(max(Grid_whole$CWB)-min(Grid_whole$CWB))
Grid_whole$COOR<- paste(Grid_whole$DOY, Grid_whole$CWB, sep = "_")

# Scaling the data to 0-1
Daily_cor_PER$DOY_s<- (Daily_cor_PER$DOY-min(Daily_cor_PER$DOY))/(max(Daily_cor_PER$DOY)-min(Daily_cor_PER$DOY))
Daily_cor_PER$CWB_n_s<- (Daily_cor_PER$CWB_n-min(Daily_cor_PER$CWB_n))/(max(Daily_cor_PER$CWB_n)-min(Daily_cor_PER$CWB_n))

# Preparing levels (each graph) for loop
Graphs<- unique(Daily_cor_PER$CODE)

# Preparing resulting grid
Grid_IDW<- Grid_whole

# For cycle for ordinary kriging interpolation

for (i in c(1:length(Graphs))) {
  
  # Selecting Graph
  Set<- Daily_cor_PER[Daily_cor_PER$CODE==Graphs[i],]
  Set<- na.omit(Set)
  
  # Selecting part of grid needed
  Grid<- Grid_whole[Grid_whole$CWB_s> min(Set$CWB_n_s) & Grid_whole$CWB_s< max(Set$CWB_n_s),]
  Grid_result<- Grid_whole[Grid_whole$CWB_s> min(Set$CWB_n_s) & Grid_whole$CWB_s< max(Set$CWB_n_s),]
  
  # Transformation to spatial formats
  
  sp::coordinates(Set) = ~ DOY_s+CWB_n_s
  
  sp::gridded(Grid) = ~ DOY_s+CWB_s
  
  # Inverse-distance weighted (IDW)
  IDW <- gstat::idw(formula = COR~1, locations = Set, newdata = Grid, maxdist = 0.2, idp=7)
  
  # Saving the results
  Grid_result$COR<- IDW$var1.pred
  
  Grid_IDW<- merge(Grid_IDW, Grid_result[,c("COOR", "COR")], by.x="COOR", by.y="COOR", all.x=T)
  colnames(Grid_IDW)[length(colnames(Grid_IDW))]<- Graphs[i]
  
  print(i)
  
}

write.table(Grid_IDW, "Data/Temporary_data/Grid_IDW.txt", row.names = F, col.names = T, sep = "\t")


#==========================================================================================================#
#####                                     3. Median filter of IDW                                       ####
#==========================================================================================================#

Grid_IDW<-read.table("Data/Temporary_data/Grid_IDW.txt", dec = ".", header = T)
Grid_IDW[,c(1,4,5)]<- NULL

Graphs<- colnames(Grid_IDW)[3:32]

Grid_median<- Grid_IDW[,c(1,2)]
Grid_median$COOR<- paste(Grid_median$DOY, Grid_median$CWB, sep = "_")

for (i in c(1:length(Graphs))) {
  
  Graph<- Graphs[i]
  Data<- Grid_IDW[,c(1,2,2+i)]
  Data<- drop_na(Data)
  
  Raster<- rasterFromXYZ(Data[,c(2,1,3)])
  Raster<- rast(Raster)
  
  Foc<- focal(Raster, w=5, fun = "median")
  
  Foc <- terra::as.data.frame(Foc, xy = TRUE, na.rm = FALSE)
  
  Foc$COOR<- paste(Foc$x, Foc$y, sep = "_")
  colnames(Foc)[3]<- Graph
  
  Grid_median<- merge(Grid_median, Foc[c(4,3)], by.x="COOR", by.y="COOR", all.x=T)
  
}

write.table(Grid_median, "Data/Temporary_data/Grid_median.txt", row.names = F, col.names = T, sep = "\t")

#==========================================================================================================#
#####                               4. Categorization of climate signal                                 ####
#==========================================================================================================#

Grid_median_all<-read.table("Data/Temporary_data/Grid_median.txt", dec = ".", header = T)
Grid_median<- Grid_median_all[,c(1:3)]
Grid_median[, c(4:13)]<- Grid_median_all %>% dplyr::select(contains("Var"))



Species<- c("PISY", "PCAB","ABAL", "FASY", "QUsp")

Categorization<- as.data.frame(Grid_median[,1]); colnames(Categorization)<- "COOR"

for (i in c(1:5)) {
  
  Spec<- Species[i]
  Data<- Grid_median %>% dplyr::select(contains(Spec))
  Data<- cbind(Grid_median[,c(3,2)], Data)
  Data<- drop_na(Data)
  
  Data<- Data %>% mutate(CAT = case_when( Data[,3] < 0.2 & Data[,3] > -0.2 & Data[,4] < 0.2 & Data[,4] > -0.2 ~ '00',
                                          Data[,3] >= 0.2  & Data[,4] >= 0.2 ~ '++',
                                          Data[,3] <= -0.2  & Data[,4] <= -0.2 ~ '--',
                                          Data[,3] >= 0.2  & Data[,4] <= -0.2 ~ '+-',
                                          Data[,3] <= -0.2  & Data[,4] >= 0.2 ~ '-+',
                                          Data[,3] < 0.2  & Data[,4] >= 0.2 ~ '0+',
                                          Data[,3] < 0.2  & Data[,4] <= -0.2 ~ '0-',
                                          Data[,3] <= -0.2  & Data[,4] < 0.2 ~ '-0',
                                          Data[,3] >= 0.2  & Data[,4] < 0.2 ~ '+0'))
  
  colnames(Data)[5]<- Spec
  Data$COOR<- paste(Data$DOY, Data$CWB, sep = "_")
  
  Categorization<- merge(Categorization, Data[c(6,5)], by.x="COOR", by.y="COOR", all.x=T)
  
}

write.table(Categorization, "Data/Temporary_data/Categorization.txt", row.names = F, col.names = T, sep = "\t")
