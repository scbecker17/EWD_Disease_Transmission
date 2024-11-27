# 1a_Density_Metric_Code.R ####

# library(remotes)
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# devtools::install_github("gfalbery/ggregplot")

# packages.to.install <- c("adehabitatHR", "magrittr", "data.table", "tidyverse")
# install.packages(packages.to.install)

library(tidyverse); library(data.table); library(magrittr); library(ggregplot); library(adehabitatHR); library(INLA); library(ggregplot)
library(cowplot)
theme_set(theme_cowplot())

#Load Data and remove euthanized dragons
FullDragons <- read.csv("Data/Dragon_Sight_Data2.csv", header = T)

FullDragons %>% 
  group_by(Name) %>% 
  summarise(Cohort = min(Season)) %>% 
  ungroup -> 
  CohortDragons

FullDragons %<>%
  # mutate_at(vars(contains("Date")), lubridate::ymd) %>% 
  mutate_at(vars(contains("Date")), lubridate::dmy) %>% 
  arrange(Date)

FullDragons %<>% 
  mutate(ActiveFungus = as.numeric(Date.diagnosed <= Date))

FullDragons %<>%
  mutate_at("ActiveFungus", 
            ~ifelse(is.na(.x), 0, .x))

FullDragons %<>% filter(Long < 250, Lat > 600)

#Just extract movement of diseased dragons
Infecteds <- FullDragons %>% filter(ActiveFungus == 1) %>% data.frame

Infecteds %<>% filter(!is.na(Long), !is.na(Lat)) %>% 
  filter(Season > 3)

(Seasons <- Infecteds$Season %>% unique %>% sort)

#Create seasonal Spatial Points Data Frames for movement of diseased dragons
SPDF <- SpatialPointsDataFrame(data = Infecteds[,c("Long", "Lat", "Season")], 
                               coords = Infecteds[,c("Long", "Lat")])

SPDF <- SPDF[,"Season"]

#Create heatmaps for each season showing density of diseased dragons across the study site
KUDL <- kernelUD(SPDF, 
                 extent = 0,
                 same4all = TRUE, 
                 grid = 500)

i <- 1

DFList <- list()

#Create seasonal maps showing the density of diseased dragons experienced by each individual at each sighting
for(i in 2:length(Seasons)){
  
  print(Seasons[i])
  
  DF <- FullDragons %>% filter(Season == Seasons[i]) %>% data.frame
  
  KUDL2 <- KUDL[[i]]
  
  KUDLRaster <- KUDL2 %>% raster::raster() 
  
  KUDLRaster %>% 
    raster::extract(DF[,c("Long", "Lat")]) ->
    DF$InfectedDensity.Annual
  
  # KUDL2 <- KUDL[[i-1]]
  # 
  # KUDLRaster <- KUDL2 %>% raster::raster()
  # 
  # KUDLRaster %>%
  #   raster::extract(DF[,c("Long", "Lat")]) ->
  #   DF$InfectedDensity.Annual_t_1
  
  DFList[[i]] <- DF
  
}

ReknittedDF <- DFList %>% bind_rows()

# MeanDF <- 
#   ReknittedDF %>% group_by(Season, Name) %>% 
#   summarise_at(c("ActiveFungus", "InfectedDensity.Annual"), ~mean(.x, na.rm = T))

#Get seasonal average density
Dis_Dens <- ReknittedDF %>% dplyr::select("Name", "Season", "Date", "Time", "InfectedDensity.Annual") %>% 
  group_by(Season, Name, Date, Time) %>% 
  summarise_at(c("InfectedDensity.Annual"), ~mean(.x, na.rm = T))

FullDragons2 <- left_join(FullDragons, Dis_Dens, by = c("Name", "Season", "Date", "Time"))

write.csv(FullDragons2, "Data/Data(Spatial).csv", row.names = F)
