#1b_Social_Metric_Code.R

#Install required packages
# packages.to.install <- c("spatsoc", "asnipe", "sf")
# install.packages(packages.to.install)

library(spatsoc); library(data.table); library(asnipe); library(sf); library(cowplot);library(tidyverse)

data <- read.csv("Data/Data(Spatial).csv", header = T)

data <- data %>% dplyr::select(Name, Date, Time, Season, Lat, Long, ActiveFungus) %>% filter(Season > 2)

#Read the date and time properly in R
data$Time<-gsub("7am-10am", "07:00", data$Time)
data$Time<-gsub("10am-1pm", "07:00", data$Time)
data$Time<-gsub("1pm-3pm", "13:00", data$Time)
data$datetime<-paste(data$Date, data$Time, sep=" ")
str(data)
data$datetime<-strptime(data$datetime, format=c("%Y-%m-%d %H:%M"))
data$datetime<-as.POSIXct(data$datetime)
head(data$datetime)

#Convert latitude/longitude coordinates into X/Y coordinates
data$Lat<- ((((data$Lat/1000)+27)/60)+27)*-1
data$Long<-as.numeric(data$Long)
data$Long<-(((data$Long/1000)+1)/60)+153

names(data)[5:6]<-c("Y","X")
coordinates <- data[, c("X", "Y")]
coordinates <- st_as_sf(coordinates, coords = c("X", "Y"), crs = 4326)  
coordinates_utm <- st_transform(coordinates, crs = st_crs(32756))  
data$X <- st_coordinates(coordinates_utm)[, 1]
data$Y <- st_coordinates(coordinates_utm)[, 2]
head(data)
plot(data$X, data$Y, col = as.factor(data$Name))

# #### trim outliers 
# data1 <- data[data$X < 502000,]
# data1 <- data1[data1$Y > 6962000,]
# data1 <- data1[data1$X > 501650,]
# data1 <- data1[data1$Y < 6962500,]
# plot(data1$X, data1$Y, col=as.factor(data$Name))

seasons<-split(data, data$Season)
length(seasons) 

Sociality_list <- list()

i <- 1

for (i in 1:length(seasons)) {
  data1 <- seasons[[i]]

  data1 <- data1 %>% 
    group_by(Name, datetime) %>% 
    summarise(
      Season = mean(Season, na.rm = F),
      X = mean(X, na.rm = F),
      Y = mean(Y, na.rm = F),
      ActiveFungus = mean(ActiveFungus, na.rm = F)
    ) %>% 
    arrange(datetime)
  
  data1 <-  data.table::setDT(data1)
  group_times(
    DT = data1, 
    datetime = 'datetime')
  
  disease <- data1 %>% dplyr::select(Name, timegroup, ActiveFungus)
  
  #Determine if dragons sighted in the same survey are interacting (within 1.85m of one another)
  edgelist <- edge_dist(
    DT = data1,
    id = 'Name',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    threshold = 1.85,
    splitBy = "Season",
    returnDist = T) 
  
  edgelistD <- merge(edgelist, disease, by.x = c("ID2", "timegroup"), by.y = c("Name", "timegroup"), all.x = T)
  
  #Generate seasonal social metrics for each dragon
  Sociality <- edgelistD %>%
    group_by(ID1, Season) %>%
    summarise(
      Sightings = n(), 
      Infected_Associations = sum(!is.na(ID2) & ActiveFungus == 1),
      Infect_Assoc_Sight =  sum(!is.na(ID2) & ActiveFungus == 1) / n(),
      .groups = "drop") %>% 
    rename(Name = ID1) 
  
  Sociality_list[[i]] <- as.data.frame(Sociality)
  
  print(
    i
  )
  
}

final <- do.call("rbind",Sociality_list)

write.csv(final, "Data/Data(Social).csv", row.names = F)



