# 2_INLA_Model.R ####

library(tidyverse); library(cowplot); library(patchwork); library(INLA); library(colorspace)
library(magrittr); library(conflicted); library(ggregplot)

theme_set(theme_cowplot())

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

Dragons <- read.csv("Data/Data(Spatial).csv", header = T)

#Trim data
month_to_remove <- c("5", "6", "7", "8")
Dragons$Date<-strptime(Dragons$Date, format=c("%Y-%m-%d"))
Dragons$Date<-as.POSIXct(Dragons$Date)
Dragons$Month <- month(Dragons$Date)
Dragons$Year <- year(Dragons$Date)

for(i in 1:length(month_to_remove)){
  Dragons <- subset(Dragons, Month != month_to_remove[i])
}

Dragons1 <- 
  Dragons %>% filter(Year > 2014) %>% 
  group_by(Name, Season, Sex, AgeClass, ActiveFungus) %>% 
  filter(Sex != 'Unknown') %>%
  summarise_all(~mean(.x, na.rm = T)) %>% 
  ungroup()

Dragons1 %<>% 
  mutate(X = Long, Y = Lat)

#Add in social metric
Social <- read.csv("Data/Data(Social).csv", header = T)

Dragons2 <- merge(Dragons1, Social, by = c("Name", "Season"))

#Set fixed effects
Covar <- c("Sex", "AgeClass", "Season", "Time.in.pop",
           "InfectedDensity.Annual",
           "Infect_Assoc_Sight")

#Average each individuals data per season
Dragons2 %>% 
  filter(!Sex == "Unknown", !AgeClass == "Unknown", Sightings >= 20) %>% 
  filter(Season %in% 2:13) %>% 
  mutate(Season = as.numeric(as.character(Season))) %>% 
  arrange(Season) %>% 
  dplyr::select(Name, Season, ActiveFungus, Sightings,
                X, Y, all_of(Covar)) %>% #na.omit %>% 
  ungroup %>% droplevels %>% 
  mutate(SeasonNumeric = as.numeric(as.character(Season))) -> 
  TestDF1

TestDF1 %<>% 
  mutate_at(c("Season", "Sex", "AgeClass"), as.factor)

TestDF1 <- as.data.frame(TestDF1)

write.csv(TestDF1, "Data/INLA_Data.csv", row.names = F)

#Run Model
IM <- INLAModelAdd(Data = TestDF1, 
                   Response = "ActiveFungus", 
                   Explanatory = Covar,
                   Random = "Name", RandomModel = "iid",
                   Family = "binomial",
                   AddSpatial = T,
                   Groups = T, GroupVar = "Season"
)

saveRDS(IM, file = "Outputs/INLA_Model.rds")

