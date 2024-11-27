# 3_Generate_Figures.R ####

library(tidyverse); library(cowplot); library(patchwork); library(INLA); library(colorspace)
library(magrittr); library(conflicted); library(ggregplot)
theme_set(theme_cowplot())

#Prevalence over time figure

TestDF1 <- read.csv("Data/INLA_Data.csv")

TestDF1 %>% filter(Season %in% 5:13) %>%
  mutate(Season = factor(Season, levels = 5:13)) %>%
  ggplot(aes(x = X, y = Y, color = as.factor(ActiveFungus))) + geom_point() +
  facet_wrap(~ Season, labeller = labeller(Season = years)) +
  labs(x = "Long",
       y = "Lat",
       color = "Disease Status"
  ) + 
  scale_color_manual(values = c("blue", "red"), labels = c("Healthy", "Diseased")) +
  theme(text = element_text(size = 12))

ggsave("Figures/Dis_Prev_Over_Time.jpeg", units = "mm", height = 120, width = 200, dpi = 600)

#Model Output figure
IM <- readRDS("Outputs/INLA_Model.rds")

plot <- SmoothOutput(Data = IM$Data,
                     Model = IM$Spatial$Model, Mesh = IM$Spatial$Mesh, Locations = IM$Data[,c("X", "Y")],
                     Covariates = c(Covar), Response = "ActiveFungus",
                     OutputCovariates = c("Infect_Assoc_Sight"),
                     # HoldFactors = c("Season" = "13"),
                     Output = "Data", Family = "Binomial",
                     # Output = "Link", Family = "Binomial",
                     LineAlpha = 0.1,
                     AddPoints = T, TestDF = IM$Data, PointAlpha = 0.1,
                     TextColour = "Red", PointColour = "Red",
                     AddP = T, AddEstimate = T, LimitClip = F)

plot1 <- plot[[1]] + labs(x = "Diseased Conspecifics Associations", y = "Infection Status")

plot <- SmoothOutput(Data = IM$Data,
                     Model = IM$Spatial$Model, Mesh = IM$Spatial$Mesh, Locations = IM$Data[,c("X", "Y")],
                     Covariates = c(Covar), Response = "ActiveFungus",
                     OutputCovariates = c("InfectedDensity.Annual"),
                     # HoldFactors = c("Season" = "13"),
                     Output = "Data", Family = "Binomial",
                     # Output = "Link", Family = "Binomial",
                     LineAlpha = 0.1,
                     AddPoints = T, TestDF = IM$Data, PointAlpha = 0.1,
                     TextColour = "Red", PointColour = "Red",
                     AddP = T, AddEstimate = T, LimitClip = F)

plot2 <- plot[[1]] + labs(x = "Diseased Conspecific Density of Space Utilised", y = "Infection Status")

plot3 <- plot1 + plot2
ggsave("Figures/Dis_Both.jpeg", units = "mm", height = 120, width = 240, dpi = 600)
