# Bioluminscence MAR440 R Script

# Load relevant libraries
install.packages('devtools')
devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(tidyverse)
library(vegan)
library(corrplot)
library(dbplyr)

# Remove any riff raff objects
rm(list = ls())

# Load dataset
data <- readr::read_delim("biolum_all_data.csv", delim = ",")

h2 <- readr::read_delim("hydrogen_data.csv", delim = ",")

ocean <- readr::read_delim("oceanography_data.csv", delim = ",")


# View data
colnames(data)[colnames(data) == "flECO-AFL"] ="fluor"
View(data)
str(data)
unique(data$STATION)
unique(data$Year)

# Filter by Year and Station Names

# 2019

data_19<- 
  data %>%
  filter(Year == 2019)

str(data_19)
unique(data_19$STATION)

data_19_B <-
  data_19 %>%
  filter(STATION == "ST_1" | STATION == "ST_2" | STATION == "ST_5" | STATION == "ST_6")

View(data_19_B)
unique(data_19_B$STATION)
unique(data_19_B$depSM)

#find average fluorescence values 
dat1a <- filter(data_19_B, depSM > 0 & depSM <= 5)
View(dat1a)
mean(dat1a$fluor)
mean(dat1a$oxy0)

dat1b <- filter(data_19_B, depSM > 5 & depSM <= 10)
mean(dat1b$fluor)
mean(dat1b$oxy0)

dat1c <- filter(data_19_B, depSM > 10 & depSM <= 15)
mean(dat1c$fluor)
mean(dat1c$oxy0)

dat1d <- filter(data_19_B, depSM > 15 & depSM <= 20)
mean(dat1d$fluor)
mean(dat1d$oxy0)

dat1e <- filter(data_19_B, depSM > 20 & depSM <= 25)
mean(dat1e$fluor)
mean(dat1e$oxy0)

dat1f <- filter(data_19_B, depSM > 25 & depSM <= 30)
mean(dat1f$fluor)
mean(dat1f$oxy0)

dat1g <- filter(data_19_B, depSM > 30 & depSM <= 35)
mean(dat1g$fluor)
mean(dat1g$oxy0)

dat1h <- filter(data_19_B, depSM > 35 & depSM <= 40)
mean(dat1h$fluor)
mean(dat1h$oxy0)

dat1i <- filter(data_19_B, depSM > 40 & depSM <= 44)
mean(dat1i$fluor)
mean(dat1i$oxy0)

#create lists and data table
fluor1 <- c(1.204765, 0.2999725, 0.1837588, 1.001329, 3.103845, 1.250982, 0.63835, 0.60815, 0.645)
oxy1 <- c(2.158427, 1.531225, 0.8633529, 0.7181333, 0.56181, 0.5367364, 0.52874, 0.5256, 0.52375)
year1 <- c(2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019)
depth1 <- c("5m","10m","15m","20m", "25m" , "30m", "35m" ,"40m","45m")



# 2020

data_20<- 
  data %>%
  filter(Year == 2020)

str(data_20)
unique(data_20$STATION)


# 2021 (all station names are from Byfjord)

data_21 <-
  data %>% 
  filter(Year == 2021)

str(data_21)
unique(data_21$STATION)
View(data_21)

#find average fluorescence values 
dat3a <- filter(data_21, prDM > 0 & prDM <= 5)
View(dat3a)
mean(dat3a$fluor)
mean(dat3a$oxy0)

dat3b <- filter(data_21, prDM > 5 & prDM <= 10)
mean(dat3b$fluor)
mean(dat3b$oxy0)

dat3c <- filter(data_21, prDM > 10 & prDM <= 15)
mean(dat3c$fluor)
mean(dat3c$oxy0)

dat3d <- filter(data_21, prDM > 15 & prDM <= 20)
mean(dat3d$fluor)
mean(dat3d$oxy0)

dat3e <- filter(data_21, prDM > 20 & prDM <= 25)
mean(dat3e$fluor)
mean(dat3e$oxy0)

dat3f <- filter(data_21, prDM > 25 & prDM <= 30)
mean(dat3f$fluor)
mean(dat3f$oxy0)

dat3g <- filter(data_21, prDM > 30 & prDM <= 35)
mean(dat3g$fluor)
mean(dat3g$oxy0)

dat3h <- filter(data_21, prDM > 35 & prDM <= 40)
mean(dat3h$fluor)
mean(dat3h$oxy0)

dat3i <- filter(data_21, prDM > 40 & prDM <= 44)
mean(dat3i$fluor)
mean(dat3i$oxy0)

fluor3 <- c(0.01192298, 0.005226378, 0.00434183, 0.005274841, 0.00759498, 0.088118801, 0.00639058, 0.007522219, 0.00394402)
year3 <- c(2021,2021,2021,2021,2021,2021,2021,2021,2021)
depth3 <- c("5m","10m","15m","20m","25m","30m","35m","40m","45m")

# 2022

data_22 <-
  data %>% 
  filter(Year == 2022)

str(data_22)
unique(data_22$STATION)
View(data_22)

# filter by station

data_22_B <-
  data_22 %>%
  filter(STATION == "ST_1" | STATION == "ST_2" | STATION == "ST_5" | STATION == "ST_B1"| STATION == "ST_B2"| STATION == "ST_B3"| STATION == "ST_SB1"|STATION == "ST_SB2")

View(data_22_B)
unique(data_19_B$STATION)
unique(data_19_B$depSM)

#find average fluorescence values 
dat4a <- filter(data_22_B, depSM > 0 & depSM <= 5)
View(dat4a)
mean(dat4a$fluor)
mean(dat4a$oxy0)

dat4b <- filter(data_22_B, depSM > 5 & depSM <= 10)
mean(dat4b$fluor)
mean(dat4b$oxy0)

dat4c <- filter(data_22_B, depSM > 10 & depSM <= 15)
mean(dat4c$fluor)
mean(dat4c$oxy0)

dat4d <- filter(data_22_B, depSM > 15 & depSM <= 20)
mean(dat4d$fluor)
mean(dat4d$oxy0)

dat4e <- filter(data_22_B, depSM > 20 & depSM <= 25)
mean(dat4e$fluor)
mean(dat4e$oxy0)

dat4f <- filter(data_22_B, depSM > 25 & depSM <= 30)
mean(dat4f$fluor)
mean(dat4f$oxy0)

dat4g <- filter(data_22_B, depSM > 30 & depSM <= 35)
mean(dat4g$fluor)
mean(dat4g$oxy0)

dat4h <- filter(data_22_B, depSM > 35 & depSM <= 40)
mean(dat4h$fluor)
mean(dat4h$oxy0)

dat4i <- filter(data_22_B, depSM > 40 & depSM <= 44)
mean(dat4i$fluor)
mean(dat4i$oxy0)

fluor4 <- c(0.02209348, 0.02288034, 0.01191442, 0.01686683, 0.02956554, 0.01450777, 0.01432648, 0.01469937, 0.01504335)
oxy4 <- c(2.70339, 2.466014, 1.626642, )
year4 <- c(2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022)
depth4 <- c("5m","10m","15m","20m","25m","30m","35m","40m","45m")

# 2023

data_23 <-
  data %>% 
  filter(Year == 2023)

str(data_23)
unique(data_23$STATION)

# filter by station

data_23_B <-
  data_23 %>%
  filter(STATION == "ST_1" | STATION == "ST_2" | STATION == "ST_3" | STATION == "ST_4"| STATION == "ST_5"| STATION == "ST_6")

unique(data_23_B$STATION)
unique(data_23_B$depSM)
max(data_23_B$depSM)

#find average fluorescence values 
dat5a <- filter(data_23_B, depSM > 0 & depSM <= 5)
View(dat5a)
mean(dat5a$fluor)
mean(dat5a$oxy0)

dat5b <- filter(data_23_B, depSM > 5 & depSM <= 10)
mean(dat5b$fluor)
mean(dat5b$oxy0)

dat5c <- filter(data_23_B, depSM > 10 & depSM <= 15)
mean(dat5c$fluor)
mean(dat5c$oxy0)

dat5d <- filter(data_22_B, depSM > 15 & depSM <= 20)
mean(dat4d$fluor)
mean(dat4d$oxy0)

dat4e <- filter(data_22_B, depSM > 20 & depSM <= 25)
mean(dat4e$fluor)
mean(dat4e$oxy0)

dat4f <- filter(data_22_B, depSM > 25 & depSM <= 30)
mean(dat4f$fluor)
mean(dat4f$oxy0)

dat4g <- filter(data_22_B, depSM > 30 & depSM <= 35)
mean(dat4g$fluor)
mean(dat4g$oxy0)

dat4h <- filter(data_22_B, depSM > 35 & depSM <= 40)
mean(dat4h$fluor)
mean(dat4h$oxy0)

dat4i <- filter(data_22_B, depSM > 40 & depSM <= 44)
mean(dat4i$fluor)
mean(dat4i$oxy0)

data_new <- c(fluor1, fluor3, fluor4)
data_new
data_new2 <-  
  tibble::as_tibble(data_new)
data_new2

#nMDS

#get dissimilarities
vegdist(x = data_new2, method = "bray", diag = TRUE, upper = TRUE)

# use the metaMDS function to perform an nMDS on these data
newdat_nmds <- metaMDS(comm = data_new2, 
                     distance = "bray",      
                     k = 2,                  
                     autotransform = FALSE)  

# check the stress value
newdat_nmds$stress #9.22e-05 so might not have enough data here

# check the stress plot
stressplot(newdat_nmds) #r2 = 1 so too good

# create a blank ordination plot
ordiplot(newdat_nmds, type = "n")

# we then add the communities to this plot
orditorp(newdat_nmds, display = "sites", cex = 1.25, air = 0.01)

# extract nMDS scores from the nmds_ben object
newdat_nmds$points

# write this into a dataframe called nmds_dat
newdat1 <- 
  tibble::as_tibble(newdat_nmds$points)

# view the dataframe
View(newdat1)

# create a dataframe called site_dat with site-related information
year <- c(year1, year3, year4)
depth <- c(depth1, depth3, depth4)

finaldat <- cbind(newdat1, year)
finaldat <- cbind(finaldat, depth)
finaldat

# plot the nMDS manually using ggplot
cols = c( "forestgreen", "blue", "goldenrod1")
ggplot(data = finaldat,
       mapping = aes(x = MDS1, y = MDS2, colour = as.factor(year), shape = depth)) +
  scale_shape_manual(values = c(8,9,10,11,12,13,14,15,16), name = "Depth")+
  geom_point(size = 2.5) +
  scale_colour_manual(values = cols, name = "Year") +
  labs(title = "nMDS of Fluorescence Across Years and Depth")+
  theme_classic()

# THIS DOESN'T WORK!!

#CREATE NEW DATASET THAT SUBSETS DATA BASED ON PEAK DEPTH
  #may also need to subset by stations
    #fluor at 15m where seafloor is 16 will be lower than 15m where seafloor is 45m 
#THEN ADD RELEVANT PARAMETERS AND CREATE nMDS

# subset peak depth between 15m and 30m for weather parameters

peak <- filter(data, prDM >= 15 & prDM <= 30)
View(peak)  

View(h2)
View(ocean)
View(precip)

#remove variables not needed or select only fluor, year, and depth
d <- peak$prDM
f <- peak$fluor
y <- peak$Year

peak_2 <- cbind(d,f)
peak_2 <- cbind(peak_2, y)
head(peak_2)

# filter and edit weather data 
h_data <- filter(h2, max_depth >= 20 & max_depth <= 30)
h_data <- filter(h_data, station == "BYFJORDEN")
head(h_data)

o_data <- filter(ocean, depth >= 20 & depth <= 30)
o_data <- filter(o_data, station_name == "BYFJORDEN")
o_data <- transform(o_data, salinity = as.numeric(salinity))
head(o_data)

o19 <- filter(o_data, year == 2019 & month != 10)
mean(o19$water_temperature)
mean(o19$salinity)
mean(o19$oxygen)

o20 <- filter(o_data, year == 2020 & month != 10)
mean(o20$water_temperature)
mean(o20$salinity)
mean(o20$oxygen)

o21 <- filter(o_data, year == 2021 & month != 10)
mean(o21$water_temperature)
mean(o21$salinity)
mean(o21$oxygen)

o22 <- filter(o_data, year == 2022 & month != 10)
mean(o22$water_temperature)
mean(o22$salinity)
mean(o22$oxygen)

p19 <- filter(precip, Year == 2019 & Date != 10)
head(p19)
mean(p19$Mean_precip2)

p20 <- filter(precip, Year == 2020 & Date != 10)
mean(p20$Mean_precip2)

p21 <- filter(precip, Year == 2021 & Date != 10)
mean(p21$Mean_precip2)

p22 <- filter(precip, Year == 2022 & Date != 10)
mean(p22$Mean_precip2)

p23 <- filter(precip, Year == 2023 & Date != 10)
mean(p23$Mean_precip2)

wt <- c(8.58,8.78125,8.285, 8.3875)
salt <- c(31.995,31.6,31.45,31.22375)
oxy <- c(0.1,0.1,0.1,0.1)
yr <- c(2019,2020,2021,2022)
rain <- c(3.105, 1.6775, 2.7375, 3.4225, 1.975)

