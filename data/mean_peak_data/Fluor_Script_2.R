# MDS BAD SO MAKING NEW SCRIPT

# subset data by depth occurs and compare across years

# need to find method other than MDS that finds similarity or dissimilarity across years

# perhaps include weather (temp, salinity, turbidity), depth, time, 

library(patchwork)
library(tidyverse)
library(vegan)
library(corrplot)
library(dbplyr)

# Remove any riff raff objects
rm(list = ls())

# Load dataset
data <- readr::read_delim("byfjorden_clean_data_fixed.csv", delim = ",")
#data <- data %>% drop_na(oxy0)
#data <- data %>% drop_na(T0)
#data <- data %>% drop_na(sal00) --> do at later step not here!

View(data)

h2 <- readr::read_delim("hydrogen_data.csv", delim = ",")

ocean <- readr::read_delim("oceanography_data.csv", delim = ",")

precip <- readr::read_delim("precip.csv", delim = ",")

colnames(data)[colnames(data) == "flECO-AFL"] ="fluor"
View(data)

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
#peak depth is 20m to 30m where applicable as that is closest to ideal based on whats allowed by dataset
h_data <- filter(h2, max_depth >= 20 & max_depth <= 30)
h_data <- filter(h_data, station == "BYFJORDEN")
head(h_data) 
unique(h_data$year) #only has data for 2019, 2020, and 2021 so don't use

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
yr <- c(2019,2020,2021,2022,2023)
rain <- c(3.105, 1.6775, 2.7375, 3.4225, 1.975)

# subset fluor data by year and peak depth
# using 15m to 30m 

View(data)

d19 <- filter(data, Year == 2019)
d19 <- filter(d19, p >= 15 & p <= 30)
d19 <- d19 %>% drop_na(oxy0)
d19 <- d19 %>% drop_na(T0)
d19 <- d19 %>% drop_na(sal00)
mean(d19$fluor)
max(d19$fluor)
mean(d19$T0)
mean(d19$sal00)
mean(d19$oxy0)
mean(d19$upoly0)


d20 <- filter(data, Year == 2020)
d20 <- filter(d20, p >= 15 & p <= 30)
d20 <- d20 %>% drop_na(oxy0)
d20 <- d20 %>% drop_na(T0)
d20 <- d20 %>% drop_na(sal00)
mean(d20$fluor)
max(d20$fluor)
mean(d20$T0)
mean(d20$sal00)
mean(d20$oxy0)
mean(d20$upoly0)

d21 <- filter(data, Year == 2021)
d21 <- filter(d21, p >= 15 & p <= 30)

mean(d21$fluor)
max(d21$fluor)
mean(d21$T0)
mean(d21$sal00)
mean(d21$oxy0)
mean(d21$upoly0)

d22 <- filter(data, Year == 2022)
d22 <- filter(d22, p >= 15 & p <= 30)
d22 <- d22 %>% drop_na(oxy0)
d22 <- d22 %>% drop_na(T0)
d22 <- d22 %>% drop_na(sal00)
View(d22)
mean(d22$fluor)
max(d22$fluor)
mean(d22$T0)
mean(d22$sal00)
mean(d22$oxy0)
mean(d22$upoly0)


d23 <- filter(data, Year == 2023)
d23 <- filter(d23, p >= 15 & p <= 30)
d23 <- d23 %>% drop_na(oxy0)
d23 <- d23 %>% drop_na(T0)
d23 <- d23 %>% drop_na(sal00)
mean(d23$fluor)
max(d23$fluor)
mean(d23$T0)
mean(d23$sal00)
mean(d23$oxy0)
mean(d23$upoly0)


mean_fluor <- c(1.748721, 1.31729, 1.347998, 1.049857, 0.9414324)
max_fluor <- c(3.565, 3.38832, 2.603949, 2.766995, 1.8907)
ctd_temp <- c(9.296467, 9.184763, 8.819719, 8.517036, 8.722148)
ctd_sal <- c(31.14992, 30.93664, NaN, 30.52411, 30.52692)
ctd_oxy <- c(0.6345105, 0.2830011, 0.6999938, 0.6671266, 0.6329027)
ctd_turb <- c(1.496704, NaN, 0.1301967, 0.1010116, 4.909176)

# use cbind to create new data set

data_good <- cbind(yr, mean_fluor)
data_good <- cbind(data_good, max_fluor)
data_good <- cbind(data_good, rain)
data_good <- cbind(data_good, ctd_temp)
data_good <- cbind(data_good, ctd_oxy)
data_good <- cbind(data_good, ctd_turb)

Head(data_good)
View(data_good)