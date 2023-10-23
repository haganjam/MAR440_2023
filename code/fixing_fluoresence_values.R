##NO 2023 DATA YET AVAILABLE##

# cleaning work space
rm(list = ls())


# importing libraries
library(tidyverse)
library(vegan)
library(corrplot)
library(readr)


# loading data made in python (fixing_depth script) 
# and taking out nan values
data_2019 <- read_csv("2019_fixed_depth.csv")
ctd_data_2019 <- data_2019[!is.na(data_2019[, "flECO-AFL"]), ]

data_2020 <- read_csv("2020_fixed_depth.csv")
ctd_data_2020 <- data_2020[!is.na(data_2020[, "flECO-AFL"]), ]

data_2021 <- read_csv("2021_fixed_depth.csv")
ctd_data_2021 <- data_2021[!is.na(data_2021[, "flECO-AFL"]), ]

data_2022 <- read_csv("2022_fixed_depth.csv")
ctd_data_2022 <- data_2022[!is.na(data_2022[, "flECO-AFL"]), ]

#data_2023 <- read_csv("2023_fixed_depth.csv")
#ctd_data_2023 <- data_2023[!is.na(data_2023[, "flECO-AFL"]), ]



#aggregate allows to do the mean for each different name of row
mean_fl_2019 <- aggregate(`flECO-AFL` ~ Station, data = ctd_data_2019 , FUN = mean)
mean_fl_2020 <- aggregate(`flECO-AFL` ~ Station, data = ctd_data_2020 , FUN = mean)
mean_fl_2021 <- aggregate(`flECO-AFL` ~ Station, data = ctd_data_2021 , FUN = mean)
mean_fl_2022 <- aggregate(`flECO-AFL` ~ Station, data = ctd_data_2022 , FUN = mean)
#mean_fl_2023 <- aggregate(`flECO-AFL` ~ Station, data = ctd_data_2023 , FUN = mean)


#how many row there are in each station

ncounts_2019 <- table(ctd_data_2019$Station)
ncounts_2019
#ST_1 ST_2 ST_6 
#87   34   50 

ncounts_2020<- table(ctd_data_2020$Station)
ncounts_2020
#ST_1 ST_2 ST_3 ST_8 
#317  436  128  372

ncounts_2021<- table(ctd_data_2021$Station)
ncounts_2021
#ST_1_thu  ST_1_tue  ST_1_wed ST_10_tue ST_10_wed  ST_2_thu  ST_2_tue  ST_2_wed  ST_3_thu  ST_3_tue  ST_3_wed  ST_9_thu  ST_9_wed 
#327       129       133       425       384       443       424       451       180       191       162       381       420 

ncounts_2022<- table(ctd_data_2022$Station)
ncounts_2022
#ST_1    ST_2    ST_3    ST_5   ST_B1 ST_B1_1   ST_B2 ST_B2_1   ST_B3 
#150     347     402     184     380     231     426     431     441 
 

ncounts_2023<- table(ctd_data_2023$Station)
ncounts_2023
 

#data set of mean for each station
mean_chalfix_2019 <- data.frame(chlafix = rep(c(1.0189943, 0.5900618, 1.0756980), #repeating means as many times as the station repeats in the dataset
                                              times = c( 87, 34, 50)))

mean_chalfix_2020 <- data.frame(chlafix = rep(c(1.2783182,1.0712599, 0.8737698, 1.1369077), 
                                              times = c(317, 436, 128, 372)))

mean_chalfix_2022 <- data.frame(chlafix = rep(c(0.02157192, 0.01797055, 0.01850940, 0.01731120, 0.01765645, 	
                                                0.01956201, 0.01695221, 0.01728983, 0.01709844),
                                              times = c(150, 347, 402, 184, 380, 231, 426, 431, 441)))


mean_chalfix_2021 <- data.frame(chlafix = rep(c(0.005621596, 0.006323961, 0.006745638, 0.006096962, 0.005367578, 0.005806811, 
                                                0.005819510, 0.005683676, 0.005071846, 0.004660333, 0.004601915, 0.006178134, 
                                                0.005708327),
                                              times = c(425, 384, 327, 129, 133, 443, 424, 451, 180, 191, 162, 381, 420)))


#mean_chalfix_2023 <- data.frame(chlafix = rep(c(3.834334, 3.453522, 2.052686, 2.670307, 3.592229, 3.802996),
#                                              times = c(2354, 11507,  2887,  4908,  1540,  3561)))

#dividing fluorescence by the mean
ctd_data_2019$`flECO-AFL` <-ctd_data_2019$`flECO-AFL` / mean_chalfix_2019$chlafix
ctd_data_2020$`flECO-AFL` <-ctd_data_2020$`flECO-AFL` / mean_chalfix_2020$chlafix
ctd_data_2021$`flECO-AFL` <-ctd_data_2021$`flECO-AFL` / mean_chalfix_2021$chlafix
ctd_data_2022$`flECO-AFL` <-ctd_data_2022$`flECO-AFL` / mean_chalfix_2022$chlafix
ctd_data_2023$`flECO-AFL` <-ctd_data_2023$`flECO-AFL` / mean_chalfix_2023$chlafix


#test plots

fix_ctd_data_2019_ST_1 <- 
  ctd_data_2019 %>%
  filter(Station=="ST_1")

ggplot(data = fix_ctd_data_2019_ST_1,
       mapping = aes(x = prDM, y =`flECO-AFL`)) +
  geom_point() +
  theme_classic()


fix_ctd_data_2021_ST_1_thu <- 
  ctd_data_2021 %>%
  filter(Station=="ST_1_thu")

ggplot(data = fix_ctd_data_2021_ST_1_thu,
       mapping = aes(x = prDM, y =`flECO-AFL`)) +
  geom_point() +
  theme_classic()



###Extracting####

#fixed_flur_byfjorden_clean_data <- rbind(ctd_data_2019, ctd_data_2020, ctd_data_2021, ctd_data_2022, ctd_data_2023)
write.csv(ctd_data_2019, file = "fullfix_ctd_data_2019.csv", row.names = FALSE)
write.csv(ctd_data_2020, file = "fullfix_ctd_data_2020.csv", row.names = FALSE)
write.csv(ctd_data_2021, file = "fullfix_ctd_data_2021.csv", row.names = FALSE)
write.csv(ctd_data_2022, file = "fullfix_ctd_data_2022.csv", row.names = FALSE)
write.csv(ctd_data_2023, file = "fullfix_ctd_data_2023.csv", row.names = FALSE)




