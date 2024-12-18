# Veena Mendiratta updated October 17, 2023
# total flights by day of month data downloaded from here - January and July, 2019 to 2023
# https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FGJ&QO_fu146_anzr=b0-gvzr

# set your working directory
setwd("~/Desktop/R-work/2023-NU-EDA/Data")
library(tidyverse)   
library(ggplot2) 

## Total number of flights analysis by day of month for January and July - 2019, 2020, 2021, 2022, 2023
## Plot data to observe impact of COVID on number of flights

## Read in data
jan2019 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/January2019.csv", header = TRUE, sep = ",")
jul2019 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/July2019.csv", header = TRUE, sep = ",")
jan2020 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/January2020.csv", header = TRUE, sep = ",")
jul2020 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/July2020.csv", header = TRUE, sep = ",")
jan2021 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/January2021.csv", header = TRUE, sep = ",")
jul2021 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/July2021.csv", header = TRUE, sep = ",")
jan2022 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/January2022.csv", header = TRUE, sep = ",")
jul2022 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/July2022.csv", header = TRUE, sep = ",")
jan2023 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/January2023.csv", header = TRUE, sep = ",")
jun2023 <- read.csv("~/Desktop/R-work/2023-NU-EDA/Data/June2023.csv", header = TRUE, sep = ",")

####################################################################
## see column names
names(jan2019)

## Filter out flights that were CANCELLED (=1)
jan2019 <- filter(jan2019, CANCELLED == 0)
jul2019 <- filter(jul2019, CANCELLED == 0)
jan2020 <- filter(jan2020, CANCELLED == 0)
jul2020 <- filter(jul2020, CANCELLED == 0)
jan2021 <- filter(jan2021, CANCELLED == 0)
jul2021 <- filter(jul2021, CANCELLED == 0)
jan2022 <- filter(jan2022, CANCELLED == 0)
jul2022 <- filter(jul2022, CANCELLED == 0)
jan2023 <- filter(jan2023, CANCELLED == 0)
jun2023 <- filter(jun2023, CANCELLED == 0)

## Plots
## Plot number of flights for January by Year
ggplot() +
  geom_line(data = as.data.frame(table(jan2019$DAY_OF_MONTH)), aes(Var1, Freq, color = "2019", group = 1)) +
  geom_line(data = as.data.frame(table(jan2020$DAY_OF_MONTH)), aes(Var1, Freq, color = "2020", group = 1)) +
  geom_line(data = as.data.frame(table(jan2021$DAY_OF_MONTH)), aes(Var1, Freq, color = "2021", group = 1)) +
  geom_line(data = as.data.frame(table(jan2022$DAY_OF_MONTH)), aes(Var1, Freq, color = "2022", group = 1)) +
  geom_line(data = as.data.frame(table(jan2023$DAY_OF_MONTH)), aes(Var1, Freq, color = "2023", group = 1)) +
  ylim(8000, 22500) +
  scale_color_manual(name = "Year", values = c("2019" = "navyblue", "2020" = "darkorchid1",
                                               "2021" = "brown", "2022" = "red",
                                               "2023" = "green" )) +
  xlab("Day of the month (1 to 31)") + ylab("Number of flights") +
  ggtitle("Number of Flights in January by Year -- 2019 to 2023")

## Plot number of flights for July by Year
ggplot() +
  geom_line(data = as.data.frame(table(jul2019$DAY_OF_MONTH)), aes(Var1, Freq, color = "2019", group = 1)) +
  geom_line(data = as.data.frame(table(jul2020$DAY_OF_MONTH)), aes(Var1, Freq, color = "2020", group = 1)) +
  geom_line(data = as.data.frame(table(jul2021$DAY_OF_MONTH)), aes(Var1, Freq, color = "2021", group = 1)) +
  geom_line(data = as.data.frame(table(jul2022$DAY_OF_MONTH)), aes(Var1, Freq, color = "2022", group = 1)) +
  geom_line(data = as.data.frame(table(jun2023$DAY_OF_MONTH)), aes(Var1, Freq, color = "2023", group = 1)) +
  ylim(8000, 22500) +
  scale_color_manual(name = "Year", values = c("2019" = "navyblue", "2020" = "darkorchid1",
                                               "2021" = "brown", "2022" = "red",
                                               "2023" = "green" )) +
  xlab("Day of the month (1 to 31)") + ylab("Number of flights") +
  ggtitle("Number of Flights in July by Year -- 2019 to 2023")

## In the plots observe impact of COVID on number of flights
## Sharp drop in flights in July 2020 as compared to January 2020
####################################################################
## Note that for 2023 data is for January and June; update when July data is available