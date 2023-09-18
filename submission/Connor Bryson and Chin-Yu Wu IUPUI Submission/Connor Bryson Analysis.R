
# Connor's Working Directory (Desktop)
setwd("C:/Users/cdbry/Desktop/Sports Analytics/Competitions/2024 Uconn Data Challenge/2024-UCSAS-Data-Challenge/submission/Connor Bryson and Chin-Yu Wu IUPUI Submission")


# Loading Libraries
library(tidyverse)

# Importing Data
paris_quad <- read_csv(file = "./original data/data_2022_2023.csv")

tokyo_quad <- read_csv(file = "./original data/data_2017_2021.csv")

glimpse(paris_quad)

paris_quad |> 
  group_by()