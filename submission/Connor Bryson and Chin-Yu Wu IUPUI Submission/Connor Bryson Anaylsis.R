# Connor Bryson Initial Analysis

# Desktop WD
setwd("C:/Users/cdbry/Desktop/Sports Analytics/Competitions/2024 Uconn Data Challenge/2024-UCSAS-Data-Challenge/submission/Connor Bryson and Chin-Yu Wu IUPUI Submission")

# Laptop WD


# OBJECTIVE: Create a Model that compares expected medal counts with different combinations of 5 team members





# Loading Packages
library(tidyverse)

dat <- read_csv("./cleandata/data_2022_2023.csv")

glimpse(dat)






# Removing Rows Where Player did not Compete
dat <- dat |> 
  filter(!is.na(Score))






# Women and Men datasets

women <- dat |> 
  filter(Gender == "w")

men <- dat |> 
  filter(Gender == "m")   



dat |> 
  group_by(Rank, FirstName, LastName,Country, Competition, Apparatus) |>
  arrange(by = desc(Rank)) |> 
  summarise(count = n(),
            best_score = max(Score)) |> 
  print(n = 50)


dat |> 
  group_by(Competition, Apparatus, Score) |> 
  arrange(Rank, desc(Score)) |> 
  select(FirstName, LastName, Country, Competition, Rank, Score) |> 
  print(n = 50)





# Men ---------------------------------------------------------------------

# Men's Apparatus'
# "HB"  "PH"  "FX"  "PB"  "SR"  "VT1" "VT"  "VT2" "hb" 

men |> 
ggplot(mapping = aes(x = Apparatus, y = Score))+
  geom_boxplot()

# Men's Rounds 
# "qual"      "AAfinal"   "TeamFinal" "final"     "AAqual" 

men_qual <- men |> 
  filter(Round == "qual")


unique(men$Round)





# Women -------------------------------------------------------------------

# Women's Apparatus'
# "BB"  "FX"  "UB"  "VT1" "VT"  "VT2"



women |> 
  ggplot(mapping = aes(x = Apparatus, y = Score))+
  geom_boxplot()

unique(women$Apparatus)

write.csv(women, "women_2022_2023.csv")


?write.csv

