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


# Removing Duplicate Rows
dat <- dat |> distinct()


# Renaming Round names

dat <- dat |> 
  mutate(Round = case_when(
    Round == "AAfinal" ~ "final_AA",
    Round == "TeamFinal" ~ "final_Team",
    Round == "AAqual" ~ "qual_AA",
    TRUE ~ Round
  ))




# Women and Men datasets

women <- dat |> 
  filter(Gender == "w")

men <- dat |> 
  filter(Gender == "m")   


# Arranging by Apparartus and Round 
# Finding out which players competed in all around final and which didn't.

women <- women |> 
  arrange(Competition, Country, FirstName, LastName, Apparatus, desc(Round)) 



women_vt_app <- women |> 
  filter(Apparatus %in% c("VT", "VT1", "VT2")) |> 
  mutate(
    Apparatus = case_when(
      Apparatus == "VT1" &
        lead(Apparatus) %in% c("VT", "VT1", "BB", "FX", "UB") ~ "VT",
      TRUE ~ Apparatus
    )
  )






# Women -------------------------------------------------------------------


unique(women$Apparatus)

unique(women$Competition)

women <- women |> 
  mutate(
    Apparatus = case_when(
      FirstName == lead(FirstName) &
        LastName == lead(LastName) &
        Competition == lead(Competition) &
        Apparatus == "VT1" &
        lead(Apparatus) %in% c("VT", "VT1") ~ "VT",
      TRUE ~ Apparatus
    )
  )



for (i in unique(women$Competition)){
  comp <- women |> filter(Competition == i)
  cat(i,"\n" , unique(comp$Round), "\n", unique(comp$Apparatus), "\n\n")
}

# 

women |> 
  ggplot(aes(x = Apparatus, y = Score))+
  geom_boxplot()
  







# Breakdown by Round









# VT Qual and VT Final are Separate





women <- women |> 
  mutate(
    Apparatus = case_when(
      FirstName == lead(FirstName) &
      LastName == lead(LastName) &
      Competition == lead(Competition) &
      Apparatus == "VT1" &
      lead(Apparatus) %in% c("VT", "VT1") ~ "VT",
      TRUE ~ Apparatus
    )
  )







#Athletes who were going for the individual apparatus final
women_vt_qual <- women_qual |> 
  filter(Apparatus %in% c("VT", "VT1", "VT2")) |> 
  arrange(Competition, Country, FirstName, LastName, Apparatus)


women_vt_qual <- women_vt_qual |> 
  mutate(
    Apparatus = case_when(
      Apparatus == "VT1" &
      lead(Apparatus) %in% c("VT", "VT1") ~ "VT",
      TRUE ~ Apparatus
    )
  )

women_vt_qual_just_allaround <- women_vt_qual |> 
  filter(Apparatus %in% c("VT"))

women_vt_qual_app_allaround <- women_vt_qual |> 
  filter(Apparatus %in% c("VT1","VT2"))





  






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

men_by_player <- men |> 
  arrange(Competition, Country, Apparatus, LastName, FirstName, )

women_qual <- women |> 
  filter(Round == "qual") 

women_apparatus_final <- women |> 
  filter(Round == "final")

women_team_final <- women |> 
  filter(Round == "TeamFinal")

women_all_around_final <- women |> 
  filter(Round == "AAfinal")


# Women -------------------------------------------------------------------

# Women's Apparatus'
# "BB"  "FX"  "UB"  "VT1" "VT"  "VT2"



women |> 
  ggplot(mapping = aes(x = Apparatus, y = Score))+
  geom_boxplot()

unique(women$Apparatus)

write.csv(women, "women_2022_2023.csv")


?write.csv



# Regression --------------------------------------------------------------

unique(men_qual$Apparatus)

men_qual_hb <- men_qual |> 
  filter(Apparatus == "HB")

ggplot(men_qual_hb, aes(x = Rank, y = Score)) +
  geom_point() +
  scale_x_log10()

men_qual_hb_line <- lm(Score ~ D_Score + E_Score ,data = men_qual_hb)  

plot(men_qual_hb_line)  
  
#   

