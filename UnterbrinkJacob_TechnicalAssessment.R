# Jacob Unterbrink
# 08/21/2022
# OKC Thunder: Technical Assessment

thunder_data <- read.csv("shots_data.csv")
str(thunder_data)
library(tidyverse)

# Devide court into shot zones:
thunder_data <- thunder_data %>% mutate(zone = ifelse((x >= 22 | x <= -22) & y <= 7.8, "C3",
                                               ifelse(sqrt(x^2 + y^2) >= 23.75, "NC3",
                                                      "2PT")))

# Compute team shot distribution:
zone_percentages <- function(team_name = NULL){
  
  team_sub = thunder_data[thunder_data$team == team_name,]
  total_shots = nrow(team_sub)
  
  `2PT_pct` = length(which(team_sub$zone == "2PT"))/total_shots
  NC3_pct = length(which(team_sub$zone == "NC3"))/total_shots
  C3_pct = length(which(team_sub$zone == "C3"))/total_shots
  
  shot_df = as.data.frame(cbind(`2PT_pct`, NC3_pct, C3_pct))
  
  return(shot_df)
}

team_a_pcts <- zone_percentages(team_name = "Team A")
team_b_pcts <- zone_percentages(team_name = "Team B")

# Compute eFG% for each team (by zone):
eFG_percent <- function(team_name = NULL){
  
  team_sub_1 = thunder_data[thunder_data$team == team_name & thunder_data$zone == "NC3",]
  team_sub_2 = thunder_data[thunder_data$team == team_name & thunder_data$zone == "C3",]
  team_sub_3 = thunder_data[thunder_data$team == team_name & thunder_data$zone == "2PT",]
  
  FGM_1 = length(which(team_sub_1$fgmade == 1))
  THREE_PM_1 = length(which(team_sub_1$zone != "2PT" & team_sub_1$fgmade == 1))
  FGA_1 = length(which(team_sub_1$team == team_name))
  
  FGM_2 = length(which(team_sub_2$fgmade == 1))
  THREE_PM_2 = length(which(team_sub_2$zone != "2PT" & team_sub_2$fgmade == 1))
  FGA_2 = length(which(team_sub_2$team == team_name))
  
  FGM_3 = length(which(team_sub_3$fgmade == 1))
  THREE_PM_3 = length(which(team_sub_3$zone != "2PT" & team_sub_3$fgmade == 1))
  FGA_3 = length(which(team_sub_3$team == team_name))
  
  eFG_p_1 = (FGM_1 + 0.5 * THREE_PM_1)/FGA_1
  eFG_p_2 = (FGM_2 + 0.5 * THREE_PM_2)/FGA_2
  eFG_p_3 = (FGM_3 + 0.5 * THREE_PM_3)/FGA_3
  
  eFG_df = as.data.frame(cbind(eFG_p_1, eFG_p_2, eFG_p_3))
  names(eFG_df) = c("eFG%_NC3", "eFG%_C3", "eFG%_2PT")
  
  return(eFG_df)
}

eFG_team_a <- eFG_percent(team_name = "Team A")
eFG_team_b <- eFG_percent(team_name = "Team B")
