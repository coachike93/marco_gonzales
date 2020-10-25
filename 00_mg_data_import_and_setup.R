##Margo Gonzales set-up and data import##

setwd("/Users/ikewade/Desktop/Marco_Gonzales")

set.seed(44)
# install.packages("dplyr")
# install.packages("randomForest")
# install.packages("glmnet")
library(tidyr)
library(dplyr)
library(stringr)
library(randomForest)
library(glmnet)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("reshape2")
library("reshape2")


convert_percentages <- function(x) {
  as.numeric(str_remove_all(as.character(x), "%"))/100
}

##Import downloaded fangraphs pitching stats from 2016 to 2020
fgp <- read.csv("fg_b.csv")
fgp %>% filter(Name == "Marco Gonzales") %>% 
  arrange(Season)

##Convert the percentage metrics to usable numbers
fgp$LOB. <- convert_percentages(fgp$LOB.)
fgp$LD. <- convert_percentages(fgp$LD.)
fgp$FB. <- convert_percentages(fgp$FB.)
fgp$IFFB. <- convert_percentages(fgp$IFFB.)
fgp$GB. <- convert_percentages(fgp$GB.)
fgp$FB..1 <- convert_percentages(fgp$FB..1)
fgp$SL. <- convert_percentages(fgp$SL.)
fgp$CB. <- convert_percentages(fgp$CB.)
fgp$CH. <- convert_percentages(fgp$CH.)
fgp$O.Swing. <- convert_percentages(fgp$O.Swing.)
fgp$Z.Swing. <- convert_percentages(fgp$Z.Swing.)
fgp$Swing. <- convert_percentages(fgp$Swing.)
fgp$O.Contact. <- convert_percentages(fgp$O.Contact.)
fgp$Z.Contact. <- convert_percentages(fgp$Z.Contact.)
fgp$Contact. <- convert_percentages(fgp$Contact.)
fgp$Zone. <- convert_percentages(fgp$Zone.)
fgp$SwStr. <- convert_percentages(fgp$SwStr.)
fgp$K. <- convert_percentages(fgp$K.)
fgp$BB. <- convert_percentages(fgp$BB.)
fgp$Pull. <- convert_percentages(fgp$Pull.)
fgp$Cent. <- convert_percentages(fgp$Cent.)
fgp$Oppo. <- convert_percentages(fgp$Oppo.)
fgp$Soft. <- convert_percentages(fgp$Soft.)
fgp$Med. <- convert_percentages(fgp$Med.)
fgp$Hard. <- convert_percentages(fgp$Hard.)
fgp$HR.FB <- convert_percentages(fgp$HR.FB)
fgp$Barrel. <- convert_percentages(fgp$Barrel.)
fgp$HardHit. <- convert_percentages(fgp$HardHit.)


##QA##
fgp %>% filter(Name == "Marco Gonzales") %>% 
  arrange(Season)

##Add in a few more metrics. In particular we'll want to see IPS and WPS later in the analysis.
fgp <- fgp %>% mutate(uBB. = (BB-IBB)/(TBF-IBB)) %>% 
  mutate(HR. = HR/TBF) %>% 
  mutate(Start.IP = round(Start.IP, 0)+(Start.IP-round(Start.IP, 0))/0.3) %>% 
  mutate(HR.9 = 9*HR/Start.IP) %>% 
  mutate(Strike. = Strikes/Pitches) %>% 
  mutate(K_BB. = K.-BB.) %>% 
  mutate(K_BB = SO/BB) %>% 
  mutate(IPS = Start.IP/GS) %>% 
  mutate(WPS = W/GS)


#### STATCAST DATA ####

##function to convert factor variables to usable numerics
fact_to_num <- function(x) {
  as.numeric(as.character(x))
}

##Import downloaded file of all MG pitches from 2018-20
mg_statcast <- read.csv("marco_gonzales_statcast.csv") %>% 
  mutate(year = substr(game_date, 1, 4)) %>% 
  mutate(launch_speed = fact_to_num(launch_speed),
         release_speed = fact_to_num(release_speed),
         release_spin_rate = fact_to_num(release_spin_rate),
         release_pos_x = fact_to_num(release_pos_x),
         release_pos_y = fact_to_num(release_pos_y),
         release_pos_z = fact_to_num(release_pos_z)
  )

##Quick QA
mg_statcast %>% select(launch_speed) %>% View()