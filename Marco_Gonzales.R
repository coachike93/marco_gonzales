######### Marco Gonzales Analysis ##########

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


### Predicting Wins with IPS and ERA ###

##Filter the data down
simple_w_model_data <- fgp %>% 
  filter(GS > 10) %>% 
  select(Season, Name, W, IPS, ERA, WPS)

##Check out who is in the data
simple_w_model_data %>% filter(Season == 2020) %>% 
  arrange(IPS)

##Generate the linear regression model
simple_w_model <- lm(WPS ~ ., data=simple_w_model_data %>% select(-W, -Name, -Season))
summary(simple_w_model)
##.39 R2. Not actually a particularly good model, but this is not unexpected
##Team offense and random luck plays a huge role in predicting Ws, but it's good to see that
##about 40% of the variation in W comes from depth of start and run prevention.

simple_w_model_data %>% filter(Season == 2020, Name == "Marco Gonzales")

print(simple_w_model)

eWPS = simple_w_model$coefficients[1] + 
        simple_w_model$coefficients[2] * simple_w_model_data %>% 
                                          filter(Season == 2020, Name == "Marco Gonzales") %>% 
                                          select(IPS) + 
        simple_w_model$coefficients[3] * simple_w_model_data %>% 
                                          filter(Season == 2020, Name == "Marco Gonzales") %>% 
                                          select(ERA) 
  
eWPS * 11
##5.01 - 

#### FINDING #####
## we would expect MG to have 5 wins in 11 starts based on his IPS and ERA

#####TABLE DATA: PITCHERS WITH SIMILAR IPS, ERA, and their Wins per 30 starts###########
simple_w_model_data %>% 
  filter(IPS <= 6.67,
         IPS >= 6) %>% 
  filter(ERA <= 3.25,
         ERA >= 2.95) %>% 
  arrange(ERA) %>% 
  mutate(W_per_30 = round(WPS*30, 0),
         IPS = round(IPS, 1)) %>% 
  select(Season, Name, IPS, ERA, W_per_30)


######### STATCAST ############

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


##Data set with pitch mix by year and velocity on the pitches
pitch_mix_by_year <- mg_statcast %>% group_by(pitch_type, year) %>% 
  mutate(pitches=n(),
         velo = mean(release_speed)) %>% 
  select(pitch_type, year, pitches, velo) %>% 
  distinct() %>%
  inner_join(mg_statcast %>% group_by(year) %>% mutate(total_pitches = n()) %>% select(year, total_pitches) %>% distinct(), by="year") %>% 
  mutate(perc_pitch = pitches/total_pitches)

pitch_mix_by_year


##Chart Pitch Mix Over Time
ggplot(data=pitch_mix_by_year %>% 
         filter(pitch_type %in% c("CH", "CU", "FC", "SI", "FF")), aes(x=year, y=perc_pitch, group=pitch_type)) +
  geom_line(aes(color=pitch_type))+
  geom_point(aes(color=pitch_type))

##Clear spike in sinkers and drop in changeups



hard_hit_by_year <- mg_statcast %>% 
  filter(bb_type != "null",
         launch_speed != "null",
         bb_type %in% c("line_drive", "fly_ball")) %>% 
  group_by(year) %>% 
  mutate(air_avg_ev = mean(launch_speed),
         air_HardHit. = sum(launch_speed >=95)/n()) %>% 
  select(year, air_avg_ev, air_HardHit.) %>% 
  distinct()

hard_hit_by_year
##Average EV goes down, but the hardhit rate skyrockets in 2020

##Data for HR Indicator table
hr_indicator_plot_data <- bip_by_type %>% 
  filter(bb_type == "ground_ball") %>% 
  select(year, perc_bip) %>% 
  inner_join(hard_hit_by_year, by="year") %>% 
  ungroup() %>% 
  select(Year = year, GB. = perc_bip, AirHardHit. = air_HardHit., -bb_type)


#### TABLE DATA - GB and HardHit on LD + FB by year ######
hr_indicator_plot_data %>% arrange(Year)



####Pitch mix CSW%
mg_statcast %>% select(description) %>% distinct()

##evaluate if the pitches have changed by release, velo, or spin
pitch_shape <- mg_statcast %>% 
  filter(pitch_type != "null") %>% 
  group_by(year, pitch_type) %>% 
  mutate(velo = mean(release_speed),
         spin = mean(release_spin_rate),
         rp_x = mean(release_pos_x),
         rp_y = mean(release_pos_y),
         rp_z = mean(release_pos_z)) %>% 
  select(year, pitch_type, velo, spin, rp_x, rp_y, rp_z) %>% 
  distinct() %>% 
  ungroup()

pitch_shape %>% filter(pitch_type=="FC") %>% arrange(year)
##Pretty clear drops in Velo and spin. Un-moving release points

##Lets get CSW rate here and see how that's doing
csw <- mg_statcast %>% filter(description %in% c("called_strike", "swinging_strike", "swinging_strike_blocked")) %>% 
  group_by(year, pitch_type) %>% 
  mutate(CSW = n()) %>% 
  select(year, pitch_type, CSW) %>% 
  distinct() %>% 
  ungroup()

##Look across all pitches, total CSW numbers
csw %>% group_by(year) %>% 
  mutate(total_CSW = sum(CSW)) %>% 
  select(year, total_CSW) %>% 
  distinct() %>% 
  ungroup() %>% 
  inner_join(pitch_mix_by_year %>% ungroup() %>% select(year, total_pitches) %>% distinct(), by="year") %>% 
  mutate(CSW. = total_CSW/total_pitches)
##Went from 26.5% in 2019 to 29.7% in 2020.

##Whiffs by pitch
whiff <- mg_statcast %>% filter(description %in% c("swinging_strike", "swinging_strike_blocked")) %>% 
  group_by(year, pitch_type) %>% 
  mutate(whiff = n()) %>% 
  select(year, pitch_type, whiff) %>% 
  distinct() %>% 
  ungroup()

##CSW by pitch and Whiff by pitch
csw_rate <- csw %>% 
  inner_join(pitch_mix_by_year, by=c("year", "pitch_type")) %>% 
  inner_join(whiff, by=c("year", "pitch_type")) %>% 
  mutate(CSW. = round(CSW/pitches, 3),
         whiff. = round(whiff/pitches, 3))

#### Table data - CSW and Whiff rate by pitch ####
csw_rate %>% 
  filter(year > 2017) %>% 
  group_by(pitch_type) %>% 
  mutate(CSW = sum(CSW),
         whiff = sum(whiff),
         pitches = sum(pitches)) %>% 
  select(pitch_type, CSW, whiff, pitches) %>% 
  distinct() %>% ungroup() %>% 
  mutate(CSW. = CSW/pitches,
         whiff. = whiff/pitches) %>% 
  filter(pitch_type %in% c("CU", "SI", "CH", "FC")) %>% 
  arrange(desc(CSW.)) %>% 
  View()

### Lets look just at SInkers and Changeups
csw_rate %>% filter(pitch_type %in% c("CH", "SI"), year > 2018) %>% arrange(year, pitch_type)
csw_rate %>% filter(year == 2020) %>% arrange(whiff.)


### Lets checkout Velocity
pitch_velo_plot_data <- pitch_mix_by_year %>%
  filter(pitch_type %in% c("CU", "SI", "FC", "CH", "FF")) %>% 
  mutate(pitch_type_nice = if(pitch_type=="CU"){"Curveball"}
         else if(pitch_type == "SI") {"Sinker"}
         else if(pitch_type == "FC") {"Cutter"}
         else if(pitch_type == "FF") {"Fourseam"}
         else {"Changeup"})

##Plot Velo over time
ggplot(data = pitch_velo_plot_data %>% filter(pitch_type !=), aes(year, velo, group=pitch_type_nice, color=pitch_type_nice, shape=pitch_type_nice)) +
  geom_point(size=3) + 
  geom_line(size = 1) + 
  theme_light() + 
  labs(title = "Marco Gonzales Velocity by Year",
       subtitle = "(2018-20)",
       x = "Year",
       y = "Release Speed in MPH",
       color="Pitch Type",
       shape="Pitch Type") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


##Look at pitch mix over time.
ggplot(data = pitch_velo_plot_data, aes(year, perc_pitch, group=pitch_type_nice, color=pitch_type_nice, shape=pitch_type_nice)) +
  geom_point(size=3) + 
  geom_line(size = 1) + 
  theme_light() + 
  labs(title = "Marco Gonzales Pitch Mix by Year",
       subtitle = "(2019-20)",
       x = "Year",
       y = "% of Pitches",
       color="Pitch Type",
       shape="Pitch Type") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

##Try a stacked bar chart viz. Meh. I think a table will do better
ggplot(pitch_velo_plot_data %>% 
         filter(year>2018) %>% 
         filter(pitch_type != "FF") %>% 
         mutate(label = paste0(pitch_type_nice, ": ", 100*round(perc_pitch, 3), "%")), aes(fill=pitch_type_nice, y=perc_pitch, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  theme_minimal() + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4)



