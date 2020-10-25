##### HR Indicators #####

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