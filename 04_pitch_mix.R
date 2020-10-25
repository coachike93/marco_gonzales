##### Pitch Mix Analysis #####


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

####### FINDING #######
### No real changes to the pitches besides dropping Velo and Spin. ###
#######################

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



