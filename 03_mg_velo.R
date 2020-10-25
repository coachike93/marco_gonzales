#### VELO TIME ####


### Lets checkout Velocity
pitch_velo_plot_data <- pitch_mix_by_year %>%
  filter(pitch_type %in% c("CU", "SI", "FC", "CH", "FF")) %>% 
  mutate(pitch_type_nice = if(pitch_type=="CU"){"Curveball"}
         else if(pitch_type == "SI") {"Sinker"}
         else if(pitch_type == "FC") {"Cutter"}
         else if(pitch_type == "FF") {"Fourseam"}
         else {"Changeup"})

##Plot Velo over time
pitch_velo_plot_data$pitch_type_nice <- factor(pitch_velo_plot_data$pitch_type_nice, 
                                               levels = c("Sinker", "Cutter", "Changeup", "Curveball"))


ggplot(data = pitch_velo_plot_data 
       %>% filter(pitch_type != "FF"), 
       aes(year, velo, group=pitch_type_nice, color=pitch_type_nice, shape=pitch_type_nice)) +
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
