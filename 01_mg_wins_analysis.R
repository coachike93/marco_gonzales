####### WINS ANALYSIS #######


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
