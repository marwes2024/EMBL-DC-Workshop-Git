library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)

select(surveys, plot_id, species_id, weight)
select(surveys, -record_id, -species_id)

filter(surveys, year == 1995)
filter(surveys, year == 1995, sex == "M") # , interpreted as &

surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)

#nested funtions
surveys_sml2 <- select(filter(surveys, weight < 5), species_id, sex, weight)
#pipe %>% --> strg + shift + m

surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)

surveys %>% 
  filter(year < 1995) %>%
  select(year, sex, weight)

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg*2.2) %>% 
  View()

#
surveys %>%
  filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarise(mean_weight = mean(weight, na.rm = T)) 
#mutate instead of summarize also creates a new column, but differently (?)

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T)) %>% 
  tail() #alternative: print(n = 15)

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% #filter two things at the same time
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T)) %>% 
  tail() #alternative: print(n = 15)

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T), 
            min_weight = min(weight, na.rm = T)) %>% #add two columns with summaries at the same time
  tail() #alternative: print(n = 15)

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T), 
            min_weight = min(weight, na.rm = T)) %>%
  arrange(min_weight) #arrange rows by min weight (ascending, automatically)

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T), 
            min_weight = min(weight, na.rm = T)) %>%
  arrange(desc(min_weight)) #arrange rows by min weight (descending, as specified with function)

surveys %>% 
  count(sex, species) %>% #counts cells after grouping by several columns
  arrange(species, desc(n))

#exercises
summary(surveys)
#3.3 number of observations
surveys %>% 
  count(plot_type)

surveys %>% 
  count(plot_type, species)
  
#3.4
surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
    summarise(mean_hf_length = mean(hindfoot_length), 
              min_hf_length = min(hindfoot_length), 
              max_hf_length = max(hindfoot_length), 
              n = n()) %>% #n gives the current group size
  View()

#3.5
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>%
  filter(weight == max(weight)) %>% 
  select(year, genus, species_id, weight) %>%
  arrange(year)

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>%
  filter(weight == max(weight)) %>% 
  select(year, genus, species_id, weight) %>%
  arrange(year) %>%
  unique() %>% #remove duplicates
  View ()
