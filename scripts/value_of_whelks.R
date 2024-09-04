names(combined_data)


`Value(£)`

#-----------------
# quick look at most commonly caught species

value <- combined_data %>% 
  filter(species_name =='Whelks') %>% 
  group_by(year) %>% 
  summarise(total_landed_weight = sum(landed_weight_tonnes), total_value = sum(`Value(£)`), total_value_000s = sum(Value...000s.)) %>% 
  arrange(desc(year))

value <- value %>% mutate(Total_value_000s = if_else(is.na(total_value_000s ),total_value/1000, total_value_000s ))


# ke
value_k_and_e <- landings_dat_k_and_e %>% 
  filter(species_name =='Whelks') %>%
  group_by(year) %>% 
  summarise(total_landed_weight = sum(landed_weight_tonnes), total_value = sum(`Value(£)`), total_value_000s = sum(Value...000s.)) %>% 
  arrange(desc(year))

value_k_and_e <- value_k_and_e %>% mutate(Total_value_000s = if_else(is.na(total_value_000s ),total_value/1000, total_value_000s ))
