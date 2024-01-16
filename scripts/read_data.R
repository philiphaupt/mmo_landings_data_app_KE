# aim
# read in landgindata

library(tidyverse)

# dat <- read_csv("./data/uk_landings_by_port_2021.csv")
dat21 <- readxl::read_xlsx("./data/uk_landings_by_port_2021.xlsx", sheet = "uk_landings_by_port_2021")
dat22 <- readxl::read_xlsx("./data/uk_landings_by_port_2022.xlsx", sheet = "data")


dat <- rbind(dat21, dat22)

rm(dat21, dat22)

brixham_feb <- dat %>% filter(`Port of landing` == "Dungeness",
                                  `Month` == 2,
                                  `Length Group` == "10m&Under")

brixham_feb <- rbind(brixham_feb21, brixham_feb22)
rm(brixham_feb21, brixham_feb22)

#-----------------
# quick look at most commonly caught species

species_list_brix_feb <- brixham_feb %>% 
  group_by(`Species name`, `Species code`) %>% 
  summarise(total_landed_weight = sum(`Landed Weight (tonnes)`)) %>% 
  arrange(desc(total_landed_weight))

# plot two years
species_monthly_trends <- dat %>% filter(`Port of landing` == "Dungeness",
                           `Length Group` == "10m&Under") %>% 
  ggplot()+
  geom_boxplot(aes(x = factor(month.abb[Month], levels = month.abb)
                   , `Landed Weight (tonnes)`))+
    facet_wrap(~ `Species name`,
               scales = "free_y"
               )+
  xlab("Month")

# plot two years with rotated x-axis labels
species_monthly_trends <- dat %>%
  filter(`Port of landing` == "Dungeness",
         `Length Group` == "10m&Under") %>% 
  ggplot(aes(x = factor(Month, levels = 1:12, labels = month.name), `Landed Weight (tonnes)`)) +
  geom_boxplot() +
  facet_wrap(~ `Species name`, scales = "free_y") +
  xlab("Month") +
  ylab("Landed Weight (tonnes)") +
  ggtitle("Monthly landed weight trends for Dungeness (10m and under)") +
  theme(axis.text.x = element_text(angle = 90))


species_monthly_trends

ggsave( "species_monthly_catch_trends.png", height = 50, width = 50, units = "cm")
