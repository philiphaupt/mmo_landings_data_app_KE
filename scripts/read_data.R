# aim
# read in landing data

library(tidyverse)
library(data.table)

# mmo landings data

# Date 2024-01-15

# Philip Haupt philip.haupt@kentandessex-ifca.com

# App that shows MMO landings data by port, with filters to display the data in a graph, 
# and then a second component which compares the recent data to the historic data.

#--------------
# Load libraries
library(shiny)
library(tidyverse)
library(readr)
library(data.table)
library(readODS)
library(plotrix)
#---------------
# Function to read and process CSV files
read_csv_file <- function(file_path) {
  read_csv(file_path, locale = locale(encoding = "latin1")) %>%
    rename(
      year = `Year`,
      month = `Month`,
      port_of_landing = matches("(?i)port.*landing"),
      port_nationality = `Port Nationality`,
      vessel_nationality = matches("(?i)vessel*nationality"),
      length_group = `Length Group`,
      gear_category = matches("(?i)gear*category"),
      species_code = matches("(?i)species.*code"),
      species_name = matches("(?i)species.*name"),
      species_group = matches("(?i)species.*group"),
      live_weight_tonnes = `Live Weight (tonnes)`,
      landed_weight_tonnes = `Landed Weight (tonnes)`,
      value_000s = matches("(?i)value.*(£000s)")
    )
}


# Function to read and process ODS files
read_ods_file <- function(file_path) {
  # Assuming your sheet names are like "Published_dataset_2023", "Published_dataset_2024", etc.
  sheet_name <- grep("^Data", list_ods_sheets (file_path), value = TRUE)
  
  if (length(sheet_name) == 0) {
    stop("No sheet found with a name starting with 'Data'")
  }
  
  read_ods(file_path, sheet = sheet_name[1]) %>%
    rename(
      year = `Year`,
      month = matches("(?i)month"),
      port_of_landing = matches("(?i)port.*landing"),
      port_nationality = matches("(?i)port.*nationality"),
      vessel_nationality = matches("(?i)vessel.*nationality"),
      length_group = matches("(?i)length.*group"),
      gear_category = matches("(?i)gear*category"),
      species_code = matches("(?i)species.*code"),
      species_name = matches("(?i)species.*name"),
      species_group = matches("(?i)species.*group"),
      live_weight_tonnes = matches("(?i)live.*Weight.*(tonnes)"),
      landed_weight_tonnes = matches("(?i)landed.*weight.*(tonnes)"),
      value_000s = matches("(?i)value.*(£000s)")
    )
}
#---------------------


# kent and essex ports
ports <- combined_data %>%  filter(port_nationality == "UK - England" ,
                          vessel_nationality == "UK - England") %>% 
                               distinct(port_nationality,port_of_landing)


ports_kent_and_essex <- ports %>% filter(port_of_landing %in% c("Dover","Deal","Dungeness", "Folkestone", "Hythe", "Leigh-On-Sea", "Margate","Ramsgate", "Sheerness", "Tollesbury", "Walton-On-Naze", "West Mersea", "Whitstable", "Wivenhoe", "Maldon", "Bradwell", "Clacton", "Harwich", "Queenborough", "Brightlingsea", "Rochester", "Faversham", "Isle Of Sheppey", "Herne Bay", "Burnham-On-Crouch", "Pagelsham", "Broadstairs", "	
Southend-On-Sea"))

landings_dat_k_and_e <- combined_data %>% filter(port_of_landing %in% ports_kent_and_essex$port_of_landing)

#-----------------
# quick look at most commonly caught species

species_list_k_and_e <- landings_dat_k_and_e %>% 
  group_by(species_name, species_code) %>% 
  summarise(total_landed_weight = sum(landed_weight_tonnes)) %>% 
  arrange(desc(total_landed_weight))

# Filter out rows with NA:
species_list_k_and_e <- species_list_k_and_e %>%
  dplyr::filter(!is.na(species_name), !is.na(species_code))
# Fill missing values:
species_list_k_and_e <- species_list_k_and_e %>%
  tidyr::fill(species_name, .direction = "downup")

#plot

# Summarize total landed weight per species and arrange in descending order
plot_data <- species_list_k_and_e %>%
  group_by(species_name) %>%
  summarise(total_weight = sum(total_landed_weight, na.rm = TRUE)) %>%
  arrange(desc(total_weight)) %>%
  slice_head(n = 35)  # Keep only the top 35 species

# Create the horizontal bar plot with cornflower blue color
ggplot(plot_data, aes(x = reorder(species_name, total_weight), y = total_weight)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +  # Change bar color
  coord_flip() +  # Rotate plot to horizontal
  labs(title = "Top 35 Species by Total Landed Weight",
       x = "Species Name",
       y = "Total Landed Weight") +
  theme_minimal()





# # plot two years with rotated x-axis labels
# species_monthly_trends <- dat %>%
#   filter(`Port of landing` == "Dungeness",
#          `Length Group` == "10m&Under") %>% 
#   ggplot(aes(x = factor(Month, levels = 1:12, labels = month.name), `Landed Weight (tonnes)`)) +
#   geom_boxplot() +
#   facet_wrap(~ `Species name`, scales = "free_y") +
#   xlab("Month") +
#   ylab("Landed Weight (tonnes)") +
#   ggtitle("Monthly landed weight trends for Dungeness (10m and under)") +
#   theme(axis.text.x = element_text(angle = 90))
# 
# 
# species_monthly_trends

ggsave( "species_monthly_catch_trends.png", height = 50, width = 50, units = "cm")
