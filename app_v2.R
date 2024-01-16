# mmo landings data -for whelks

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



# Directory containing the files
data_directory <- "C:/Users/philip.haupt/OneDrive - Kent & Essex Inshore Fisheries and Conservation Authority/data_analysis/mmo_landings/mmo_landings_data_app_KE/data/"

# List all CSV files
csv_files <- list.files(data_directory, pattern = "\\.csv$", full.names = TRUE)

# List all ODS files
ods_files <- list.files(data_directory, pattern = "\\.ods$", full.names = TRUE)

# Read and combine CSV files
csv_data <- bind_rows(lapply(csv_files, read_csv_file))

# Read and combine ODS files
ods_data <- bind_rows(lapply(ods_files, read_ods_file))

# Combine CSV and ODS data
combined_data <- bind_rows(csv_data, ods_data)

# Print the combined data
print(combined_data)

rm(csv_data, csv_files, files_ods, ods_data, csv_data) # housekeeping


# filter historical keeping only relevant ports - to do
#kent_essex_ports <- combined_data %>% distinct(`port_of_landing`) # create a list of relevant ports from supplied data


# MMO landings app 2
# Define the UI
ui <- fluidPage(
  titlePanel("Monthly landed weight trends from MMO landings data."),
  sidebarLayout(
    sidebarPanel(
      # Input for port of landing
      selectInput("port", "Port of landing", choices = sort(unique(combined_data$`port_of_landing`)), selected = "Whitstable"),
      # Input for species name
      selectInput("species", "Species name", choices = sort(unique(combined_data$`species_name`)), selected = "Whelks")
    ),
    mainPanel(
      # Output plot for landed weight
      plotOutput("species_monthly_trends"),
      # Output plot for mean catch weight by year
      plotOutput("monthly_mean_catches_by_year"),
      # Output plot for mean value by year
      plotOutput("mean_value_by_year")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Create reactive filter for the dataset
  filtered_data <- reactive({
    combined_data %>%
      filter(`port_of_landing` == input$port,
             `species_name` == input$species)
  })
  
  # Output plot for landed weight
  output$species_monthly_trends <- renderPlot({
    # Plot two years
    ggplot(filtered_data() %>% filter(year < 2023), aes(x = factor(month, levels = 1:12, labels = month.name), y = `landed_weight_tonnes`)) +
      geom_boxplot() +
      geom_point(data = filtered_data() %>% filter(year == 2023), 
                 aes(x = factor(month, levels = 1:12, labels = month.name), 
                     y = mean(`landed_weight_tonnes`, na.rm = TRUE),
                     color = "salmon",
                     size = 2
                 )
      ) +
      xlab("Month") +
      ylab("Landed Weight (tonnes)") +
      ggtitle(paste0("Monthly landed weight trends for ", input$port, " - ", input$species)) +
      geom_text(aes(label = input$species), x = 2, y = 90, size = 5, color = "gray") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 0.5, size = 20),
        text = element_text(size = 20),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "skyblue"),
        panel.grid.minor.y = element_blank()
      ) +
      guides(color = "none", size = "none")
  })
  
  # Output plot for mean catch weight by year
  output$monthly_mean_catches_by_year <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(year), y = sum(`landed_weight_tonnes`, na.rm = TRUE))) +
      geom_bar(stat = "identity", fill = "goldenrod1") +
      xlab("Year") +
      ylab("Total landed weights (tonnes) per year") +
      ggtitle(paste0("Total landed weights (tonnes) by Year for ", input$port, " - ", input$species)) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 0.5, size = 15),
        text = element_text(size = 15),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "skyblue"),
        panel.grid.minor.y = element_blank()
      )
  })
  
  
  
  # Output plot for mean value by year
  output$mean_value_by_year <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(year), y = sum(`Value...000s.`, na.rm = TRUE))) +
      geom_bar(stat = "identity", fill = "skyblue") +
      xlab("Year") +
      ylab("Yearly Total Value (£ 000s)") +
      ggtitle(paste0("Total Value by Year for ", input$port, " - ", input$species)) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 0.5, size = 15),
        text = element_text(size = 15),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "skyblue"),
        panel.grid.minor.y = element_blank()
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)