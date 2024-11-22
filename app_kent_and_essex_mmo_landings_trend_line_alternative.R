# app MMO all Kent and Essex ports

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
#data_directory <- "C:/Users/Philip.Haupt/OneDrive - KEIFCA/data_analysis/mmo_landings/mmo_landings_data_app_KE/data/"
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

#------------------------------------------------------------------
# Read in Kent and Essex ports

ports <- combined_data %>%  filter(port_nationality == "UK - England" ,
                                   vessel_nationality == "UK - England") %>% 
  distinct(port_nationality,port_of_landing)

ports_dir <- "C:/Users/philip.haupt/OneDrive - Kent & Essex Inshore Fisheries and Conservation Authority/data_analysis/mmo_landings/mmo_landings_data_app_KE/ports/"
#ports_dir <- "C:/Users/Philip.Haupt/OneDrive - KEIFCA/data_analysis/mmo_landings/mmo_landings_data_app_KE/ports/"

ports_kent_and_essex <- read_csv(paste0(ports_dir,"harbours_and_ports_kent_and_essex.csv"))


landings_dat_k_and_e <- combined_data %>% filter(port_of_landing %in% ports_kent_and_essex$ports)
#----------------------------------------------------------------

# MMO landings app : Kent and Essex IFCA ports by year
# Define the UI
# ui <- fluidPage(
#   titlePanel("Annual landed weight trends from MMO landings data."),
#   sidebarLayout(
#     sidebarPanel(
#       # Input for species name
#       selectInput("species", "Species name", choices = sort(unique(combined_data$`species_name`)), selected = "Cod")
#     ),
#     mainPanel(
#       # Output plot for landed weight
#       plotOutput("species_monthly_trends"),
#       # Output plot for mean catch weight by year
#       plotOutput("annual_catches_by_year"),
#       # Output plot for mean value by year
#       plotOutput("mean_value_by_year")
#     )
#   )
# )
ui <- fluidPage(
  titlePanel("Annual landed weight trends from MMO landings data."),
  sidebarLayout(
    sidebarPanel(
      # Input for species name
      selectInput("species", "Species name", choices = sort(unique(combined_data$species_name)), selected = "Cod"),
      # Download buttons for each graph
      downloadButton("download_monthly_trends", "Download Monthly Trends Plot"),
      downloadButton("download_annual_catches", "Download Annual Catches Plot"),
      downloadButton("download_mean_value", "Download Mean Value Plot")
    ),
    mainPanel(
      # Output plots
      plotOutput("species_monthly_trends"),
      plotOutput("annual_catches_by_year"),
      plotOutput("mean_value_by_year")
    )
  )
)


# Define the server
server <- function(input, output) {
    # Reactive data filtering
    filtered_data <- reactive({
      landings_dat_k_and_e %>%
        filter(species_name == input$species)
    })
  
  # Output plot for landed weight by month - PER TRIP! (not displaying > 2 *  SD in boxplots)
  output$species_monthly_trends <- renderPlot({
    # Filter data to exclude outliers beyond 2 SD within each year
    filtered_plot_data <- filtered_data() %>%
      group_by(year) %>%
      mutate(
        mean_landed_weight = mean(landed_weight_tonnes, na.rm = TRUE),
        sd_landed_weight = sd(landed_weight_tonnes, na.rm = TRUE)
      ) %>%
      filter(
        landed_weight_tonnes >= mean_landed_weight - 2 * sd_landed_weight,
        landed_weight_tonnes <= mean_landed_weight + 2 * sd_landed_weight
      ) %>%
      ungroup()
    

    ggplot(filtered_plot_data %>% filter(year < 2023),
           aes(
             x = factor(month, levels = 1:12, labels = month.name),
             y = landed_weight_tonnes
           )) +
      geom_boxplot(outlier.shape = NA) + # Do not display outliers in the boxplot
      geom_point(
        data = filtered_data() %>% filter(year == 2023),
        aes(
          x = factor(month, levels = 1:12, labels = month.name),
          y = mean(landed_weight_tonnes, na.rm = TRUE),
          #color = "aquamarine",
          size = 2
        ),
        fill = "aquamarine",
        color = "aquamarine"
      ) +
      xlab("Month") +
      ylab("Landed Weight (tonnes)") +
      ggtitle(paste0(
        "Kent and Essex - Monthly landed weight trends for ",
        input$species
      )) +
      theme_classic() +
      theme(
        axis.text.x = element_text(
          angle = 90,
          hjust = 0.5,
          size = 20
        ),
        text = element_text(size = 20),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "skyblue"),
        panel.grid.minor.y = element_blank()
      ) +
      guides(color = "none", size = "none")
  })
  
  # Download Monthly Trends Plot
  output$download_monthly_trends <- downloadHandler(
    filename = function() {
      paste0("Monthly_Trends_", input$species, ".png")
    },
    content = function(file) {
      # Open PNG device
      png(file, width = 800, height = 450)
      
      # Generate the plot
      filtered_plot_data <- filtered_data() %>%
        group_by(year) %>%
        mutate(
          mean_landed_weight = mean(landed_weight_tonnes, na.rm = TRUE),
          sd_landed_weight = sd(landed_weight_tonnes, na.rm = TRUE)
        ) %>%
        filter(
          landed_weight_tonnes >= mean_landed_weight - 2 * sd_landed_weight,
          landed_weight_tonnes <= mean_landed_weight + 2 * sd_landed_weight
        ) %>%
        ungroup()
      
      print(
        ggplot(filtered_plot_data %>% filter(year < 2023), 
               aes(x = factor(month, levels = 1:12, labels = month.name), 
                   y = landed_weight_tonnes)) +
          geom_boxplot(outlier.shape = NA) +
          geom_point(data = filtered_data() %>% filter(year == 2023), 
                     aes(x = factor(month, levels = 1:12, labels = month.name), 
                         y = mean(landed_weight_tonnes, na.rm = TRUE),
                         size = 2),
                     fill = "aquamarine",
                     color = "aquamarine") +
          xlab("Month") +
          ylab("Landed Weight (tonnes)") +
          ggtitle(paste0("Kent and Essex - Monthly landed weight trends for ", input$species)) +
          theme_classic()+
          theme(
            axis.text.x = element_text(
              angle = 90,
              hjust = 0.5,
              size = 20
            ),
            text = element_text(size = 20),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_line(color = "skyblue"),
            panel.grid.minor.y = element_blank()
          ) +
          guides(color = "none", size = "none")
      )
      
      # Close PNG device
      dev.off()
    }
  )

  # Output plot for Total catch weight by year
  output$annual_catches_by_year <- renderPlot({
    # Evaluate the reactive expression and group/summarize the data
    plot_data <- filtered_data() %>% 
      group_by(species_name, year) %>% 
      summarise(`Annual landed weight (tonnes)` = sum(landed_weight_tonnes, na.rm = TRUE), .groups = "drop")
    
    # Plot the data
    ggplot(plot_data, aes(x = as.numeric(as.character(year)), y = `Annual landed weight (tonnes)`)) +
      geom_point(color = "magenta1", size = 5, alpha = 0.5) + # Add points
      geom_smooth(method = "loess", se = TRUE, color = "magenta4", fill = "violet") + # Add smooth line with confidence interval
      xlab("Year") +
      ylab("Total landed weights (tonnes) per year") +
      ggtitle(paste0("Total landed weights (tonnes) by Year for ", input$species)) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 0.5, size = 15),
        text = element_text(size = 18),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "skyblue"),
        panel.grid.minor.y = element_blank()
      )
  })

  # Download Annual Catches Plot
  output$download_annual_catches <- downloadHandler(
    filename = function() {
      paste0("Annual_Catches_", input$species, ".png")
    },
    content = function(file) {
      # Open PNG device
      png(file, width = 800, height = 450)
      
      # Generate the plot
      print(
        filtered_data() %>%
          group_by(year) %>%
          summarise(`Annual landed weight (tonnes)` = sum(landed_weight_tonnes, na.rm = TRUE)) %>%
          ggplot(aes(x = as.numeric(as.character(year)), y = `Annual landed weight (tonnes)`)) +
          geom_point(color = "magenta1", size = 3, alpha = 0.5) +
          geom_smooth(method = "loess", se = TRUE, color = "magenta4", fill = "violet") +
          xlab("Year") +
          ylab("Total landed weights (tonnes) per year") +
          ggtitle(paste0("Total landed weights (tonnes) by Year for ", input$species)) +
          theme_classic()+
          theme(
            axis.text.x = element_text(angle = 90, hjust = 0.5, size = 15),
            text = element_text(size = 24),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_line(color = "skyblue"),
            panel.grid.minor.y = element_blank()
          )
      )
      
      # Close PNG device
      dev.off()
    }
  )
  
  
}

# Run the app
shinyApp(ui = ui, server = server)

