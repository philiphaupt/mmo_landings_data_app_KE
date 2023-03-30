#Original  App that shows MMO landings data by port, with filters to display the data in a graph, 
# and then a second component which compares the recent data to the historic data.

#--------------
# Load libraries
library(shiny)
library(tidyverse)
#---------------
# Load the dataset
# Read in historic data
manual_dir <- getwd()
data_path <- paste0(manual_dir,"./data/historic_data")   # path to the data
files <- dir(data_path) # get file names
# dir(data_path)
# read in the data
data <- tibble(filename = files) %>% # create a data frame
  
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ readxl::read_xlsx(file.path(data_path, .), sheet = "data")))

# data
# convert into data frame
dat <- unnest(data, cols = c(file_contents),keep_empty = TRUE)
#------------------------------

# Define the UI
ui <- fluidPage(
  titlePanel("Monthly landed weight trends from MMO landings data."),
  sidebarLayout(
    sidebarPanel(
      # Input for year range
      sliderInput("year_range", "Year Range", 
                  min(dat$Year), max(dat$Year), 
                  value = c(min(dat$Year), max(dat$Year)), 
                  step = 1,
                  sep = "",
                  #labels = scales::comma
      ),
      # Input for month range
      sliderInput("month_range", "Month Range", min(dat$Month), max(dat$Month), value = c(min(dat$Month), max(dat$Month)), step = 1),
      # Input for port of landing
      selectInput("port", "Port of landing", choices = unique(dat$`Port of landing`)),
      # Input for length group
      selectInput("length", "Length Group", choices = unique(dat$`Length Group`)),
      # Input for species name
      selectInput("species", "Species name", choices = unique(dat$`Species name`))
    ),
    mainPanel(
      # Output plot
      plotOutput("species_monthly_trends")
    )
    
  )
)



# Define the server
server <- function(input, output) {
  
  # Create reactive filter for the dataset
  filtered_data <- reactive({
    dat %>%
      filter(`Port of landing` == input$port,
             `Length Group` == input$length,
             `Species name` == input$species,
             Year >= input$year_range[1] & Year <= input$year_range[2],
             Month >= input$month_range[1] & Month <= input$month_range[2])
  })
  
  
  
  # Create the plot
  output$species_monthly_trends <- renderPlot({
    # Plot two years
    ggplot(filtered_data(), aes(x = factor(Month, levels = 1:12, labels = month.name), `Landed Weight (tonnes)`)) +
      geom_boxplot() +
      xlab("Month") +
      ylab("Landed Weight (tonnes)") +
      ggtitle(paste0("Monthly landed weight trends for ", input$port)) +
      geom_text(aes(label = input$species), x = 2, y = 90, size = 5, color = "gray") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 0.5, size = 20),
        text = element_text(size = 20),
        #axis.text.y = element_text(size = 20),
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