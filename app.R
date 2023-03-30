# App that shows MMO landings data by port, with filters to display the data in a graph, 
# and then a second component which compares the recent data to the historic data.

#--------------
# Load libraries
library(shiny)
library(tidyverse)
library(data.table)
#---------------
# Load the dataset
# Read in historic data
manual_dir <- getwd()
data_path <- paste0(manual_dir,"./data/historic_data/")   # path to the data
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
# recent data
rec_data_path <- paste0(manual_dir,"./data/recent_data/")   # path to the data
rec_files <- dir(rec_data_path) # get file names

# read in the data
rec_data <- tibble(filename = rec_files) %>% # create a data frame
    
    # holding the file names
    mutate(file_contents = map(filename,          # read files into
                               ~ readxl::read_xlsx(file.path(rec_data_path, .), sheet = "Data")))

# data
# convert into data frame
rec_dat <- unnest(rec_data, cols = c(file_contents),keep_empty = TRUE)
rec_dat_for_comb <- rec_dat %>% dplyr::select(
    filename,
    Year = year,
    Month = month,
    `Port of landing` = `port land`,
    #`Length Group`,
    `Gear category`,
    `Species code` = species_code,
    `Species name` = `species`,
    `Species group` = `species_group`,
    `Live Weight (tonnes)` = liveWgtTonnes,
    `Landed Weight (tonnes)` = landedWgtTonnes,
    `Value£`
    ) %>% 
    mutate(`Value (£000s)` = `Value£`/1000) %>% 
    select(-`Value£`)

# filter historical keeping only relevant ports
kent_essex_ports <- rec_dat_for_comb %>% distinct(`Port of landing`) # create a list of relevant ports from supplied data
hist_dat_kent_essex <- dat %>% dplyr::filter(`Port of landing` %in% unlist(kent_essex_ports)) %>% select(-c(`Vessel nationality`,`Port Nationality`, `Length Group` ))
comb_dat <- rbind(hist_dat_kent_essex, rec_dat_for_comb) # this is a single data set containing historical and recent data
rm(data, dat, rec_data, rec_dat_for_comb, kent_essex_ports, hist_dat_kent_essex, data_path, files, manual_dir, rec_data_path, rec_files) # housekeeping

#------------------------------

# Define the UI
ui <- fluidPage(
    titlePanel("Monthly landed weight trends from MMO landings data."),
    sidebarLayout(
        sidebarPanel(
            # Input for year range
            # sliderInput("year_range", "Year Range", 
            #             min(comb_dat$Year), max(comb_dat$Year), 
            #             value = c(min(comb_dat$Year), max(comb_dat$Year)), 
            #             step = 1,
            #             sep = "",
            #             #labels = scales::comma
            # ),
            # Input for month range
            #sliderInput("month_range", "Month Range", min(comb_dat$Month), max(comb_dat$Month), value = c(min(comb_dat$Month), max(comb_dat$Month)), step = 1),
            # Input for port of landing
            selectInput("port", "Port of landing", choices = sort(unique(comb_dat$`Port of landing`))),
            # Input for length group
            #selectInput("length", "Length Group", choices = unique(comb_dat$`Length Group`)),
            # Input for species name
            selectInput("species", "Species name", choices = sort(unique(comb_dat$`Species name`)))
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
        comb_dat %>%
            filter(`Port of landing` == input$port,
                   #`Length Group` == input$length,
                   `Species name` == input$species)#,
                   #Year >= input$year_range[1] & Year <= input$year_range[2],
                   #Month >= input$month_range[1] & Month <= input$month_range[2])
    })
    

    output$species_monthly_trends <- renderPlot({
        # Plot two years
        ggplot(filtered_data() %>% filter(Year < 2023), aes(x = factor(Month, levels = 1:12, labels = month.name), `Landed Weight (tonnes)`)) +
            geom_boxplot() +
            geom_point(data = filtered_data() %>% filter(Year == 2023), 
                       aes_(x = quote(factor(Month, levels = 1:12, labels = month.name)), 
                            y = quote(`Landed Weight (tonnes)`),
                            color = "salmon",
                            size = 2
                            #show.legend = FALSE
                            )
                       ) +
            xlab("Month") +
            ylab("Landed Weight (tonnes)") +
            ggtitle(paste0("Monthly landed weight trends for ", input$port)) +
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
            )+
            guides(color = "none",
                   size = "none")
    })
}

# Run the app
shinyApp(ui = ui, server = server)
