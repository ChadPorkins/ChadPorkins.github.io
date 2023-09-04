library(shiny)
library(ggplot2)
library(leaflet)

# Set the path to the folder containing the CSV files
folder_path <- "June/"

# Get the list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = "Points_Fields_June_.*\\.csv", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Iterate over each CSV file
for (file in csv_files) {
  # Read the CSV file
  data <- read.csv(file)
  
  # Extract the year from the file name
  year <- gsub("Points_Fields_June_", "", basename(file))
  year <- gsub("\\.csv", "", year)
  
  # Create a new column named "Year" with the extracted year
  data$Year <- year
  
  # Add the data to the combined data frame
  combined_data <- rbind(combined_data, data)
}

# Define the UI
ui <- fluidPage(
  titlePanel("Multiple Time Series Plots with Maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("id_select", "Select ID:", choices = sort(unique(combined_data$ID)), selected = 1)
    ),
    mainPanel(
      uiOutput("plot_header"),
      plotOutput("timeseries_plot"),
      leafletOutput("map_plot"),
      textOutput("plot_hover")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Generate the plot header based on ID
  output$plot_header <- renderUI({
    selected_id <- input$id_select
    location <- combined_data$Location[combined_data$ID == selected_id][1]
    header <- h3(paste("ID", selected_id, "-", location))
    header
  })
  
  # Create time series plots for each ID
  output$timeseries_plot <- renderPlot({
    selected_id <- input$id_select
    selected_data <- combined_data[combined_data$ID == selected_id, ]
    
    if (nrow(selected_data) <= 1) {
      ggplot(selected_data, aes(Year, first)) +
        geom_point(color = "steelblue") +
        labs(title = paste("Time Series Plot for ID", selected_id),
             x = "Year",
             y = "LST (C)") +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    } else {
      ggplot(selected_data, aes(Year, first, group = 1)) +
        geom_line(color = "steelblue") +
        geom_point(aes(color = factor(ID))) +
        geom_text(aes(label = Year), vjust = -1) +
        labs(title = paste("Time Series Plot for ID", selected_id),
             x = "Year",
             y = "LST (C)") +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
  })
  
  # Display the Year when hovering over a point
  output$plot_hover <- renderText({
    req(input$timeseries_plot_hover)
    hover <- nearPoints(combined_data, input$timeseries_plot_hover, threshold = 10, maxpoints = 1)
    if (nrow(hover) > 0) {
      paste("Year:", hover$Year)
    }
  })
  
  # Create map for each ID
  output$map_plot <- renderLeaflet({
    selected_id <- input$id_select
    selected_data <- combined_data[combined_data$ID == selected_id, ]
    
    leaflet(selected_data) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = ~Location)
  })
}

# Run the application
shinyApp(ui, server)