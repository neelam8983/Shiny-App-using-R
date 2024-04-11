library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)

# Assuming your data is named final_AirBnBdata
# You may need to adjust the column names accordingly

paris_data <- final_AirBnBdata %>%
  filter(city == "Paris")

paris_data1 <- AirBnBData %>%
  filter(city == "Paris")
# Convert the date column to a Date object
paris_data$date <- as.Date(paris_data$first_review)

# Create a new variable for the quarter
paris_data$quarter <- quarters(paris_data$date)

# Define unique zip codes
unique_zipcodes <- unique(paris_data$zipcode)

# Grouping by host_id and host_name, then calculating the count of unique ids
apartments_per_owner <- paris_data1 %>%
  group_by(host_id, host_name) %>%
  summarise(number_of_apartments = n_distinct(id)) %>%
  arrange(desc(number_of_apartments))

# Grouping by zipcode and calculating the average price
average_price_by_zipcode <- paris_data %>%
  group_by(zipcode) %>%
  summarise(average_price = mean(as.numeric(gsub("\\$", "", price)), na.rm = TRUE))

# Define UI
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Explore Airbnb Data in Paris"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Explore Data", tabName = "explore", icon = icon("chart-bar"))
    ),
    # Move the Zip Code dropdown to the right-most position
    selectizeInput("zipcode", "Choose a Zip Code:",
                   choices = unique_zipcodes,
                   options = list(
                     placeholder = 'Select a Zip Code',
                     style = 'color: #228B22; background-color: #FFE4E1; border-color: #228B22;'
                   ),
                   width = "100%"  # Set width to 100% for full width
    ),
    # Move the Choose a variable dropdown below Choose a Zip Code
    selectInput("variable", "Choose a variable:",
                choices = setdiff(names(paris_data), "price"))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "explore",
        fluidRow(
          column(
            width = 12,  # Set width to 12 for full width
            fluidRow(
              box(
                width = 6,
                title = "Visit Frequency by Quarter",
                plotOutput("quarterChart", height = 300),  # Set the height of Visit Frequency
                # Set color for the Visit Frequency box
                style = 'background-color: #6495ED; color: #FFFFFF; border-color: #6495ED;'
              ),
              box(
                width = 6,
                title = "Scatter Plot",
                plotOutput("dynamicPlot", height = 300),
                # Set background color for the Scatter Plot box
                style = 'background-color: #87CEEB; color: #000000; border-color: #87CEEB;'
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Average Price",
                verbatimTextOutput("averagePriceCard"),
                # Set color for the Average Price card
                style = 'background-color: #6495ED; color: #FFFFFF; border-color: #6495ED;'
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Apartments Table",
                tableOutput("apartmentsTable"),
                # Set color for the Apartments Table box
                style = 'background-color: #6495ED; color: #FFFFFF; border-color: #6495ED;'
              )
            )
          )
        )
      )
    )
  )
)


# ...

# Define server
server <- function(input, output) {
  # Create dynamic plot based on input variable
  output$dynamicPlot <- renderPlot({
    variable <- input$variable
    selected_zipcode <- input$zipcode
    
    filtered_data <- paris_data %>%
      filter(zipcode == selected_zipcode)
    
    if (is.numeric(filtered_data[[variable]])) {
      # Scatter plot for numeric variables
      ggplot(filtered_data, aes_string(x = variable, y = "price")) +
        geom_point() +
        labs(title = paste("Relationship between Price and", variable),
             x = variable,
             y = "Price")
    } else {
      # Bar plot for categorical variables
      ggplot(filtered_data, aes_string(x = variable, fill = variable)) +
        geom_bar() +
        labs(title = paste("Distribution of Price across", variable),
             x = variable,
             y = "Count")
    }
  })
  
  
  # Show table of apartments per owner
  output$apartmentsTable <- renderTable({
    head(apartments_per_owner, 10)
  })
  
  # Show average price card
  output$averagePriceCard <- renderText({
    selected_zipcode <- input$zipcode
    avg_price <- average_price_by_zipcode %>%
      filter(zipcode == selected_zipcode) %>%
      pull(average_price)
    
    paste("Average Price for Zip Code", selected_zipcode, ":", avg_price)
  })
  
  # Quarter chart
  output$quarterChart <- renderPlot({
    selected_zipcode <- input$zipcode
    visit_frequency_data <- paris_data %>%
      filter(zipcode == selected_zipcode) %>%
      group_by(quarter) %>%
      summarise(visit_frequency = n())
    
    ggplot(visit_frequency_data, aes(x = quarter, y = visit_frequency, group = 1)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Visit Frequency by Quarter (Zip Code", selected_zipcode, ")"),
           x = "Quarter",
           y = "Visit Frequency")
  })
}

# Run the Shiny app
shinyApp(ui, server)
