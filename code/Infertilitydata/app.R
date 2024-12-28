# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(ggcorrplot)

# Load the built-in 'infert' dataset
data("infert")

# Clean and preprocess data
infert_clean <- infert %>%
  mutate(education = recode(education,
                            "0-5yrs" = "No High School",
                            "6-11yrs" = "Some High School",
                            "12+ yrs" = "High School and Beyond"),
         induced_factor = as.factor(induced))  # Convert 'induced' to a factor

# Define the Shiny UI
ui <- fluidPage(
  # Title
  titlePanel("Infertility Data Insights"),
  
  # Sidebar Layout
  sidebarLayout(
    sidebarPanel(
      # User input: Filter by education level
      selectInput("educationFilter", "Select Education Level:", 
                  choices = c("All", unique(infert_clean$education)), 
                  selected = "All"),
      
      # User input: Filter by age range
      sliderInput("ageFilter", "Select Age Range:",
                  min = min(infert$age), max = max(infert$age),
                  value = c(20, 40)),
      
      # User input: Select variable to visualize
      selectInput("variable", "Choose Variable to Analyze:", 
                  choices = c("induced", "spontaneous", "age"),
                  selected = "induced"),
      
      # Button to download the dataset
      downloadButton("downloadData", "Download Dataset"),
      
      # Button to download filtered data
      downloadButton("downloadFilteredData", "Download Filtered Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("infertPlot")),
        tabPanel("Summary Stats", DTOutput("summaryTable")),
        tabPanel("Age Distribution Boxplot", plotOutput("ageBoxPlot")),
        tabPanel("Correlation Heatmap", plotOutput("corrHeatmap"))
      )
    )
  )
)

# Define the Shiny Server
server <- function(input, output) {
  
  # Reactive dataset filtered by education level and age range
  filteredData <- reactive({
    data <- infert_clean
    if (input$educationFilter != "All") {
      data <- data %>% filter(education == input$educationFilter)
    }
    data <- data %>% filter(age >= input$ageFilter[1], age <= input$ageFilter[2])
    return(data)
  })
  
  # Generate the selected plot
  output$infertPlot <- renderPlot({
    data <- filteredData()
    
    # Ensure data is not empty
    if (nrow(data) == 0) {
      return(NULL)  # If no data is available after filtering, return NULL
    }
    
    variable <- input$variable
    
    if (variable == "induced") {
      ggplot(data, aes(x = induced_factor, fill = education)) +
        geom_bar(position = "dodge") +
        labs(title = "Induced Abortions by Education Level", 
             x = "Induced Abortions (0 = No, 1 = Yes)", y = "Count") +
        theme_minimal() +
        theme(legend.title = element_blank())
    } else if (variable == "spontaneous") {
      ggplot(data, aes(x = spontaneous)) +
        geom_histogram(binwidth = 1, fill = "forestgreen", alpha = 0.7) +
        labs(title = "Distribution of Spontaneous Abortions",
             x = "Number of Spontaneous Abortions", y = "Count") +
        theme_minimal()
    } else if (variable == "age") {
      ggplot(data, aes(x = age)) +
        geom_histogram(binwidth = 2, fill = "coral", alpha = 0.7) +
        labs(title = "Age Distribution of Participants",
             x = "Age", y = "Count") +
        theme_minimal()
    }
  })
  
  # Provide interpretation for the plot
  output$plotDescription <- renderText({
    variable <- input$variable
    if (variable == "induced") {
      "This plot shows the count of induced abortions, categorized by education level."
    } else if (variable == "spontaneous") {
      "This histogram displays the distribution of the number of spontaneous abortions in the dataset."
    } else if (variable == "age") {
      "This histogram visualizes the age distribution of participants in the study."
    }
  })
  
  # Summary table of filtered data
  output$summaryTable <- renderDataTable({
    summary(filteredData())
  })
  
  # Boxplot for age distribution by education level
  output$ageBoxPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = education, y = age)) +
      geom_boxplot() +
      labs(title = "Age Distribution by Education Level", x = "Education Level", y = "Age") +
      theme_minimal()
  })
  
  # Correlation heatmap
  output$corrHeatmap <- renderPlot({
    data <- filteredData()
    corr_matrix <- cor(select(data, age, spontaneous, induced))
    ggcorrplot(corr_matrix, lab = TRUE, type = "lower")
  })
  
  # Download handler for the full dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("infert_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(infert_clean, file)
    }
  )
  
  # Download handler for the filtered dataset
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      paste("filtered_infert_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
