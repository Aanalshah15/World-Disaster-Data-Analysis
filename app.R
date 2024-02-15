#Upload all the necessary libraries

library(shiny)
library(shinyjs)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(reshape2)
library(DT)
library(dplyr)

# Load the dataset
data = read.csv("riskdata.csv")

# UI part for the Shiny App
ui <- dashboardPage(
  dashboardHeader(title = "World Disaster Data"),
  dashboardSidebar( width = 300,
                    selectInput("country", "Select Country:", choices = c(unique(data$Region)), selected = 1),
                    selectInput("year", "Select Year:", choices = c(unique(data$Year)), selected = 1)
  ),
  dashboardBody(
    
    
    
    fluidRow(
      # Dynamic valueBoxes
      valueBoxOutput("exposureBox"),
      
      valueBoxOutput("wriBox"),
      valueBoxOutput("vulnerabilityBox"),
      valueBoxOutput("SusceptibilityBox"),
      valueBoxOutput("Lack_of_Coping_Capabilities_Box"),
      valueBoxOutput("Lack_of_Adaptive_Capacities_Box")
    ),
    
    br(),
    
    # To show graphical plots in app
    box(plotOutput("plot")),
    box(plotOutput("plot2")),
    
    # To show Data table in App
    tabItem("data", fluidPage(
      h1("World Disaster Data"),
      dataTableOutput("datatable")
    ))
    
  )
  
)

# Server side of the shiny App
server <- function(input,output){
  
  # Filter data from country and year
  df_country <- reactive({data %>% filter(Region == input$country)})
  df_country_year <- reactive({data %>% filter(Region == input$country, Year == input$year)})
  
  output$exposureBox <- renderValueBox({
    valueBox(
      df_country_year()[1,"Exposure"], "Exposure", icon = icon("list"),
      color = "yellow"
    )
  })
  
  # To render value of WRI
  output$wriBox <- renderValueBox({
    valueBox(
      df_country_year()[1,"WRI"],"WRI (World Risk Index)", icon = icon("stats", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  # To render value of Vulnerability
  output$vulnerabilityBox <- renderValueBox({
    valueBox(
      df_country_year()[1,"Vulnerability"], "Vulnerability", icon = icon("folder-open"),
      color = "olive"
    )
  })
  
  # To render value of Susceptibility
  output$SusceptibilityBox <- renderValueBox({
    valueBox(
      df_country_year()[1,"Susceptibility"], "Susceptibility", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  # To render value of Lack of Coping Capabilities
  output$Lack_of_Coping_Capabilities_Box <- renderValueBox({
    valueBox(
      df_country_year()[1,"Lack_of_Coping_Capabilities"], "Lack of Coping Capabilities", icon = icon("tasks"),
      color = "aqua"
    )
  })
  
  #To render value of Lack of Adaptive Capacities
  output$Lack_of_Adaptive_Capacities_Box <- renderValueBox({
    valueBox(
      df_country_year()[1,"Lack_of_Adaptive_Capacities"], " Lack of Adaptive Capacities", icon = icon("th-list"),
      color = "maroon"
    )
  })
  
  
  # Render the graphical plots using renderPlot()
  
  output$plot <- renderPlot({
    
    
    
    
    # Reshape the data into a long format
    df_melt <- melt(df_country(), id.vars = "Year", measure.vars = c("WRI", "Exposure", "Vulnerability"))
    
    # Plot the data using geom_col()
    ggplot(df_melt, aes(x = Year, y = value, fill = variable)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
      ggtitle("WRI, Exposure, and Vulnerability by Year") +
      xlab("Year") +
      ylab("Count") +
      theme(legend.position = "bottom")
  })
  
  
  output$plot2 <- renderPlot({
    
    # Reshape the data into a long format
    df_country_long <- tidyr::pivot_longer(df_country(),
                                           cols = c("Exposure_Category", "WRI_Category", "Vulnerability_Category", "Susceptibility_Category"))
    
    # Plot the data using geom_bar()
    ggplot(df_country_long, aes(x = "", fill = value)) +
      geom_bar(width = 1) +
      facet_wrap(~ name, nrow = 2, ncol = 2) +
      coord_polar(theta = "y") +
      ggtitle("Category Distribution") +
      scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
      theme_void()
    
  })
  
  # Render the Data table using renderDataTable()
  output$datatable <- renderDataTable(data)
  
  
}

shinyApp(ui, server)