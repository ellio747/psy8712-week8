library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)

ui <- fluidPage(
    titlePanel("Shiny Week 8"),
    sidebarLayout(
        #sidebarPanels can be used to stack each of the inputs on the lefthand side
        sidebarPanel(
            selectInput("gender_select", #inputId for the gender selection
                        "Select Gender", #label for the gender selection selector
                        choices = c("Male", "Female", "All"), #defined choices; could use from unique(skinny_data$gender), but there is no All -> requires if function in server
                        selected = "All"), #Part 3, Line 4 asks to display All
            selectInput("se_show", #inputId for regression error band
                        "Error Bar Visible", #label for regression error band selector
                        choices = c("Display Error Band", "Suppress Error Band"), #Part 3, Line 5 defines the two choice options
                        selected = "Display Error Band"), #Display is the default according to Part 3, Line 5
            checkboxInput("by_date", #inputId for the date selection
                        "Include Participants Prior to July 1, 2017", #label for checkbox
                        value = TRUE #Part 3, Line 6 says to include by default
            )
        ),
        mainPanel( # Part 3, Line 3 asks to display the ggplot2 scatterplot; placing in the mainPanel provides best visual
           plotOutput("corPlot")
        )
    )
)
# This output of the corPlot is defined in the UI
server <- function(input, output) {
    output$corPlot <- renderPlot({
      # Part 3, Line 2 asks to import the datafile and slide instructions call for using RDS
      skinny_data <- readRDS("week8_tbl_skinny.rds") # selects from the same subdirectory as the shiny app
      
      # Filtering pipeline for if the gender is something other than All (default)
      if(input$gender_select != "All") {
        skinny_data <- skinny_data %>% 
          filter(gender == input$gender_select)
      }
      
      # This variable will define the input of se_show, allowing it to act as TRUE in the geom_smooth(se = )
      se_display <- input$se_show == "Display Error Band"
      
      # Filtering pipeline for if the date checkbox is de-selected
      if(!input$by_date) {
        skinny_data <- skinny_data %>% 
          filter(timeStart >= as.POSIXct("2017-07-01")) # This filters who took before 1 July 2017
      }
      
      # Plotting function
      ggplot(skinny_data, aes(x = x_mean, y = y_mean)) +
        geom_point() +
        geom_smooth(method = "lm", se = se_display, color = "purple") +
        scale_x_continuous("Q1-Q6 Mean Scores") +
        scale_y_continuous("Q8-Q10 Mean Scores") +
        xlim(c(1.5,4.75)) + # I added limits to maintain the same dimensions of the plot in the shiny app. The limits of the x_mean were obtained by using max() and min() in the console
        ylim(c(1,4.5)) # The limits of the y_mean were obtained by using max() and min() in the console
    })
}

shinyApp(ui = ui, server = server)