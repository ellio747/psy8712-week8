
library(shiny)
ui <- fluidPage(
    titlePanel("Shiny Week 8"),
    sidebarLayout(
        sidebarPanel(
            selectInput("gender",
                        "Select Gender",
                        choices = c("Male", "Female", "All"),
                        selected = "All"),
            selectInput("se_show",
                        "Error Bar Visible",
                        choices = c("Display Error Band", "Suprress Error Band"),
                        selected = "Display Error Band"),
            checkboxInput("by_date",
                        "Include Participants Prior to July 1, 2017",
                        value = TRUE)
        ),
        mainPanel(
           plotOutput("corPlot")
        )
    )
)

server <- function(input, output) {
    output$corPlot <- renderPlot({
      skinny_data <- readRDS("skinny_data.rds")
      
      ggplot(skinny_data, aes(x = x_mean, y = y_mean)) +
        geom_point(position = "jitter", width = .3) +
        geom_smooth(method = "lm", se = TRUE, color = "purple")
    })
}

shinyApp(ui = ui, server = server)

# Save for later
# library(rsconnect)
#     rsconnect::deployApp('path/to/your/app')
