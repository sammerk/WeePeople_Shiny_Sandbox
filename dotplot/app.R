library(shiny)
library(ggplot2)
library(ggdist) # For geom_dots()

# Define the UI
ui <- fluidPage(
    titlePanel("Interactive Dot Plot"),
    plotOutput("plot", click = "plot_click"),
    verbatimTextOutput("click_info") # Optional: To show click info
)

# Define the Server
server <- function(input, output, session) {
    
    # Reactive data storage for points
    points <- reactiveVal(data.frame(x = character(0), y = numeric(0)))
    
    # Render the ggplot
    output$plot <- renderPlot({
        # Create the base plot
        ggplot() +
            scale_x_discrete(limits = c("men", "women")) +
            scale_y_continuous(limits = c(0, 20)) +
            geom_dots(data = points(), aes(x = x, y = y), binwidth = 0.5) +
            theme_minimal() +
            labs(x = NULL, y = "Y-axis")
    })
    
    # Handle plot clicks
    observeEvent(input$plot_click, {
        click <- input$plot_click
        x_pos <- ifelse(click$x < 1.5, "men", "women") # Dichotomous x-axis
        y_pos <- click$y
        
        # Add the new point to the reactive data
        new_point <- data.frame(x = x_pos, y = y_pos)
        points(rbind(points(), new_point))
    })
    
    # Optional: Display click information
    output$click_info <- renderPrint({
        input$plot_click
    })
}

# Run the app
shinyApp(ui, server)
