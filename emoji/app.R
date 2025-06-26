library(shiny)
library(ggdist) 
library(tibble)
library(ggplot2)


# Define the UI
ui <- fluidPage(
    titlePanel("Interactive Dot Plot"),
    plotOutput("plot", click = "plot_click"),
    verbatimTextOutput("click_info") # Optional: To show click info
)

# Define the Server
server <- function(input, output, session) {
    
    # Reactive data storage for points
    points <- reactiveVal(
        tibble(x = factor(character(0), levels = c("men", "women")),
                   y = numeric(0)))
    
    # Render the ggplot
    output$plot <- renderPlot({
        ggplot(points(), aes(x = x, y = y)) + 
            geom_dots(aes(shape = x), 
                      dotsize = .35,
                      overflow = "compress",
                      stackratio = 1.5,
                      binwidth = 1,
                      fill = NA) + 
            scale_shape_manual(values = c("women" = "👧", "men" = "👦")) +
            scale_x_discrete(limits = c("men", "women")) +
            scale_y_continuous(limits = c(0, 10), breaks = 1:10) +
            theme_minimal() +
            theme(panel.grid.minor = element_blank())
    })
    
    # Handle plot clicks
    observeEvent(input$plot_click, {
        click <- input$plot_click
        x_pos <- ifelse(click$x < 1.5, "men", "women") # Dichotomous x-axis
        icon <- factor(
            ifelse(click$x < 1.5, 
                   sample(LETTERS, 1), 
                   sample(letters, 1)),
            levels = c(letters, LETTERS)) # Icon based on x-axis
        y_pos <- round(click$y)
        
        # Add the new point to the reactive data
        new_point <- 
            data.frame(x = x_pos, y = y_pos, icon = icon)
        points(rbind(points(), new_point))
    })
    
    # Optional: Display click information
    output$click_info <- renderPrint({
        head(points())
    })
}

# Run the app
shinyApp(ui, server)
