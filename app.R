library(shiny)
library(ggplot2)
library(ggtext)  # For enhanced text rendering in ggplot

# Define the UI
ui <- fluidPage(
    titlePanel("Interactive ggplot: Font Awesome Icons"),
    sidebarLayout(
        sidebarPanel(
            helpText("Click on the plot: Different Font Awesome icons will appear based on the Y-value."),
            actionButton("clear", "Clear Icons")
        ),
        mainPanel(
            # Load Font Awesome styles for the app
            tags$head(tags$link(rel = "stylesheet", 
                                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")),
            plotOutput("plot", click = "plot_click")
        )
    )
)

# Define the server logic
server <- function(input, output, session) {
    
    # Reactive value to store clicked points and their corresponding icons
    points <- reactiveVal(data.frame(x = numeric(0), y = numeric(0), icon = character(0)))
    
    # Add a point with a specific Font Awesome icon based on the y-value when the user clicks
    observeEvent(input$plot_click, {
        # Define the Font Awesome icons based on Y value
        icon <- ifelse(input$plot_click$y > 5, "\uf004", "\uf135")  # Heart and Rocket icons
        new_point <- data.frame(
            x = input$plot_click$x, 
            y = input$plot_click$y, 
            icon = icon
        )
        updated_points <- rbind(points(), new_point)
        points(updated_points)
    })
    
    # Clear points when the clear button is clicked
    observeEvent(input$clear, {
        points(data.frame(x = numeric(0), y = numeric(0), icon = character(0)))
    })
    
    # Render the plot
    output$plot <- renderPlot({
        ggplot(data = points(), aes(x = x, y = y)) +
            geom_text(aes(label = icon), family = "FontAwesome", size = 8) +  # Use Font Awesome font
            xlim(0, 10) +
            ylim(0, 10) +
            theme_minimal() +
            labs(title = "Click to Add Font Awesome Icons", x = "X-axis", y = "Y-axis") +
            theme(plot.title = element_markdown())
    })
}

# Run the app
shinyApp(ui = ui, server = server)
