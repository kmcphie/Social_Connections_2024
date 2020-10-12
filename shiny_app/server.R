# Read in datasets created using the 'gather_raw_data.R' script.

responses <- readRDS("responses.rds")

# Define server logic.

server <- function(input, output) {
  output$line_plot <- renderPlot({
    # Generate type based on input$plot_type from ui
    
    ifelse(
      input$plot_type == "a",
      
      # If input$plot_type is "a", plot histogram of "waiting" column 
      # from the faithful dataframe
      
      x   <- faithful[, 2],
      
      # If input$plot_type is "b", plot histogram of "eruptions" column
      # from the faithful dataframe
      
      x   <- faithful[, 1]
    )
    
    # Draw the histogram with the specified number of bins
    
    hist(x, col = 'darkgray', border = 'white')
  })
}