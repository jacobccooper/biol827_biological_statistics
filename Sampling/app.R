#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Effects of sample size"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          numericInput( 
            "bins", 
            "Numeric input", 
            value = 15, 
            min = 1, 
            max = 200)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           verbatimTextOutput("fullsummary")
        )
        
        
        #absolutePanel(renderText(output$sum))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  set.seed(8675309)
  full.x <- rnorm(n = 200, mean = 0, sd = 1)
  
  mu.x <- mean(full.x) |> round(3)
  sd.x <- sd(full.x) |> round(3)
  shap.x <- shapiro.test(full.x)
  
  output$fullsummary <- renderPrint({
    set.seed(8675309)
    x.norm <- sample(full.x, size = input$bins)
    mu.samp <- mean(x.norm) |> round(3)
    sd.samp <- sd(x.norm) |> round(3)
    shap.samp <- shapiro.test(x.norm)
    
    print(paste0("Original population: mean = 0, sd = 1"))
    print(paste0("Full sample: mean = ", mu.x,
                 ", sd = ", sd.x, 
                 ", p = ", round(shap.x$p.value, 3)))
    print(paste0("Your subsample: mean = ", mu.samp,
                 ", sd = ", sd.samp, 
                 ", p = ", round(shap.samp$p.value, 3)))
  })
    
    output$distPlot <- renderPlot({
      set.seed(8675309)
        x.norm <- sample(full.x, size = input$bins)
        mu.samp <- mean(x.norm) |> round(3)
        sd.samp <- sd(x.norm) |> round(3)
        shap.samp <- shapiro.test(x.norm)
      
        # generate bins based on input$bins from ui.R
        # bins <- seq(min(x.norm), max(x.norm), length.out = input$bins + 1)
        if(input$bins < 50){bins = 8}
        if(input$bins >= 50){bins = 20}

        # draw the histogram with the specified number of bins
        hist(x.norm, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Random value',
             main = 'Histogram from a random normal distribution')
    }
    
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
