
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(colourpicker)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Phyllotaxis app"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("angle",
                        "Magic angle",
                        min = 1,
                        max = 3500,
                        value = 1871), 
            
            sliderInput("points", "Number of points",
                                                 min = 100,
                                                 max = 3000,
                                                 value = 1530),
            sliderInput("shape", "Shape",
                        min = 1,
                        max = 20,
                        value = 1),
            sliderInput("psize",
                        "Point size",
                        min = 1,
                        max = 200,
                        value = 155), 
           numericInput("alpha", "Transparency", min = 0.1,
                        max = 1,
                        value = 0.1, 0.1 ),
            colourInput("color", "Point color", "white"),
            colourInput("background", "Background color", "black")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot",height = 700, width = 700)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        angle <- 0.05 * input$angle

        # draw the histogram with the specified number of bins
       
        points <- input$points
        
        t <- (1:points) * angle
        x <- sin(t)
        y <- cos(t)
        
        df <- data.frame(t, x, y)
        
        p <- ggplot(df, aes(x*t, y*t))
        p + geom_point(aes(size = t), shape = input$shape, alpha = input$alpha, size = input$psize, color = input$color) +
            theme(panel.background = element_rect(fill=input$background),
                  panel.grid = element_blank(), 
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  legend.position = "none")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
