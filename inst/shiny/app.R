#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bbh)

# Create cache function
cache <- list("bbh" = fetch_sst(name = "bbh"))

getorfetch <- function(name = "bbh"){
    xx <- lapply(name,
                 function(nm){
                     if(nm %in% names(cache)){
                         x <- cache[[nm]]
                     }
                     else{
                         x <- fetch_sst(name = nm)
                         cache[[nm]] <<- x
                     }
                     return(x)
                 }) |>
        bind_rows_sst()
    
    return(xx)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SST Browser"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
            selectInput(
                "site",
                "Select sites",
                c("BBH", "E01"),
                selected = "BBH",
                multiple = TRUE,
                selectize = FALSE,
                width = NULL,
                size = NULL
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plotwindow")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plotwindow <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        x <- getorfetch(name = input$site)
        plot(x)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
