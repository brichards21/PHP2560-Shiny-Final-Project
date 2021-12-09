#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(latex2exp)

# this is the file name of the code Breanna uploaded
# make sure to source the right file
source("simulation_updated.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Project"),
    
    p("At time 0, this is the number of individuals we have that are in each of the six categories."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("timing",
                        "Time",
                        c("Start", "Middle")),
            
            numericInput("exposed",
                      "Number of Individuals Exposed to COVID",
                      411), # this is the default values
            
            numericInput("infectives",
                      "Number of Individuals who are Infective",
                      34),
            
            numericInput("quarantined",
                      "Number of Individuals in Quarantine",
                      0),
            
            numericInput("diagnosed",
                      "Number of Individuals Diagnosed with COVID",
                      0),
            
            numericInput("recovered",
                      "Number of Individuals Recovered from COVID",
                      0),
            
            checkboxInput("exp",
                               "Exposed"),
            
            checkboxInput("inf",
                               "Infectives"),
            
            checkboxInput("diag",
                               "Diagnosed"),
            
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           sliderInput("transmission_rate", "Transmission Rate:",
                       min = 0, max = 1,
                       value = 0.5, step = 0.1),
           
           sliderInput("exp_inf", "Exposed to Infected Rate:",
                       min = 0, max = 1,
                       value = 0.5),
           
           
           plotOutput("test_plot"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$test_plot <- renderPlot({
        
        # code to generate data
        # we can change the various values of epsilon, lambda, delta, etc
        # once we figure out our inputs
        example_data <- get_diagnosed(e = input$exposed, i = input$infectives,
                      q = input$quarantined, j = input$diagnosed, r = input$recovered,
                      epsilon = (1/3)*(2/5), lambda = (1/3)*(3/5), delta = (15/100)*(1/21),
                      theta = 1/3, sigma = 1/3, gamma = 1/21, k = 0.2,
                      b = input$transmission_rate)
        
        
        df <- example_data %>%
            pivot_longer(!Time, names_to = "people_type", values_to = "num_people")
        
        # returns empty plot if nothing is checked
        p <- ggplot(data = df, aes(x = Time, y = num_people)) +
            labs(title = "Number of Cases Over Time", ylab = "Number of People")
        
        # see if diagnosed is checked
        if(input$diag) {
            p <- p + geom_line(data = df[df$people_type == "Diagnosed", ],
                               aes(x = Time, y = num_people, colour = "Diagnosed"))
        }
        # see if exposed is checked
        if(input$exp) {
            p <- p + geom_line(data = df[df$people_type == "Exposed", ],
                               aes(x = Time, y = num_people, colour = "Exposed"))
        }
        # see if infected is checked
        if(input$inf) {
            p <- p + geom_line(data = df[df$people_type == "Infectives", ],
                               aes(x = Time, y = num_people, colour = "Infectives"))
        }
        
        p <- p  + guides(col=guide_legend(title="Group"))
        
        
        return(p)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
