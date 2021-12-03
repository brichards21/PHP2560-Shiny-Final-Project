#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
            
            textInput("exposed",
                      "Number of Individuals Exposed to COVID",
                      "411"), # this is the default values
            
            textInput("infectives",
                      "Number of Individuals who are Infective",
                      "34"),
            
            textInput("quarantined",
                      "Number of Individuals in Quarantine",
                      "0"),
            
            textInput("diagnosed",
                      "Number of Individuals Diagnosed with COVID",
                      "0"),
            
            textInput("recovered",
                      "Number of Individuals Recovered from COVID",
                      "0"),
            
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
        
        
        ## code below is used to generate the plot
        # this should be a function of its own outside the server
        count_next <- function(t, counts,
                               delta, theta, gamma, sigma, epsilon, lambda, k) {
            e_t <- counts[1] + (bt(t) * (k * counts[1] + counts[2])) - ((epsilon + lambda) * counts[1])
            i_t <- counts[2] + (epsilon * counts[1]) - ((delta + theta) * counts[2])
            q_t <- counts[3] + (lambda * counts[1]) - (sigma * counts[3])
            j_t <- counts[4] + (theta * counts[2]) + (sigma * counts[3]) - ((delta + theta) * counts[4])
            r_t <- counts[5] + (gamma * counts[4])

            return(c(e_t, i_t, q_t, j_t, r_t))
        }
        
        t <- seq(0, 400, 1)
        colnames <- c("e", "i", "q", "d", "r")
        temp <- data.frame(matrix(nrow = length(t), ncol = 5))
        colnames(temp) <- c("e", "i", "q", "d", "r")
        
        current_nums <- as.numeric(c(input$exposed, input$infectives, input$quarantined,
                                     input$diagnosed, input$recovered))
        temp[1,] <- current_nums

        for (i in 1:(length(t) - 1)) {
            current_nums <- count_next((i - 1), current_nums,
                                       delta, theta, gamma, sigma, epsilon, lambda, k)
            temp[i + 1, ] <- current_nums

        }
        
        # above should be placed inside of a function
        
        # p <- ggplot(data = temp) + geom_point(aes(t, d))
        p <- plot(t, temp$d)
        return(p)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
