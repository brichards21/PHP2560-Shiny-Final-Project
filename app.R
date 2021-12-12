#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(tidyr)

source("sixtynine.R")
source("run_animation.R")
source("checkcondition.R")


ui <- fluidPage(
    # Our App has 3 main tabs that the user will be able to explore 
    
    # Application title
    titlePanel("How infectious disease spreads"),
    
    mainPanel( 
        tabsetPanel(type = "tabs", 
                    tabPanel("What is this app doing",
                             img(src = "SARSmodel_img.png", height = 350, width = 788),
                             actionButton("explanation", "What does this mean?"),
                             h3("What does this app do?"),
                             p("SARS is an infectious disease that spreads 
                      via droplets spread through contact with an infected individual (Zhou et al., 2004). 
                      This disease was identified in the early 2000s and due to its high infectivity was able 
                      to spread quickly and internationally (Zhou et al., 2004). Through drastic public health 
                      measures the spread of the disease was controled, but peaked mathematical modler's 
                      interest (Zhou et al., 2004). In order to have an idea of how this disease was spreading 
                      the authors created a mathematical model that allows for them to predict the number of 
                      indviduals that contracted SARS over a given amount of days (Zhou et al., 2004). 
                      In their model they have a multitude of parameters that they estimate based on their 
                      given data, to predict the number of indviduals diagnosed with SARS on a given day based 
                      on the starting valuesand parameter values (Zhou et al., 2004). The authors utilize their 
                      own data to estimate the model parameters, but we find that this model could be beneficial 
                      for other similar infectious diseases. Thus we have created this app which allows for 
                      public health officials to input parameters that were estimated based on their own data 
                      to predict the number of sick, exposed or infected indviduals after t days."), 
                             p("In this section we hope to have an interactive version of their model, where 
                      a user can click on the box and there is a pop up that tells them what each 
                     compartment is in the model"),
                             p("Citation"),
                             p("Zhou, Y., Ma, Z., &amp; Brauer, F. (2004). 
                     A discrete epidemic model for SARS transmission and control in China. 
                     Mathematical and Computer Modelling, 40(13), 1491–1506. https://doi.org/10.1016/j.mcm.2005.01.007 ")
                    ),
                    
                    tabPanel("Set your parameters", 
                             selectInput("Whenstart", "When do you want to start", c(Start = "S", Middle = "M", Random ="R")), 
                             # We want a user to get different input options based on when they are starting
                             # We use a conditional panel 
                             conditionalPanel(
                                 condition = "input.Whenstart == 'M'||input.Whenstart == 'R'", 
                                 sidebarPanel(numericInput("exposed", "Number of Individuals Exposed to COVID",
                                                           477),
                                              numericInput("infectives",
                                                           "Number of Individuals who are Infective",
                                                           286), 
                                              numericInput("quarantined",
                                                           "Number of Individuals in Quarantine",
                                                           191),
                                              
                                              numericInput("diagnosed",
                                                           "Number of Individuals Diagnosed with COVID",
                                                           848),
                                              
                                              numericInput("recovered",
                                                           "Number of Individuals Recovered from COVID",
                                                           1213))
                                 
                             ),
                             conditionalPanel(
                                 condition = "input.Whenstart == 'S'", 
                                 sidebarPanel(numericInput("exposed", "Number of Individuals Exposed to COVID",
                                                           411),
                                              numericInput("infectives",
                                                           "Number of Individuals who are Infective",
                                                           286))
                                 
                             ),
                             # We have escaped the conditional panels, but fear they return again 
                             mainPanel(fluidRow(column(width=6, offset = 0,
                                                       sliderInput("delta", "death rate" , 0, 1, (15/100)*(1/21)), 
                                                       sliderInput("epsilon", "Exposed -> Infective" , 0, 1, (1/3)*(2/5)),
                                                       sliderInput("lambda", "Exposed -> Quarentined" , 0, 1, (1/3)*(3/5)),
                                                       sliderInput("sigma", "Quarenrined -> Diagnosed" , 0, 1, 1/3),
                                                       sliderInput("theta", "Infected -> Diagnosed" , 0, 1, 1/3), 
                                                       sliderInput("gamma", "Diagnosed -> Recovered" , 0, 1, 1/21), 
                                                       sliderInput("k", "k", 0, 1, .1)),
                                                column(width = 4, offset = 2, 
                                                       p("This model assuumes that infectivity is a function of time, our model assumes 
                                                         the function, but a user may want to change the infectivity rate."), 
                                                       radioButtons("Infection", "Would you like to change infectivity rate to a constant?", c(Yes = "Y", No = "N")),
                                                       # If the user wants to chose an infectivity rate to a constant we let them
                                                       conditionalPanel(
                                                           condition = "input.Infection == 'Y'", 
                                                           sliderInput("infectvity", "Infectivity Rate" , 0, 1, .5)
                                                       ), 
                                                       # This button is the key, when the user presses this button 
                                                       # they run the simulation or are told to change their parameters
                                                       ))
                             )
                    ),
                    tabPanel("Results", 
                             sidebarLayout(sidebarPanel(checkboxGroupInput("lines", "What do you want to see on the plot?", c("Exposed", "Infectives", "Quarantined", "Diagnosed", "Recovered"), selected = "Exposed")), 
                                           mainPanel(plotlyOutput("test_plot")))
                    )
        )
    )
)




# Define server logic required to get our plots and 
server <- function(input, output){
    
    # The first thing we need to do is have our randomness come into play on our conditional
    # panel 
    observeEvent(input$explanation, {
      
      showModal(modalDialog(
        title = "Parameters",
        
        tags$div(
          "- bullet 1", 
          tags$br(),
          "- bullet 2",
          tags$br(),
        )
      ))
    })
      
    observeEvent(req(input$Whenstart == "R"), {
        intial <- checkcondition(runif(1), runif(1), runif(1), runif(1), runif(1))
        updateSliderInput(inputId = "delta", value = intial$delta)
        updateSliderInput(inputId = "epsilon", value = intial$epsilon)
        updateSliderInput(inputId = "lambda", value = intial$lambda)
        updateSliderInput(inputId = "sigma", value = runif(1))
        updateSliderInput(inputId = "theta", value = intial$theta)
        updateSliderInput(inputId = "gamma", value = intial$gamma)
    })
    observeEvent(req(input$Whenstart != "R"), {
        updateSliderInput(inputId = "delta", value = (15/100)*(1/21))
        updateSliderInput(inputId = "epsilon", value = (1/3)*(2/5))
        updateSliderInput(inputId = "lambda", value = (1/3)*(3/5))
        updateSliderInput(inputId = "sigma", value = 1/3)
        updateSliderInput(inputId = "theta", value = 1/3)
        updateSliderInput(inputId = "gamma", value = 1/21)
    })
    
    
    # Apply our simulation function based on the parameters the user inputs 
  
    # Create a validation parameter 

    
    
    ModelValues <- reactive({
        validate(
            need(sum(input$epsilon, input$lambda) <= 1, "The sum of Exposed -> Infected and Exposed -> Quarantined should be less than 1"), 
            need(sum(input$deathrate, input$theta) <= 1, "The sum of deathrate and Infected -> Diagnosed should be less than 1"), 
            need(sum(input$deathrate, input$theta) <= 1, "The sum of deathrate and Diagnosed -> Recovered should be less than 1")
        )
        if (input$Infection == 'Y' & input$Whenstart == 'S') {
            get_diagnosed(input$exposed, input$infectives, 50, 
                          50, 50, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k, as.numeric(input$infectvity))
        } else if (input$Infection == 'N' & input$Whenstart == 'S'){
            get_diagnosed(input$exposed, input$infectives, 50, 
                          50, 50, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k)
            
        } else if (input$Infection == 'Y' & input$Whenstart == 'M'){
            get_diagnosed(input$exposed, input$infectives, input$quarantined, 
                          input$diagnosed, input$recovered, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k, as.numeric(input$infectvity))
        } else if (input$Infection == 'N' & input$Whenstart == 'M'){
            get_diagnosed(input$exposed, input$infectives, input$quarantined, 
                          input$diagnosed, input$recovered, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k)
        } else if (input$Infection == 'Y' & input$Whenstart == 'R'){
            get_diagnosed(input$exposed, input$infectives, input$quarantined, 
                          input$diagnosed, input$recovered, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k, as.numeric(input$infectvity))
        } else if (input$Infection == 'N' & input$Whenstart == 'R'){
            get_diagnosed(input$exposed, input$infectives, input$quarantined, 
                          input$diagnosed, input$recovered, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k)
        }
    
    })
    
    # Now we have the data and can 
    
    Newdf <- reactive({ ModelValues() %>% filter(people_type %in% input$lines) })
    
    output$test_plot <- renderPlotly({
       run_animation(Newdf())
    }) 
    
   
    
    # Next create a plot of the number of people in each class over time using 
    # an animated plot. 
    
    
    # We also want to create some reactive output that tells the user what the maximum number 
    # day when the maximum 
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

