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
library(docstring)
library(tidyverse)
source("ourfunctions.R")


ui <- fluidPage(
    # Our App has 4 main tabs that the user will be able to explore 
    # The first is a tab that explains the model we are implementing and how we implement it in our model
    # The second tab gives the user an overview of the model they are implementing 
    # The third tab allows the user to set their parameters 
    # The third gives them the output of a plot, table of max values they selected and text that explains
    
    # Application title
    titlePanel("The Spread of Infectious Disease"),
    
    mainPanel( 
        
        # Tab to explain the simulation, nothing but text in here 
        
        tabsetPanel(type = "tabs", 
                    tabPanel("Purpose",
                             h3("What does this app do?"),
                             p("SARS is an infectious disease that spreads 
                                via droplets spread through contact with an infected individual (Zhou et al., 2004). 
                                This disease was identified in the early 2000s and due to its high infectivity was able 
                                to spread quickly and internationally (Zhou et al., 2004). Through drastic public health 
                                measures the spread of the disease was controlled, but peaked mathematical modler's 
                                interest (Zhou et al., 2004). In order to have an idea of how this disease was spreading 
                                the authors created a mathematical model that allows for them to predict the number of 
                                indviduals that contracted SARS over a given amount of days (Zhou et al., 2004). 
                                In their model they have a multitude of parameters that they estimate based on their 
                                given data, to predict the number of indviduals diagnosed with SARS on a given day based 
                                on the starting valuesand parameter values (Zhou et al., 2004). After seeing how accurate
                                this model is we believe it could be beneficial 
                                for other similar infectious diseases. Thus we have created this app which allows for 
                                public health officials to input parameters that were estimated based on their own data 
                                to predict the number of diagnosed, exposed, infected, quarantined, and recovered 
                                indviduals after t days. 
                               "),
                             h3("How we implement this model"),
                             p("In order to create an app with the greatest utility possible, we 
                               wanted the user to be able to be able to start at the onset of an epidemic, 
                               the middle of one or if the user is unsure of the parameters they start in the middle 
                               with ranomly generated parameters. The number of people in each of the model
                               comparatments when starting in the middle or random is the same as given in the paper (Zhou et al., 2004).
                               We make an assumption that when the pandemic is starting that there will be 50 indviduals in the 
                               quarantine, diagnosed and recovered compartments, but allow the user to change this value, which we cap at 100."),
                             h3("User Guide"),
                             p("First things first, this model is complicated, with a lot of parameters, so to get a better picture 
                               of how this works please click on the Visual Representation tab, which allows for 
                               you to see the model drawn out from the Zhou et al., paper with a button that tells you about the
                               parameters (2004). After you have a good understanding of the model and what we refer to as these 
                               parameters please go to the parameters tab and select when you want to begin your simulation, start, middle, or 
                               a simulation with random parameters. Adjust the inputs in the parramters tab to fit your data or, if you want you
                               want to simulate using the parameters Zhou. et al, estimated  leave them as the defaults (2004). Once 
                               you're all set please hit the run simulation button, and go to the results tab this will 
                               display an animated plot and table that contains the day with the most indviduals
                               for the compartments you want to explore. If you want to see multiple lines at one time it is 
                               best to check all lines at once, if you check a new compartment while the 
                               simiulation is running it will reload the graph in the middle 
                               of the animation. If you want to update this simulation then 
                               return to the parameters tab and adjust and once you're ready hit the run simulation 
                               button and you will get new results."), 
                             p("Citation"),
                             p("Zhou, Y., Ma, Z., &amp; Brauer, F. (2004). 
                     A discrete epidemic model for SARS transmission and control in China. 
                     Mathematical and Computer Modelling, 40(13), 1491–1506. https://doi.org/10.1016/j.mcm.2005.01.007 "),
                             p("This app was created by Breanna Richards, Nancy Liu and Tim Hedspeth for PHP 2560")
                    ), 
                    
                    # This next tab gives the visualization of the model and explanation of the paramters 
                    tabPanel("Visual Representation",
                             h3("What does this model look like"), 
                             img(src = "SARSmodel_img.png", height = 350, width = 787),
                             p("Image from Zhou et al., 2004"), 
                             actionButton("explanation", "What does this mean?")
                    ), 
                    
                    # This panel allows the user to modify the parameters of the model which are implemented
                    # in the functions we created and updates the output 
                    tabPanel("Set Your Parameters", 
                             selectInput("Whenstart", "When do you want to start", c(Start = "S", Middle = "M", Random ="R")), 
                             
                             # We want a user to get different input options based on when they are starting
                             # We use a conditional panel to check when the user wants to start and change the 
                             # compartment values based on this 
                             conditionalPanel(
                                 condition = "input.Whenstart == 'M'||input.Whenstart == 'R'", 
                                 sidebarPanel(numericInput("exposed1", "Number of Individuals Exposed to COVID",
                                                           477, min = 0),
                                              numericInput("infectives1",
                                                           "Number of Individuals who are Infective",
                                                           286, min = 0), 
                                              numericInput("quarantined1",
                                                           "Number of Individuals in Quarantine",
                                                           191, min = 0),
                                              
                                              numericInput("diagnosed1",
                                                           "Number of Individuals Diagnosed with COVID",
                                                           848, min = 0),
                                              
                                              numericInput("recovered1",
                                                           "Number of Individuals Recovered from COVID",
                                                           1213, min = 0))
                                 
                             ),
                             conditionalPanel(
                                 condition = "input.Whenstart == 'S'", 
                                 sidebarPanel(numericInput("exposed2", "Number of Individuals Exposed to COVID",
                                                           411, min = 0),
                                              numericInput("infectives2",
                                                           "Number of Individuals who are Infective",
                                                           286, min = 0), 
                                              numericInput("quarantined2",
                                                           "Number of Individuals in Quarantine",
                                                           50, min = 0, max = 100),
                                              
                                              numericInput("diagnosed2",
                                                           "Number of Individuals Diagnosed with COVID",
                                                           50, min = 0, max = 100),
                                              
                                              numericInput("recovered1",
                                                           "Number of Individuals Recovered from COVID",
                                                           50, min = 0, max = 100), 
                                              p("We assume that 50 people are in these categories at the start, 
                                                but you may update them, since this is the start we limit to
                                                100 in these categories"))
                                 
                             ),
                             
                             # The parameters can be changed for any of the start times and once this 
                             mainPanel(fluidRow(column(width=5, offset = 0,
                                                       sliderInput("delta", HTML("<p>Death Rate (&delta;)</p>") , 0, 1, (15/100)*(1/21)), 
                                                       sliderInput("epsilon", HTML("<p>Exposed -> Infective (&epsilon;)</p>") , 0, 1, (1/3)*(2/5)),
                                                       sliderInput("lambda", HTML("<p>Exposed -> Quarantined (&lambda;)</p>") , 0, 1, (1/3)*(3/5)),
                                                       sliderInput("sigma", HTML("<p>Quarantined -> Diagnosed (&sigma;)</p>") , 0, 1, 1/3),
                                                       sliderInput("theta", HTML("<p>Infected -> Diagnosed (&theta;)</p>") , 0, 1, 1/3)), 
                                                column(width = 5, offset = 2,
                                                       sliderInput("gamma", HTML("<p>Diagnosed -> Recovered (&gamma;)</p>") , 0, 1, 1/21), 
                                                       sliderInput("k", "Infectivity Fraction (k)", 0, 1, .1),
                                                       p("This model assumes that infectivity is a function of time,
                                                         but you can make it constant if you would like."), 
                                                       radioButtons("Infection", "Would you like to change infectivity rate to a constant?", c(Yes = "Y", No = "N"), selected = "N"),
                                                       # If the user wants to change the infectivity rate to a constant we let them, but default to 
                                                       # using the function given by Zhou et al 
                                                       conditionalPanel(
                                                           condition = "input.Infection == 'Y'", 
                                                           sliderInput("infectvity", "Infectivity Rate" , 0, 1, .5)
                                                       ), 
                                                       # This button is the key, when the user presses this button 
                                                       # they run the simulation or are told to change their parameters if they 
                                                       # dont work for the model
                                                       p("Are you ready?"),
                                                       actionButton("RunSim", "Run Simulation")), 
                             ))
                    ),
                    
                    # This panel will display the results in an animated plot and will give a table of the max values 
                    tabPanel("Results", 
                             sidebarLayout(sidebarPanel(checkboxGroupInput("lines", "What compartments do you want to see on the plot?", c("Exposed", "Infectives", "Quarantined", "Diagnosed", "Recovered"), selected = "Exposed")), 
                                           mainPanel(tags$br(),
                                                     p("Please note that the graph may take a minute to load, reset and add a new compartment to the plot"),
                                                     plotlyOutput("plot"), 
                                                     br(),
                                                     textOutput("title"), 
                                                     br(),
                                                     tableOutput("df")))
                    )
        )
    )
)




# Define server logic required to get our plots and 
server <- function(input, output){
    
    # This shows the parameters of the model in the second tab when they click the action button What does this mean
    observeEvent(input$explanation, {
        
        showModal(modalDialog(
            title = "Parameters",
            
            tags$div(
                "The boxes correspond to the number of individuals in a certain group at a certain time t",
                tags$br(),
                
                HTML("<p>- <strong>E(t)</strong> refers to the number of individuals in the exposed group at time t</p>"), 
                HTML("<p>- <strong>I(t)</strong> refers to the number of individuals in the infectives group at time t</p>"),
                HTML("<p>- <strong>Q(t)</strong> refers to the number of individuals in the quarantined group at time t</p>"),
                HTML("<p>- <strong>J(t)</strong> refers to the number of individuals in the diagnosed group at time t</p>"),
                HTML("<p>- <strong>R(t)</strong> refers to the number of individuals in the recovered group at time t</p>"),
                tags$br(),
                "Rate Parameters:",
                tags$br(),
                HTML("<p>- <strong>&beta;(t)</strong> refers to the transmission rate at time t</p>"),
                
                HTML("<p>- <strong>&delta;</strong> refers to the rate of infected and diagnosed individuals dying</p>"),
                
                HTML("<p>- <strong>&epsilon;</strong> refers to the rate of exposed individuals moving into the infectives group</p>"),
                
                HTML("<p>- <strong>&lambda;</strong> refers to the rate of exposed individuals moving into the quarantined group</p>"),
                
                HTML("<p>- <strong>&sigma;</strong> refers to the rate of quarantined individuals moving into the diagnosed group</p>"),
                
                HTML("<p>- <strong>&theta;</strong> refers to the rate of infected individuals moving into the diagnosed group</p>"),
                
                HTML("<p>- <strong>&gamma;</strong> refers to the rate of diagnosed individuals moving into the recovered group</p>"),
                
                HTML("<p>- <strong>k</strong> refers to the infectivity fraction for the exposed individuals compared to individuals in the infectives group</p>"),
                
                tags$br(),
                "All parameters are positive and the following inequalities hold:",
                tags$br(),
                HTML("<p>- 0 < &epsilon; + &lambda; < 1</p>"),
                HTML("<p>- 0 < &delta; + &theta; < 1</p>"),
                HTML("<p>- 0 < &sigma; < 1</p>"),
                HTML("<p>- 0 < &delta; + &gamma; < 1</p>")
            )
        ))
    })
    
    
    
    # The first thing we need to do is have our randomness come into play on our conditional
    # panel, the model assumes that some parameters cant have a sum greater than 1  so we created 
    # a function to validate this for our random values 
    observeEvent(req(input$Whenstart == "R"), {
        
        # check the conditions using our function to make sure the random values are valid for the mode 
        intial <- checkcondition(runif(1), runif(1), runif(1), runif(1), runif(1))
        updateSliderInput(inputId = "delta", value = intial$deathrate)
        updateSliderInput(inputId = "epsilon", value = intial$epsilon)
        updateSliderInput(inputId = "lambda", value = intial$lambda)
        updateSliderInput(inputId = "sigma", value = runif(1))
        updateSliderInput(inputId = "theta", value = intial$theta)
        updateSliderInput(inputId = "gamma", value = intial$gamma)
    })
    
    # If we're not in random we want to default back to the parameters set by Zhou. et al, 2004 
    observeEvent(req(input$Whenstart != "R"), {
        updateSliderInput(inputId = "delta", value = (15/100)*(1/21))
        updateSliderInput(inputId = "epsilon", value = (1/3)*(2/5))
        updateSliderInput(inputId = "lambda", value = (1/3)*(3/5))
        updateSliderInput(inputId = "sigma", value = 1/3)
        updateSliderInput(inputId = "theta", value = 1/3)
        updateSliderInput(inputId = "gamma", value = 1/21)
    })
    
    
    
    # Apply our simulation function based on the parameters the user inputs 
    
    
    ModelValues <- eventReactive(input$RunSim, {
        
        # First validate that the model assumptions in terms of addition of two parmaters are met 
        
        validate(
            need(sum(input$epsilon, input$lambda) <= 1, "The sum of Exposed -> Infected and Exposed -> Quarantined should be less than 1"), 
            need(sum(input$deathrate, input$theta) <= 1, "The sum of Death Rate and Infected -> Diagnosed should be less than 1"), 
            need(sum(input$deathrate, input$gamma) <= 1, "The sum of Death Rate and Diagnosed -> Recovered should be less than 1")
        )
        
        # We now check to see what time and infection rate the users have 
        # This allows us to run our model for the parameters set, 
        # we must check when they're starting and what they're doing with regard to the infectvitivty rate 
        # Each calls the function we wrote for the Zhou et al model
        
        if (input$Infection == 'Y' & input$Whenstart == 'S') {
            get_diagnosed(input$exposed2, input$infectives2, input$quarantined2, 
                          input$diagnosed2, input$recovered2, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k, as.numeric(input$infectvity))
            
        } else if (input$Infection == 'N' & input$Whenstart == 'S'){
            get_diagnosed(input$exposed2, input$infectives2, input$quarantined2, 
                          input$diagnosed2, input$recovered2, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k)
            
        } else if (input$Infection == 'Y' & input$Whenstart == 'M'){
            get_diagnosed(input$exposed1, input$infectives1, input$quarantined1, 
                          input$diagnosed1, input$recovered1, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k, as.numeric(input$infectvity))
            
        } else if (input$Infection == 'N' & input$Whenstart == 'M'){
            get_diagnosed(input$exposed1, input$infectives1, input$quarantined1, 
                          input$diagnosed1, input$recovered1, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k)
            
        } else if (input$Infection == 'Y' & input$Whenstart == 'R'){
            get_diagnosed(input$exposed1, input$infectives1, input$quarantined1, 
                          input$diagnosed1, input$recovered1, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k, as.numeric(input$infectvity))
            
        } else if (input$Infection == 'N' & input$Whenstart == 'R'){
            get_diagnosed(input$exposed1, input$infectives1, input$quarantined1, 
                          input$diagnosed1, input$recovered1, input$epsilon,
                          input$lambda, input$delta, 
                          input$theta, input$sigma, input$gamma, input$k)
        }
        
    })
    
    
    # The step above returns us a data frame with all the information we get from the model
    # Since the user can pick what compartments they want to see we create a data frame with just this info
    
    Newdf <- reactive({ ModelValues() %>% filter(people_type %in% input$lines) })
    
    # Once we have the data we now create our animated plot of the data  
    
    output$plot <- renderPlotly({
        run_animation(Newdf())
    }) 
    
    # Create a caption for the plot when the Run simulation button is hit 
    
    output$title <- renderText({
        Newdf()
        return(paste0("The figure above shows the number of people in each selected group over the span of 400 days
                      while the table below gives the day which has the most indviduals in each group.")) })
        
    
    
    # Create a table that tells the user about the maximum number of people in each selected 
    # group and the day that it occurs using a function we created 
    
    output$df <- renderTable(
        expr = create_table(Newdf()),
        align = 'c',
        width = "100%"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)