#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


ui <- fluidPage(

    # Application title
    titlePanel("How infectious disease spreads"),
    mainPanel( 
        tabsetPanel(type = "tabs", 
                    tabPanel("What is this app doing",
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
                     Mathematical and Computer Modelling, 40(13), 1491–1506. https://doi.org/10.1016/j.mcm.2005.01.007 ")),
      tabPanel("Set your parameters", 
               selectInput("Dropdown", "When do you want to start", c("Day 1", "Middle of Pandemic")), 
               textInput("Population", "What is your popultion size?" , "Enter a numeric value"),
               textInput("Infected", "How many people are infected?" , "Enter a numeric value"), 
               textInput("Exposed", "How many people have been exposed?" , "Enter a numeric value")
      ), 
      tabPanel("Results")
        )
    
  )
        )



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #reactive for results
    #observe is what we observe that does not depend ion anything 

    
}

# Run the application 
shinyApp(ui = ui, server = server)
