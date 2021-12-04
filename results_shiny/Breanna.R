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
    
    #p("I'm plain  text."),
    
    
    tabsetPanel(
        tabPanel("What is  this app doing"),
        tabPanel("Set your parameters"),
        tabPanel("Results")
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            uiOutput("user_input"),
            checkboxGroupInput("people-type")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    df <- function(e, i, q, j, r){
        days <- 400
        for (t in 1:(days - 1)) {
            e[t+1] <- e[t] + b_func(t)*((k*e[t])+i[t]) - (epsilon+lambda)*e[t]
            i[t+1] <- i[t] + epsilon*e[t] - (delta + theta)*i[t]
            q[t+1] <- q[t] + lambda*e[t] - sigma*q[t] 
            j[t+1] <- j[t] + theta*i[t] + sigma*q[t] - (delta + gamma)*j[t]
            r[t+1] <- r[t] + gammaa*j[t]
            
            t <- t + 1
        }
        
        all_data <- as.data.frame(cbind(Time = 1:days, Exposed = e, Infectives = i, Quarantined = q, Diagnosed = j))
        
        return(all_data)
    }
    
    df_filtered <- reactive({
        df <- df_user()
        #  react to what user is changing
        # Creates a list of the studies checked and then filters the data 
        # based on this information
        return(df[df$study %in% input$selected_studies,])
    }) 
    
    
    
    
    output$user_input <- renderUI({
        df <- df_user()
        tagList(
            checkboxGroupInput("selected_studies", "Studies Included",
                               choices = levels(df$study),
                               selected = levels(df$study)
            ))
    })
    
    output$distPlot <-renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
