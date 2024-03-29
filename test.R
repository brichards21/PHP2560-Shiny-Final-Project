#
# Demonstration app for PHP 1560/2560: Version II
#


library(docstring)
library(tidyverse)
library(shiny)
library(tableone) # for prettier tables to display
library(kableExtra)

getData <- function(df){
    #' Returns the loaded data after converting relevant columns to factors
    #' @return the study data frame
    df$study <- as.factor(df$study)
    df$treat <- as.factor(df$treat)
    return(df)
}


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Welcome to my app!"),
    
    # Adding text
    h3("Let me tell you all about my app"),  #header (size 3)
    p("There is nothing much to tell yet..."), #paragraph

    # Adding dynamic checkboxes for which studies to include in a side panel
    sidebarPanel(
        fileInput("df_file", "Upload a file"),
        uiOutput("user_input")
    ),
    
    # Adding a main panel with a title and plot output
    mainPanel(
        img(src = "SARSmodel_img.png", height = 250, width = 400),
        actionButton("plot_desc", "Tell me about the image."),
        textOutput("meanDifference"),
        plotOutput("distPlot"),
        htmlOutput("treatmentTable")
    )
)

# Define server logic 
server <- function(input, output) {
    
    # Data preparation - will be reactive when a new file is uploaded
    df_user <- reactive({
        req(input$df_file)
        df <- read_csv(input$df_file$datapath) %>% getData()
    })
    
    # A reactive call will update anytime the input used is changed
    df_filtered <- reactive({
        # Creates a list of the studies checked and then filters the data 
        # based on this information
        df <- df_user()
        return(df[df$study %in% input$selected_studies,])
    }) 
    
    # dyanmic UI to show the studies to select from
    output$user_input <- renderUI({
        df <- df_user()
        tagList(
            checkboxGroupInput("selected_studies", "Studies Included",
                               choices = levels(df$study),  
                               selected = levels(df$study))
        )
        # update numeric input 
        
        
    })
    
    # pop-up text 
    plotPopup <- observeEvent(input$plot_desc, {
        showModal(modalDialog(title = "Plot Description", 
                    HTML("
                    - E(t) is the number of individuals exposed to SARS at a given time point t 
                    \n
                    - I(t) is the number of infected individuals at a given time point t 
                         \n
                         
                    - "))) 
    })
                            
    # Defining the output density plot
    output$distPlot <- renderPlot({
        # Creates a density plot of the measure by treatment and control groups
        df_f <- df_filtered()
        
        p <- ggplot(data = df_f) + 
            geom_density(aes(x=measure, color=treat, fill=treat), alpha=0.2) +
            labs(x="Measurement", y = "Density", 
                 title = "Distribution of Measurement by 
                 Treatment/Control Group") 
        return(p)
    })
    
    # Adding a text header with difference
    output$meanDifference <- renderText({
        # Creates a text header with the mean difference in measurement between 
        # and control groups
        
        df_f <- df_filtered()
        diff <- mean(df_f$measure[df_f$treat == 1]) - 
            mean(df_f$measure[df_f$treat == 0])
        return(paste0("The mean difference between the treatment and control 
                      group is ", round(diff,2), "."))
    })
    
    # output$treatmentTable <- renderDataTable({
    #     df_f <- df_filtered()
    # })
    
    output$treatmentTable <- reactive({
        # Creates a table with variable distributions by study
        
        # Only show a table if more than one study is selected
        req(length(input$selected_studies) > 1)

        # Summarize by study
        df_f <- df_filtered()

        # Create a table stratified by studies
        if (length(unique(df_f$study)) > 1){
            tab <- CreateTableOne(vars = c("treat", "age", "measure"),
                                  strata = c("study"), data = df_f,
                                  factorVars = c("treat"), addOverall = TRUE)
        } else{
            tab <- CreateTableOne(vars = c("treat", "age", "measure"),
                                   data = df_f, factorVars = c("treat"))
        }
        tab <- kableone(tab, format = "html") %>%
            kable_styling(
                font_size = 15,
                bootstrap_options = c("striped", "hover", "condensed")
            )
        return(tab)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
