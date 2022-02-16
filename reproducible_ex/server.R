library(shiny)

### Define helper functions
# function to discern whether the variable is 'continuous'
is_cont <- function(var) {
    is.numeric(var) &
        length(unique(var)) > 2 # numeric and has more than dichotomous values
}

reset_rv <- function(rv, which = "all"){
    if(length(which)==1 & which == "all"){
        for(name in names(rv)){
            rv[[name]] <- NULL
        }
    }else{
        for(element in which){
            rv[[element]] <- NULL
        }   
    }
    return(rv)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    ## Initialize the reactive value object to store information
    rv <- reactiveValues(data = NULL, # place to save uploaded data,
                         vars = NULL, # place to save var names) 
    )
    
  
    ## File upload and inspection
    # read file whenever new upload is made
    observe({
        req(input$data)
        is.csv <-
            input$data$type == "text/csv" # check whether file is of .csv format
        feedbackDanger(inputId = "data",
                       show = !is.csv,
                       text = "File has to be in .csv format")
        req(is.csv) # read the file to the reactive object only when has correct format
        rv$data <- rio::import(input$data$datapath) %>% na.omit()
    })
    
    # Data preview
    output$rvvars <- renderPrint(rv$vars)
    
    # Update select input << THIS IS WHAT DOES NOT FUNCTION!
    observeEvent(rv$vars, {
        updateSelectInput(inputId = "vars",
                          choices = rv$vars)
    })
    
    # Reset
    observeEvent(input$`reset`, {
        req(input$`reset`)
        shinyjs::reset()
        rv <- reset_rv(rv)
    })

    observeEvent(rv$data, {
        if(is.null(rv$data)){
            rv$vars <- NULL
        }else{
            rv$vars <- colnames(rv$data)[unlist(lapply(rv$data, is_cont))]
        }
    })
})
