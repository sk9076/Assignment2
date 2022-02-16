library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    h4("Upload your data in .csv format"),
    # UI to upload the file with feedback to warn when format is incorrect
    useShinyFeedback(),
    fileInput(
        inputId = "data",
        label = "",
        placeholder = ".csv format only",
        multiple = F
    ),

    # Shows rv var
    h4("rv$var"),
    verbatimTextOutput("rvvars"),
    
    # Shows the first 5 rows of the uploaded datset when succesful
    selectInput("vars",
                "Continuous variables",
                choices = NULL),
    
    actionButton("reset", "Reset and start again")
))
