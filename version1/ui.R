# Assignment 2, Version 1
# Sooyoung Kim (sk9076@nyu.edu)

require(pacman)
pacman::p_load(shiny, shinyFeedback, shinyWidgets, DT, bslib, sass, curl)

### Define custom theme
theme_ks <- bslib::bs_theme(
    version = ,
    bg = "#FfFfFf",
    fg = "#183a5a", # dark navy (title, base font and stuff)
    primary = "#183a5a", # tab font color and hover block color etc
    secondary = "#353842", # (for messages that don't need to stand out)
    success = "#0b5b67", # dark green
    info = "#3e83a8", # ligher dark blue (text that are informative not critical)
    warning = "#EFb758", # mustard-ish yellow
    danger = "#C34129", # blood orange
    base_font = font_google("Source Sans Pro"),
    code_font = font_google("Tinos"),
    heading_font = font_google("Oswald"),
    font_scale = 0.9
)

### Define UIs for each step separately

## Brief intro page
intro <- tabPanel(
    value = "intro",
    title = "Overview",
    h4("Hello!"),
    HTML(
        "<p>This application compares 2 separate linear regression fit of the models derived from the same dataset.
         To run this app without any issue, you will need; </p>
                       <ul>
                          <li>a dataset in <u><b>.csv </u></b>format; and</li>
                          <li><u><b>at least ONE</u></b> continuous variable in the dataset to be used as outcome variable</li>

                        </ul>
         <p> Two models will share a <b> common outcome variable</b> of your choice. The model fit comparison will be
         conducted using the <b>the F-statistics</b> if two models are <u>nested</u> (meaning the predictor(s) of one
         model is a subset of the other model), or the <b>Alkaike Information Criterion (AIC)</b>
         if two models are <u>NOT</u> nested. </p>
         You can click the <b> Start </b> button to begin. Enjoy!<br>"
    ),
    
    div(style = "display:inline-block; float:right;",
        actionButton("start", "Start",
                     style="background-color: #0b5b67; border-color: transparent"))
)

## UI to upload the dataset
step1 <- tabPanel(
    value = "upload",
    title = "1. Upload data",
    
    h4("Step 1. Upload your data in .csv format"),
    # UI to upload the file with feedback to warn when format is incorrect
    useShinyFeedback(),
    fileInput(
        inputId = "data",
        label = "",
        placeholder = ".csv format only",
        multiple = F
    ),
    
    # Shows the first 5 rows of the uploaded datset when succesful
    HTML("<b>Data preview (first 5 rows):</b><br>"),
    div(style = 'overflow-y:scroll;',
        tableOutput("tab")),
    
    # Button to move to the next step
    div(style = "display:inline-block; float:right;",
        actionButton("1_2", "Next >",
                     style="background-color: #0b5b67; border-color: transparent"))
    
)

step2 <- tabPanel(
    value = "choose_outcome",
    title = "2. Select outcome variable",
    
    h4("Step 2. Choose one continuous outcome variable"),
    selectInput(
        inputId = "outcome_var",
        label = "",
        choices = NULL
    ),
    
    textOutput("cont_warning"),
    HTML("<br>"),
    
    # Button to move to the previous/next step
    div(
        style = "display:inline-block; float:right;",
        actionButton("2_1", "< Previous",
                     style="background-color: #EFb758; border-color: transparent"),
        actionButton("2_3", "Next >",
                     style="background-color: #0b5b67; border-color: transparent")
    )
    
    
)

step3 <- tabPanel(
    value = "choose_predictors",
    title = "3. Select Predictors",
    
    h4("Step 3. Choose two sets of predictors"),
    HTML("<br><b>"),
    fluidRow(column(
        6,
        multiInput(
            inputId = "predictor_set1",
            label = "Model 1",
            choices = ""
        )
    ),
    column(
        6,
        multiInput(
            inputId = "predictor_set2",
            label = "Model 2",
            choices = ""
        )
    )),
    HTML("</b>"),
    
    textOutput("check_predictors"),
    
    # Button to move to the previous/next step
    div(
        style = "display:inline-block; float:right;",
        actionButton("3_2", "< Previous",
                     style="background-color: #EFb758; border-color: transparent"),
        actionButton("3_4", "Next >",
                     style="background-color: #0b5b67; border-color: transparent")
    )
    
    
)

result <- tabPanel(
    value = "show_results",
    title = "4. Compare two models",
    
    h4("Step 4. Comparison of model fit"),
    
    HTML("<br>"),
    textOutput("mod1"),
    textOutput("mod2"),
    
    HTML("<br><b>R output</b><br>"),
    verbatimTextOutput("results"),
    
    HTML("<b>"),
    textOutput("interpret"),
    HTML("</b><br>"),
    
    # Button to move to the previous/next step
    div(
        style = "display:inline-block; float:right;",
        actionButton("4_3", "< Previous",
                     style="background-color: #EFb758; border-color: transparent"),
        actionButton("4_1", "Start again",
                     style="background-color: #0b5b67; border-color: transparent")
    )
)

### Compile each step to one tab sets
tab_steps <- navlistPanel(id = "steps",
                          intro,
                          step1,
                          step2,
                          step3,
                          result)


### Define UI for application
shinyUI(
    fluidPage(
        # Application title
        titlePanel("Assignment 2, Version 1"),
        h5("Comparing the model fit of two linear regression models"),
        
        HTML("<p>Sooyoung Kim (sk9076@nyu.edu)</p>"),
        
        hr(),
        
        # Change theme
        theme = theme_ks,
        
        tags$head(tags$style(
            # Set the color for validate() messages and tab color
            HTML("
              .shiny-output-error-validation {
                color: #C34129;
              }
              .well {
                background-color: #F0E6D7;
                border-color: transparent;
                font-weight: bold;
              }
              ")
    
        )),
    
        
        # Set to use shinyjs for 'start again'(reset) button
        useShinyjs(),
        
        # Sidebar with step-by-step instructions
        tab_steps
    )
)