# Assignment 2, Version 2
# Sooyoung Kim (sk9076@nyu.edu)

### Load packages
require(pacman)
pacman::p_load(
    shiny,
    shinyFeedback,
    shinyWidgets,
    shinyalert,
    tidyverse,
    ggplot2,
    magrittr,
    shinyjs,
    gt,
    gtsummary,
    waiter,
    bslib,
    sass,
    curl,
    sortable,
    ggthemr
)

### Define custom theme
theme_ks <- bslib::bs_theme(
    version = ,
    bg = "#FfFfFf",
    fg = "#183a5a",
    # dark navy (title, base font and stuff)
    primary = "#183a5a",
    # tab font color and hover block color etc
    secondary = "#353842",
    # (for messages that don't need to stand out)
    success = "#0b5b67",
    # dark green
    info = "#3e83a8",
    # ligher dark blue (text that are informative not critical)
    warning = "#EFb758",
    # mustard-ish yellow
    danger = "#C34129",
    # blood orange
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
        "<p>This application lets you fit linear regression models using the dataset of your choice.
         To run this app without any issue, you will need; </p>
                       <ul>
                          <li>a dataset in <u><b>.csv </u></b>format; and</li>
                          <li><u><b>at least ONE</u></b> continuous variable in the dataset to be used as outcome variable</li>

                        </ul>
         <p>Once you fit the model, you will be able to save the<b> summary of the results</b> and model fit statistics, represented by
         the <b>Alkaike Information Criterion (AIC)</b>. You can call the saved results from the drop-down menu whenever you want.</p>
         <p>Additionally, you can choose models from the saved list to compare their leave-one-out cross-validation errors. </p>
         <p>At any step, if you'd like to re-start by uploading a new data set, click the <b>Reset and start again</b> button on the
         upper-right corner.</p>
         You can click the <b> Start </b> button to begin. Enjoy!<br><br>"
    ),
    
    div(
        style = "display:inline-block; float:right;",
        actionButton("start", "Start",
                     style = "background-color: #0b5b67; border-color: transparent")
    )
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
    
    # Shows the first 5 rows of the uploaded dataset when successful
    HTML("<b>Data preview (first 5 rows):</b><br>"),
    div(style = 'overflow-y:scroll;',
        tableOutput("tab")),
    
    # Action button to move to the next step
    fluidRow(column(12,
                    # Button to move to the next step
                    div(
                        actionButton("1_2", "Fit a model >",
                                     style = "background-color: #0b5b67; border-color: transparent")
                    ),
                    align = "center"))
    
)

## UI to choose the model specification
step2 <- tabPanel(
    value = "fit_model",
    title = "2. Fit a liner regression model",
    
    h4("Step 2. Choose your model variable(s)"),
    fluidRow(column(
        6,
        h5("Choose one continuous outcome variable")
    ),
    column(
        6,
        h5("Choose as many predictors as you want")
    )),
    
    fluidRow(
        column(
            6,
            selectInput(
                inputId = "outcome_var",
                label = "",
                choices = NULL
            ),
            
            textOutput("cont_warning")
        ),
        column(
            6,
            multiInput(
                inputId = "predictor_set",
                label = "",
                choices = ""
            ),
            textOutput("check_predictors")
        )
    ),
    
    fluidRow(column(
        12,
        # Button to move to the previous/next step
        div(
            actionButton("2_1", "< Go back",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("2_3", "See results >",
                         style = "background-color: #0b5b67; border-color: transparent")
        ),
        align = "center"
    ))
)

## UI to see the fitted regression model
result <- tabPanel(
    value = "see_result",
    title = "3. Fitted model summary",
    
    h4("Step 3. Check the results and save them to access late"),
    
    autoWaiter(id = "model_summary",
               html = spin_loader()),
    
    # parsed out model equation
    textOutput("model_equation"),
    # model AIC value
    textOutput("model_AIC"),
    
    h5("Regression table"),
    gt_output("model_summary"),
    
    
    HTML("<br>"),
    
    fluidRow(column(
        12,
        # Button to move to the previous/next step
        div(
            actionButton("3_2_discard", "Discard and fit a new model",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("3_2_save", "Save and fit a new model",
                         style = "background-color: #0b5b67; border-color: transparent"),
            HTML("<br><br>-OR-<br><br>"),
            actionButton("3_4", "Save and go see all saved models",
                         style = "background-color: #0b5b67; border-color: transparent")
        ),
        align = "center"
    ))
    
)

## UI to see all saved models
saved <- tabPanel(
    value = "see_saved",
    title = "4. See all saved models",
    
    h4("Step 4. See all saved models fit on the current dataset"),
    
    HTML("<br>"),
    
    h5("Choose a model to see the saved summary"),
    selectInput("saved_mods",
                "",
                choices = NULL,
                width = "90%"),
    
    tags$div(style = "font-weight:bold; text-align:right;", textOutput("saved_sum_text")),
    
    fluidRow(column(
        12,
        HTML("<br>"),
        # Button to move to the previous/next step
        div(
            actionButton("4_2", "Fit a new model",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("4_5", "Compare models",
                         style = "background-color: #0b5b67; border-color: transparent")
        ),
        HTML("<br>"),
        HTML("<br>"),
        
        align = "center"
    )),
    
    textOutput("saved_AIC"),
    
    h5("Regression table"),
    gt_output("saved_summary"),
)

## UI to compare the model fit
compare <- tabPanel(
    value = "compare_cve",
    title = "5. Compare models",
    
    h4("Step 5. Compare the model fit using your choice of criteria"),
    
    h5("Choose the models you want to compare (max. 5)"),
    uiOutput("choose_compare"),
    
    fluidRow(
        column(
            6,
            h5("Fit comparison criterion"),
            radioButtons(
                inputId = "criteria",
                label = "",
                choices = c("AIC", "Leave-one-out Cross-validation")
            ),
            align = "left"
        ),
        
        column(
            6,
            div(
                style = "display:inline-block; ",
                HTML("<br><br>"),
                actionButton(
                    inputId = "start_compare",
                    label = "Compare",
                    style = "background-color: #0b5b67; border-color: transparent"
                )
            ),
            align = "right"
        )
    ),
    
    # Plot of comparison result
    plotOutput("plot_compare"),
    
    # Brief interpretation of the plot
    tags$div(style = "font-weight: bold; text-align: center;", textOutput("text_compare")),
    
    HTML("<br>"),
    
    
    
    fluidRow(column(
        12,
        # Button to move to the previous/next step
        div(
            style = "display:inline-block; ",
            actionButton("5_4", "Go back to see all models",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("5_2", "Fit a new model",
                         style = "background-color: #0b5b67; border-color: transparent"),
            style = "display:center-align;"
        ),
        align = "center"
    ))
)


### Compile each step to one tab sets
tab_steps <- navlistPanel(id = "steps",
                          intro,
                          step1,
                          step2,
                          result,
                          saved,
                          compare)


### Define UI for application
shinyUI(
    fluidPage(
        # Application title
        titlePanel("Assignment 2, Version 2"),
        h5("Fitting linear regression models"),
        
        HTML("<p>Sooyoung Kim (sk9076@nyu.edu)</p>"),
        
        # Change theme
        theme = theme_ks,
        
        tags$head(tags$style(
            # Set the color for validate() messages and tab color
            HTML(
                "
              .shiny-output-error-validation {
                color: #C34129;
              }
              .well {
                background-color: #F0E6D7;
                border-color: transparent;
                font-weight: bold;
              }
              "
            )
            
        )),
        
        
        # Set to use shinyjs
        useShinyjs(),
        
        fluidRow(column(12,
                        div(
                            actionButton("reset", "Reset and start again",
                                         style = "background-color: #C34129; border-color: transparent")
                        ),
                        align = "right")),
        
        hr(),
        
        # Sidebar with step-by-step instructions
        fluidRow(column(12, tab_steps))
    )
)