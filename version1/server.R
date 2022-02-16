# Assignment 2, Version 1
# Sooyoung Kim (sk9076@nyu.edu)

pacman::p_load(shiny,
               shinyFeedback,
               shinyWidgets,
               tidyverse,
               ggplot2,
               magrittr,
               shinyjs,
               DT)

library(shiny)

### Define helper functions
# function to compare two sets of predictors (character vectors)
compare_preds <- function(pred1, pred2) {
    if (length(pred1) == 0 |
        length(pred2) == 0) {
        return("null")
    } # at least one model has no predictor
    if (setequal(pred1, pred2)) {
        return("equal")
    } # two sets are equal
    if (sum(pred1 %in% pred2) == length(pred1) |
        sum(pred2 %in% pred1) == length(pred2)) {
        return("nested")
    } # one is nested to the other
    return("different") # none of the above
}

# function to discern whether the variable is 'continuous'
is_cont <- function(var) {
    is.numeric(var) &
        length(unique(var)) > 2 # numeric and has more than dichotomous values
}

# function to parse out model equation string in a form 'outcome ~ predictors'
model_equation <- function(outcome, predictors) {
    sprintf("%s ~ %s",
            outcome,
            paste0(predictors, collapse = " + "))
}


### Define server logic required for the app
shinyServer(function(input, output, session) {
    ## File upload and inspection
    
    # initialize the reactive objective to save data
    rv <- reactiveValues(dat = NULL)
    
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
    output$tab <- renderTable({
        req(input$data)
        head(rv$data, n = 5)
    })
    
    
    ## Populate the list of potential outcome variable (continuous vars only)
    
    # Filter out continuous variables from the data
    opt_outcome <-
        reactive(colnames(rv$data)[lapply(rv$data, is_cont) %>% unlist()])
    no_contvar <- reactive(length(opt_outcome()) == 0)
    
    ## Update list of possible outcome variables to the UI
    observeEvent(rv$data, {
        req(rv$data)
        updateSelectInput(inputId = "outcome_var",
                          choices = opt_outcome())
    })
    
    # Warning when there's no cont. var in the dataset
    output$cont_warning <- renderText({
        if (no_contvar()) {
            validate(
                "The app cannot recognize any continuous variable in this dataset. Please check your .csv file or use different dataset."
            )
        }
    })
    
    
    ## Update the UI to choose predictor sets
    observeEvent(input$outcome_var, {
        req(input$outcome_var)
        # Populate the list of potential predictors based on the choice of outcome
        opt_predictor <-
            colnames(rv$data)[!colnames(rv$data) == input$outcome_var]
        # UI for model 1
        updateMultiInput(session = session,
                         inputId = "predictor_set1",
                         choices = opt_predictor)
        # UI for model 2
        updateMultiInput(session = session,
                         inputId = "predictor_set2",
                         choices = opt_predictor)
    })
    
    ## Update the comparison of two predictor sets (see the helper function compare_preds())
    compared <-
        eventReactive(
            list(input$predictor_set1, input$predictor_set2),
            compare_preds(input$predictor_set1, input$predictor_set2)
        )
    
    ## Validation step to check whether two models are null or equal
    output$check_predictors <- renderText({
        if (compared() == "null") {
            validate("Each model needs to have at least one predictor!")
        } else if (compared() == "equal") {
            validate("Two models are exactly the same. Choose different predictors!")
        } else{
            ""
        }
    })
    
    
    ## Run linear regression
    
    # Model 1
    m1_formula <- reactive({
        model_equation(input$outcome_var,
                       input$predictor_set1)
    })
    
    m1 <- eventReactive(input$`3_4`, {
        req(length(input$predictor_set1) != 0)
        lm(eval(parse(text = m1_formula())),
           data = rv$data)
    })
    
    # Model 2
    
    m2_formula <- reactive({
        model_equation(input$outcome_var,
                       input$predictor_set2)
    })
    
    m2 <- eventReactive(input$`3_4`, {
        req(length(input$predictor_set1) != 0)
        lm(eval(parse(text = m2_formula())),
           data = rv$data)
    })
    
    ## Compare the fit
    fit_compare <- reactive(if (compared() == "nested") {
        anova(m1(), m2())
    } else{
        AIC(m1(), m2())
    })
    
    ## Print results
    
    # Model formula in a text form
    output$mod1 <- renderText(paste0("Model 1: ",
                                     m1_formula()))
    output$mod2 <- renderText(paste0("Model 2: ",
                                     m2_formula()))
    
    # Fit test results
    output$results <- renderPrint({
        fit_compare()
    })
    
    # Simple interpretaton
    output$interpret <- renderText({
        req(compared() %in% c("different", "nested"))
        # For nested model
        if (compared() == "nested") {
            sprintf(
                "Model %i has %s better fit to the data than Model %i. %s(p-value %.2f)",
                ifelse(fit_compare()$RSS[1] > fit_compare()$RSS[2], 2, 1),
                ifelse(
                    fit_compare()$`Pr(>F)`[2] < 0.05,
                    "significantly",
                    ""
                ),
                ifelse(fit_compare()$RSS[1] > fit_compare()$RSS[2], 1, 2),
                ifelse(
                    fit_compare()$`Pr(>F)`[2] < 0.05,
                    "",
                    "\nHowever, the difference in fit is not statistically significant based on the F-statistics. "
                ),
                fit_compare()$`Pr(>F)`[2]
            )
        } else{
            # For non-nested model
            sprintf(
                "Model %i has better fit to the data than Model %i based on Akaike Information Criterion (AIC)",
                ifelse(fit_compare()$AIC[1] > fit_compare()$AIC[2], 2, 1),
                ifelse(fit_compare()$AIC[1] > fit_compare()$AIC[2], 1, 2)
            )
        }
    })
    
    
    ## From here to the end is the step controls (simplify this by function)
    
    observeEvent(input$start, {
        req(input$start)
        shinyjs::reset()
        rv$data <- NULL
        updateTabsetPanel(inputId = "steps", selected = "upload")
    })
    
    observeEvent(input$`1_2`, {
        req(input$`1_2`)
        updateTabsetPanel(inputId = "steps", selected = "choose_outcome")
    })
    
    observeEvent(input$`2_3`, {
        req(input$`2_3`)
        updateTabsetPanel(inputId = "steps", selected = "choose_predictors")
    })
    
    observeEvent(input$`3_4`, {
        req(input$`3_4`)
        updateTabsetPanel(inputId = "steps", selected = "show_results")
    })
    
    observeEvent(input$`2_1`, {
        req(input$`2_1`)
        updateTabsetPanel(inputId = "steps", selected = "upload")
    })
    
    observeEvent(input$`3_2`, {
        req(input$`3_2`)
        updateTabsetPanel(inputId = "steps", selected = "choose_outcome")
    })
    
    observeEvent(input$`4_3`, {
        req(input$`4_3`)
        updateTabsetPanel(inputId = "steps", selected = "choose_predictors")
    })
    
    observeEvent(input$`4_1`, {
        req(input$`4_1`)
        shinyjs::reset()
        rv$data <- NULL
        updateTabsetPanel(inputId = "steps", selected = "upload")
    })
})
