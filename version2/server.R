# Assignment 2, Version 2
# Sooyoung Kim (sk9076@nyu.edu)

### Load packages
require(pacman)
pacman::p_load(shiny,
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
               sortable, 
               ggthemr, 
               caret, 
               boot)

### Set plot theme
ggthemr("fresh")


### Define helper functions

# function to discern whether the variable is 'continuous'
is_cont <- function(var) {
    is.numeric(var) &
        length(unique(var)) > 2 # numeric and has more than dichotomous values
}

# function to parse out model equation string in a form 'outcome ~ predictors'
model_equation <- function(outcome, predictors) {
    sprintf("%s ~ %s",
            outcome,
            paste0(sort(predictors), collapse = " + "))
}

# function to reset the regression results temporarily cached
reset_vars <- function(default){
    shinyjs::reset("outcome_var")    
    shinyjs::reset("predictor_set")
}

# function to reset reactive val object to NULL
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

# check whether real model call is made
is.real.call <- function(rv, saved_mods){
    !is.null(rv$res) & saved_mods != "" & 
        !is.null(saved_mods) &
        saved_mods %in% unlist(lapply(rv$res, function(x) unlist(x[[1]])))
}

# function to save fitted linear model
save_model <- function(res, mod){
    # check if is already saved
    if(mod[[1]] %in% unlist(lapply(res, function(x) x[[1]]))){
        shinyalert(
            title = "Model already exists",
            text = "This model already exists in the saved list. Choose different model to save.",
            type = "info"
        )
        return(res)
    }else{
        # save
        if(is.null(res)){
            res <- list(mod)
        }
        else{
            res <- append(res, list(mod))
        }        
        
        return(res)
    }
    
}

### Define server logic required for the app
shinyServer(function(input, output, session) {
    
    ## Initialize the reactive value object to store information
    rv <- reactiveValues(data = NULL, # place to save uploaded data
                         opt_outcome = NULL, # place to filter out continuous vars
                         res = NULL # place to save results
    ) 
    
    temp <- reactiveValues(res = NULL, # place to temporarily save fitted model before saving
                           mod = NULL, # place to temporarily hold results
                           chosen_outcome = NULL
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
    output$tab <- renderTable({
        req(input$data)
        head(rv$data, n = 5)
    })
    
    
    ## Populate the list of potential outcome variable (continuous vars only)
    
    # Filter out continuous variables from the data
    observeEvent(rv$data, {
        if(is.null(rv$data)){
            rv$opt_outcome <- ""
        }else{
            rv$opt_outcome <- colnames(rv$data)[unlist(lapply(rv$data, is_cont))]
        }
        updateSelectInput(inputId = "outcome_var",
                          choices = rv$opt_outcome
        )
    }, ignoreNULL=F)
    
    no_contvar <- eventReactive(rv$opt_outcome,
                                {length(rv$opt_outcome) == 0})
    

    # Warning when there's no cont. var in the dataset
    output$cont_warning <- renderText({
        if(is.null(input$outcome_var)|input$outcome_var==""){
            validate(
                "Choose at least ONE outcome variable."
            )
        }
        if (no_contvar()) {
            validate(
                "The app cannot recognize any continuous variable in this dataset. Please check your .csv file or use different dataset."
            )
        }
    })
    
    
    ## Update the chosen outcome
    observeEvent(input$outcome_var, {
        temp$chosen_outcome <- input$outcome_var
    })
    
    ## Update the UI to choose predictor sets
    observeEvent(temp$chosen_outcome, {
        if(is.null(temp$chosen_outcome)){
            opt_predictor <- ""
        }else{
            # Populate the list of potential predictors based on the choice of outcome
            opt_predictor <-
                colnames(rv$data)[!colnames(rv$data) == temp$chosen_outcome]
        }
        
        # UI for model 1
        updateMultiInput(session = session,
                         inputId = "predictor_set",
                         choices = opt_predictor)
    }, ignoreNULL=F)
    
    
    
    ## Validation step to check there is at least one predictor
    output$check_predictors <- renderText({
        if (length(input$predictor_set)==0) {
            validate("The model needs to have at least one predictor!")
        } else{
            ""
        }
    })
    
    
    ## Run linear regression
    
    model_formula <- reactive({
        model_equation(temp$chosen_outcome,
                       input$predictor_set)
    })
    
    mod <- eventReactive(input$`2_3`, {
        req(length(input$predictor_set) != 0 & !is.null(temp$chosen_outcome))
        glm(eval(parse(text = model_formula())),
            data = rv$data)
    })
    
    observe({
        req(mod())
        temp$res <- list(model_formula(),
                         AIC(mod()),
                         mod())
    }
    )
    
    ## Print results
    
    # Model formula in a text form
    output$model_equation <- renderText({
        req(temp$res)
        paste0("Model equation: ",
               temp$res[[1]])
    })
    
    # Regression table 
    output$model_summary <- render_gt({
        req(temp$res)
        as_gt(tbl_regression(temp$res[[3]]))
    },
    align = "left")
    
    # model AIC
    output$model_AIC <- renderText({
        sprintf("Model AIC: %.2f", temp$res[[2]])
    })
    
    ## Updated the list of saved models
    observeEvent(rv$res, {
        if(!is.null(rv$res)){
        updateSelectInput(
            inputId = "saved_mods",
            choices = lapply(rv$res, function(x) x[[1]]),
            selected = lapply(rv$res, function(x) x[[1]])[1]
        )}else{
            updateSelectInput(
                inputId = "saved_mods",
                choices = ""
            )
        }
    }, ignoreNULL=F)
    
    output$saved_sum_text <- renderText({
        req(rv$res)
        sprintf("Total %i models saved", length(rv$res))
    })
    
    
    ## Call saved model
    observe({
        # read the file to the reactive object only when it is the right call
        req(input$saved_mods)
        req(is.real.call(rv, input$saved_mods)) 
        temp$mod <- rv$res[[which(unlist(lapply(rv$res, function(x) x[[1]]== input$saved_mods)))]]
    })
    
    output$saved_AIC <- renderText({
        require(temp$mod)
        sprintf("Model AIC: %.2f", temp$mod[[2]])
    })
    output$saved_summary <- render_gt({
        require(temp$mod)
        if(!is.null(temp$mod)){
            as_gt(tbl_regression(temp$mod[[3]]))
        }else{NULL}
    },
    align = "left")
    
    ## populate the UI to compare models when there are >1 saved models
    output$choose_compare <- renderUI({
        if(length(rv$res)>1){
            bucket_list(
                header = "",
                group_name = "grp_mod",
                orientation = "horizontal",
                # all available models
                add_rank_list(
                    input_id = "from",
                    text = "Drag from here",
                    labels = lapply(rv$res, function(x) x[[1]])
                ),
                add_rank_list(
                    input_id = "to",
                    text = "To here",
                    labels = NULL
                )
            )
        }
    })
    
    ## If less than 2 model is chosen, do not hold the white space for the plot (it's ugly)
    observe({
        input$start_compare
        chosen <- isolate(input$to)
        if (length(chosen) <= 1) {
            hide("plot_compare")
        } else {
            show("plot_compare")
        }
    })
    
    ## Plot summarizing the model comparison
    output$plot_compare <- renderPlot({
        input$start_compare # dependent on this action button
        
        chosen <- isolate(input$to)
        criteria <- isolate(input$criteria)
        require(length(chosen)>1)
        
        chosen_mod <- rv$res[unlist(
            lapply(rv$res, 
                   function(x, chosen){
                       x[[1]] %in% chosen
                   },
                   chosen = chosen
            ))
        ]
        
        data.frame(
            x = paste0("Model ", 1:length(chosen)),
            y = if(criteria == "AIC"){
                unlist(lapply(chosen_mod, function(x) x[[2]]))
            }else{
                unlist(lapply(chosen_mod, function(x, dat){
                    cv.glm(data = dat, 
                           glmfit = glm(eval(parse(text=x[[1]])), data =dat))$delta[2]},
                    dat = rv$data))
                
            }
        ) %>% ggplot(aes(x, y)) + geom_point(aes(color = x), size = 5) +
            scale_color_discrete(
                guide = guide_legend(ncol = 1),
                labels = paste0("Model ", 1:length(chosen), ": ", unlist(lapply(chosen_mod, function(x) x[[1]])))
            )+
            ylab(ifelse(criteria == "AIC", "AIC", "LOOCV Error"))+
            theme(legend.title = element_blank(),
                  legend.position = "top") +
            ggtitle(sprintf("Model fit comparison using %s", ifelse(criteria == "AIC", "AIC", "LOOCV")))
        
        
    })
    
    
    ## Text summarizng the model comparison
    output$text_compare <- renderText({
        input$start_compare # dependent on this action button
        
        chosen <- isolate(input$to)
        criteria <- isolate(input$criteria)
        #req(length(chosen)>1)
        if(length(chosen)<=1){
            validate("Choose at least two models to compare")
        }else{
            chosen_mod <- rv$res[unlist(
                lapply(rv$res, 
                       function(x, chosen = isolate(input$to)){
                           x[[1]] %in% chosen
                       }
                ))
            ]
            
            sprintf("The best model based on %s is %s",
                    ifelse(criteria == "AIC", "AIC", "LOOCV"),
                    chosen_mod[[which(unlist(lapply(chosen_mod, function(x,y){
                        x[[2]]==min(y)
                    }, y = unlist(lapply(chosen_mod, function(x) x[[2]])))))
                    ]][[1]]
            )
        }
    })
    
    ## From here to the end is the step controls 
    observeEvent(input$`reset`, {
        req(input$`reset`)
        shinyjs::reset()
        rv <- reset_rv(rv)
        temp <- reset_rv(temp)
        updateTabsetPanel(inputId = "steps", selected = "upload")
    })
    
    observeEvent(input$start, {
        req(input$start)
        shinyjs::reset()
        rv <- reset_rv(rv)
        temp <- reset_rv(temp)
        
        updateTabsetPanel(inputId = "steps", selected = "upload")
    })
    
    observeEvent(input$`1_2`, {
        req(input$`1_2`)
        if(is.null(rv$data)){
        shinyalert(
            title = "Data not uploaded",
            text = "Please upload data before proceeding",
            type = "warning"
        )}else{
        updateTabsetPanel(inputId = "steps", selected = "fit_model")
        }
    })
    
    observeEvent(input$`2_1`, {
        req(input$`2_1`)
        updateTabsetPanel(inputId = "steps", selected = "upload")
    })
    
    observeEvent(input$`2_3`, {
        req(input$`2_3`)
        if(length(input$predictor_set) == 0 | is.null(temp$chosen_outcome)){
            shinyalert(
                title = "Model not specified",
                text = "Choose an outcome AND at least one predictor to proceed.",
                type = "warning"
            )
        }else{
            updateTabsetPanel(inputId = "steps", selected = "see_result")
        }
    })
    
    observeEvent(input$`3_2_discard`, {
        req(input$`3_2_discard`)
        reset_vars(NULL)
        temp <- reset_rv(temp, c("res", "chosen_outcome"))
        
        
        updateTabsetPanel(inputId = "steps", selected = "fit_model")
    })
    
    observeEvent(input$`3_2_save`, {
        req(input$`3_2_save`)
        rv$res <- save_model(rv$res, temp$res)
        reset_vars(NULL)
        temp <- reset_rv(temp, c("res", "chosen_outcome"))
        
        
        updateTabsetPanel(inputId = "steps", selected = "fit_model")
    })
    
    observeEvent(input$`3_4`, {
        req(input$`3_4`)
        rv$res <- save_model(rv$res, temp$res)
        reset_vars(NULL)
        temp <- reset_rv(temp, c("res", "chosen_outcome"))
        
        
        updateTabsetPanel(inputId = "steps", selected = "see_saved")
    })
    
    observeEvent(input$`4_2`, {
        req(input$`4_2`)
        reset_vars(NULL)
        temp <- reset_rv(temp, c("res", "chosen_outcome"))
        
        updateTabsetPanel(inputId = "steps", selected = "fit_model")
    })
    
    observeEvent(input$`4_5`, {
        req(input$`4_5`)
        if(length(rv$res)<2){
            shinyalert(
                title = "Insufficient models to compare",
                text = "Choose at least 2 models to compare",
                type = "warning"
            )
        }else{
            reset_vars(NULL)
            temp <- reset_rv(temp, c("res", "chosen_outcome"))
            
            updateTabsetPanel(inputId = "steps", selected = "compare_cve")
        }
    })
    
    observeEvent(input$`5_4`, {
        req(input$`5_4`)
        reset_vars(NULL)
        temp <- reset_rv(temp, c("res", "chosen_outcome"))
        
        updateTabsetPanel(inputId = "steps", selected = "see_saved")
    })
    observeEvent(input$`5_2`, {
        req(input$`5_2`)
        reset_vars(NULL)
        temp <- reset_rv(temp, c("res", "chosen_outcome"))
        
        updateTabsetPanel(inputId = "steps", selected = "fit_model")
    })
})
