# Load packages
if(!require(shiny)){install.packages('shiny')}
if(!require(DT)){install.packages('DT')}
if(!require(lavaan)){install.packages('lavaan')}
if(!require(simsem)){install.packages('simsem')}
if(!require(rhandsontable)){install.packages('rhandsontable')}
if(!require(tidyr)){install.packages('tidyr')}
if(!require(snow)){install.packages('snow')}
if(!require(pbapply)){install.packages('pbapply')}
if(!require(readr)){install.packages('readr')}
library(shiny); library(lavaan);library(snow);
library(rhandsontable); library(tidyr);library(simsem);library(DT);
library(pbapply);library(readr);

# Define UI for application
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),

  #Jumbotrons are pretty, they make nice headers
  tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
           tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', "How Many Peabody's Quadruplets do I need for my model?"),
           p('Power, Bias and Coverage Simulations to know the Minimum Number of Quadruplets to build')
  ),

  # *- Set button and text colors -------------------------------------------
  
  tags$head(
    tags$style(HTML('#clicks1{background-color:#428BCA; color: white}',
                    '#tab2to3{background-color:#428BCA; color: white}',
                    '#tab3to4{background-color:#428BCA; color: white}',
                    '#autoRes{background-color:#4CAF50; color: white}',
                    '#simu{background-color:#428BCA; color: white}',
                    '#step2_para_warning{color: red}',
                    '#step2_para_all{color: red}',
                    '#step2_para_success{color: blue}',
                    '#step2_model_warning{color: red}',
                    '#step4_model_warning{color: red}',
    ))
  ),
  
  # *- Set app header -------------------------------------------------------
  
  fluidRow(
    column(width = 12,
           HTML(paste(tags$strong("quadSim", style = "font-size:40px;"),
                      "v0.0.1")),
           h4("Simulations for the Minimum Number of Quadruplets"),
           HTML(paste(
             "Peabody with MIMIC as a theoretical sound control for social desirability: simulations for Likert and Forced-Choice.", 
             sep = " ")),
           style = "padding-bottom: 10px;"
    )
  ),

  # *- Set sidebar with "how to" guide for the app --------------------------
  
  sidebarLayout(
    sidebarPanel(
      h4("How to Use This App"),
      
      # Step 1
      tags$div(
        HTML(paste(
          tags$b('Step 1. Specify Population Model'), 
          '. Enter your analysis model using lavaan syntax. Examples of ',
          'formula types that define a structural equation model include ', 
          '(more information',
          tags$a(href = "http://lavaan.ugent.be/tutorial/syntax1.html",
                 " here"), "):",
          sep = "")
        )
      ),
      
      tags$ul(
        tags$li(tags$code("=~"), '"is measured by"'), 
        tags$li(tags$code("~"), '"is regressed on"'), 
        tags$li(tags$code("~~"), '"is correlated with"'),
        tags$li(tags$code("|"), '"has the thresholds"')
        
      ),
      p('Click "Set Model" to set the analysis model and continue to Step 2.'
      ),
      
      # Step 2
      tags$div(
        HTML(paste(
          tags$b('Step 2. Set Parameter Values for the population model'), 
          '. Fill in the "Value" column with the ',
          'population value for each parameter, then check the boxes in the ',
          '"Effect" column for the parameters you would like to detect. ',
          'Click "Confirm Parameter Values" to continue to Step 3.',
          sep = "")),
        style = "padding-bottom: 10px;"
      ),
      
      
      # Step 3
      tags$div(
        HTML(paste(
          tags$b('Step 3. Estimate Power'), 
          '. Set your sample size, number of simulations and other parameters, then click ',
          '"Estimate Power via Simulations" to run your power analysis.',
          sep = "")
        )
      )
    )
    ,
    
    # *- Set main interface ---------------------------------------------------
    
    mainPanel(
      tabsetPanel(
        id = "tabby",
        
        
        # *--- Step 1 -------------------------------------------------------------
        
        tabPanel(
          "1. Specify Model", value = "tab1",
          br(),
          column(
            8,
            wellPanel(
              
              # Create text box for users to enter analysis model
              textAreaInput(
                inputId = "text1",
                label = "Enter your analysis model below (with fixed parameters and thresholds, when applied):",
                
                # Pre-fill with sample syntax
                 value = "
f1 =~ it1 + it2 + it3 + quadruplets1 + quadruplets2 + quadruplets3 + quadruplets4
f2 =~ it4 + it5 + it6
SocialDesirability =~ quadruplets1 + quadruplets2 + quadruplets3 + quadruplets4
                   
it4 ~ SocialDesirability
it5 ~ SocialDesirability
it6 ~ SocialDesirability

f1 ~~ 0*SocialDesirability
f2 ~~ 0*SocialDesirability

f1 ~~ f2

it1 | t1+t2+t3+t4
it2 | t1+t2+t3+t4
it3 | t1+t2+t3+t4
it4 | t1+t2+t3+t4
it5 | t1+t2+t3+t4
it6 | t1+t2+t3+t4
quadruplets1 | t1+t2+t3+t4
quadruplets2 | t1+t2+t3+t4
quadruplets3 | t1+t2+t3+t4
quadruplets4 | t1+t2+t3+t4
",
                # Allow users to resize text box
                resize = "both", rows = 12, cols = 80),
              radioButtons(
                  inputId = "stdlv.radio", 
                  label = "How would you like to set the scale of your latent factors?",
                  choices = list("Fix variances of latent variables" = 1,
                                 "Fix first factor loadings" = 2),
                  selected = 1),
            
              helpText('We recommend starting with a medium number of',
                       'quadruplets (e.g., 4 quadruplets = 12 items) to get a rough',
                       'estimate of power before trying it',
                       'with a higher or lower number of quadruplets',
                       '(e.g., 1 or 48 quadruplets).'),
              # Add button for model setting
              actionButton(
                inputId = "clicks1",
                label = "Set Model")
            )
          )
        ),
        
        
        
       
        
        # *--- Step 2 -------------------------------------------------------------
        
        tabPanel(
          "2. Set Parameter Values", value = "tab2",
          
          # Display instructions
          helpText('Your model parameter table is shown below.',
                   'You can use it like an Excel spreadsheet.',
                   '(e.g., double-click on a "Value" cell to edit).',
                   br(),
                   'Not sure what values to set the parameters at?',
                   tags$ul(
                     tags$li('If you need help with setting factor',
                             'loadings or latent regression coefficients,', 
                             'click the "Help" tab for suggestions.')
                   )),
          
          # Display interactive parameter table
          rHandsontableOutput("AnalysisMod"),
          
          # Add buttons for various functions (see server() below for details)
          actionButton(inputId = "tab2to1", 
                       label = "Back to Step 1 (Values are Saved)"),

          actionButton(inputId = "tab2to3", 
                       label = "Confirm Parameter Values"),
          
          # Display warning on model detection
          textOutput("step2_model_warning"),

          # Display warning on parameter selection
          textOutput("step2_para_warning"),
          
          # Display warning on parameter values
          textOutput("step2_para_all"),
          
          # Display success on parameter selection
          textOutput("step2_para_success")
          
        ),
        
        # *--- Step 3 -------------------------------------------------------------
        
        tabPanel(
          "3. Estimate Power with Simulations", value = "tab3",
          br(),
          column(12,
                 wellPanel(
                   
                   # Simulation setup
                   fluidRow(
                     column(4,
                            numericInput(inputId = "sampleN", 
                                         label = "Set your sample size", 
                                         value = 500, min = 1, step = 1)),
                     column(4,
                            numericInput(inputId = "p_alpha", 
                                         label = "Set your alpha level", 
                                         value = .05, min = .00001, max = 1)),
                     column(4,
                            numericInput(inputId = "seed",
                                         label = "Set seed for simulations",
                                         value = 17052023))),
                   numericInput(inputId = "ksim", 
                               label = "Set number of simulations",
                               value = 100, min = 1, step = 10, 
                               max = 10000),
                  
                  
                  selectInput("estimators", "What's your desired estimator? ", c("WLSMV","DWLS","ML","GLS","WLS","ULS","DLS","PML","MLM","MLMVS","MLMV","MLF",
                              "MLR","WLSM","WLSMVS","ULSM","ULSMVS","ULSMV")),
                   conditionalPanel(
                                   condition = c("input.estimators == 'ML'"),
                                   selectInput("ml", "What's the lavaan ML estimation likelihood method?",
                                               choices = list("default" = 1, "wishart" = 2),
                                               selected = 1
                                   ))
                   ,
                  conditionalPanel(
                    condition = c("input.estimators == 'WLSMV'"),
                    selectInput("ordered", "Does your model have ordered indicators?",
                                choices = list("All indicators are ordered" = 1,
                                               "Some indicators are ordered" = 2,
                                               "No ordered indicators" = 3),
                                selected = 3
                  )),
                  conditionalPanel(
                    condition = c("input.estimators == 'DWLS'"),
                    selectInput("ordered", "Does your model have ordered indicators?",
                                choices = list("All indicators are ordered" = 1,
                                               "Some indicators are ordered" = 2,
                                               "No ordered indicators" = 3),
                                selected = 3
                    )),
                  conditionalPanel(
                    condition = c("input.estimators == 'ULSMV'"),
                    selectInput("ordered", "Does your model have ordered indicators?",
                                choices = list("All indicators are ordered" = 1,
                                               "Some indicators are ordered" = 2,
                                               "No ordered indicators" = 3),
                                selected = 3
                    )),
                  conditionalPanel(
                    condition = c("input.estimators == 'PML'"),
                    selectInput("ordered", "Does your model have ordered indicators?",
                                choices = list("All indicators are ordered" = 1,
                                               "Some indicators are ordered" = 2,
                                               "No ordered indicators" = 3),
                                selected = 3
                    )),
                  
                  helpText('We recommend starting with a low number of',
                  'simulations (e.g., 100) to get a rough',
                  'estimate of power before confirming it',
                  'with a higher number of simulations',
                  '(e.g., 1000). The larger the number,', 
                  'the longer simulations will take.'),


                   # Add button to initiate simulations
                   actionButton(inputId = "simu", 
                                label = "Estimate Parameters via Simulations"),
                  
                  # Display results of simulations
                  h3("How many simulations converged?"),
                  textOutput("convall"),
                  div(DT::dataTableOutput ("converge"), style = "font-size:120%"),
                  h3("Why the simulations did not converged?"),
                  div(DT::dataTableOutput ("nonconverge"), style = "font-size:120%"),
                  
                  div(DT::dataTableOutput ("resultstable"), style = "font-size:120%"),
                  textOutput("powertable_note")),
                 
                 textOutput("step4_model_warning")
          )

        ),
        
        # *--- Help ---------------------------------------------------------------
        
        tabPanel(
          "Help", value = "tab5",
          br(),
          
          # Factor loading
          h4("What factor loading strength should I specify?"),
          p("If you have a measure with a known reliability estimate",
            "(e.g., Cronbach's alpha), you can estimate the average factor",
            "loading strength of individual items in that measure using the",
            "Spearman-Brown prophecy formula with the calculator below."),
          
          # Set up Spearman-Brown calculator
          wellPanel(
            
            # Inputs
            fluidRow(
              column(9,
                     sliderInput(
                       "alpha", 
                       label = paste("Reliability estimate of measure",
                                     "(e.g., Cronbach's alpha)"),
                       min = 0, max = .99, value = .80)),
              column(3,
                     numericInput("nitem", 
                                  label = "Number of items", 
                                  min = 1, value = 3))
            ),
            
            # Output
            verbatimTextOutput("lambda.est")
          ),
          
          # Structural effect size
          h4("What structural effect sizes should I specify?"),
          p("The effect size of a structural parameter (e.g., regression",
            "coefficient between two latent factors) in SEM is often",
            "different from the effect size estimated from regressions using",
            "observed variables, because the structural parameter estimate",
            "could be disattenuated from measurement error. For example, if",
            "prior research found a correlation of .3 between two raw scores,",
            "the effect size of the corresponding true scores is likely larger",
            "(though note this is not necessarily the case if the effect size",
            "from observed variables is estimated in a multivariate path",
            "model; see e.g., Cole & Preacher, 2014). The calculator below",
            "allows you to disattenuate the effect size (in correlation)",
            "between two observed variables using the Spearman's correction."
          ),
          
          # Set up Spearman's correction calculator
          wellPanel(
            
            # Inputs
            sliderInput("raw.corr", 
                        label = "Correlation between observed variables A and B",
                        min = 0, max = .99, value = .30),
            fluidRow(
              column(6,
                     sliderInput("reliability1", 
                                 label = "Reliability of variable A",
                                 min = 0, max = .99, value = .80)),
              column(6,
                     sliderInput("reliability2", 
                                 label = "Reliability of variable B",
                                 min = 0, max = .99, value = .80))),
            
            # Output
            verbatimTextOutput("latent.corr")
          )
        ),
        # *--- References ---------------------------------------------------------------
        
        tabPanel(
          "References", value = "tab6",
          br(),
          fluidRow(
            column(width = 12,
                   h3("In this app, we use the following programming languages and packages:"),
                   HTML(paste(format(citation(), style = "text"),
                              br(),
                              br(),
                              format(citation(package = "simsem"), style = "text"),
                              br(),
                              br(),
                              format(citation(package = "lavaan"), style = "text"),
                              br(),
                              br(),
                              format(citation(package = "DT"), style = "text"),
                              br(),
                              br(),
                              format(citation(package = "snow"), style = "text"),
                              br(),
                              br(),
                              format(citation(package = "tidyr"), style = "text"),
                              br(),
                              br(),
                              format(citation(package = "rhandsontable"), style = "text"),
                     sep = "")),
                   style = "padding-bottom: 10px;"
            ),
            column(width = 12,
                   h4("This app structure is a modificated version of the pwrSEM app code:"),
                   HTML(paste("Wang, Y. A., & Rhemtulla, M. (2021). Power analysis for parameter estimation in structural equation modeling: A discussion and tutorial. Advances in Methods and Practices in Psychological Science.",
                     sep = "\n")),
                   style = "padding-bottom: 10px;"
            )
            
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session){
  
  # Estimated factor loading output
  output$lambda.est <- renderText({
    paste0("Estimated average factor loading per item: ",
           round(sqrt(input$alpha/(input$nitem + (1 - input$nitem)*input$alpha)
           ), 2)
    )
  })
  
  # Estimated structural effect size output
  output$latent.corr <- renderText({
    paste0("Estimated correlation between latent variables A and B: ",
           round(input$raw.corr/(sqrt(input$reliability1*input$reliability2)
           ), 2)
    )
  })
 
  
  # Events reactive to cross-tab navigation buttons  ------------------------
  
  observeEvent(input$tab2to1, {
    updateTabsetPanel(session, "tabby", selected = "tab1")
  })
  
  observeEvent(input$tab2to3, {
    updateTabsetPanel(session, "tabby", selected = "tab3")
  })
  
  
  observeEvent(input$clicks1, {
    updateTabsetPanel(session, "tabby", selected = "tab2")
  })

  observeEvent(input$clicks1, {
    updateTabsetPanel(session, "tabby", selected = "tab2")
  })
  
  observeEvent(input$tab3to4, {
    updateTabsetPanel(session, "tabby", selected = "tab4")
  })
  
  # Events reactive to "Set Model" in Step 1 --------------------------------
  
  # Assign reactive object
  mg <- eventReactive(input$clicks1, { 
    
    # Get parameter data for visualization
    am <- as.data.frame(lavaanify(input$text1)[, c(1:4, 9, 11)])

    # Generate parameter table
    am$effect <- FALSE # By default, no parameter is selected in "Effect"
    
    # regression coefficient
    am_idRG <- which(am$op == "~")
    
    # factor loading
    am_idMR <- which(am$op == "=~")
    
    # total variance
     am_idTV <- which(am$op == "~~" & am$lhs == am$rhs & 
                        am$lhs %in% lavNames(input$text1, type = "lv"))
     
   # covariance
    am_idTC <- which(am$op == "~~" & am$lhs != am$rhs &
                       am$lhs %in% lavNames(input$text1, type = "lv"))
    
    # residual covariance
     am_idRC <- which(am$op == "~~" & am$lhs != am$rhs &
                        !(am$lhs %in% lavNames(input$text1, type = "lv")))

    # intercept
    am_idIT <- which(am$op == "~1")
    
    # labelled parameter
    am_idLB <- which(am$op == ":=")
    
    am_idTH <- which(am$op == "|")
    
    

    # Add description of each parameter by type
    am$description <- NA
    am$description[am_idRG] <- paste(am$lhs[am_idRG], "is regressed on", 
                                     am$rhs[am_idRG], sep = " ")
    am$description[am_idMR] <- paste(am$lhs[am_idMR], "is measured by", 
                                     am$rhs[am_idMR], sep = " ")
    am$description[am_idTV] <- paste("Total variance of", am$lhs[am_idTV], 
                                     sep = " ")
     am$description[am_idTC] <- paste("Variance of", am$lhs[am_idTC], 
                                     "covaries with variance of", 
                                     am$rhs[am_idTC], sep = " ")
     am$description[am_idRC] <- paste("Residual of", am$lhs[am_idRC], 
                                      "covaries with residual of", 
                                      am$rhs[am_idRC], sep = " ")
    am$description[am_idIT] <- paste("Intercept of", am$lhs[am_idIT], sep = " ")
    am$description[am_idLB] <- "Labelled parameter"
    am$description[am_idTH] <- paste("Threshold parameter", am$rhs[am_idTH], sep = " ")
    
    
    if(input$stdlv.radio == 1){
          am[am_idTV,]$ustart <- 1

        }
        else{
          indexFL <- which(!duplicated(am$lhs[am_idMR]))
              am[indexFL,]$ustart <- 1

              am[am_idTV,]$ustart <- NA
              
            }
    
    # Display parameter type
    am$type <- NA
    am$type[am_idRG] <- "regression coefficient"
    am$type[am_idMR] <- "factor loading"
    am$type[am_idTV] <- "total variance"
    am$type[am_idTC] <- "covariance"
    am$type[am_idRC] <- "residual covariance"
    am$type[am_idIT] <- "intercept"
    am$type[am_idLB] <- "labelled parameter"
    am$type[am_idTH] <- "threshold parameter"
    
    # Make table more readable
    am <- tidyr::unite(am, "parameter", lhs:rhs, sep = " ")
    am <- am[, c(1, 2, 4, 6, 3, 7, 5
                 )]
    names(am) <- c("Row", "Parameter", "Label", "Description", "Value", "Type"
                   ,"Effect")
    
    # Return parameter table and diagram
    return(am)
    
  })
    
    observeEvent(input$clicks1, {  
      
      # Render interactive parameter table
      output$AnalysisMod <- renderRHandsontable({
        table <- as.data.frame(mg())
        
        tableremove <- which(is.na(table$Description))
         
        table <- table[-c(tableremove),]
        
        
        # Set table dimensions
        rhandsontable(table, rowHeaders = NULL, stretchH = "all", 
                      height = 300,selectCallback = TRUE) %>%
          
          # Set all columns other than "Value" to read-only
          hot_col(col = c("Row", "Parameter", "Label", "Description", "Type"
                          ), readOnly = T) %>%
        
          # Highlight cell selection
          hot_table(highlightCol = T, highlightRow = T) %>%
          
          # Disable row and column editing
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      })
      updateTabsetPanel(session, "tabby", selected = "tab2")
      
    })
    
    output$step2_para_warning <- output$step2_para_all <- 
      output$step2_para_success <-
      output$step2_model_warning <- output$step3_para_warning <- output$step4_model_warning <-

      renderText("")
    

    observeEvent(input$text1, {
      
      test_model_enter <- try(lavaanify(input$text1), silent = TRUE)
      
      # Test if model is entered
      if (inherits(test_model_enter, "try-error")) {
        output$step2_model_warning <- renderText(
          "No model detected. Did you enter a model in Step 1?")
      }
      else{
        observeEvent(input$AnalysisMod,{
        if (TRUE %in% is.na(hot_to_r(input$AnalysisMod)$Value)) {
          
          output$step2_para_all <- 
            renderText("All parameter values need to be specified.")
          output$step2_model_warning <- 
            output$step2_para_warning <- output$step2_para_success <- output$step4_model_warning <-
            renderText("")
          }
        else if (!(TRUE %in% hot_to_r(input$AnalysisMod)$Effect)) {
          
          output$step2_para_warning <- 
            renderText("Please select at least one parameter as the target effect.")
          output$step2_model_warning <-
            output$step2_para_success <- output$step2_para_all <- output$step4_model_warning <-
            renderText("")
          
        }
        else {
          
          updateTabsetPanel(session, "tabby", selected = "tab4")
          output$step2_para_success <- 
            renderText("Parameter values confirmed.")
          output$step2_model_warning <-
            output$step2_para_warning <- output$step2_para_all <- output$step4_model_warning <-
            renderText("")
        }
        
        })
      }
    }
)

      observeEvent(input$simu,{
        
                     textbase <- lavaanify(input$text1)
                     
                     if(any(textbase$op %in% "|")==TRUE){
                       exclude <- which(textbase$op == "~~" & textbase$lhs == textbase$rhs &
                                          !(textbase$lhs %in% lavNames(textbase, type = "lv")))
                       
                      exclude1 <- which(textbase$op == "~*~")
                     
                      textbase <- textbase[-c(exclude,exclude1),]
                     }
                     else{
                       exclude <- which(textbase$op == "~~" & textbase$lhs == textbase$rhs &
                                          !(textbase$lhs %in% lavNames(textbase, type = "lv")))
                       
                       textbase <- textbase[-c(exclude),]
                       
                     }
                     
                     

                     textbase[, "ustart"] <- hot_to_r(input$AnalysisMod)[,"Value"]
                     textbase$multiply <- "*"
                     
                     text2.t <- readr::format_delim(textbase[,c("lhs","op","ustart","multiply","rhs")],
                                                    delim = " ",
                                                    na = "",
                                                    col_names = FALSE,
                                                    quote = "none",
                                                    eol = "\n")

                     
                     observeEvent(input$simu,{
                       # Set progress bar
                       withProgress(message = 'Calculation in progress',
                                    detail = 'This may take a while...', value = 0, {

                       if(input$ordered == 1){
                         order <- TRUE
                         
                         if (input$stdlv.radio == 1 & (any(c("WLSMV","DWLS","PML","ULSMV") %in% input$estimators) == TRUE)){
                           
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = TRUE,
                                                     ordered = order,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                         else if (input$stdlv.radio == 2 & (any(c("WLSMV","DWLS","PML","ULSMV") %in% input$estimators) == TRUE)){
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = FALSE,
                                                     ordered = order,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                         else if (input$stdlv.radio == 1 & (any(c("WLSMV","DWLS","PML","ULSMV") %in% input$estimators) == FALSE)){
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = TRUE,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                         else if (input$stdlv.radio == 2 & (any(c("WLSMV","DWLS","PML","ULSMV") %in% input$estimators) == FALSE)){
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = FALSE,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                       }
                       else if(input$ordered == 2){
                         order <- unique(textbase$lhs[which(textbase$op == "|")])
                         
                         if (input$stdlv.radio == 1 & (any(c("WLSMV","DWLS","PML","ULSMV") %in% input$estimators) == TRUE)){
                           
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = TRUE,
                                                     ordered = order,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                         else if (input$stdlv.radio == 2 & (any(c("WLSMV","DWLS","PML","ULSMV") %in% input$estimators) == TRUE)){
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = FALSE,
                                                     ordered = order,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                         else if (input$stdlv.radio == 1 & (any(c("WLSMV","DWLS","PML","ULSMV") %in% input$estimators) == FALSE)){
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = TRUE,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                         else if (input$stdlv.radio == 2 & (any(c("WLSMV","DWLS","PML","ULSMV") %in% input$estimators) == FALSE)){
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = FALSE,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                       }
                       else{
                         order <- FALSE
                         
                         if (input$stdlv.radio == 1 & input$ml == 2){
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = TRUE,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE,
                                                     likelihood = "wishart")
                         }
                         else if (input$stdlv.radio == 2 & input$ml == 2){
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = FALSE,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE,
                                                     likelihood = "wishart")
                         }
                         
                         else if (input$stdlv.radio == 1){
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = TRUE,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                         else{
                           simResults <- simsem::sim(nRep=input$ksim,
                                                     n = input$sampleN,multicore = TRUE, numProc = parallel::detectCores() - 1L,
                                                     generate = text2.t,
                                                     model = input$text1,
                                                     lavaanfun = "sem",
                                                     estimator = input$estimators,
                                                     seed = input$seed,
                                                     std.lv = FALSE,
                                                     completeRep = FALSE,
                                                     stopOnError = FALSE, silent = TRUE)
                         }
                       }
                       
                                      a<- inherits(try(simResults), "try-error")
                                      
                                      res <-  sapply(a,function(el)
                                      {
                                        tryCatch({
                                          #Expression that might throw an error
                                          a == TRUE
                                        }, warning = function(w) {
                                          print("Warning. Minor Problems!") #warning handling
                                        }, error = function(e) {
                                          print("Error!. Major Problems!") #error handling
                                        }, finally = {
                                          #possible clean-up code.
                                        }
                                        )})
                                      
                     # Test if model is entered
                     if (res==TRUE) {
                         output$convall <- renderText("No simulation condition converged.")
                         output$converge <-  DT::renderDataTable(data.frame())
                         output$nonconverge <-  DT::renderDataTable(data.frame())
                         output$resultstable <- DT::renderDataTable(data.frame())
                       output$step4_model_warning <- renderText(
                         "Error on simulations. Please revise your input and population model, change seed or add more quadruplets.")
                     }
                     else{
    
                     getcol <- hot_to_r(input$AnalysisMod)
                     
                     index <- which(getcol$Effect == TRUE)
                     
                     rowname <- getcol[index,]$Parameter
                     
                     rowname<-stringr::str_replace_all(string = rowname, pattern = " ", repl = "")
                     
                     results0 <- simsem::summaryParam(simResults,
                                                      digits = 3,
                                                      alpha = input$p_alpha,
                                                      detail = TRUE)[,c(1,
                                                                        3,
                                                                        4,
                                                                        8,
                                                                        9,
                                                                        11
                                                      )]
                     
                     results <- subset(results0, rownames(results0) %in% unlist(rowname))
                     
                     if (inherits(try(summaryConverge(simResults)), "try-error") == TRUE){
                       output$converge <- renderTable(data.frame())
                       output$nonconverge <- renderTable(data.frame())
                       
                       output$resultstable <- DT::renderDataTable({
                         results
                       }, rownames = TRUE, width = 15)
                     }
                     else{
                         conv <- as.data.frame(summaryConverge(simResults)$Converged)
                         colnames(conv) <- c("Count")
                         rownames(conv) <- c("Converged","Non-Converged")
                         rea <- as.data.frame(summaryConverge(simResults)$`Nonconvergent Reasons`)
                         colnames(rea) <- c("Count")
                         
                         output$convall <- renderText("")
                         output$converge <-  DT::renderDataTable({conv},width = 15)
                         output$nonconverge <-  DT::renderDataTable({rea}, width = 15)
                         
                         output$resultstable <- DT::renderDataTable({
                           results
                         }, rownames = TRUE, width = 15)
                         output$step4_model_warning <- renderText("")
                         
                     }

                      output$powertable_note <- renderText({
                       paste('Estimate Average is the Average of parameter estimates across all replications. ',
                             'Power(Not equal 0) is the proportion of significant replications (p < .05) when testing whether the parameters are different from zero. ',
                             'Coverage is The percentage of (1-alpha)% confidence interval covers parameters underlying the data. ',
                             'Rel Bias is the Relative Bias of simulated estimates of a parameter.',
                             sep = "")
                     
                        })
                      }
                       }
                     )
                   })
                   }

      )                         
      }

# Run the application 
shinyApp(ui = ui, server = server)
