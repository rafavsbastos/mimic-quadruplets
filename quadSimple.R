# Load packages
if(!require(shiny)){install.packages('shiny')}
if(!require(DT)){install.packages('DT')}
if(!require(lavaan)){devtools::install_github('yrosseel/lavaan')}
if(!require(simsem)){devtools::install_github('simsem/simsem/simsem')}
if(!require(rhandsontable)){install.packages('rhandsontable')}
if(!require(tidyr)){install.packages('tidyr')}
if(!require(snow)){install.packages('snow')}
library(shiny); library(lavaan);library(snow);
library(rhandsontable); library(tidyr);library(simsem);library(DT)

# Define UI for application
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
           tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 'How Many Quadruplets I need for my model?'),
           p('Simulation for the Minimum Number of Quadruplets')
  ),

  # *- Set button and text colors -------------------------------------------
  
  tags$head(
    tags$style(HTML('#clicks1{background-color:#428BCA; color: white}',
                    'step4_model_warning{color: red}'
    ))
  ),
  
  # *- Set app header -------------------------------------------------------
  
  fluidRow(
    column(width = 12,
           HTML(paste(tags$strong("quadSimple", style = "font-size:40px;"),
                      "v0.0.1")),
           h4("Simulation for the Minimum Number of Quadruplets"),
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
          tags$b('Specify Population Model'), 
          '. Enter your instrument characteristics in the boxes on the right. ',
          ' Set the sample size you wish to collect ', 
          '(more information',
          tags$a(href = "https://lakens.github.io/statistical_inferences/08-samplesizejustification.html",
                 " here"), ").",
          'Set the number of simulations you wish to do. With higher number of 
          simulations, you might get more accurate estimates.
          However, it will take a long time if you set the number of simulations
          to be enormous. In addition, set the seed you wish (i.e., a random 
          number that allow you to recover the same simulation results) or let 
          the default. The content factors are the factor you wish to measure, like
          "Well-being", "Extroversion","Intelligence" etc. Do not include Social desirability as a factor here.
          Items outside of the quadruplets are items that measures only the content
          factor (i.e., common items that are not manipulated to become a quadruplet).
          In this simulation app, we are not considering ordered indicators. For a more robust application, look at que quadSim version.
          ',
          sep = "")
        )
      ),
      

    )
    ,
    
    # *- Set main interface ---------------------------------------------------
    
    mainPanel(
      tabsetPanel(
        id = "tabby",
        
        
        # *--- Step 1 -------------------------------------------------------------
        
        tabPanel(
          "1. Specify Instrument Characteristics", value = "tab1",
          br(),
          column(
            8,
            wellPanel(
              
              # Create text box for users to enter analysis model
              numericInput(inputId = "sampleN", 
                           label = "Set your sample size:", 
                           value = 500, min = 1, step = 50),
              numericInput(inputId = "ksim", 
                           label = "Set the number of simulations to test:", 
                           value = 100, min = 1, step = 1),
              numericInput(inputId = "alpha", 
                           label = "Set the alpha level:", 
                           value = .05, min = 0, step = .01),
              numericInput(inputId = "seed", 
                           label = "Set the seed of the simulations:", 
                           value = 25052023, min = 1, step = 1),
              numericInput(inputId = "nfactors", 
                           label = "How many content factors do you have?", 
                           value = 3, min = 1, step = 1),
              numericInput(inputId = "nquads", 
                           label = "Insert the number of quadruplets for each factor:", 
                           value = 1, min = 1, step = 1),
              numericInput(inputId = "nouts", 
                           label = "Insert the number of items outside of the quadruplets for each factor:", 
                           value = 5, min = 1, step = 1),
              numericInput(inputId = "flmean", 
                           label = "Insert the mean of factor loadings on items outside of quadruplets:", 
                           value = .7, min = 1, step = .1),
              numericInput(inputId = "sdmean", 
                           label = "Insert the mean of factor loadings on the quadruplets items (social desirability factor loadings):", 
                           value = .5, min = 1, step = .1),

            
              helpText('We recommend starting with a medium number of',
                       'quadruplets (e.g., 4 quadruplets = 16 items) to get a rough',
                       'estimate of power before trying it',
                       'with a higher or lower number of quadruplets',
                       '(e.g., 1 or 48 quadruplets).'),
              # Add button for model setting
              actionButton(
                inputId = "clicks1",
                label = "Set Model"),
              
              textOutput("step4_model_warning"),
              
              #Display results of simulations
                        h3("How many simulations converged?"),
                        div(DT::dataTableOutput ("converge"), style = "font-size:120%"),
                        textOutput("converge1"),
                        h3("Why the simulations did not converged?"),
                        div(DT::dataTableOutput ("nonconverge"), style = "font-size:120%"),
                        textOutput("nonconverge1"),
              

                        div(DT::dataTableOutput ("resultstable"), style = "font-size:120%"),

                        textOutput("powertable_note")
              )
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
                   h4("This app is a modification and it's based on the pwrSEM app:"),
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
    updateTabsetPanel(session, "tabby", selected = "click1")
  })

  
      observeEvent(input$clicks1,{
                   # Set progress bar
                   withProgress(message = 'Calculation in progress',
                                detail = 'This may take a while...', value = 0, {
                     
                     quadsFL <- rep(input$flmean, 4*input$nquads)
                     quadsFL <- quadsFL*c(1,-1,-1,1)
                     
                     quadruplets <- paste0("f",1:input$nfactors, "=~",quadsFL,"*","quadruplets",1:(4*input$nquads*input$nfactors), sep = "\n")
                     
                     content <- paste0("f",1:input$nfactors, "=~",input$flmean ,"*","item",1:(input$nfactors*input$nouts), sep = "\n")
                     
                     factors <- c(content, quadruplets, sep = "\n")
                     
                     quadsSD <- rep(input$sdmean, 4*input$nquads)
                     quadsSD <- quadsSD*c(-1,1,-1,1)
                     
                     socialdesirability <- paste0("SD =~", quadsSD,"*quadruplets",1:(4*input$nquads*input$nfactors), sep = "\n")
                     
                     regressions <- paste0("item",1:(input$nfactors*input$nouts),"~",input$sdmean,"*SD", sep = "\n")
                     
                     correlations <- paste0("f",1:input$nfactors, "~~0*SD")
                     
                     
                     quadrupletsm <- paste0("f",1:input$nfactors, "=~","quadruplets",1:(4*input$nquads), sep = "\n")
                     
                     contentm <- paste0("f",1:input$nfactors, "=~","item",1:(input$nfactors*input$nouts), sep = "\n")
                     
                     factorsm <- c(contentm, quadrupletsm, sep = "\n")
                     
                     socialdesirabilitym <- paste0("SD =~", "quadruplets",1:(4*input$nquads), sep = "\n")
                     
                     regressionsm <- paste0("item",1:(input$nfactors*input$nouts),"~SD", sep = "\n")
                     
                     generatetest <- c(factors, socialdesirability, regressions, correlations, sep = "\n")
                     
                     modeltest <- c(factorsm, socialdesirabilitym, regressionsm,correlations, sep = "\n")
                     
                     
                     simResults <- simsem::sim(nRep=input$ksim,
                                               n = input$sampleN,
                                               generate = generatetest,
                                               model = modeltest,
                                               lavaanfun = "sem",
                                               estimator = "WLSMV",
                                               seed = input$seed,
                                               std.lv = TRUE,
                                               ordered = FALSE,
                                               completeRep = FALSE,
                                               stopOnError = FALSE,
                                               multicore = TRUE,
                                               numProc = parallel::detectCores() - 1L,
                                               silent = TRUE)
                     

                     
                     test_results_enter <- try(simsem::summaryParam(simResults))
                     
                     
                     observeEvent(input$clicks1,{
                     # Test if model is entered
                     if (inherits(test_results_enter, "try-error")) {
                       output$step4_model_warning <- renderText(
                         "Error on simulations. Please revise your input and population model, change seed or add more quadruplets.")
                     }
                     else{
    
                     
                     results <- simsem::summaryParam(simResults,
                                                      digits = 3,
                                                      alpha = .05,
                                                      detail = TRUE)[,c(1,
                                                                        3,
                                                                        4,
                                                                        8,
                                                                        9,
                                                                       11
                                                                       )]
                     
                     
                     didconverge <-try(summaryConverge(simResults))
                     
                     if (inherits(didconverge, "try-error") == TRUE){
                       output$converge1 <- renderText("All replications were converged.")
                       output$nonconverge1 <- renderText("All replications were converged.")
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
                       
                     }
                       
                     output$resultstable <- DT::renderDataTable({
                       results
                     }, rownames = TRUE, width = 15)
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
