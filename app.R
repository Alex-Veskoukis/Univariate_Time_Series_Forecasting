
rm(list = ls());gc();
library(prophet)
library(data.table)
library(shiny)
library(stats)
library(shinyWidgets)
library(forecast)
library(Metrics)
library(DT)
library(rugarch)
library(forecTheta)
library(highcharter)
library(shinythemes)
library(shinyalert)
library(shinyjs)
library(shinyFeedback)
# library(shinybusy)

source("helper_functions.R")
#   __________________ #< 32434cad3820760be475483b522ed57d ># __________________
#   UI                                                                      ####

ui <- fluidPage(theme = shinytheme("superhero"),
                tags$style(style),
                useShinyjs(),
                useShinyalert(),
                useShinyFeedback(), 
                h2('Time Series Upload'),
                inputPanel(
                  fileInput("i_file", "Upload your CSV file"),
                  selectInput('variable','Select Series',choices = NULL),
                  awesomeRadio(inputId = "frequency_known", 
                               label = "Is frequency known? ", 
                               choices = c("Yes" = 1, 
                                           "No" = 2), 
                               selected = 2),
                  uiOutput('frequency_type'),
                  br()),
                br(),
                conditionalPanel(condition = "(input.variable.length > 0)",
                                 actionButton('submit', "Submit")),
                br(),
                hr(),
                hidden(div(align = 'left',
                           id = 'seriesdiv',
                           highchartOutput('series'))), 
                br(),
                hr(),
                h2('Evaluation Scheme'),
                awesomeCheckboxGroup(inputId = "Algorithm", 
                                     label = "Select Algorithm to Evaluate", 
                                     choices = c("DRIFT",
                                                 "NAIVE",
                                                 "(S)ARIMA" = 'ARIMA',
                                                 "ETS", 
                                                 'TBATS',
                                                 'PROPHET', 
                                                 'THETA',
                                                 'NNETAR'),
                                     selected = NULL, 
                                     inline = TRUE),
                uiOutput('ensemble_params'),
                br(),
                awesomeRadio(inputId = "evaluation_type", 
                             label = "Evaluation type", 
                             choices = c("Validation set" = 1, 
                                         "Cross Validation" = 2), 
                             selected = 1),
                uiOutput('evaluation_type_parameters'),
                br(),
                h3('Algorithms Settings'),
                tabsetPanel(type="tabs",
                            
                            #build forecasting panels
                            tabPanel("NAIVE", icon = icon("line-chart"), h4("NAIVE"),
                                     br(),
                                     inputPanel(
                                       checkboxInput('seasonal_naive','Seasonal',value = T)
                                     ), 
                                     value=0),
                            tabPanel("(S)ARIMA", icon = icon("line-chart"), h4("(S)ARIMA"),
                                     br(),
                                     inputPanel(
                                       numericInput('max.p','Maximum value of AR terms',value = 5),
                                       numericInput('max.q','Maximum value of MA terms',value = 5),
                                       numericInput('max.P','Maximum value of seasonal AR terms',value = 2),
                                       numericInput('max.Q','Maximum value of seasonal MA terms',value = 2),
                                       numericInput('max.order','Maximum value of p+q+P+Q if model selection is not stepwise',value = 5),
                                       numericInput('max.d','Maximum value of difference order',value = 2),
                                       numericInput('max.D','Maximum value of seasonal difference order',value = 1),
                                       selectInput('arima_ic','Information criterion to evaluate best (S)ARIMA', choices = c("aicc", "aic", "bic"), selected = "aic"),
                                       selectInput('arima_lambda','Box-Cox transformation parameter', choices = c("No transformation" = 'NULL', "Find optimal" = 'auto'), selected = "NULL"),
                                       checkboxInput('allowdrift','Consider models with drift terms',value = T), 
                                       checkboxInput('allowmean','Consider models with a non-zero mean',value = T),
                                       checkboxInput('seasonal','Allow search to seasonal models?',value = T),
                                       checkboxInput('approximation','Approximate estimation? (faster but less accurate)',value = T),
                                       checkboxInput('stepwise','Stepwise selection? (faster but less accurate)',value = T)
                                       # ,
                                       # conditionalPanel(condition = 'input.frequency > 2 & output.obs >= 2*input.frequency',
                                       #                  checkboxInput('arima_decomposition','Decomposition',value = F))
                                       
                                     ), 
                                     value=1),
                            tabPanel("ETS", icon = icon("line-chart"), h4("ETS"), 
                                     br(), 
                                     inputPanel(
                                       
                                       selectInput(inputId = 'errortype',
                                                   label = 'Error Type',
                                                   choices = c('Additive'="A", 'multiplicative'="M", 'Automatically selected'= "Z"),
                                                   selected = 'Z'),
                                       selectInput(inputId = 'trendtype',
                                                   label = 'Trend Type',
                                                   choices = c('None'="N",'Additive'="A", 'multiplicative'="M", 'Automatically selected'= "Z"),
                                                   selected = 'Z'),
                                       selectInput(inputId = 'seasontype',
                                                   label =   'Season Type',
                                                   choices = c('None'="N",'Additive'="A", 'multiplicative'="M", 'Automatically selected'= "Z"),
                                                   selected = 'Z'),
                                       selectInput('ets_lambda','Box-Cox transformation parameter', 
                                                   choices =  c("No transformation" = 'NULL', "Find optimal" = 'auto'), 
                                                   selected = "NULL"),
                                       selectInput('opt.crit','Optimization criterion', 
                                                   choices = c("Mean Square Error" = 'mse', 
                                                               "Average MSE over first nmse forecast horizons" = 'amse',
                                                               "Standard deviation of residuals" = 'sigma',
                                                               "Mean of absolute residuals" = 'mae',
                                                               "Log-likelihood" = 'lik'), selected = "lik"),
                                       selectInput('ets_ic','Information criterion to evaluate best ETS model', choices = c("aicc", "aic", "bic"), selected = "aic"),
                                       checkboxInput('allow.multiplicative.trend', 'Always include multiplicative trends?',value = T)
                                     ),
                                     value=2),
                            tabPanel("TBATS", icon = icon("line-chart"), h4("TBATS"), 
                                     br(), 
                                     inputPanel(
                                       checkboxInput('use.arma.errors','Use ARMA errors',value = T)),
                                     value=3),
                            tabPanel("PROPHET", icon = icon("line-chart"), h4("PROPHET"), 
                                     br(), 
                                     inputPanel(
                                       selectInput(label = 'Growth', inputId = 'growth', choices = c( 'linear', 'flat'),selected = "linear"),
                                       selectInput(label = 'Seasonality mode', inputId = 'seasonality.mode', choices = c('additive', 'multiplicative'),selected = "additive"),
                                       selectInput(label = 'Yearly seasonality', inputId = 'yearly.seasonality', choices = c('auto', TRUE, FALSE),selected = "auto"),
                                       selectInput(label = 'Weekly seasonality', inputId = 'weekly.seasonality', choices = c('auto', TRUE, FALSE),selected = "auto"),
                                       selectInput(label = 'Daily seasonality', inputId = 'daily.seasonality', choices = c('auto', TRUE, FALSE),selected = "auto"),
                                       numericInput(inputId = 'n.changepoints', label = 'Maximum number of trend changepoints', value = 25, min = 0, max = 100),
                                       numericInput(inputId = 'changepoint.range', label = 'Changepoint range', value = 0.8, min = 0.1, max = 1)),
                                     value=4),
                            
                            tabPanel("THETA", icon = icon("line-chart"), h4("THETA"), 
                                     br(), 
                                     inputPanel(
                                       selectInput(inputId = 'theta_model',
                                                   label = 'Select model',
                                                   choices = c('Dynamic Optimised Theta Model', 
                                                               'Dynamic Standard Theta Model', 
                                                               'Optimised Theta Model',
                                                               'Standard Theta Model',
                                                               'Standard Theta Method (STheta)'),
                                                   selected = 'Dynamic Optimised Theta Model'),
                                       selectInput(inputId = 's', 
                                                   label = 'Seasonality', 
                                                   choices = c('Multiplicative' = 'T', 'Additive'='additive', 'Unknown'='NULL'),
                                                   selected = 'NULL'),
                                       conditionalPanel(condition = "input.theta_model != 'Standard Theta Method (STheta)'",
                                                        selectInput(label = 'Optimization Method', inputId = 'opt.method', choices = c('Nelder-Mead', 'L-BFGS-B', 'SANN'),selected = 'Nelder-Mead'))
                                     ),
                                     value=5),
                            
                            tabPanel("NNETAR", icon = icon("line-chart"), h4("NNETAR"), 
                                     br(), 
                                     inputPanel(
                                       numericInput(inputId = 'number_of_seasonal_lags',
                                                    label = 'Number of seasonal lags',
                                                    value = 1,
                                                    max = 30,
                                                    min = 1,
                                                    step = 1),
                                       numericInput(inputId = 'repeats',
                                                    label = 'Repeats',
                                                    value = 20,
                                                    max = 100,
                                                    min = 1,
                                                    step = 1),
                                       selectInput(inputId = 'nnetar_lambda', 
                                                   label = 'Lamda transformation', 
                                                   choices = c('Auto' = 'auto',  'No transformation'='NULL'),
                                                   selected = 'auto'),
                                       checkboxInput(inputId = 'scale.inputs','Scale inputs',value = F)
                                     ),
                                     value=6),
                            # tabPanel("GARCH", icon = icon("line-chart"), h4("GARCH"), 
                            #          br(), 
                            #          inputPanel(
                            #            column(12, h4('Variance model'),
                            #                   selectInput(inputId = 'model', label = 'Model', choices = c( "sGARCH", "fGARCH", "eGARCH", "gjrGARCH", "apARCH" , "iGARCH" , "csGARCH"),selected = "sGARCH"),
                            #                   numericInput('grach_variance_p','AR order',value = 1, min=0 , max = 10,step=1),
                            #                   numericInput('grach_variance_q','MA order',value = 1,min=0 , max = 10,step=1),
                            #                   selectInput(inputId = 'submodel', label = 'Submodel', choices = c( "GARCH", "TGARCH", "AVGARCH", "NGARCH", "NAGARCH", "APARCH","GJRGARCH" , "ALLGARCH"),selected = "GARCH")
                            #                   ),
                            #            column(12,h4('Mean model'),
                            #                   numericInput('grach_mean_p','AR order',value = 1,min=0 , max = 10,step=1),
                            #                   numericInput('grach_mean_q','MA order',value = 1, min=0 ,max = 10,step=1),
                            #                   checkboxInput('include.mean', 'Include mean', value = ),
                            #                   checkboxInput('archm', 'include ARCH volatility in the mean regression', value = ),
                            #                   selectInput('archpow', 'Use st.deviation or variance?', choices = c('std'=1,'Variance'=2)),
                            #                   checkboxInput('arfima', 'Use fractional differencing in the ARMA regression'),
                            #                   ),
                            #            column(12,selectInput(inputId = 'distribution.model', 
                            #                                 label = h4('Distribution model'), 
                            #                                 choices = c('student-t' ="std" ,
                            #                                             'skew-student'="sstd" ,
                            #                                             'generalized error distribution'="ged",
                            #                                 'skew-generalized error distribution'="sged",
                            #                                 'the normal inverse gaussian distribution'="nig" ,
                            #                                 'Generalized Hyperbolic'= "ghyp" , 
                            #                                 "Johnson's SU" = "jsu"),
                            #                                 selected = "std")),
                            #            column(12,
                            #            selectInput(inputId = 'solver', label = h4('Select solver'),
                            #                        choices = c( "nlminb", "solnp", "lbfgs", "gosolnp", "nloptr" , "hybrid"),
                            #                        selected = "solnp"))),
                            #          
                            #          
                            #          value=4),
                            id = "timeSeriesTabs"),
                br(),
                br(),
                actionButton('evaluate','Evaluate'),
                br(),
                br(),
                dataTableOutput('results'),
                uiOutput('dt_row_selector'),
                br(),
                # div(
                #   class = "container",
                shiny::uiOutput("dynamic_tabs"),
                # ),
                br(),
                br(),
                uiOutput('forecast_widget_container'),
                uiOutput('forecast_tabs_container')
)














####
#   __________________ #< c80e0aeda84da32ec88df7b57ee4735e ># __________________
#   SERVER                                                                  ####
server <- function(input, output, session) {
  
  reactiveVariables <- reactiveValues()
  reactiveVariables$timeSeries_submitted <- F
  reactiveVariables$evaluation_done <- F
  
  observeEvent(input$i_file,{
    inFile <- input$i_file
    if (is.null(inFile))
      return(NULL)
    mySeries <- fread(inFile$datapath, encoding = 'UTF-8')
    if(nrow(mySeries) > 5){
    reactiveVariables$Series <- mySeries
    reactiveVariables$SeriesNames <- names(mySeries)
    } else{
      showToast(type = "error",message = 'To few observations', .options = myToastOptions)
    }
  })
  
  observeEvent(reactiveVariables$SeriesNames,{
    if(!is.null(reactiveVariables$SeriesNames) && length(reactiveVariables$SeriesNames) >= 0)
      updateSelectInput(session = session,
                        inputId = 'variable',
                        choices = reactiveVariables$SeriesNames)
  })
  
  
  output$frequency_type <- renderUI({
    if(input$frequency_known == 1){
      selectInput(inputId = 'frequency',
                  label = 'Frequency of observations',
                  choices =  c('Daily' = 1,
                               'Weekly' = 7, 
                               'Monthly' = 12,
                               'Quarterly' = 4,
                               'Yearly' = 365),
                  selected = 1)
    } 
  })
  
  output$evaluation_type_parameters <- renderUI({
    req(reactiveVariables$Series)
    req(input$variable)
    series <- reactiveVariables$Series[[input$variable]]
    if(input$evaluation_type == 1){
      sliderInput('holdout', 'Hold-Out observations', value = floor(length(series)*0.3), min = 1, max = floor(length(series)/2), step = 1)
    }
    else{
      # tagList(
        sliderInput('horizon', 'Horizon to evaluate', value = 1, min = 1, max = floor(length(series)/4), step = 1)
      #,
      #   sliderInput('window', 'Observations rolling window', value = 1, min = 1, max = floor(length(series)/4), step = 1),
      #   sliderInput('initial', 'Initial observations history', value = 1, min = 1, max = floor(length(series)/4), step = 1)
      # )
    }
  })
  

  
  ##  .................. #< 1160059a5e062dc3e74ef06c65f639c0 ># ..................
  ##  Time series reading                                                     ####
  
  observeEvent(input$submit,{
    myTimeSeries <- as.numeric(reactiveVariables$Series[[input$variable]])
    if(sum(is.na(myTimeSeries)) > 0){
      showToast(
        "error", 
        "An error occured reading the data. Make sure there are not missing values and the column contains only numbers.", 
        .options = myToastOptions
      )
    } else {
      if(input$frequency_known == 1 & !is.null(input$frequency)){
        reactiveVariables$TotalSeries <- ts(myTimeSeries[!is.na(myTimeSeries)], frequency = as.numeric(input$frequency))
      } else {
        reactiveVariables$TotalSeries <- ts(myTimeSeries[!is.na(myTimeSeries)], 
                                            frequency = findfrequency(myTimeSeries[!is.na(myTimeSeries)]))
        
      }
      updateActionButton(session, inputId = 'submit',  icon = icon('check'))
      reactiveVariables$timeSeries_submitted <- T
      reactiveVariables$evaluation_done <- F
    }
  })
  
  observeEvent(reactiveVariables$timeSeries_submitted ,{
    if(reactiveVariables$timeSeries_submitted == T){
      shinyjs::show('seriesdiv')
    } else {
      shinyjs::hide('seriesdiv')
    }
  })
  
  ##  .................. #< d3886288115023d7a08569cc0540baa2 ># ..................
  ##  Chart time series                                                       ####
  
  output$series <- renderHighchart({
    req(reactiveVariables$timeSeries_submitted == T)
    series <- as.vector(reactiveVariables$TotalSeries)
    if(input$evaluation_type == 1){
      total_obs <- length(series)
      obs_to_hold_out <- input$holdout#floor((isolate(input$holdout)/100) * total_obs)
      bandPlotEnd <- total_obs - obs_to_hold_out

      highchart()  %>% 
        hc_xAxis(title = list(text = "Observation number"),
                 plotLines = list(list(color = '#ffffff',
                                       width = 1, 
                                       zIndex = 1.67,
                                       dashStyle =  'Solid',
                                       value = bandPlotEnd)),
                 plotBands=list(list(
                   color="#34495e",
                   from= -1,
                   to=bandPlotEnd,
                   label=list(
                     style = list(color = '#ffffff'),
                     text="Train Set",
                     align="center"
                   )
                 ),
                 list(
                   color="#34495e",
                   from= bandPlotEnd,
                   to=total_obs,
                   label=list(
                     style = list(color = '#ffffff'),
                     text="Test Set",
                     align="center"
                   )
                 )), labels = list(style = list(color ='#ffffff')))%>%
        hc_yAxis(title = list(text =  input$variable),
                 allowDecimals =F,
                 labels = list(style = list(color ='#ffffff'))) %>%
        hc_tooltip(useHTML= T,
                   followPointer= T,
                   shared = T,
                   padding = 2,
                   animation= T,
                   table = F) %>%
        hc_add_series(series, type = "line",name = input$variable,color = '#619CFF',
                      tooltip = list(pointFormat = "<b > Value :</b> {point.y:.0f} ")) %>%
        hc_chart(zoomType = "xy") %>% 
        hc_add_theme(hc_theme_flatdark())
    } else {
      highchart()  %>% 
        hc_xAxis(title = list(text = "Observation number"),
                 labels = list(style = list(color ='#ffffff')))%>%
        hc_yAxis(title = list(text =  input$variable),
                 allowDecimals =F,
                 labels = list(style = list(color ='#ffffff'))) %>%
        hc_tooltip(useHTML= T,
                   followPointer= T,
                   shared = T,
                   padding = 2,
                   animation= T,
                   table = F) %>%
        hc_add_series(series, type = "line",name = input$variable,color = '#619CFF',
                      tooltip = list(pointFormat = "<b > Value :</b> {point.y:.0f} ")) %>%
        hc_chart(zoomType = "xy") %>% 
        hc_add_theme(hc_theme_flatdark())
    }
  })
  

  
  
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Enseble selection                                                       ####
  
  output$ensemble_params <- renderUI({
    req(input$evaluation_type == 1)
    if(length(input$Algorithm) >= 2){
      selectInput(inputId = 'ensemble_algorithms',
                  label = 'Pick Algorithms to ensemble',
                  choices = input$Algorithm,
                  multiple = T)
    }
  })
  
  
  observeEvent(input$ensemble_algorithms, {
    if (length(input$ensemble_algorithms) < 2) {
      showFeedbackWarning(
        inputId = "ensemble_algorithms",
        text = "Need at least 2 algorithms"
      )  
    } else {
      hideFeedback("ensemble_algorithms")
    }
  },ignoreNULL = T)
  
  to_ensemble <- reactive({
    if(input$evaluation_type == 1 && length(input$ensemble_algorithms) >= 2){
      input$ensemble_algorithms
    } else {
      NULL
    }
  })
  
  observeEvent(c(input$i_file,
                 input$variable,
                 # input$holdout,
                 # input$horizon,
                 # input$evaluation_type,
                 input$frequency_known,
                 input$frequency),{
                   updateActionButton(session, inputId = 'submit',  icon = icon(NULL))
                   reactiveVariables$timeSeries_submitted <- F
                 })
  
  
  
  ##  .................. #< e6b0845ddb2be58be6d88276c18d8b7f ># ..................
  ##  Evaluate Algorithms                                                     ####
  
  observeEvent(input$evaluate,{
    if(length(input$Algorithm) > 0){
      if (reactiveVariables$timeSeries_submitted == T) {
        showModal(Modal(text = ""))
        reactiveVariables$Forecasts <- list()
        
        if(input$evaluation_type == 1){
          if(!is.null(reactiveVariables$TotalSeries)){
            total_obs <- length(reactiveVariables$TotalSeries)
            obs_to_hold_out <- input$holdout#floor((input$holdout/100) * total_obs)
            reactiveVariables$Series_to_Fit <- head(reactiveVariables$TotalSeries, total_obs - obs_to_hold_out)
            reactiveVariables$Series_to_Evaluate <- tail(reactiveVariables$TotalSeries, obs_to_hold_out)
            
          }
        }
        
        
        message('Evaluating')
        forecasts <- list()
        tryCatch({
        ### . . . . . . . . .. #< c53d563091a835d35f8171cf29dd65b3 ># . . . . . . . . ..
        ### DRIFT                                                                   ####
        
        if("DRIFT" %in% input$Algorithm) {
          message('Evaluating DRIFT')
          if(input$evaluation_type == 1){
            forecasts$DRIFT <- list()
            forecastFit <- rwf(reactiveVariables$Series_to_Fit, h=length(reactiveVariables$Series_to_Evaluate), drift=TRUE)
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts$DRIFT[['Fit']] <- forecastFit$model
            forecasts$DRIFT[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
            forecasts$DRIFT[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$DRIFT[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$DRIFT[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
          } else {
            forecastFit <- time_series_cv(y = reactiveVariables$TotalSeries,
                                          forecastfunction = rwf,
                                          h = input$horizon,
                                          window = 5,
                                          initial = floor(length(reactiveVariables$TotalSeries)/2)) 
            if(input$horizon > 1){
              forecastFit_dt <- as.data.table(forecastFit)
              forecasts$DRIFT[['MAE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})]
              forecasts$DRIFT[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})]
              forecasts$DRIFT[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})]
            } else {
              forecasts$DRIFT[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
              forecasts$DRIFT[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
              forecasts$DRIFT[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
            }
          }
        }
        
        ### . . . . . . . . .. #< b1639019df2cce3e8a36505079eed67a ># . . . . . . . . ..
        ### NAIVE                                                                   ####
        
        if("NAIVE" %in% input$Algorithm) {
          message('Evaluating NAIVE')
          if(input$evaluation_type == 1){
            forecasts$NAIVE <- list()
            if(input$seasonal_naive == T){
              forecastFit <- snaive(reactiveVariables$Series_to_Fit, h = length(reactiveVariables$Series_to_Evaluate))
            }else{
              forecastFit <- naive(reactiveVariables$Series_to_Fit, h = length(reactiveVariables$Series_to_Evaluate))
            }
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts$NAIVE[['Fit']] <- forecastFit$model
            forecasts$NAIVE[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
            forecasts$NAIVE[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$NAIVE[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$NAIVE[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
          } else{
            if(input$seasonal_naive == T){
              forecastFit <- time_series_cv(y = reactiveVariables$TotalSeries,
                                            forecastfunction = snaive, 
                                            h = input$horizon,
                                            window = 5,
                                            initial = floor(length(reactiveVariables$TotalSeries)/2)) 
            }else{
              forecastFit <- time_series_cv(y = reactiveVariables$TotalSeries,
                                            forecastfunction = naive, 
                                            h = input$horizon,
                                            window = 5,
                                            initial = floor(length(reactiveVariables$TotalSeries)/2)) 
            }
            if(input$horizon > 1){
              forecastFit_dt <- as.data.table(forecastFit)
              forecasts$NAIVE[['MAE']]   <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})]
              forecasts$NAIVE[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})]
              forecasts$NAIVE[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})]
            } else {
              forecasts$NAIVE[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
              forecasts$NAIVE[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
              forecasts$NAIVE[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
            }
          }
        }
        
        ### . . . . . . . . .. #< b3a6167b3ff6b20c0cbaae7bdce45775 ># . . . . . . . . ..
        ### ARIMA                                                                   ####
        
        if("ARIMA" %in% input$Algorithm) {
          message('Evaluating ARIMA')
          if(input$evaluation_type == 1) {
            forecasts$ARIMA <- list()
            lambda <- if(input$arima_lambda == 'NULL') NULL else 'auto'
            fit <- try(auto.arima(y = reactiveVariables$Series_to_Fit,
                                  max.p = input$max.p,
                                  max.q = input$max.q,
                                  max.P = input$max.P,
                                  max.Q = input$max.Q,
                                  max.order = input$max.order,
                                  max.d = input$max.d,
                                  max.D = input$max.D,
                                  ic = input$arima_ic,
                                  lambda = lambda,
                                  allowdrift = input$allowdrift,
                                  allowmean = input$allowmean,
                                  seasonal = input$seasonal,
                                  approximation = input$approximation,
                                  stepwise = input$stepwise))
            if (is(fit, 'try-error')) {
              forecasts$ARIMA <- NULL
              showToast(
                "error", 
                "An error occured evaluating optimal ARIMA model.", 
                .options = myToastOptions
              )
            } else {
              forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
              forecastFit_dt <- as.data.table(forecastFit)
              forecasts$ARIMA[['Fit']] <- fit
              forecasts$ARIMA[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
              forecasts$ARIMA[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$ARIMA[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$ARIMA[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            }
  
          }else{
            ### cv
            forecasts$ARIMA <- list()
            forecast_Arima <-function(x,h){ 
              lambda <- if(input$arima_lambda == 'NULL') NULL else 'auto'
              fit = auto.arima(x,
                               max.p = input$max.p,
                               max.q = input$max.q,
                               max.P = input$max.P,
                               max.Q = input$max.Q,
                               max.order = input$max.order,
                               max.d = input$max.d,
                               max.D = input$max.D,
                               ic = input$arima_ic,
                               lambda = lambda,
                               allowdrift = input$allowdrift,
                               allowmean = input$allowmean,
                               seasonal = input$seasonal,
                               approximation = input$approximation,
                               stepwise = input$stepwise)
              forecast(fit,h)
            }
            forecastFit <- try(time_series_cv(y = reactiveVariables$TotalSeries,
                                              forecastfunction = forecast_Arima, 
                                              h = input$horizon,
                                              window = 5,
                                              initial = floor(length(reactiveVariables$TotalSeries)/2)) )
            if (is(forecastFit, 'try-error')) {
              showToast(
                "error", 
                "An error occured evaluating optimal ARIMA model.", 
                .options = myToastOptions
              )
              forecasts$ARIMA <- NULL
            } else {
              if(input$horizon > 1){
                forecastFit_dt <- as.data.table(forecastFit)
                forecasts$ARIMA[['MAE']]   <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})]
                forecasts$ARIMA[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})]
                forecasts$ARIMA[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})]
              } else {
                forecasts$ARIMA[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
                forecasts$ARIMA[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
                forecasts$ARIMA[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
              }
            }
          }    
        }
        
        ### . . . . . . . . .. #< ee13e415784cd35d52f0e3b05f4bb074 ># . . . . . . . . ..
        ### ETS                                                                     ####
        
        if("ETS" %in% input$Algorithm){
          message('Evaluating ETS')
          if(input$evaluation_type == 1) {
            forecasts$ETS <- list()
            model <- paste0(input$errortype,input$trendtype,input$seasontype)
            lambda <- if(input$ets_lambda == 'NULL') NULL else 'auto'
            fit <- try(ets(reactiveVariables$Series_to_Fit,
                           model=model, 
                           ic = input$ets_ic,
                           opt.crit = input$opt.crit,	
                           allow.multiplicative.trend = input$allow.multiplicative.trend,
                           lambda = lambda))
            if (is(fit, 'try-error')) {
              forecasts$ETS <- NULL
              showToast(
                "error", 
                "An error occured evaluating optimal ETS  model", 
                .options = myToastOptions
              )
            } else {
              forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
              forecastFit_dt <- as.data.table(forecastFit)
              forecasts$ETS[['Fit']] <- fit
              forecasts$ETS[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
              forecasts$ETS[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$ETS[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$ETS[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            }
          } else {
            ### cv
            model <- paste0(input$errortype,input$trendtype,input$seasontype)
            forecasts$ETS <- list()
            forecast_ETS <-function(x,h){ 
              lambda <- if(input$ets_lambda == 'NULL') NULL else 'auto'
              fit = ets(x,
                        model=model, 
                        ic = input$ets_ic,
                        opt.crit = input$opt.crit,	
                        allow.multiplicative.trend = input$allow.multiplicative.trend,
                        lambda = lambda)
              forecast(fit,h)
            }
            forecastFit <- try(time_series_cv(y = reactiveVariables$TotalSeries,
                                              forecastfunction = forecast_ETS,
                                              h = input$horizon,
                                              window = 5,
                                              initial = floor(length(reactiveVariables$TotalSeries)/2)) )
            if (is(forecastFit, 'try-error')) {
              forecasts$ETS <- NULL
              showToast(
                "error", 
                "An error occured evaluating optimal ETS model", 
                .options = myToastOptions
              )
            } else {
              if(input$horizon > 1){
                forecastFit_dt <- as.data.table(forecastFit)
                forecasts$ETS[['MAE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})]
                forecasts$ETS[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})]
                forecasts$ETS[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})]
              } else {
                forecasts$ETS[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
                forecasts$ETS[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
                forecasts$ETS[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
              }
            }
          }
        } 
        
        ### . . . . . . . . .. #< 5105bd2a0698cfb3f50d3e304810eb7a ># . . . . . . . . ..
        ### TBATS                                                                   ####
        
        if("TBATS" %in% input$Algorithm){
          message('Evaluating TBATS')
          if(input$evaluation_type == 1) {
            forecasts$TBATS <- list()
            fit <- try(tbats(reactiveVariables$Series_to_Fit,use.arma.errors = input$use.arma.errors))
            if (is(fit, 'try-error')) {
              forecasts$TBATS <- NULL
              showToast(
                "error", 
                "An error occured evaluating TBATS  model", 
                .options = myToastOptions
              )
            } else {
              forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
              forecastFit_dt <- as.data.table(forecastFit)
              forecasts$TBATS[['Fit']] <- fit
              forecasts$TBATS[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
              forecasts$TBATS[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$TBATS[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$TBATS[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            }
          } else {
            forecasts$TBATS <- list()
            forecast_TBATS <-function(x,h){ 
              fit = tbats(x,use.arma.errors = input$use.arma.errors)
              forecast(fit,h)
            }
            forecastFit <- try(time_series_cv(y = reactiveVariables$TotalSeries,
                                              forecastfunction = forecast_TBATS, 
                                              h = input$horizon,
                                              window = 5,
                                              initial = floor(length(reactiveVariables$TotalSeries)/2)))
            if (is(forecastFit, 'try-error')) {
              forecasts$TBATS <- NULL
              showToast(
                "error", 
                "An error occured evaluating TBATS  model", 
                .options = myToastOptions
              )
            } else {
              if(input$horizon > 1){
                forecastFit_dt <- as.data.table(forecastFit)
                forecasts$TBATS[['MAE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})]
                forecasts$TBATS[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})]
                forecasts$TBATS[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})]
              } else {
                forecasts$TBATS[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
                forecasts$TBATS[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
                forecasts$TBATS[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
              }
              
            }
          }
          
        }
        
        ### . . . . . . . . .. #< 094ca0b0d463fea81283d604e5c9f5bf ># . . . . . . . . ..
        ### PROPHET                                                                 ####
        
        if("PROPHET" %in% input$Algorithm){
          message('Evaluating PROPHET')
          if(input$evaluation_type == 1) {
            forecasts$PROPHET <- list()
            
            if(input$frequency_known == 1 & !is.null(input$frequency)){
              freq_int <- as.numeric(input$frequency)
            } else {
              freq_int <- findfrequency(reactiveVariables$TotalSeries)
            }
            
            valid_frequency <-  c( 1, 7, 12, 4, 365)
            names(valid_frequency) <-  c('day', 'week', 'month', 'quarter', 'year')
            check <- data.table(valid_frequency = valid_frequency, 
                                freq_int = freq_int, 
                                name = names(valid_frequency))
            check[,diff := abs(valid_frequency - freq_int)]
            freq <- check[which.min(diff),name]
            
            dt <- data.table(ds=seq.Date(from = Sys.Date(),
                                         length.out = length(reactiveVariables$Series_to_Fit),
                                         by = freq),
                             y = reactiveVariables$Series_to_Fit)
            
            
            # browser()
            fit <- try(prophet(dt, 
                               growth = input$growth,
                               n.changepoints = input$n.changepoints,
                               changepoint.range = input$changepoint.range, 
                               yearly.seasonality = input$yearly.seasonality,
                               weekly.seasonality = input$weekly.seasonality,
                               daily.seasonality = input$daily.seasonality,
                               seasonality.mode = input$seasonality.mode))
            if (is(fit, 'try-error')) {
              forecasts$PROPHET <- NULL
              showToast(
                "error", 
                "An error occured evaluating PROPHET  model", 
                .options = myToastOptions
              )
            } else {
              future <- make_future_dataframe(fit, 
                                              periods = length(reactiveVariables$Series_to_Evaluate),
                                              freq = freq)
              forecast <- predict(fit, future)
              forecastFit_dt <- as.data.table(forecast)
              forecastFit_dt <- tail(forecastFit_dt[,.(`Point Forecast` = yhat)], length(reactiveVariables$Series_to_Evaluate))
              forecasts$PROPHET[['Fit']] <- fit
              forecasts$PROPHET[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
              forecasts$PROPHET[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$PROPHET[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$PROPHET[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            }
          } else {
            forecasts$PROPHET <- list()
            # browser()
            if(input$frequency_known == 1 & !is.null(input$frequency)){
              freq_int <- as.numeric(input$frequency)
            } else {
              freq_int <- findfrequency(reactiveVariables$TotalSeries)
            }
            
            valid_frequency <-  c( 1, 7, 12, 4, 365)
            names(valid_frequency) <-  c('day', 'week', 'month', 'quarter', 'year')
            check <- data.table(valid_frequency = valid_frequency, freq_int = freq_int, name = names(valid_frequency))
            check[,diff := abs(valid_frequency - freq_int)]
            freq <- check[which.min(diff),name]
            
            
            
            dt <- data.table(ds=seq.Date(from = Sys.Date(),
                                         length.out = length(reactiveVariables$TotalSeries),
                                         by = freq),
                             y = reactiveVariables$TotalSeries)
            
            forecastFit <- try(crossValidationProphet(dt = dt, 
                                                      horizon = input$horizon,
                                                      window = 5,
                                                      initial = floor(length(reactiveVariables$TotalSeries)/2)))
            
            if (is(forecastFit, 'try-error')) {
              forecasts$PROPHET <- NULL
              showToast(
                "error", 
                "An error occured evaluating PROPHET  model", 
                .options = myToastOptions
              )
            } else {
              
              if(input$horizon > 1){
                forecasts$PROPHET[['MAE']]  <- forecastFit[Metric == 'MAE', !c('Metric')] 
                forecasts$PROPHET[['RMSE']]  <- forecastFit[Metric == 'RMSE', !c('Metric')] 
                forecasts$PROPHET[['MAPE']]  <- forecastFit[Metric == 'MAPE', !c('Metric')] 
              } else {
                forecasts$PROPHET[['MAE']]  <- forecastFit[Metric == 'MAE', !c('Metric')][[1]] 
                forecasts$PROPHET[['RMSE']]  <- forecastFit[Metric == 'RMSE', !c('Metric')][[1]] 
                forecasts$PROPHET[['MAPE']]  <- forecastFit[Metric == 'MAPE', !c('Metric')][[1]]  
              }
              
            }
            
            
            
            
            
            
          }
        }
        
        ### . . . . . . . . .. #< e2767db5fbd90bebf92a595374770134 ># . . . . . . . . ..
        ### GARCH (Unavailable)                                                     ####
        
        
        if("GARCH" %in% input$Algorithm & input$evaluation_type == 1) {
          forecasts$GARCH <- list()
          spec <- ugarchspec(
            variance.model = list(model = input$model,
                                  garchOrder = c(input$grach_variance_p,input$grach_variance_q),
                                  submodel = input$submodel),
            mean.model = list(armaOrder = c(input$grach_mean_p,input$grach_mean_q),
                              include.mean = input$include.mean,
                              archm = input$archm,
                              archpow = as.numeric(input$archpow),
                              arfima = input$arfima),
            distribution.model = input$distribution.model)
          
          # browser()
          fit <- tryCatch(ugarchfit(spec=spec, 
                                    solver = input$solver,
                                    data=reactiveVariables$Series_to_Fit),
                          error = function(e){NULL},
                          warning = function(w){NULL})
          
          
          if (is.null(fit)) {
            forecasts$GARCH <- NULL
          } else {
            
            forecastFit <- ugarchboot(fit,
                                      method=c("Partial","Full")[1],
                                      n.ahead = length(reactiveVariables$Series_to_Evaluate),
                                      n.bootpred=2000,n.bootfit=1000)
            forecastFit_dt <- data.table('Point Forecast' = as.vector(forecastFit@forc@forecast$seriesFor))
            forecasts$GARCH[['Fit']] <- fit
            forecasts$GARCH[['Forecast']] <-forecastFit_dt[, `Point Forecast`]
            forecasts$GARCH[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$GARCH[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$GARCH[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
          }
          
          
          # if(input$GARCH_seasonality_decomposition == T){
          #   forecasts$GARCH_seasonality_decomposition <- list()
          #   decomposed_ts <- stl(reactiveVariables$TotalSeries,s.window = "periodic")
          #   seasonaly_adjusted <- reactiveVariables$TotalSeries - decomposed_ts$time.series[,'seasonal']
          #   seasonaly_adjusted_to_Fit <- head(seasonaly_adjusted, length(reactiveVariables$Series_to_Fit))
          #   seasonal_to_add <- tail(decomposed_ts$time.series[,'seasonal'], length(reactiveVariables$Series_to_Evaluate))
          #   fit <- try(auto.GARCH(seasonaly_adjusted_to_Fit,
          #                         max.p = input$max.p,
          #                         max.q = input$max.q,
          #                         max.P = input$max.P,
          #                         max.Q = input$max.Q,
          #                         max.order = input$max.order,
          #                         max.d = input$max.d,
          #                         max.D = input$max.D,
          #                         ic = input$arima_ic,
          #                         allowdrift = input$allowdrift,
          #                         allowmean = input$allowmean,
          #                         seasonal = input$seasonal,
          #                         stepwise = T))
          #   if (is(fit, 'try-error')) {
          #     forecasts$GARCH_seasonality_decomposition <- NULL
          #   } else {
          #     forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
          #     forecastFit <- forecastFit$mean + seasonal_to_add
          #     forecastFit_dt <- as.data.table(forecastFit)
          #     names(forecastFit_dt) <- 'Point Forecast'
          #     forecasts$GARCH_seasonality_decomposition[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
          #     # forecasts$GARCH[['AIC']]  <- fit$aic
          #     forecasts$GARCH_seasonality_decomposition[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
          #     forecasts$GARCH_seasonality_decomposition[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
          #     forecasts$GARCH_seasonality_decomposition[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
          #   }
          # }
        }
        
        ### . . . . . . . . .. #< 8025fc324c9392644d0bfd5c01722bdc ># . . . . . . . . ..
        ### THETA                                                                   ####
        
        if("THETA" %in% input$Algorithm) {
          message('Evaluating THETA')
          theta_model <- function(y, model, opt.method, s, h){
            if(length(y) <= frequency(y)){
              if(s=='NULL'){
                ss <- 'additive'
              }else if(s=='additive'){
                ss <- 'additive'
              }else{
                ss <- TRUE
              }
            } else {
              if(s=='NULL'){
                ss <- NULL
              }else if(s=='additive'){
                ss <- 'additive'
              }else{
                ss <- TRUE
              }
            }
            switch(model,
                   'Dynamic Optimised Theta Model' = dotm(y=y, opt.method=opt.method, s=ss, h=h), 
                   'Dynamic Standard Theta Model' = dstm(y=y, opt.method=opt.method, s=ss, h=h), 
                   'Optimised Theta Model' = otm(y=y, opt.method=opt.method, s=ss, h=h),
                   'Standard Theta Model' = stm(y=y, opt.method=opt.method, s=ss, h=h),
                   "Standard Theta Method (STheta)" = stheta(y=y,  s=ss, h=h))
          }
          
          if(input$evaluation_type == 1){
            forecasts$THETA <- list()
            forecastFit <- theta_model(y = reactiveVariables$Series_to_Fit,
                                       h = length(reactiveVariables$Series_to_Evaluate) + 1,
                                       opt.method = input$opt.method,
                                       s = input$s,
                                       model = input$theta_model)
            
            forecasting_mean <- head(forecastFit$mean,length(reactiveVariables$Series_to_Evaluate))
            forecasts$THETA[['Fit']] <- forecastFit$method
            forecasts$THETA[['Forecast']] <-  forecasting_mean
            forecasts$THETA[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecasting_mean)
            forecasts$THETA[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecasting_mean)
            forecasts$THETA[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecasting_mean)
          } else{
            forecasts$THETA <- list()
            forecastFit <- try(time_series_cv(y = reactiveVariables$TotalSeries,
                                              forecastfunction = theta_model, 
                                              h = input$horizon+1,
                                              opt.method = input$opt.method,
                                              s = input$s,
                                              model = input$theta_model,
                                              window = 5,
                                              initial = floor(length(reactiveVariables$TotalSeries)/2))) 
            
            if (is(forecastFit, 'try-error')) {
              forecasts$THETA <-  NULL
              showToast(
                "error", 
                "An error occured evaluating THETA  model", 
                .options = myToastOptions
              )
            } else{
              
              if(input$horizon > 1){
                forecastFit_dt <- as.data.table(forecastFit)
                forecasts$THETA[['MAE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})][,1:input$horizon]
                forecasts$THETA[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})][,1:input$horizon]
                forecasts$THETA[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})][,1:input$horizon]
              } else {
                forecasts$THETA[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
                forecasts$THETA[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
                forecasts$THETA[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
              }
              
            }
            
          }
        }
        
        ### . . . . . . . . .. #< 0f7d6d2743442bb708d684621e3247f7 ># . . . . . . . . ..
        
        
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
        ### NNETAR                                                                  ####
        
        if("NNETAR" %in% input$Algorithm){
          message('Evaluating NNETAR')
          if(input$evaluation_type == 1) {
            forecasts$NNETAR <- list()
            lambda <- if(input$nnetar_lambda == 'NULL') NULL else 'auto'
            fit <- try(nnetar(y = reactiveVariables$Series_to_Fit,
                              P = input$number_of_seasonal_lags,
                              lambda  = lambda, 
                              scale.inputs = input$scale.inputs,
                              repeats = input$repeats))
            if (is(fit, 'try-error')) {
              forecasts$NNETAR <- NULL
              showToast(
                "error", 
                "An error occured evaluating NNETAR  model", 
                .options = myToastOptions
              )
            } else {
              forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
              forecastFit_dt <- data.table(`Point Forecast` = forecastFit$mean)
              forecasts$NNETAR[['Forecast']] <- as.vector(forecastFit_dt[,`Point Forecast`])
              forecasts$NNETAR[['MAE']]  <- mae(as.vector(reactiveVariables$Series_to_Evaluate), as.vector(forecastFit_dt[, `Point Forecast`]))
              forecasts$NNETAR[['RMSE']] <- rmse(as.vector(reactiveVariables$Series_to_Evaluate), as.vector(forecastFit_dt[, `Point Forecast`]))
              forecasts$NNETAR[['MAPE']] <- mape(as.vector(reactiveVariables$Series_to_Evaluate), as.vector(forecastFit_dt[, `Point Forecast`]))
            }
          } else {
            forecasts$NNETAR <- list()
            forecast_NNETAR <-function(x,h){ 
              lambda <- if(input$nnetar_lambda == 'NULL') NULL else 'auto'
              fit = nnetar(y = x,
                           P = input$number_of_seasonal_lags,
                           lambda  = lambda, 
                           scale.inputs = input$scale.inputs,
                           repeats = input$repeats)
              forecast(fit,h)
            }
            forecastFit <- try(time_series_cv(y = reactiveVariables$TotalSeries,
                                              forecastfunction = forecast_NNETAR, 
                                              h = input$horizon,
                                              window = 5,
                                              initial = floor(length(reactiveVariables$TotalSeries)/2)))
            if (is(forecastFit, 'try-error')) {
              forecasts$NNETAR <- NULL
              showToast(
                "error", 
                "An error occured evaluating NNETAR  model", 
                .options = myToastOptions
              )
            } else {
              if(input$horizon > 1){
                forecastFit_dt <- as.data.table(forecastFit)
                forecasts$NNETAR[['MAE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})]
                forecasts$NNETAR[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})]
                forecasts$NNETAR[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})]
              } else {
                forecasts$NNETAR[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
                forecasts$NNETAR[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
                forecasts$NNETAR[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
              }
              
            }
          }
          
        }
  
        ### ENSEMPLE                                                                ####
        if( !is.null(to_ensemble()) & input$evaluation_type == 1){
          message('Evaluating ENSEMBLE')
          forecasts$ENSEMBLE <- list()
          ensemble_list <- list()
          fit_list <- list()
          to_addup_algorithms <- setdiff(names(forecasts),"ENSEMBLE")
          for(alg in  to_addup_algorithms){
            dt <- data.table(forecasts[[alg]][['Forecast']])
            names(dt) <- names(forecasts[alg])
            ensemble_list[[alg]] <- dt
            fit_list[[alg]] <- forecasts[[alg]]['Fit']
          }
          ensemble_dt <- Reduce(f = cbind,ensemble_list,)
          ensemble_dt[, `Point Forecast` := rowMeans(.SD)]
          forecasts$ENSEMBLE[['Forecast']] <- ensemble_dt[, `Point Forecast`]
          forecasts$ENSEMBLE[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, ensemble_dt[, `Point Forecast`])
          forecasts$ENSEMBLE[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, ensemble_dt[, `Point Forecast`])
          forecasts$ENSEMBLE[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, ensemble_dt[, `Point Forecast`])
        }
        ### CLOSING                                                                 ####
        reactiveVariables$evaluation_done <- T
        reactiveVariables$Forecasts <- forecasts
        removeModal()
        message('Left evaluation')
        
        },
        error = function(e){removeModal()
          showToast(type = "error",message = paste0(toString(e)), .options = myToastOptions)
        },
        warning = function(w){removeModal()
          showToast(type = "error",message = paste0(toString(w)), .options = myToastOptions)
        })
      } else {
        showToast(type = "info",message = "Submit the input data before evaluating", .options = myToastOptions)
     }
    }else{
      showToast(type = "info",message = "You need to pick at least one algorithm to evaluate", .options = myToastOptions)
   }
  })
  
  
  
  ##  .................. #< 24c696a83dee6360f154817b64f527d8 ># ..................
  ##  Evaluation Resutls Table                                                ####
  
  output$results <- renderDataTable({
    req(reactiveVariables$timeSeries_submitted == T)
    req(reactiveVariables$evaluation_done)
    final_results <- copy(reactiveVariables$Forecasts)
    req(length(final_results) >= 1)
    result_list <- list()
    for(i in  1:length(final_results)){
      result_list[[i]] <- data.table('Algorithm' = names(final_results[i]),
                                     'RMSE' = final_results[[i]][['RMSE']],
                                     'MAE' = final_results[[i]][['MAE']],
                                     'MAPE' = final_results[[i]][['MAPE']])
      
    }
    
    result_dt <- rbindlist(result_list,fill = T)
    reactiveVariables$Results <- result_dt
    datatable(result_dt, caption = HTML("<h3 style='color:white'>Evalution results</h3>"),
              rownames = FALSE,
              height = paste0(200*nrow(result_dt),'px'),
              options = list(pageLength = 10,
                             width="100%", 
                             scrollY = F,
                             scrollX = T,
                             dom = 't',
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#34495e', 'color': '#fff'});",
                               "}"
                             )))
  })
  
  output$dt_row_selector <- renderUI({
    req(reactiveVariables$timeSeries_submitted == T)
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts
    req(length(final_results) >= 1)
    checkboxInput("dt_sel", "Select or deselect all")
  })
  
  dt_proxy <- DT::dataTableProxy("results")
  
  observeEvent(input$dt_sel, {
    req(reactiveVariables$timeSeries_submitted == T)
    if (isTRUE(input$dt_sel)) {
      DT::selectRows(dt_proxy, input$results_rows_all)
    } else {
      DT::selectRows(dt_proxy, NULL)
    }
  })

  output$dynamic_tabs <- shiny::renderUI({
    req(reactiveVariables$timeSeries_submitted == T)
   
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts
    req(length(final_results) >= 1)
    Tabs <- names(reactiveVariables$Forecasts)
    
    if(input$evaluation_type == 1){
        chart_lines <- sapply(Tabs, function(x){
          series <- as.vector(reactiveVariables$TotalSeries)
          forecasts <- as.vector(reactiveVariables$Forecasts[[x]]$Forecast)
          dt <- data.table(Series = series)
          dt[,N := 1:.N]
          dt[N > (nrow(dt) - length(forecasts)), Fitted := forecasts]
        }, USE.NAMES = T, simplify = F)

        gap <- lapply(seq_along(chart_lines), function(x){
          shiny::tabPanel(
            title = names(chart_lines[x]),
            div(align = 'left',
                class = "panel",
                div(align = 'left',
                    class = "panel-header",
                    tags$h3(names(chart_lines[x]))
                ),
                div(align = 'left',
                    class = "panel-body",
                    highchart()  %>% 
                      hc_xAxis(title = list(text = "Observation number"),
                               plotLines = list(list(color = '#ffffff',
                                                     width = 1, 
                                                     zIndex = 1.67,
                                                     dashStyle =  'Solid',
                                                     value = (length(chart_lines[[x]]$Fitted)-sum(!is.na(chart_lines[[x]]$Fitted))))),
                               labels = list(style = list(color ='#fff')))%>%
                      hc_yAxis(title = list(text = names(chart_lines[x])),
                               allowDecimals =F,
                               labels = list(style = list(color ='#fff'))) %>%
                      hc_tooltip(useHTML= T,
                                 followPointer= T,
                                 shared = T,
                                 padding = 2,
                                 animation= T,
                                 table = F) %>%
                      hc_add_series(chart_lines[[x]], type = "line", name = "Fitted" ,
                                    color = '#F8766D',
                                    hcaes(x = N, y = "Fitted"),
                                    tooltip = list(pointFormat = "<b > Forecast :</b> {point.y:.0f} ")) %>%
                      hc_add_series(chart_lines[[x]], type = "line", name = "Series",
                                    hcaes(x = N, y = "Series"),
                                    color = '#619CFF',
                                    tooltip = list(pointFormat = "<b > Actual :</b> {point.y:.0f} ")) %>%
                      hc_chart(zoomType = "xy") %>% 
                      hc_add_theme(hc_theme_flatdark())
                )
            )
          )
        })
        do.call(what = shiny::tabsetPanel, 
                args = gap %>% append(list(type = "tabs", id   = "evaluation_charts")))
    } else {
          metrics <- reactiveVariables$Results 
          if(!identical(colnames(metrics) , c('Algorithm', 'RMSE', 'MAE', 'MAPE'))) {
          mae_metrics <- metrics[,c('Algorithm',colnames(metrics)[colnames(metrics) %like% 'MAE']), with = F]
          rmse_metrics <- metrics[,c('Algorithm',colnames(metrics)[colnames(metrics) %like% 'RMSE']), with = F]
          mape_metrics <- metrics[,c('Algorithm',colnames(metrics)[colnames(metrics) %like% 'MAPE']), with = F]
          
         
          
          mae_metrics <- melt(mae_metrics, measure.vars = c(colnames(metrics)[colnames(metrics) %like% 'MAE']))[
            , c('variable', 'Metric') := .(gsub('MAE.','',variable),'MAE')]
          rmse_metrics <- melt(rmse_metrics, measure.vars = c(colnames(metrics)[colnames(metrics) %like% 'RMSE']))[
            , c('variable', 'Metric') := .(gsub('RMSE.','',variable),'RMSE')]
          mape_metrics <- melt(mape_metrics, measure.vars = c(colnames(metrics)[colnames(metrics) %like% 'MAPE']))[
            , c('variable', 'Metric') := .(gsub('MAPE.','',variable),'')]
          
          chart_list <- list('MAE' = mae_metrics, 'RMSE' = rmse_metrics, 'MAPE' = mape_metrics)
          gap <- lapply(seq_along(chart_list), function(x){
            shiny::tabPanel(
              title = names(chart_list)[x],
              div(align = 'left',
                  class = "panel",
                  div(align = 'left',
                      class = "panel-header",
                      tags$h3(names(chart_list)[x])
                  ),
                  div(align = 'left',
                      class = "panel-body",
                      highchart()  %>% 
                        hc_xAxis(title = list(text = "Horizon"),
                                 labels = list(style = list(color ='#fff')))%>%
                        hc_yAxis(title = list(text = names(chart_list)[x]),
                                 allowDecimals =F,
                                 labels = list(style = list(color ='#fff'))) %>%
                        hc_tooltip(useHTML= T,
                                   followPointer= T,
                                   shared = T,
                                   padding = 2,
                                   animation= T,
                                   table = F) %>%
                        hc_add_series(chart_list[[x]], type = "line",
                                      hcaes(x = variable, y = value, group = Algorithm)) %>%
                        hc_chart(zoomType = "xy") %>% 
                        hc_add_theme(hc_theme_flatdark())
                  )
              )
            )
          })
          do.call(what = shiny::tabsetPanel, 
                  args = gap %>% append(list(type = "tabs", id   = "evaluation_charts")))
          }
    }
    
  })

  output$forecast_widget_container <- renderUI({
    req(reactiveVariables$timeSeries_submitted == T)
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts
    req(length(final_results) >= 1)
    tagList(
      h2('Forecast'),
      actionButton(inputId = 'forecast_ahead',
                   label = "Forecast ahead with the selected row from the evaluation's results table"),
      numericInput('forecast_horizon','Forecast horizon',value =1, min=1,max =100,width = '100px'))
  })
  
  output$forecast_tabs_container <- renderUI({
    req(reactiveVariables$timeSeries_submitted == T)
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts
    req(length(final_results) >= 1)
    shiny::uiOutput("dynamic_tabs2")
  })
  ##  .................. #< 6a18d31a9e8543e6ad7168bda94b7535 ># ..................
  ##  Forecasting                                                             ####
  
  observeEvent(input$forecast_ahead,{
    req(reactiveVariables$timeSeries_submitted == T)
    if(length(input$results_rows_selected) == 0){
      showToast(
        "info", 
        'Select an model (row) from the evalution results', 
        .options = myToastOptions
      )
    } else {
      
      showModal(Modal(text = ""))
      message('Forecasting')
      forecasts_ahead <- list()
      tryCatch({
      Result_dt <- reactiveVariables$Results
      Algorithms <- reactiveVariables$Results[input$results_rows_selected,Algorithm]
      
      #### . . . . . . . . .. #< 2f343b2d516182ef8966454ae20d2a53 ># . . . . . . . . ..
      #### DRIFT                                                                  ####
      
      if("DRIFT" %in% Algorithms) {
        message('Forecasting DRIFT')
        forecasts_ahead$DRIFT <- list()
        forecastFit <- rwf(reactiveVariables$TotalSeries, h=input$forecast_horizon, drift=TRUE)
        forecastFit_dt <- as.data.table(forecastFit)
        forecasts_ahead$DRIFT[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
      }
      
      ### . . . . . . . . .. #< a0921dd4a3b7bc310493699c734bab0a ># . . . . . . . . ..
      ### NAIVE                                                                   ####
      
      if ("NAIVE" %in% Algorithms) {
        message('Forecasting NAIVE')
        forecasts_ahead$NAIVE <- list()
        if (input$seasonal_naive == T) {
          forecastFit <- snaive(reactiveVariables$TotalSeries, h = input$forecast_horizon)
        } else{
          forecastFit <- naive(reactiveVariables$TotalSeries, h = input$forecast_horizon)
        }
        forecastFit_dt <- as.data.table(forecastFit)
        forecasts_ahead$NAIVE[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
      }
      
      ### . . . . . . . . .. #< d2c567bb885ae625779fb22fdb87a87b ># . . . . . . . . ..
      ### ARIMA                                                                   ####
      
      if("ARIMA" %in% Algorithms) {
        message('Forecasting ARIMA')
        forecasts_ahead$ARIMA <- list()
        lambda <- if(input$arima_lambda == 'NULL') NULL else 'auto'
        fit <- try(auto.arima(reactiveVariables$TotalSeries,
                              max.p = input$max.p,
                              max.q = input$max.q,
                              max.P = input$max.P,
                              max.Q = input$max.Q,
                              max.order = input$max.order,
                              max.d = input$max.d,
                              max.D = input$max.D,
                              lambda = lambda,
                              allowdrift = input$allowdrift,
                              allowmean = input$allowmean,
                              seasonal = input$seasonal,
                              approximation = input$approximation,
                              stepwise = input$stepwise))
        if (is(fit, 'try-error')) {
          forecasts_ahead$ARIMA <- NULL
        } else {
          forecastFit <- forecast(fit, input$forecast_horizon)
          forecastFit_dt <- as.data.table(forecastFit)
          forecasts_ahead$ARIMA[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
        }
      }
      
      ### . . . . . . . . .. #< 044c8add4ad5eb01d51d2e75078f2d8d ># . . . . . . . . ..
      ### ETS                                                                     ####
      
      if("ETS" %in% Algorithms){
        message('Forecasting ETS')
        forecasts_ahead$ETS <- list()
        model <- paste0(input$errortype,input$trendtype,input$seasontype)
        lambda <- if(input$ets_lambda == 'NULL') NULL else 'auto'
        fit <- try(ets(reactiveVariables$TotalSeries,
                       model=model, 
                       ic = input$ets_ic,
                       opt.crit = input$opt.crit,	
                       allow.multiplicative.trend = input$allow.multiplicative.trend,
                       lambda = lambda))
        if (is(fit, 'try-error')) {
          forecasts_ahead$ETS <- NULL
        } else {
          forecastFit <- forecast(fit, input$forecast_horizon)
          forecastFit_dt <- as.data.table(forecastFit)
          forecasts_ahead$ETS[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
        }
      } 
      
      ### . . . . . . . . .. #< 76a4e2451752cd2f1b608013513b382d ># . . . . . . . . ..
      ### TBATS                                                                   ####
      
      if("TBATS" %in% Algorithms){
        message('Forecasting TBATS')
        forecasts_ahead$TBATS <- list()
        fit <- try(tbats(reactiveVariables$TotalSeries,use.arma.errors = input$use.arma.errors))
        if (is(fit, 'try-error')) {
          forecasts_ahead$TBATS <- NULL
        } else {
          forecastFit <- forecast(fit, input$forecast_horizon)
          forecastFit_dt <- as.data.table(forecastFit)
          forecasts_ahead$TBATS[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
        }
      }
      
      ### . . . . . . . . .. #< 13683ac583e4e01c71d5ca3f56d576fa ># . . . . . . . . ..
      ### PROPHET                                                                 ####
      
      if("PROPHET" %in% Algorithms){
        message('Forecasting PROPHET')
        forecasts_ahead$PROPHET <- list()
        if(input$frequency_known == 1 & !is.null(input$frequency)){
          freq_int <- as.numeric(input$frequency)
        } else {
          freq_int <- findfrequency(reactiveVariables$TotalSeries)
        }
        
        valid_frequency <-  c( 1, 7, 12, 4, 365)
        names(valid_frequency) <-  c('day', 'week', 'month', 'quarter', 'year')
        check <- data.table(valid_frequency = valid_frequency, freq_int = freq_int, name = names(valid_frequency))
        check[,diff := abs(valid_frequency - freq_int)]
        freq <- check[which.min(diff),name]
        
        dt <- data.table(ds=seq.Date(from = Sys.Date(),
                                     length.out = length(reactiveVariables$TotalSeries),
                                     by = freq),
                         y = reactiveVariables$TotalSeries)
        
        fit <- try(prophet(dt, 
                           growth = input$growth,
                           n.changepoints = input$n.changepoints,
                           changepoint.range = input$changepoint.range, 
                           yearly.seasonality = input$yearly.seasonality,
                           weekly.seasonality = input$weekly.seasonality,
                           daily.seasonality = input$daily.seasonality,
                           seasonality.mode = input$seasonality.mode))
        if (is(fit, 'try-error')) {
          forecasts_ahead$PROPHET <- NULL
        } else {
          future <- make_future_dataframe(fit, 
                                          periods = input$forecast_horizon,
                                          freq = freq)
          forecast <- predict(fit, future)
          forecastFit_dt <- as.data.table(forecast)
          forecastFit_dt <- tail(forecastFit_dt[,.(`Point Forecast` = yhat,
                                                   `Lo 95` = yhat_lower,
                                                   `Hi 95` = yhat_upper)], input$forecast_horizon)
          forecasts_ahead$PROPHET[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
        }
      }
      
      ### . . . . . . . . .. #< e6f0a7fb3efbd4c2f3ef790de27a3b41 ># . . . . . . . . ..
      ### GARCH (Unavailable)                                                     ####
      
      if("GARCH" %in% Algorithms & input$evaluation_type == 1) {
        forecasts_ahead$GARCH <- list()
        spec <- ugarchspec(
          variance.model = list(model = input$model,
                                garchOrder = c(input$grach_variance_p,input$grach_variance_q),
                                submodel = input$submodel),
          mean.model = list(armaOrder = c(input$grach_mean_p,input$grach_mean_q),
                            include.mean = input$include.mean,
                            archm = input$archm,
                            archpow = as.numeric(input$archpow),
                            arfima = input$arfima),
          distribution.model = input$distribution.model)
        
        fit <- tryCatch(ugarchfit(spec=spec, 
                                  solver = input$solver,
                                  data=reactiveVariables$TotalSeries),
                        error = function(e){NULL},
                        warning = function(w){NULL})
        
        
        if (is.null(fit)) {
          forecasts_ahead$GARCH <- NULL
        } else {
          
          forecastFit <- ugarchboot(fit,
                                    method=c("Partial","Full")[1],
                                    n.ahead = input$forecast_horizon,
                                    n.bootpred=2000,n.bootfit=1000)
          forecastFit_dt <- data.table('Point Forecast' = as.vector(forecastFit@forc@forecast$seriesFor),
                                       'Lo 95' = as.vector(forecastFit@forc@forecast$seriesFor) - as.vector(forecastFit@forc@forecast$sigmaFor) ,
                                       'Hi 95' = as.vector(forecastFit@forc@forecast$seriesFor) + as.vector(forecastFit@forc@forecast$sigmaFor))
          forecasts_ahead$GARCH[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
        }
      }
      
      ### . . . . . . . . .. #< b5b979f6387908b18e37ac78146a6a37 ># . . . . . . . . ..
      ### THETA                                                                   ####
      
      if("THETA" %in% Algorithms) {
        message('Forecasting THETA')
        theta_model <- function(y, model, opt.method, s, h){
          if(length(y) <= frequency(y)){
            if(s=='NULL'){
              ss <- 'additive'
            }else if(s=='additive'){
              ss <- 'additive'
            }else{
              ss <- TRUE
            }
          } else {
            if(s=='NULL'){
              ss <- NULL
            }else if(s=='additive'){
              ss <- 'additive'
            }else{
              ss <- TRUE
            }
          }
          switch(model,
                 'Dynamic Optimised Theta Model' = dotm(y=y, opt.method=opt.method, s=ss, h=h), 
                 'Dynamic Standard Theta Model' = dstm(y=y, opt.method=opt.method, s=ss, h=h), 
                 'Optimised Theta Model' = otm(y=y, opt.method=opt.method, s=ss, h=h),
                 'Standard Theta Model' = stm(y=y, opt.method=opt.method, s=ss, h=h),
                 'Standard Theta Method (STheta)' = stheta(y=y,  s=ss, h=h))
        }
        forecasts_ahead$THETA <- list()
        forecastFit <- theta_model(y = reactiveVariables$TotalSeries,
                                   h = input$forecast_horizon,
                                   opt.method = input$opt.method,
                                   s = input$s,
                                   model = input$theta_model)
        if(input$forecast_horizon > 1){
          forecasting_mean <- head(forecastFit$mean,input$forecast_horizon)
          forecasting_Lo <- head(as.data.table(forecastFit$lower)$`Lo 95`,input$forecast_horizon)
          forecasting_Hi <- head(as.data.table(forecastFit$upper)$`Hi 95`,input$forecast_horizon)
        } else {
          forecasting_mean <- head(forecastFit$mean,input$forecast_horizon)
          forecasting_Lo <- head(as.data.table(forecastFit$lower)$`x`,input$forecast_horizon)
          forecasting_Hi <- head(as.data.table(forecastFit$upper)$`x`,input$forecast_horizon)
        }
        forecastFit_dt <- data.table('Point Forecast' = forecasting_mean, 
                                     'Lo 95' =  forecasting_Lo, 
                                     'Hi 95' = forecasting_Hi)
        forecasts_ahead$THETA[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
        
      }
      
      
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
      ### NNETAR                                                                  ####
      if("NNETAR" %in% Algorithms){
        message('Forecasting NNETAR')
        forecasts_ahead$NNETAR <- list()
        lambda <- if(input$nnetar_lambda == 'NULL') NULL else 'auto'
        fit <- try(nnetar(y = reactiveVariables$TotalSeries,
                          P = input$number_of_seasonal_lags,
                          lambda = lambda, 
                          scale.inputs = input$scale.inputs,
                          repeats = input$repeats))
        if (is(fit, 'try-error')) {
          forecasts_ahead$NNETAR <- NULL
        } else {
          forecastFit <- forecast(fit, h = input$forecast_horizon, level = 95, PI = T)
          forecastFit_dt <- as.data.table(forecastFit)
          forecasts_ahead$NNETAR[['Forecast']] <- forecastFit_dt
        }
        
      }
      
      
### . . . . . . . . .. #< 3de4c0e2af3c3c1fe43a27be80c5154f ># . . . . . . . . ..
      ### ENSEMBLE                                                                ####
      
      if("ENSEMBLE" %in% Algorithms){
        forecasts_ahead$ENSEMBLE <- list()
        ensemble_list <- list()
        to_addup_algorithms <- setdiff(names(forecasts_ahead),"ENSEMBLE")
        for(alg in  to_addup_algorithms){
          dt <- data.table(forecasts_ahead[[alg]][['Forecast']])
          dt <- dt[,lapply(.SD, as.numeric)]
          ensemble_list[[alg]] <- dt
        }
        ensemble_dt <- rbindlist(ensemble_list)
        ensemble_dt <- as.data.table(colMeans(ensemble_dt),keep.rownames = T)
        ensemble_dt <- transpose(ensemble_dt,make.names = 'V1')
        forecasts_ahead$ENSEMBLE[['Forecast']] <- ensemble_dt
      }
      
      ### CLOSING                                                                 ####
      reactiveVariables$Forecasts_ahead <- forecasts_ahead
      removeModal()
      message('Left forecast')
      },
      error = function(e){removeModal()
        showToast(type = "error",message = paste0(toString(e)), .options = myToastOptions)
      },
      warning = function(w){removeModal()
        showToast(type = "error",message = paste0(toString(w)), .options = myToastOptions)
      })
    }
  
  })
  
  
  output$dynamic_tabs2 <- shiny::renderUI({
    req(reactiveVariables$timeSeries_submitted == T)
    req(input$forecast_ahead)
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts_ahead
    req(length(final_results) >= 1)
    
    Tabs <- names(reactiveVariables$Forecasts_ahead)
    chart_lines <- sapply(Tabs, function(x){
      series <- as.vector(reactiveVariables$TotalSeries)
      forecasts <- reactiveVariables$Forecasts_ahead[[x]]$Forecast
      names(forecasts) <- c("Fitted","lowerLimit","upperLimit")
      forecasts <- forecasts[,lapply(.SD, as.numeric)]
      dt <- data.table(Series = c(series))
      dt[,":="(Fitted = numeric(0), lowerLimit = numeric(0), upperLimit = numeric(0))]
      # dt[nrow(dt), Fitted := Series]
      dt <- rbindlist(list(dt,forecasts),use.names = T,fill = T)
      dt[,N := 1:.N]
    }, USE.NAMES = T, simplify = F)
    
    
    gap <- lapply(seq_along(chart_lines), function(x){
      
      shiny::tabPanel(
        title = names(chart_lines[x]),
        div(
          class = "panel",
          div(
            class = "panel-header",
            tags$h3(names(chart_lines[x]))
          ),
          div(
            class = "panel-body",
            highchart()  %>% 
              hc_xAxis(title = list(text = "Observation number"),
                       labels = list(style = list(color ='#fff')))%>%
              hc_yAxis(title = list(text = names(chart_lines[x])),
                       allowDecimals =F,
                       labels = list(style = list(color ='#fff'))) %>%
              hc_tooltip(useHTML= T,
                         followPointer= T,
                         shared = T,
                         padding = 2,
                         animation= T,
                         table = F) %>%
              hc_add_series(chart_lines[[x]], type = "line",
                            name = "Forecast" ,
                            color = '#F8766D',
                            id = "Forecast",
                            hcaes(x = N, y = "Fitted"),
                            tooltip = list(pointFormat = "<b > Forecast :</b> {point.y:.0f} ")) %>%
              hc_add_series(chart_lines[[x]], type = "line",
                            name = "Series",
                            id= "Series",
                            hcaes(x = N, y = "Series"),
                            color = '#619CFF',
                            tooltip = list(pointFormat = "<b > Actual :</b> {point.y:.0f} ")) %>%
              hc_add_series(chart_lines[[x]], type = "arearange", #enableMouseTracking = F,
                            hcaes(x = N, low = lowerLimit,
                                  high =  upperLimit),name = "Confidence Interval",
                            linkedTo = "fit" ,color = 'orange', opacity = 0.2,
                            tooltip = list(pointFormat = paste0("<br> <b>Confidence Interval :</b> {point.lowerLimit: ,.0f} - {point.upperLimit: ,.0f}<br>")))%>%
              hc_chart(zoomType = "xy") %>% 
              hc_add_theme(hc_theme_flatdark())
          )
        )
      )
      
      
    })
    # seq_along(chart_lines) %>%
    #   purrr::map(~ shiny::tabPanel(
    #                  title = names(chart_lines[.x]),
    #                  div(
    #                    class = "panel",
    #                    div(
    #                      class = "panel-header",
    #                      tags$h3(names(chart_lines[.x]))
    #                    ),
    #                    div(
    #                      class = "panel-body",
    #                     
    #                      highchart()  %>% 
    #                        hc_xAxis(
    #                          labels = list(style = list(color ='#fff')))%>%
    #                        hc_yAxis(title = list(text = x),
    #                                 allowDecimals =F,
    #                                 labels = list(style = list(color ='#fff'))) %>%
    #                        hc_tooltip(useHTML= T,
    #                                   followPointer= T,
    #                                   shared = T,
    #                                   padding = 2,
    #                                   animation= T,
    #                                   table = F) %>%
    #                        hc_add_series(dt, type = "line", name = "Forecast" ,
    #                                      hcaes(x = N, y = "Forecast"),
    #                                      tooltip = list(pointFormat = "<b > Value :</b> {point.y:.0f} ")) %>%
    #                        hc_add_series(dt, type = "line", name = "Series",
    #                                      hcaes(x = N, y = "Series"),
    #                                      tooltip = list(pointFormat = "<b > Value :</b> {point.y:.0f} ")) %>%
    #                        hc_chart(zoomType = "xy") %>% 
    #                        hc_add_theme(hc_theme_flatdark())
    #                      
    #                      
    #                    )
    #                  )
    #                )
    #   ) ->gap
    
    # gap <- lapply(rv$start, function(x){
    #   hchart(cars$speed)
    # })
    
    
    do.call(what = shiny::tabsetPanel, 
            args = gap %>% append(list(type = "tabs", id   = "forecasting_charts")))
    
  })
}



shinyApp(ui, server)
