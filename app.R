
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
library(highcharter)
library(shinythemes)
library(shinyalert)
library(shinyjs)
# library(shinybusy)

source("helper_functions.R")
#### UI ####
ui <- fluidPage(theme = shinytheme("superhero"),
tags$style(style),
useShinyjs(),
useShinyalert(),
  h2('Time Series Settings'),
  inputPanel(
    fileInput("i_file", "Upload your CSV file"),
    selectInput('variable','Select Series',choices = NULL),
    awesomeRadio(inputId = "evaluation_type", 
                 label = "Evaluation type", 
                 choices = c("Validation set" = 1, 
                             "Cross Validation" = 2), 
                 selected = 1),
    uiOutput('evaluation_type_parameters'),
    br(),
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
    awesomeCheckboxGroup(inputId = "Algorithm", 
                 label = h2("Select Algorithm to Evaluate"), 
                 choices = c("NAIVE","DRIFT","ARIMA", "ETS", 'TBATS', 'PROPHET'),
                 selected = "NAIVE", 
                 inline = TRUE),
# conditionalPanel(condition = 'input.Algorithm.length > 1',
    #                  checkboxInput('ensemble','Evaluate Ensemble')),
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
                  tabPanel("ARIMA", icon = icon("line-chart"), h4("ARIMA"),
                           br(),
                           inputPanel(
                          
                             numericInput('max.p','max.p',value = 5),
                             numericInput('max.q','max.q',value = 5),
                             numericInput('max.P','max.P',value = 2),
                             numericInput('max.Q','max.Q',value = 2),
                             numericInput('max.order','max.order',value = 5),
                             numericInput('max.d','max.d',value = 2),
                             numericInput('max.D','max.D',value = 1),
                             selectInput('ic','Information criterion', choices = c("aicc", "aic", "bic"), selected = "aic"),
                             checkboxInput('allowdrift','Allow drift',value = T), 
                             checkboxInput('allowmean','Allow mean',value = T),
                             checkboxInput('seasonal','Seasonality',value = T),
                             checkboxInput('arima_decomposition','Decomposition',value = F)
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
                             checkboxInput('allow.multiplicative.trend', 'Allow multiplicative trend',value = F),
                             checkboxInput('ets_decomposition','Decomposition',value = F)
                             # ,
                             # conditionalPanel(condition = 'input.frequency > 2 & output.obs >= 2*input.frequency',
                             # checkboxInput('ets_decomposition','Decomposition',value = F))
                             ),
                           value=2),
                  tabPanel("TBATS", icon = icon("line-chart"), h4("TBATS"), 
                           br(), 
                           inputPanel(
                             checkboxInput('use.arma.errors','Use ARMA errors',value = T)),
                           value=3),
                  tabPanel("PROPHET", icon = icon("line-chart"), h4("TBATS"), 
                           br(), 
                           inputPanel(
                             selectInput(label = 'Growth', inputId = 'growth', choices = c( 'linear', 'flat'),selected = "linear"),
                             selectInput(label = 'Seasonality mode', inputId = 'seasonality.mode', choices = c('additive', 'multiplicative'),selected = "additive"),
                             selectInput(label = 'Yearly seasonality', inputId = 'yearly.seasonality', choices = c('auto', TRUE, FALSE),selected = "auto"),
                             selectInput(label = 'Weekly seasonality', inputId = 'weekly.seasonality', choices = c('auto', TRUE, FALSE),selected = "auto"),
                             selectInput(label = 'Daily seasonality', inputId = 'daily.seasonality', choices = c('auto', TRUE, FALSE),selected = "auto"),
                             numericInput(inputId = 'n.changepoints', label = 'Number of changepoints', value = 25, min = 0, max = 100),
                             numericInput(inputId = 'changepoint.range', label = 'Changepoint range', value = 0.8, min = 0.1, max = 1)),
                           value=4),
                  tabPanel("GARCH", icon = icon("line-chart"), h4("GARCH"), 
                           br(), 
                           inputPanel(
                             column(12, h4('Variance model'),
                                    selectInput(inputId = 'model', label = 'Model', choices = c( "sGARCH", "fGARCH", "eGARCH", "gjrGARCH", "apARCH" , "iGARCH" , "csGARCH"),selected = "sGARCH"),
                                    numericInput('grach_variance_p','AR order',value = 1, min=0 , max = 10,step=1),
                                    numericInput('grach_variance_q','MA order',value = 1,min=0 , max = 10,step=1),
                                    selectInput(inputId = 'submodel', label = 'Submodel', choices = c( "GARCH", "TGARCH", "AVGARCH", "NGARCH", "NAGARCH", "APARCH","GJRGARCH" , "ALLGARCH"),selected = "GARCH")
                                    ),
                             column(12,h4('Mean model'),
                                    numericInput('grach_mean_p','AR order',value = 1,min=0 , max = 10,step=1),
                                    numericInput('grach_mean_q','MA order',value = 1, min=0 ,max = 10,step=1),
                                    checkboxInput('include.mean', 'Include mean', value = ),
                                    checkboxInput('archm', 'include ARCH volatility in the mean regression', value = ),
                                    selectInput('archpow', 'Use st.deviation or variance?', choices = c('std'=1,'Variance'=2)),
                                    checkboxInput('arfima', 'Use fractional differencing in the ARMA regression'),
                                    ),
                             column(12,selectInput(inputId = 'distribution.model', 
                                                  label = h4('Distribution model'), 
                                                  choices = c('student-t' ="std" ,
                                                              'skew-student'="sstd" ,
                                                              'generalized error distribution'="ged",
                                                  'skew-generalized error distribution'="sged",
                                                  'the normal inverse gaussian distribution'="nig" ,
                                                  'Generalized Hyperbolic'= "ghyp" , 
                                                  "Johnson's SU" = "jsu"),
                                                  selected = "std")),
                             column(12,
                             selectInput(inputId = 'solver', label = h4('Select solver'),
                                         choices = c( "nlminb", "solnp", "lbfgs", "gosolnp", "nloptr" , "hybrid"),
                                         selected = "solnp"))),
                           
                           
                           value=4),
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
#### server ####
server <- function(input, output, session) {
  reactiveVariables <- reactiveValues()
  reactiveVariables$check <- F
  reactiveVariables$evaluation_done <- F
  
  observeEvent(input$i_file,{
    inFile <- input$i_file
    if (is.null(inFile))
      return(NULL)
    mySeries <- fread(inFile$datapath)
    mySeries <- mySeries[,lapply(.SD, as.numeric)]
    if(nrow(mySeries[complete.cases(mySeries)])== 0){
      shinyalert(title = "An error occured reading the data. Make sure there are not missing values and the column contains only numbers.", 
                 type = "error")
    }
    reactiveVariables$Series <- mySeries
    reactiveVariables$SeriesNames <- names(mySeries)
  })
  
  observeEvent(reactiveVariables$SeriesNames,{
    if(!is.null(reactiveVariables$SeriesNames) && length(reactiveVariables$SeriesNames) >= 0)
      updateSelectInput(session = session,
                        inputId = 'variable',
                        choices = reactiveVariables$SeriesNames)
  })
  
  output$evaluation_type_parameters <- renderUI({
    req(reactiveVariables$Series)
    req(input$variable)
    series <- reactiveVariables$Series[[input$variable]]
    if(input$evaluation_type == 1){
      sliderInput('holdout', 'Hold-Out observations', value = 1, min = 1, max = floor(length(series)/2), step = 1)
    }
    else{
      sliderInput('horizon', 'Horizon to evaluate', value = 1, min = 1, max = 5, step = 1)
    }
  })
  
  output$frequency_type <- renderUI({
    if(input$frequency_known == 1){
      selectInput(inputId = 'frequency',
                  label = 'Frequency',
                  choices =  c('day' = 1,
                               'week' = 7, 
                               'month' = 12,
                               'quarter' = 4,
                               'year' = 365),
                  selected = 1)
    } 
  })
  
  observeEvent(input$submit,{
    myTimeSeries <- reactiveVariables$Series[[input$variable]]
    if(input$frequency_known == 1 & !is.null(input$frequency)){
      reactiveVariables$TotalSeries <- ts(myTimeSeries[!is.na(myTimeSeries)], frequency = as.numeric(input$frequency))
    } else {
      reactiveVariables$TotalSeries <- ts(myTimeSeries[!is.na(myTimeSeries)], 
                                          frequency = findfrequency(myTimeSeries[!is.na(myTimeSeries)]))
      
    }
      
    if(input$evaluation_type == 1){
      if(!is.null(reactiveVariables$TotalSeries)){
        total_obs <- length(reactiveVariables$TotalSeries)
        obs_to_hold_out <- input$holdout#floor((input$holdout/100) * total_obs)
        reactiveVariables$Series_to_Fit <- head(reactiveVariables$TotalSeries, total_obs - obs_to_hold_out)
        reactiveVariables$Series_to_Evaluate <- tail(reactiveVariables$TotalSeries, obs_to_hold_out)
        reactiveVariables$Forecasts <- list()
      }
    }
    updateActionButton(session, inputId = 'submit',  icon = icon('check'))
    reactiveVariables$check <- T
    reactiveVariables$evaluation_done <- F
 })
  
observeEvent(reactiveVariables$check ,{
  if(reactiveVariables$check == T){
    shinyjs::show('seriesdiv')
  } else {
    shinyjs::hide('seriesdiv')
  }
})
  
  output$series <- renderHighchart({
    req(reactiveVariables$check == T)
    series <- as.vector(reactiveVariables$TotalSeries)
    if(input$evaluation_type == 1){
      total_obs <- length(series)
      obs_to_hold_out <- input$holdout#floor((isolate(input$holdout)/100) * total_obs)
      bandPlotEnd <- total_obs - obs_to_hold_out

      # browser()
      highchart()  %>% 
        hc_xAxis(
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
        hc_xAxis(labels = list(style = list(color ='#ffffff')))%>%
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
  
  # output$obs <-reactive({
  #   if(reactiveVariables$check == T){
  #     length(reactiveVariables$Series_to_Fit)
  #   } else {
  #     0
  #   }
  # })
  
  observeEvent(input$evaluation_type,{
    if(input$evaluation_type == 1){
      updateAwesomeCheckboxGroup(session = session,inputId = "Algorithm",choices = c("NAIVE","DRIFT","ARIMA", "ETS", 'TBATS', 'PROPHET', 'GARCH'),inline = T)
    } else {
      updateAwesomeCheckboxGroup(session = session,inputId = "Algorithm",choices = c("NAIVE","DRIFT","ARIMA", "ETS", 'TBATS'),inline = T)
    }
  })
  
  # output$frequency <-reactive({
  #   if(reactiveVariables$check == T){
  #     findfrequency(reactiveVariables$Total_Series)
  #   } else {
  #     0
  #   }
  # })
  # outputOptions(output, "obs", suspendWhenHidden = FALSE)
  
  observeEvent(c(input$i_file, input$variable, input$holdout, input$horizon, input$evaluation_type),{
    updateActionButton(session, inputId = 'submit',  icon = icon(NULL))
    reactiveVariables$check <- F
  })
  
  

  observeEvent(input$evaluate,{
    req(length(input$Algorithm) > 0)
    if (reactiveVariables$check == T) {
      showModal(Modal(text = ""))
      message('Evaluating')
      forecasts <- list()
      
      if("NAIVE" %in% input$Algorithm) {
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
            forecastFit <- tsCV(y = reactiveVariables$TotalSeries,forecastfunction = snaive, h = input$horizon) 
          }else{
            forecastFit <- tsCV(y = reactiveVariables$TotalSeries,forecastfunction = naive, h = input$horizon) 
          }
          if(input$horizon > 1){
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts$NAIVE[['MAE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})]
            forecasts$NAIVE[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})]
            forecasts$NAIVE[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})]
          } else {
            forecasts$NAIVE[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
            forecasts$NAIVE[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
            forecasts$NAIVE[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
          }
        }
      }
      
      if("DRIFT" %in% input$Algorithm) {
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
          forecastFit <- tsCV(y = reactiveVariables$TotalSeries,forecastfunction = rwf, h = input$horizon) 
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
      
      
      if("ARIMA" %in% input$Algorithm) {
           if(input$evaluation_type == 1) {
              forecasts$ARIMA <- list()
              fit <- try(auto.arima(reactiveVariables$Series_to_Fit,
                                    max.p = input$max.p,
                                    max.q = input$max.q,
                                    max.P = input$max.P,
                                    max.Q = input$max.Q,
                                    max.order = input$max.order,
                                    max.d = input$max.d,
                                    max.D = input$max.D,
                                    ic = input$ic,
                                    allowdrift = input$allowdrift,
                                    allowmean = input$allowmean,
                                    seasonal = input$seasonal,
                                    stepwise = T))
              if (is(fit, 'try-error')) {
                forecasts$ARIMA <- NULL
              } else {
                forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
                forecastFit_dt <- as.data.table(forecastFit)
                forecasts$ARIMA[['Fit']] <- fit
                forecasts$ARIMA[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
                forecasts$ARIMA[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
                forecasts$ARIMA[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
                forecasts$ARIMA[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              }
    
            if(input$arima_decomposition == T & (frequency(reactiveVariables$Series_to_Fit) >= 2 & length(reactiveVariables$Series_to_Fit)>2*frequency(reactiveVariables$Series_to_Fit))){
              forecasts$ARIMA_decomposition <- list()
              decomposed_ts <- stl(reactiveVariables$TotalSeries,s.window = "periodic",robust = T)
              seasonaly_adjusted <- reactiveVariables$TotalSeries - decomposed_ts$time.series[,'seasonal']
              seasonaly_adjusted_to_Fit <- head(seasonaly_adjusted, length(reactiveVariables$Series_to_Fit))
              seasonal_to_add <- tail(decomposed_ts$time.series[,'seasonal'], length(reactiveVariables$Series_to_Evaluate))
              fit <- try(auto.arima(seasonaly_adjusted_to_Fit,
                                    max.p = input$max.p,
                                    max.q = input$max.q,
                                    max.P = input$max.P,
                                    max.Q = input$max.Q,
                                    max.order = input$max.order,
                                    max.d = input$max.d,
                                    max.D = input$max.D,
                                    ic = input$ic,
                                    allowdrift = input$allowdrift,
                                    allowmean = input$allowmean,
                                    seasonal = input$seasonal,
                                    stepwise = T))
              if (is(fit, 'try-error')) {
                forecasts$ARIMA_decomposition <- NULL
              } else {
                forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
                forecastFit <- forecastFit$mean + seasonal_to_add
                forecastFit_dt <- as.data.table(forecastFit)
                names(forecastFit_dt) <- 'Point Forecast'
                forecasts$ARIMA_decomposition[['Fit']] <- fit
                forecasts$ARIMA_decomposition[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
                forecasts$ARIMA_decomposition[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
                forecasts$ARIMA_decomposition[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
                forecasts$ARIMA_decomposition[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              }
            }
         }else{
           ### cv
           forecasts$ARIMA <- list()
           forecast_Arima <-function(x,h){ 
             fit = auto.arima(x,
                             max.p = input$max.p,
                             max.q = input$max.q,
                             max.P = input$max.P,
                             max.Q = input$max.Q,
                             max.order = input$max.order,
                             max.d = input$max.d,
                             max.D = input$max.D,
                             ic = input$ic,
                             allowdrift = input$allowdrift,
                             allowmean = input$allowmean,
                             seasonal = input$seasonal,
                             stepwise = T)
             forecast(fit,h)
           }
           forecastFit <- try(tsCV(y = reactiveVariables$TotalSeries,forecastfunction = forecast_Arima, h = input$horizon) )
           if (is(forecastFit, 'try-error')) {
             forecasts$ARIMA <- NULL
           } else {
             if(input$horizon > 1){
               forecastFit_dt <- as.data.table(forecastFit)
               forecasts$ARIMA[['MAE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})]
               forecasts$ARIMA[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})]
               forecasts$ARIMA[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})]
             } else {
               forecasts$ARIMA[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
               forecasts$ARIMA[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
               forecasts$ARIMA[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
             }
           }
           
           if(input$arima_decomposition == T & (frequency(reactiveVariables$TotalSeries) >= 2 & length(reactiveVariables$TotalSeries)>2*frequency(reactiveVariables$TotalSeries))){
             forecasts$ARIMA_decomposition <- list()
             decomposed_ts <- stl(reactiveVariables$TotalSeries,s.window = "periodic",robust = T)
             seasonaly_adjusted <- reactiveVariables$TotalSeries - decomposed_ts$time.series[,'seasonal']
             forecastFit <- try(tsCV(y = seasonaly_adjusted,forecastfunction = forecast_Arima, h =  input$horizon) )
             if (is(forecastFit, 'try-error')) {
               forecasts$ARIMA_decomposition <- NULL
             } else {
               if(input$horizon > 1){
                 forecastFit_dt <- as.data.table(forecastFit)
                 forecasts$ARIMA_decomposition[['MAE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x), na.rm = T)})]
                 forecasts$ARIMA_decomposition[['RMSE']]  <- forecastFit_dt[,lapply(.SD, function(x){ sqrt(mean(x^2, na.rm=TRUE))})]
                 forecasts$ARIMA_decomposition[['MAPE']]  <- forecastFit_dt[,lapply(.SD, function(x){ mean(abs(x/reactiveVariables$TotalSeries), na.rm = T)})]
               } else {
                 forecasts$ARIMA_decomposition[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
                 forecasts$ARIMA_decomposition[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
                 forecasts$ARIMA_decomposition[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
               }
             }
           }
           
         }    
      }
      
      if("ETS" %in% input$Algorithm){
        if(input$evaluation_type == 1) {
          forecasts$ETS <- list()
          model <- paste0(input$errortype,input$trendtype,input$seasontype)
          fit <- try(ets(reactiveVariables$Series_to_Fit,model=model, allow.multiplicative.trend = input$allow.multiplicative.trend))
          if (is(fit, 'try-error')) {
            forecasts$ETS <- NULL
          } else {
            forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts$ETS[['Fit']] <- fit
            forecasts$ETS[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
            forecasts$ETS[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$ETS[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$ETS[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
          }
          
          if(input$arima_decomposition == T  & (frequency(reactiveVariables$Series_to_Fit) >= 2 & length(reactiveVariables$Series_to_Fit)>2*frequency(reactiveVariables$Series_to_Fit))){
            forecasts$ETS_decomposition <- list()
            decomposed_ts <- stl(reactiveVariables$TotalSeries,s.window = "periodic",robust = T)
            seasonaly_adjusted <- reactiveVariables$TotalSeries - decomposed_ts$time.series[,'seasonal']
            seasonaly_adjusted_to_Fit <- head(seasonaly_adjusted, length(reactiveVariables$Series_to_Fit))
            seasonal_to_add <- tail(decomposed_ts$time.series[,'seasonal'], length(reactiveVariables$Series_to_Evaluate))
            fit <- try(ets(seasonaly_adjusted_to_Fit,model=model, allow.multiplicative.trend = input$allow.multiplicative.trend))
            if (is(fit, 'try-error')) {
              forecasts$ETS_decomposition <- NULL
            } else {
              forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
              forecastFit <- forecastFit$mean + seasonal_to_add
              forecastFit_dt <- as.data.table(forecastFit)
              names(forecastFit_dt) <- 'Point Forecast'
              forecasts$ETS_decomposition[['Fit']] <- fit
              forecasts$ETS_decomposition[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
              forecasts$ETS_decomposition[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$ETS_decomposition[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$ETS_decomposition[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            }
          }
        } else {
          ### cv
          model <- paste0(input$errortype,input$trendtype,input$seasontype)
          forecasts$ETS <- list()
          forecast_ETS <-function(x,h){ 
            fit = ets(x,model=model, allow.multiplicative.trend = input$allow.multiplicative.trend)
            forecast(fit,h)
          }
          
          forecastFit <- try(tsCV(y = reactiveVariables$TotalSeries,forecastfunction = forecast_ETS, h = input$horizon) )
          if (is(forecastFit, 'try-error')) {
            forecasts$ETS <- NULL
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
          
          if(input$ets_decomposition == T & (frequency(reactiveVariables$TotalSeries) >= 2 & length(reactiveVariables$TotalSeries)>2*frequency(reactiveVariables$TotalSeries))){
            forecasts$ETS_decomposition <- list()
            decomposed_ts <- stl(reactiveVariables$TotalSeries,s.window = "periodic",robust = T)
            seasonaly_adjusted <- reactiveVariables$TotalSeries - decomposed_ts$time.series[,'seasonal']
            forecastFit <- try(tsCV(y = seasonaly_adjusted,forecastfunction = forecast_ETS, h = 1) )
            if (is(forecastFit, 'try-error')) {
              forecasts$ETS_decomposition <- NULL
            } else {
              forecasts$ETS_decomposition[['MAE']]  <- mean(abs(forecastFit), na.rm = T)
              forecasts$ETS_decomposition[['RMSE']] <- sqrt(mean(forecastFit^2, na.rm=TRUE))
              forecasts$ETS_decomposition[['MAPE']] <- mean(abs(forecastFit/reactiveVariables$TotalSeries), na.rm = T)
            }
          } 
        }
      } 
      
      if("TBATS" %in% input$Algorithm){
        if(input$evaluation_type == 1) {
          forecasts$TBATS <- list()
          fit <- try(tbats(reactiveVariables$Series_to_Fit,use.arma.errors = input$use.arma.errors))
          if (is(fit, 'try-error')) {
            forecasts$TBATS <- NULL
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
          
          forecastFit <- try(tsCV(y = reactiveVariables$TotalSeries,forecastfunction = forecast_TBATS, h = input$horizon) )
          if (is(forecastFit, 'try-error')) {
            forecasts$TBATS <- NULL
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
      
      
      if("PROPHET" %in% input$Algorithm & input$evaluation_type == 1){
        forecasts$PROPHET <- list()
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
      }
      
      
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
        
        
        # if(input$GARCH_decomposition == T){
        #   forecasts$GARCH_decomposition <- list()
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
        #                         ic = input$ic,
        #                         allowdrift = input$allowdrift,
        #                         allowmean = input$allowmean,
        #                         seasonal = input$seasonal,
        #                         stepwise = T))
        #   if (is(fit, 'try-error')) {
        #     forecasts$GARCH_decomposition <- NULL
        #   } else {
        #     forecastFit <- forecast(fit, length(reactiveVariables$Series_to_Evaluate))
        #     forecastFit <- forecastFit$mean + seasonal_to_add
        #     forecastFit_dt <- as.data.table(forecastFit)
        #     names(forecastFit_dt) <- 'Point Forecast'
        #     forecasts$GARCH_decomposition[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
        #     # forecasts$GARCH[['AIC']]  <- fit$aic
        #     forecasts$GARCH_decomposition[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
        #     forecasts$GARCH_decomposition[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
        #     forecasts$GARCH_decomposition[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
        #   }
        # }
      }
      
      
      
      # if(input$ensemble & input$evaluation_type == 1){
      #   forecasts$ENSEMBLE <- list()
      #   # browser()
      #   ensemble_list <- list()
      #   fit_list <- list()
      #   for(i in  1:(length(forecasts)-1)){
      #     dt <- data.table(forecasts[[i]][['Forecast']])
      #     names(dt) <- names(forecasts[i])
      #     ensemble_list[[i]] <- dt
      #     fit_list[[i]] <- forecasts[[i]]['Fit']
      #   }
      #   ensemble_dt <- Reduce(f = cbind,ensemble_list,)
      #   ensemble_dt[, `Point Forecast` := rowMeans(.SD)]
      #   forecasts$ENSEMBLE[['Fit']] <- fit_list
      #   forecasts$ENSEMBLE[['Forecast']] <- ensemble_dt[, `Point Forecast`]
      #   forecasts$ENSEMBLE[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, ensemble_dt[, `Point Forecast`])
      #   forecasts$ENSEMBLE[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, ensemble_dt[, `Point Forecast`])
      #   forecasts$ENSEMBLE[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, ensemble_dt[, `Point Forecast`])
      #   
      # }
      
      reactiveVariables$evaluation_done <- T
      reactiveVariables$Forecasts <- forecasts
      removeModal()
      message('Left evaluation')
    }

    })
  


  output$results <- renderDataTable({
      req(reactiveVariables$evaluation_done)
      final_results <- reactiveVariables$Forecasts
      req(length(final_results) >= 1)
      # names(final_results)
      result_list <- list()
      for(i in  1:length(final_results)){
        result_list[[i]] <- data.table('Algorithm' = names(final_results[i]),
                                       'RMSE' = final_results[[i]][['RMSE']],
                                       'MAE' = final_results[[i]][['MAE']],
                                       'MAPE' = final_results[[i]][['MAPE']])
   
      }

      result_dt <- rbindlist(result_list,fill = T)
      reactiveVariables$Results <- result_dt
      datatable(result_dt, 
                rownames = FALSE,
                # fillContainer = TRUE,
                height = paste0(200*nrow(result_dt),'px'),
                options = list(pageLength = 10,
                               width="100%", 
                               scrollY = F,
                               dom = 't',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#34495e', 'color': '#fff'});",
                                 "}"
                               )))%>% 
        formatStyle(names(df), backgroundColor = "#34495e", color ='#bfa423')
  })
  
  output$dt_row_selector <- renderUI({
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts
    req(length(final_results) >= 1)
    checkboxInput("dt_sel", "Select or deselect all")
  })
  
  dt_proxy <- DT::dataTableProxy("results")
  
  
  observeEvent(input$dt_sel, {
    if (isTRUE(input$dt_sel)) {
      DT::selectRows(dt_proxy, input$results_rows_all)
    } else {
      DT::selectRows(dt_proxy, NULL)
    }
  })
  
  output$forecast_widget_container <- renderUI({
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts
    req(length(final_results) >= 1)
    tagList(
    actionButton('forecast_ahead', "Forecast ahead with the selected row from the evaluation's results table"),
    numericInput('forecast_horizon','Forecast horizon',value =1, min=1,max =100))
  })
  
  
  
  output$forecast_tabs_container <- renderUI({
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts
    req(length(final_results) >= 1)
    # div(
    #   align = 'left',
    #   class = "container",
      shiny::uiOutput("dynamic_tabs2")
    # )
  })
  
  
  
  
  output$dynamic_tabs <- shiny::renderUI({
    req(input$evaluation_type == 1)
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts
    req(length(final_results) >= 1)
    Tabs <- names(reactiveVariables$Forecasts)
   
    
    
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
             hc_xAxis(
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
            args = gap %>% append(list(type = "tabs", id   = "evaluation_charts")))
    
  })
  
  
  observeEvent(input$forecast_ahead,{
    if(length(input$results_rows_selected) == 0){
      shinyalert(text = 'Select a row from the results table', type = 'info')
    } else {
  
      showModal(Modal(text = ""))
      message('Forecasting')
      forecasts_ahead <- list()
      Result_dt <- reactiveVariables$Results
      Algorithms <- reactiveVariables$Results[input$results_rows_selected,Algorithm]
        
      if ("NAIVE" %in% Algorithms) {
        forecasts_ahead$NAIVE <- list()
        if (input$seasonal_naive == T) {
          forecastFit <- snaive(reactiveVariables$TotalSeries, h = input$forecast_horizon)
        } else{
          forecastFit <- naive(reactiveVariables$TotalSeries, h = input$forecast_horizon)
        }
        forecastFit_dt <- as.data.table(forecastFit)
        forecasts_ahead$NAIVE[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
      }
      
      if("DRIFT" %in% Algorithms) {
          forecasts_ahead$DRIFT <- list()
          forecastFit <- rwf(reactiveVariables$TotalSeries, h=input$forecast_horizon, drift=TRUE)
          forecastFit_dt <- as.data.table(forecastFit)
          forecasts_ahead$DRIFT[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
      }

      if("ARIMA" %in% Algorithms) {
          forecasts_ahead$ARIMA <- list()
          fit <- try(auto.arima(reactiveVariables$TotalSeries,
                                max.p = input$max.p,
                                max.q = input$max.q,
                                max.P = input$max.P,
                                max.Q = input$max.Q,
                                max.order = input$max.order,
                                max.d = input$max.d,
                                max.D = input$max.D,
                                ic = input$ic,
                                allowdrift = input$allowdrift,
                                allowmean = input$allowmean,
                                seasonal = input$seasonal,
                                stepwise = T))
          if (is(fit, 'try-error')) {
            forecasts_ahead$ARIMA <- NULL
          } else {
            forecastFit <- forecast(fit, input$forecast_horizon)
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts_ahead$ARIMA[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
          }
          
          if(input$arima_decomposition == T & (frequency(reactiveVariables$TotalSeries) >= 2 & length(reactiveVariables$TotalSeries)>2*frequency(reactiveVariables$TotalSeries))){
            forecasts_ahead$ARIMA_decomposition <- list()
            decomposed_ts <- stl(reactiveVariables$TotalSeries,s.window = "periodic",robust = T)
            seasonaly_adjusted <- reactiveVariables$TotalSeries - decomposed_ts$time.series[,'seasonal']
            seasonaly_adjusted_to_Fit <- head(seasonaly_adjusted, length(reactiveVariables$TotalSeries))
            seasonal_to_add <- tail(decomposed_ts$time.series[,'seasonal'], input$forecast_horizon)
            fit <- try(auto.arima(seasonaly_adjusted_to_Fit,
                                  max.p = input$max.p,
                                  max.q = input$max.q,
                                  max.P = input$max.P,
                                  max.Q = input$max.Q,
                                  max.order = input$max.order,
                                  max.d = input$max.d,
                                  max.D = input$max.D,
                                  ic = input$ic,
                                  allowdrift = input$allowdrift,
                                  allowmean = input$allowmean,
                                  seasonal = input$seasonal,
                                  stepwise = T))
            if (is(fit, 'try-error')) {
              forecasts_ahead$ARIMA_decomposition <- NULL
            } else {
              forecastFit <- forecast(fit, input$forecast_horizon)
              forecastFit_dt <- as.data.table(forecastFit)
              forecastFit_dt <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
              forecastFit_dt[,c('Point Forecast', 'Lo 95' , 'Hi 95') := .(
                `Point Forecast` + seasonal_to_add, `Lo 95` + seasonal_to_add, `Hi 95` + seasonal_to_add
              )]
             forecasts_ahead$ARIMA_decomposition[['Forecast']] <- forecastFit_dt
            }
          }
      }
      
      if("ETS" %in% Algorithms){
          forecasts_ahead$ETS <- list()
          model <- paste0(input$errortype,input$trendtype,input$seasontype)
          fit <- try(ets(reactiveVariables$TotalSeries,model=model, allow.multiplicative.trend = input$allow.multiplicative.trend))
          if (is(fit, 'try-error')) {
            forecasts_ahead$ETS <- NULL
          } else {
            forecastFit <- forecast(fit, input$forecast_horizon)
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts_ahead$ETS[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
          }
          
          if(input$arima_decomposition == T  & (frequency(reactiveVariables$TotalSeries) >= 2 & length(reactiveVariables$TotalSeries)>2*frequency(reactiveVariables$TotalSeries))){
            forecasts_ahead$ETS_decomposition <- list()
            decomposed_ts <- stl(reactiveVariables$TotalSeries,s.window = "periodic",robust = T)
            seasonaly_adjusted <- reactiveVariables$TotalSeries - decomposed_ts$time.series[,'seasonal']
            seasonaly_adjusted_to_Fit <- head(seasonaly_adjusted, length(reactiveVariables$TotalSeries))
            seasonal_to_add <- tail(decomposed_ts$time.series[,'seasonal'], input$forecast_horizon)
            fit <- try(ets(seasonaly_adjusted_to_Fit,model=model, allow.multiplicative.trend = input$allow.multiplicative.trend))
            if (is(fit, 'try-error')) {
              forecasts_ahead$ETS_decomposition <- NULL
            } else {
              forecastFit <- forecast(fit, input$forecast_horizon)
              forecastFit_dt <- as.data.table(forecastFit)
              forecastFit_dt <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
              forecastFit_dt[,c('Point Forecast', 'Lo 95' , 'Hi 95') := .(
                `Point Forecast` + seasonal_to_add, `Lo 95` + seasonal_to_add, `Hi 95` + seasonal_to_add
              )]
              forecasts_ahead$ETS_decomposition[['Forecast']] <- forecastFit_dt
            }
          }

      } 
      
      if("TBATS" %in% Algorithms){
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
      
      if("PROPHET" %in% Algorithms & input$evaluation_type == 1){
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
      removeModal()
      message('Left forecast')
      reactiveVariables$Forecasts_ahead <- forecasts_ahead
    }
  })
  
  
  output$dynamic_tabs2 <- shiny::renderUI({
    req(input$forecast_ahead)
    req(input$evaluation_type == 1)
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts_ahead
    req(length(final_results) >= 1)
    Tabs <- names(reactiveVariables$Forecasts_ahead)
    
    chart_lines <- sapply(Tabs, function(x){
      series <- as.vector(reactiveVariables$TotalSeries)
      forecasts <- reactiveVariables$Forecasts_ahead[[x]]$Forecast
      names(forecasts) <- c("Fitted","lowerLimit","upperLimit")
      dt <- data.table(Series = c(series))
      dt[,":="(Fitted = NA, lowerLimit = NA, upperLimit = NA)]
      dt <- rbindlist(list(dt,forecasts),use.names = T,fill = T)
      dt[,N := 1:.N]
    }, USE.NAMES = T, simplify = F)
    
    
    # browser()
    
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
              hc_xAxis(
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
