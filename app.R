
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
# library(shinybusy)
Modal <- function(text){
  tags$div(id='modal1',
           modalDialog(
             size="l",
             easyClose = FALSE,
             footer = NULL,
             fade=F,
             tags$head(tags$style("
                       .modal-dialog {
                       top: 30%;
                       }
                       .modal-body {padding: 10px}
                       .modal-content  {-webkit-border-radius: 10px !important;
                       -moz-border-radius: 10px !important;
                       border-radius: 10px !important;}
                       .close { font-size: 16px}
                       .in {background: rgba(0, 0, 0, 0.1);}
                       }")),
             
             tags$head(tags$style( "#modal1 .modal-body{
    position: absolute;}")),
             tags$head(tags$style("#modal1 .modal-dialog{
             
                                  background-color: transparent ;
                    
                                  width:0px;
                                  height:0px;
                                  position: absolute;
                                  top: 30%;
                    
                                  left: 45%;
                                  }")),
             
             tags$head(tags$style("#modal1 .modal-body{ min-height:150px;}")),
             #              tags$head(tags$style("#modal1 .modal-backdrop.fade.in{ opacity: 1 
             #              !important;
             # filter: alpha(opacity=100) !important;
             # background: #fff;}")),
             tags$div(class="sk-folding-cube",
                      tags$div(class = 'sk-cube1 sk-cube'),
                      tags$div(class = 'sk-cube2 sk-cube'),
                      tags$div(class = 'sk-cube3 sk-cube'),
                      tags$div(class = 'sk-cube4 sk-cube')
             ) ,
             tags$div(h4(strong(paste0(text)))))
           ,
           tags$head(tags$style(type="text/css", 
                                "
.sk-folding-cube {
z-index:1000;
  margin: 20px auto;
  width: 100px;
  height: 100px;
  position: relative;
  -webkit-transform: rotateZ(45deg);
          transform: rotateZ(45deg);
}

.sk-folding-cube .sk-cube {
z-index:1000;
  float: left;
  width: 50%;
  height: 50%;
  position: relative;
  -webkit-transform: scale(1.1);
      -ms-transform: scale(1.1);
          transform: scale(1.1); 
}
.sk-folding-cube .sk-cube:before {
   font-size: 350%;
z-index:1000;
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: #00b0f0;
  -webkit-animation: sk-foldCubeAngle 2.4s infinite linear both;
          animation: sk-foldCubeAngle 2.4s infinite linear both;
  -webkit-transform-origin: 100% 100%;
      -ms-transform-origin: 100% 100%;
          transform-origin: 100% 100%;
}
.sk-folding-cube .sk-cube2 {
  -webkit-transform: scale(1.1) rotateZ(90deg);
          transform: scale(1.1) rotateZ(90deg);
}
.sk-folding-cube .sk-cube3 {
  -webkit-transform: scale(1.1) rotateZ(180deg);
          transform: scale(1.1) rotateZ(180deg);
}
.sk-folding-cube .sk-cube4 {
  -webkit-transform: scale(1.1) rotateZ(270deg);
          transform: scale(1.1) rotateZ(270deg);
}
.sk-folding-cube .sk-cube2:before {
  -webkit-animation-delay: 0.3s;
          animation-delay: 0.3s;
}
.sk-folding-cube .sk-cube3:before {
  -webkit-animation-delay: 0.6s;
          animation-delay: 0.6s; 
}
.sk-folding-cube .sk-cube4:before {
  -webkit-animation-delay: 0.9s;
          animation-delay: 0.9s;
}
@-webkit-keyframes sk-foldCubeAngle {
  0%, 10% {
    -webkit-transform: perspective(140px) rotateX(-180deg);
            transform: perspective(140px) rotateX(-180deg);
    opacity: 0; 
  } 25%, 75% {
    -webkit-transform: perspective(140px) rotateX(0deg);
            transform: perspective(140px) rotateX(0deg);
    opacity: 1; 
  } 90%, 100% {
    -webkit-transform: perspective(140px) rotateY(180deg);
            transform: perspective(140px) rotateY(180deg);
    opacity: 0; 
  } 
}

@keyframes sk-foldCubeAngle {
  0%, 10% {
    -webkit-transform: perspective(140px) rotateX(-180deg);
            transform: perspective(140px) rotateX(-180deg);
    opacity: 0; 
  } 25%, 75% {
    -webkit-transform: perspective(140px) rotateX(0deg);
            transform: perspective(140px) rotateX(0deg);
    opacity: 1; 
  } 90%, 100% {
    -webkit-transform: perspective(140px) rotateY(180deg);
            transform: perspective(140px) rotateY(180deg);
    opacity: 0; 
  }
}



"))
           
  )
}#modal
chunk <- function(x, n, force.number.of.groups = TRUE, len = length(x), groups = trunc(len/n), overflow = len%%n) { 
  if(force.number.of.groups) {
    f1 <- as.character(sort(rep(1:n, groups)))
    f <- as.character(c(f1, rep(n, overflow)))
  } else {
    f1 <- as.character(sort(rep(1:groups, n)))
    f <- as.character(c(f1, rep("overflow", overflow)))
  }
  
  g <- split(x, f)
  
  if(force.number.of.groups) {
    g.names <- names(g)
    g.names.ordered <- as.character(sort(as.numeric(g.names)))
  } else {
    g.names <- names(g[-length(g)])
    g.names.ordered <- as.character(sort(as.numeric(g.names)))
    g.names.ordered <- c(g.names.ordered, "overflow")
  }
  
  return(g[g.names.ordered])
}


ui <- fluidPage(theme = shinytheme("superhero"),
                tags$style("

input[type='search'] {
    background-color: #344d5e;
    -webkit-appearance: none;
}


input, textarea {
    color: #fff;
}

.shiny-input-panel {
background-color: #34495e!important;
}


}"),
tags$head(htmltools::tags$style("body {background-color:#34495e}")),
  h2('Time Series Settings'),
  inputPanel(
    fileInput("i_file", "Upload your CSV file"),
    selectInput('variable','Select Series',choices = NULL),
    
    selectInput('frequency', 'Frequency',
                choices = c('Quarterly'=4,'Monthly'=12,'Weekly'=52,'Daily' = 365.25),
                selected = '365.25'),
    sliderInput('holdout', 'Hold-Out %', value = 30, min = 10, max = 50, step = 1),
    br()),
br(),
    conditionalPanel(condition = "(input.variable.length > 0)",
                     actionButton('submit', "Submit")),
    br(),
    hr(),
  highchartOutput('series'),
  br(),
  hr(),
  awesomeCheckboxGroup(inputId = "Algorithm", 
               label = h2("Select Algorithm to Evaluate"), 
               choices = c("ARIMA", "ETS", 'TBATS', 'PROPHET', 'GARCH'),
               selected = "ARIMA", 
               inline = TRUE),
conditionalPanel(condition = 'input.Algorithm.length > 1',
                 checkboxInput('ensemble','Evaluate Ensemble')),
br(),
h3('Algorithms Settings'),
  tabsetPanel(type="tabs",
              
              #build forecasting panels
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
                         conditionalPanel(condition = 'input.frequency > 2 & output.obs >= 2*input.frequency',
                                          checkboxInput('arima_decomposition','Decomposition',value = F))
                         
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
                         conditionalPanel(condition = 'input.frequency > 2 & output.obs >= 2*input.frequency',
                         checkboxInput('ets_decomposition','Decomposition',value = F))
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
                         selectInput(label = 'Growth', inputId = 'growth', choices = c( 'linear', 'logistic',  'flat'),selected = "linear"),
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
                         column(12, h4('variance model'),
                                selectInput(inputId = 'model', label = 'Model', choices = c( "sGARCH", "fGARCH", "eGARCH", "gjrGARCH", "apARCH" , "iGARCH" , "csGARCH"),selected = "sGARCH"),
                                numericInput('grach_variance_p','AR order',value = 1, min=0 , max = 10,step=1),
                                numericInput('grach_variance_q','MA order',value = 1,min=0 , max = 10,step=1),
                                selectInput(label = 'submodel', inputId = 'Submodel', choices = c( "GARCH", "TGARCH", "AVGARCH", "NGARCH", "NAGARCH", "APARCH","GJRGARCH" , "ALLGARCH"),selected = "GARCH")
                                ),
                         column(12,h4('mean model'),
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
                                              selected = "std"))),
                       value=4),

              
              
              id = "timeSeriesTabs"),
br(),
br(),
actionButton('evaluate','Evaluate'),
br(),
br(),
dataTableOutput('results'),
br(),
br()
  
)

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
    reactiveVariables$Series <- mySeries
    reactiveVariables$SeriesNames <- names(mySeries)
  })
  
  observeEvent(reactiveVariables$SeriesNames,{
    if(!is.null(reactiveVariables$SeriesNames) && length(reactiveVariables$SeriesNames) >= 0)
      updateSelectInput(session = session,
                        inputId = 'variable',
                        choices = reactiveVariables$SeriesNames)
  })
  
  
  observeEvent(input$submit,{
    myTimeSeries <- reactiveVariables$Series[[input$variable]]
    reactiveVariables$TotalSeries <- ts(myTimeSeries[!is.na(myTimeSeries)], frequency = as.numeric(input$frequency))
    if(!is.null(reactiveVariables$TotalSeries)){
      total_obs <- length(reactiveVariables$TotalSeries)
      obs_to_hold_out <- floor((input$holdout/100) * total_obs)
      reactiveVariables$Series_to_Fit <- head(reactiveVariables$TotalSeries, total_obs - obs_to_hold_out)
      reactiveVariables$Series_to_Evaluate <- tail(reactiveVariables$TotalSeries, obs_to_hold_out)
      reactiveVariables$Forecasts <- list()
    }
    updateActionButton(session, inputId = 'submit',  icon = icon('check'))
    reactiveVariables$check <- T
  
    reactiveVariables$evaluation_done <- F
 })
  
  output$series <- renderHighchart({
    req(reactiveVariables$check == T)
    total_obs <- length(reactiveVariables$TotalSeries)
    obs_to_hold_out <- floor((isolate(input$holdout)/100) * total_obs)
    bandPlotEnd <- total_obs - obs_to_hold_out
    series <- as.vector(reactiveVariables$TotalSeries)
    
    highchart()  %>% 
      hc_xAxis(plotBands=list(list(
                 color="#e6ffee",
                 from= -1,
                 to=bandPlotEnd,
                 label=list(
                   text="Train Set",
                   align="center"
                 )
                 ),
                 list(
                   color="#ffe9e6",
                   from= bandPlotEnd,
                   to=total_obs,
                   label=list(
                     text="Test Set",
                     align="center"
                   )
                 )), labels = list(style = list(color ='#fff'))
               
      )%>%
      hc_yAxis(title = list(text =  input$variable),
               allowDecimals =F,
               labels = list(style = list(color ='#fff'))) %>%
      hc_tooltip(useHTML= T,
                 followPointer= T,
                 shared = T,
                 padding = 2,
                 animation= T,
                 table = F) %>%
      hc_add_series(series, type = "line",name = input$variable,
      #<br style = 'color:#F8766D'>Observation Number : {point.index:.0f}</br>
      tooltip = list(pointFormat = "<b > Value :</b> {point.y:.0f} ")) %>%
      hc_chart(zoomType = "xy") %>% 
      hc_rangeSelector(enabled = F, 
                       buttonTheme=list(
                         fill= 'none',
                         stroke= 'none',
                         'stroke-width'= 0,
                         r= 8,
                         style=list(
                           color= 'grey',
                           fontWeight= 'bold'
                         ),
                         states=list(
                           hover=list(
                           ),
                           select=list(
                             fill= 'grey',
                             style=list(
                               color= 'white'
                             )
                           )
                         )
                       ),
                       inputBoxBorderColor= 'gray',
                       inputBoxWidth= 120,
                       inputBoxHeight= 18,
                       inputStyle= list(
                         color= 'grey',
                         fontWeight= 'bold'
                       ),
                       labelStyle= list(
                         color= 'grey',
                         fontWeight= 'bold'
                       )
      )%>% 
      hc_add_theme(hc_theme_flatdark())
  })
  
  output$obs <-reactive({
    if(reactiveVariables$check == T){
      length(reactiveVariables$Series_to_Fit)
    } else {
      0
    }
  })
  outputOptions(output, "obs", suspendWhenHidden = FALSE)
  
  observeEvent(c(input$i_file, input$variable, input$holdout, input$frequency),{
    updateActionButton(session, inputId = 'submit',  icon = icon(NULL))
    reactiveVariables$check <- F
  })
  
  

  observeEvent(input$evaluate,{
    req(length(input$Algorithm) > 0)
    if (reactiveVariables$check == T) {
      showModal(Modal(text = ""))
      message('Evaluating')
      forecasts <- list()
      if ("ARIMA" %in% input$Algorithm) {
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
          decomposed_ts <- stl(reactiveVariables$TotalSeries,s.window = "periodic")
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
      }
      
      if("ETS" %in% input$Algorithm){
        forecasts$ETS <- list()
     
        model <- paste0(input$errortype,input$trendtype,input$seasontype)
        # if(frequency(reactiveVariables$Series_to_Fit) > 24){
        #   fit <- stlf(y = reactiveVariables$Series_to_Fit)
        #     
        #     try(ets(reactiveVariables$Series_to_Fit,model=model, allow.multiplicative.trend = input$allow.multiplicative.trend))
        #   
        # }
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
          decomposed_ts <- stl(reactiveVariables$TotalSeries,s.window = "periodic")
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
      }
      
      if("TBATS" %in% input$Algorithm){
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
      }
      
      
      if("PROPHET" %in% input$Algorithm){
        forecasts$PROPHET <- list()
        freq <-  switch(EXPR = input$frequency,
                       '365.25'  = "day",
                       '12'      = "month",
                       '4'       = "quarter",
                       '52'      = "week",
                       '1'       = "year")

        dt <- data.table(ds=seq.Date(from = Sys.Date(),
                                     length.out = length(reactiveVariables$Series_to_Fit),
                                     by = freq),
                         y = reactiveVariables$Series_to_Fit)

        
        
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
          future <- make_future_dataframe(fit, periods = length(reactiveVariables$Series_to_Evaluate))
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
      
      
      if ("GARCH" %in% input$Algorithm) {
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
        
        
        fit <- tryCatch(ugarchfit(spec=spec, data=reactiveVariables$Series_to_Fit),
                        error = function(e){NULL},
                        warning = function(w){NULL})

        
        if (is.null(fit)) {
          forecasts$GARCH <- NULL
        } else {
          forecastFit <- ugarchboot(fit,method=c("Partial","Full")[1],n.ahead = length(reactiveVariables$Series_to_Evaluate),n.bootpred=1000,n.bootfit=1000)
          forecastFit_dt <- data.table('Point Forecast' = as.vector(forecastFit@forc@forecast$sigmaFor))
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
      
      
      
      if(input$ensemble){
        forecasts$ENSEMBLE <- list()
        # browser()
        ensemble_list <- list()
        fit_list <- list()
        for(i in  1:(length(forecasts)-1)){
          dt <- data.table(forecasts[[i]][['Forecast']])
          names(dt) <- names(forecasts[i])
          ensemble_list[[i]] <- dt
          fit_list[[i]] <- forecasts[[i]]['Fit']
        }
        ensemble_dt <- Reduce(f = cbind,ensemble_list,)
        ensemble_dt[, `Point Forecast` := rowMeans(.SD)]
        forecasts$ENSEMBLE[['Fit']] <- fit_list
        forecasts$ENSEMBLE[['Forecast']] <- ensemble_dt[, `Point Forecast`]
        forecasts$ENSEMBLE[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, ensemble_dt[, `Point Forecast`])
        forecasts$ENSEMBLE[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, ensemble_dt[, `Point Forecast`])
        forecasts$ENSEMBLE[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, ensemble_dt[, `Point Forecast`])
        
      }
      
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
      names(final_results)
      result_list <- list()

      for(i in  1:length(final_results)){
        result_list[[i]] <- data.table('Algorithm' = names(final_results[i]),
                                       'RMSE' = round(final_results[[i]][['RMSE']],3),
                                       'MAE' = round(final_results[[i]][['MAE']],3),
                                       'MAPE' = round(final_results[[i]][['MAPE']],3))
      }

      result_dt <- rbindlist(result_list)
      browser()
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
  
  

  
}



shinyApp(ui, server)
