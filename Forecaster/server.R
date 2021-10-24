rm(list = ls());gc();

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
  
  
  # output$observations_to_account <- renderUI({
  #   req(reactiveVariables$Series)
  #   # req(nrow(reactiveVariables$Series)>0)
  #   # req(!is.null(reactiveVariables$SeriesNames)
  #   # req(length(reactiveVariables$SeriesNames) >= 0)
  #   
  #     sliderInput("observations", 
  #                 label = ("Obseravations to load"), 
  #                 min = 10,
  #                 max = nrow(reactiveVariables$Series), value = c(10, nrow(reactiveVariables$Series)))
  # })
  
  observeEvent(reactiveVariables$Series,{
    if(nrow(reactiveVariables$Series)>0 & !is.null(reactiveVariables$SeriesNames) & length(reactiveVariables$SeriesNames) >= 0){
      updateSliderInput(session = session,
      inputId = 'observations',
      min = 10,
      max = nrow(reactiveVariables$Series), value = c(10, nrow(reactiveVariables$Series)))
    }  
  },ignoreNULL = T)
  

  ##  .................. #< 1160059a5e062dc3e74ef06c65f639c0 ># ..................
  ##  Time series reading                                                     ####
  
  observeEvent(input$submit,{
    myTimeSeries <- as.numeric(reactiveVariables$Series[[input$variable]])[input$observations[1]:input$observations[2]]
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
  
  
  output$holdout_slider <- renderUI({
    req(reactiveVariables$timeSeries_submitted)
    series <- reactiveVariables$TotalSeries
    sliderInput('holdout', 'Hold-Out observations', value = floor(length(series)*0.3), min = 1, max = floor(length(series)/2), step = 1)
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
    total_obs <- length(series)
    obs_to_hold_out <- input$holdout
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
      hc_add_series(series,
                    type = "line",
                    name = input$variable,color = '#619CFF',
                    tooltip = list(pointFormat = "<b > Value :</b> {point.y:.0f} ")) %>%
      hc_chart(zoomType = "xy") %>% 
      hc_add_theme(hc_theme_flatdark())
  })
  

  

  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Enseble selection                                                       ####
  
  output$ensemble_params <- renderUI({
    if(length(input$Algorithm) >= 2){
     tagList(selectInput(inputId = 'ensemble_algorithms',
                  label = 'Pick Algorithms to ensemble',
                  choices = input$Algorithm,
                  multiple = T),
             checkboxInput('optim','Optimize ensemble wights'))
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
    if(length(input$ensemble_algorithms) >= 2){
      input$ensemble_algorithms
    } else {
      NULL
    }
  })
  
  observeEvent(c(input$i_file,
                 input$variable,
                 input$observations,
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
        if(!is.null(reactiveVariables$TotalSeries)){
          total_obs <- length(reactiveVariables$TotalSeries)
          obs_to_hold_out <- input$holdout
          reactiveVariables$Series_to_Fit <- head(reactiveVariables$TotalSeries, total_obs - obs_to_hold_out)
          reactiveVariables$Series_to_Evaluate <- tail(reactiveVariables$TotalSeries, obs_to_hold_out)
          
        }
        message('Evaluating')
        forecasts <- list()
        tryCatch({
          ### . . . . . . . . .. #< c53d563091a835d35f8171cf29dd65b3 ># . . . . . . . . ..
          ### DRIFT                                                                   ####
          
          if("DRIFT" %in% input$Algorithm) {
            message('Evaluating DRIFT')
            forecasts$DRIFT <- list()
            lambda <- if(input$drift_lambda == 'NULL') NULL else 'auto'
            forecastFit <- rwf(reactiveVariables$Series_to_Fit,
                               h=length(reactiveVariables$Series_to_Evaluate), 
                               drift=input$drift,
                               lambda = lambda)
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts$DRIFT[['Fit']] <- forecastFit$model
            forecasts$DRIFT[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
            forecasts$DRIFT[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$DRIFT[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$DRIFT[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
          }
          
          ### . . . . . . . . .. #< b1639019df2cce3e8a36505079eed67a ># . . . . . . . . ..
          ### NAIVE                                                                   ####
          
          if("NAIVE" %in% input$Algorithm) {
            message('Evaluating NAIVE')
            forecasts$NAIVE <- list()
            lambda <- if(input$naive_lambda == 'NULL') NULL else 'auto'
            forecastFit <- snaive(reactiveVariables$Series_to_Fit, 
                                  h = length(reactiveVariables$Series_to_Evaluate),
                                  lambda = input$lambda)
            
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts$NAIVE[['Fit']] <- forecastFit$model
            forecasts$NAIVE[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
            forecasts$NAIVE[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$NAIVE[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            forecasts$NAIVE[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
          }
          
          ### . . . . . . . . .. #< b3a6167b3ff6b20c0cbaae7bdce45775 ># . . . . . . . . ..
          ### ARIMA                                                                   ####
          
          if("ARIMA" %in% input$Algorithm) {
            message('Evaluating ARIMA')
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
                                  stationary = input$stationary,
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
              forecastFit <- forecast::forecast(fit, length(reactiveVariables$Series_to_Evaluate))
              forecastFit_dt <- as.data.table(forecastFit)
              forecasts$ARIMA[['Fit']] <- fit
              forecasts$ARIMA[['Forecast']] <- forecastFit_dt[, `Point Forecast`]
              forecasts$ARIMA[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$ARIMA[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
              forecasts$ARIMA[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecastFit_dt[, `Point Forecast`])
            }
          }
          
          ### . . . . . . . . .. #< ee13e415784cd35d52f0e3b05f4bb074 ># . . . . . . . . ..
          ### ETS                                                                     ####
          
          if("ETS" %in% input$Algorithm){
            message('Evaluating ETS')
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
          }
          
          ### . . . . . . . . .. #< 5105bd2a0698cfb3f50d3e304810eb7a ># . . . . . . . . ..
          ### TBATS                                                                   ####
          
          if("TBATS" %in% input$Algorithm){
            message('Evaluating TBATS')
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
          }
          
          ### . . . . . . . . .. #< 094ca0b0d463fea81283d604e5c9f5bf ># . . . . . . . . ..
          ### PROPHET                                                                 ####
          
          if("PROPHET" %in% input$Algorithm){
            message('Evaluating PROPHET')
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
          }
          

          ### . . . . . . . . .. #< 8025fc324c9392644d0bfd5c01722bdc ># . . . . . . . . ..
          ### THETA                                                                   ####
          
          if("THETA" %in% input$Algorithm) {
            message('Evaluating THETA')
            forecasts$THETA <- list()
            forecastFit <- forecast.THETA(y = reactiveVariables$Series_to_Fit,
                                          h = length(reactiveVariables$Series_to_Evaluate) + 1,
                                          opt.method = input$opt.method,
                                          s = input$s,
                                          model = input$theta_model)
            
            forecasting_mean <- head(forecastFit$mean,length(reactiveVariables$Series_to_Evaluate))
            forecasts$THETA[['Fit']] <- forecastFit
            forecasts$THETA[['Forecast']] <-  forecasting_mean
            forecasts$THETA[['MAE']]  <- mae(reactiveVariables$Series_to_Evaluate, forecasting_mean)
            forecasts$THETA[['RMSE']] <- rmse(reactiveVariables$Series_to_Evaluate, forecasting_mean)
            forecasts$THETA[['MAPE']] <- mape(reactiveVariables$Series_to_Evaluate, forecasting_mean)
          }
          
          ### . . . . . . . . .. #< 0f7d6d2743442bb708d684621e3247f7 ># . . . . . . . . ..
          ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
          ### NNETAR                                                                  ####
          
          if("NNETAR" %in% input$Algorithm){
            message('Evaluating NNETAR')
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
              forecasts$NNETAR[['Fit']] <- fit
              forecasts$NNETAR[['Forecast']] <- as.vector(forecastFit_dt[,`Point Forecast`])
              forecasts$NNETAR[['MAE']]  <- mae(as.vector(reactiveVariables$Series_to_Evaluate), as.vector(forecastFit_dt[, `Point Forecast`]))
              forecasts$NNETAR[['RMSE']] <- rmse(as.vector(reactiveVariables$Series_to_Evaluate), as.vector(forecastFit_dt[, `Point Forecast`]))
              forecasts$NNETAR[['MAPE']] <- mape(as.vector(reactiveVariables$Series_to_Evaluate), as.vector(forecastFit_dt[, `Point Forecast`]))
            }
          }
          
          ### . . . . . . . . .. #< 89a38793238aece377f57d14b2ae64d3 ># . . . . . . . . ..
          ### CES                                                                     ####
          if("CES" %in% input$Algorithm) {
            message('Evaluating CES')
            forecasts$CES <- list()
            forecastFit <- auto.ces(reactiveVariables$Series_to_Fit,
                                    models = input$CES_seasonality,
                                    ic=input$ces_ic,
                                    initial = input$initial_state,
                                    loss = input$loss,
                                    interval = input$interval,
                                    h = length(reactiveVariables$Series_to_Evaluate))
            forecasts$CES[['Fit']] <- forecastFit
            forecasts$CES[['Forecast']] <- forecastFit$forecast
            forecasts$CES[['MAE']]  <- mae(as.vector(reactiveVariables$Series_to_Evaluate), as.vector(forecastFit$forecast))
            forecasts$CES[['RMSE']] <- rmse(as.vector(reactiveVariables$Series_to_Evaluate), as.vector(forecastFit$forecast))
            forecasts$CES[['MAPE']] <- mape(as.vector(reactiveVariables$Series_to_Evaluate), as.vector(forecastFit$forecast))
          }
          
          
    
          ### ENSEMPLE                                                                ####
          if( !is.null(to_ensemble())){
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
            ensemble_dt <- Reduce(f = cbind,ensemble_list)
            if(input$optim){
              product <- try({
              X <- as.matrix(ensemble_dt)
              y <- as.matrix(as.vector(reactiveVariables$Series_to_Evaluate))
              w <- rep(1/ncol(X),ncol(X))
              ##################################################
              # From solve.QP description
              # solving quadratic programming problems of the form :
              # min(-d^T b + 1/2 b^T D b) 
              # with the constraints:
              # A^T b >= b_0.
              
              # Your problem
              # Minimize       || Xw - y ||^2     => Minimize 1/2 w'X'Xw - (y'X)w  => D=X'X , d= X'y
              # Constraint w>0,w<1, sum(w)=1      => A'w >= b0
              ##################################################
              d <- t(X) %*% y
              D <- t(X) %*% X
              A <- cbind(rep(1,ncol(X)),diag(ncol(X))) #constraint LHS
              b0 <- c(1,numeric(ncol(X))) # constraint RHS
             
              pd_D_mat <- nearPD(D)
              soln <- solve.QP(as.matrix(pd_D_mat$mat),d,A,b0,meq = 1)
              w1 <- soln$solution  # Your model wieghts
              w1[w1<0] <- 0
              res <- X %*% w1
              res[,1]
              })
              if (!is(product, 'try-error')) {
                names(w1) <- colnames(ensemble_dt)
                forecasts$ENSEMBLE[['Weights']] <- w1
                ensemble_dt[, `Point Forecast` := product]
              } else {
                showToast(
                  "error", 
                  "Unable to optimize Ensemble, calculating simple average", 
                  .options = myToastOptions
                )
                w <-  rep(1/ncol(ensemble_dt),ncol(ensemble_dt))
                names(w) <- colnames(ensemble_dt)
                forecasts$ENSEMBLE[['Weights']] <- w
                ensemble_dt[, `Point Forecast` := rowMeans(.SD)]
              }
            } else {
              w <-  rep(1/ncol(ensemble_dt),ncol(ensemble_dt))
              names(w) <- colnames(ensemble_dt)
              forecasts$ENSEMBLE[['Weights']] <- w
              ensemble_dt[, `Point Forecast` := rowMeans(.SD)]
            }
            
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
          message(toString(e))
          showToast(type = "error",message = paste0(toString(e)), .options = myToastOptions)
        }
        # ,
        # warning = function(w){removeModal()
        #   message(toString(w))
        #   showToast(type = "warning",message = paste0(toString(w)), .options = myToastOptions)
        # }
        )
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
    datatable(result_dt, caption = HTML("<h3 style='color:white'>Evalution results in Hold-Out observations</h3>"),
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
                tags$h4(names(chart_lines[x]))
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
  })
  
  output$forecast_widget_container <- renderUI({
    req(reactiveVariables$timeSeries_submitted == T)
    req(reactiveVariables$evaluation_done)
    final_results <- reactiveVariables$Forecasts
    req(length(final_results) >= 1)
    tagList(
      h2('Forecast'),
      actionButton(inputId = 'forecast_ahead',
                   label = "Forecast ahead with a selected fitted from the evaluation's results table"),
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
        models_to_forecast <- reactiveVariables$Results[input$results_rows_selected,Algorithm]
        if("ENSEMBLE" %in% models_to_forecast){
          ensemlbed_algorithms <- to_ensemble()
          Algorithms <- unique(c(ensemlbed_algorithms,
                               reactiveVariables$Results[input$results_rows_selected,Algorithm]))
        }else{
          Algorithms <- reactiveVariables$Results[input$results_rows_selected,Algorithm]
        }
        #### . . . . . . . . .. #< 2f343b2d516182ef8966454ae20d2a53 ># . . . . . . . . ..
        #### DRIFT                                                                  ####
        
        if("DRIFT" %in% Algorithms) {
          message('Forecasting DRIFT')
          forecasts_ahead$DRIFT <- list()
          forecastFit <- rwf(reactiveVariables$TotalSeries,
                             h=input$forecast_horizon, 
                             model= reactiveVariables$Forecasts[['DRIFT']][['Fit']])
          forecastFit_dt <- as.data.table(forecastFit)
          forecasts_ahead$DRIFT[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
        }
        
        ### . . . . . . . . .. #< a0921dd4a3b7bc310493699c734bab0a ># . . . . . . . . ..
        ### NAIVE                                                                   ####
        
        if ("NAIVE" %in% Algorithms) {
          message('Forecasting NAIVE')
          forecasts_ahead$NAIVE <- list()
            forecastFit <- snaive(reactiveVariables$TotalSeries,
                                  h = input$forecast_horizon, 
                                  model= reactiveVariables$Forecasts[['NAIVE']][['Fit']])
          forecastFit_dt <- as.data.table(forecastFit)
          forecasts_ahead$NAIVE[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
        }
        
        ### . . . . . . . . .. #< d2c567bb885ae625779fb22fdb87a87b ># . . . . . . . . ..
        ### ARIMA                                                                   ####
        
        if("ARIMA" %in% Algorithms) {
          message('Forecasting ARIMA')
          forecasts_ahead$ARIMA <- list()
          lambda <- if(input$arima_lambda == 'NULL') NULL else 'auto'
          fit <- try(Arima(y = reactiveVariables$TotalSeries,
                           model = reactiveVariables$Forecasts[['ARIMA']][['Fit']]))
          if (is(fit, 'try-error')) {
            forecasts_ahead$ARIMA <- NULL
          } else {
            forecastFit <- forecast::forecast(fit, input$forecast_horizon)
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts_ahead$ARIMA[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
          }
        }
        
        ### . . . . . . . . .. #< 044c8add4ad5eb01d51d2e75078f2d8d ># . . . . . . . . ..
        ### ETS                                                                     ####
        
        if("ETS" %in% Algorithms){
          message('Forecasting ETS')
          forecasts_ahead$ETS <- list()
          fit <- try(ets(y = reactiveVariables$TotalSeries,
                         use.initial.values=TRUE,
                         model= reactiveVariables$Forecasts[['ETS']][['Fit']]))
          if (is(fit, 'try-error')) {
            forecasts_ahead$ETS <- NULL
          } else {
            forecastFit <- forecast::forecast(fit, input$forecast_horizon)
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts_ahead$ETS[['Forecast']] <- forecastFit_dt[, .(`Point Forecast`, `Lo 95` , `Hi 95`)]
          }
        } 
        
        ### . . . . . . . . .. #< 76a4e2451752cd2f1b608013513b382d ># . . . . . . . . ..
        ### TBATS                                                                   ####
        
        if("TBATS" %in% Algorithms){
          message('Forecasting TBATS')
          forecasts_ahead$TBATS <- list()
          fit <- try(tbats(reactiveVariables$TotalSeries,
                           model= reactiveVariables$Forecasts[['TBATS']][['Fit']]))
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
        
        ### . . . . . . . . .. #< b5b979f6387908b18e37ac78146a6a37 ># . . . . . . . . ..
        ### THETA                                                                   ####
        
        if("THETA" %in% Algorithms) {
          message('Forecasting THETA')
          forecasts_ahead$THETA <- list()
          previousfit <- reactiveVariables$Forecasts[['THETA']][['Fit']]
          model <- previousfit$method
          opt.method <- previousfit$opt.method 
          s <- previousfit$s 
          par_ini <- previousfit$par
          forecastFit <- forecast.THETA(y = reactiveVariables$TotalSeries,
                                        h = input$forecast_horizon + 1,
                                        estimation = F,
                                        par_ini = par_ini,
                                        opt.method = opt.method,
                                        s = s,
                                        model = model)
          forecasting_mean <- head(forecastFit$mean,input$forecast_horizon)
          forecasting_Lo <- head(as.data.table(forecastFit$lower)$`Lo 95`,input$forecast_horizon)
          forecasting_Hi <- head(as.data.table(forecastFit$upper)$`Hi 95`,input$forecast_horizon)
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
          fit <- try(nnetar(y = reactiveVariables$TotalSeries,
                            model =  reactiveVariables$Forecasts[['NNETAR']][['Fit']]))
          if (is(fit, 'try-error')) {
            forecasts_ahead$NNETAR <- NULL
          } else {
            forecastFit <- forecast(fit, h = input$forecast_horizon, level = 95, PI = T)
            forecastFit_dt <- as.data.table(forecastFit)
            forecasts_ahead$NNETAR[['Forecast']] <- forecastFit_dt
          }
          
        }

        ### . . . . . . . . .. #< 3de4c0e2af3c3c1fe43a27be80c5154f ># . . . . . . . . ..
        
        ### CES                                                                     ####
        if("CES" %in% Algorithms) {
          message('Evaluating CES')
          forecasts_ahead$CES <- list()
          model <- reactiveVariables$Forecasts[["CES"]][['Fit']]
          forecastFit <- ces(reactiveVariables$TotalSeries,
                             h = input$forecast_horizon, model = model,
                             level = 0.95)
          
          forecastFit_dt <- data.table('Point Forecast' = forecastFit$forecast,
                                       'Lo 95' = forecastFit$lower,
                                       'Hi 95' = forecastFit$upper)
          forecasts_ahead$CES[['Forecast']] <- forecastFit_dt
          
        }
        
        
        ### ENSEMBLE                                                                ####
        
        if("ENSEMBLE" %in% Algorithms){
          forecasts_ahead$ENSEMBLE <- list()
          means_list <- list()
          upper_list <- list()
          lower_list <- list()
        
          to_addup_algorithms <- setdiff(names(forecasts_ahead),"ENSEMBLE")
          for(alg in  to_addup_algorithms){
            dt <- data.table(forecasts_ahead[[alg]][['Forecast']])
            dt <- dt[,lapply(.SD, as.numeric)]
            dt[is.na(`Hi 95`), 'Hi 95' :=  `Point Forecast`]
            dt[is.na(`Lo 95`), 'Lo 95' :=  `Point Forecast`]
            means_list[[alg]] <- dt[,.(`Point Forecast`)]
            upper_list[[alg]] <- dt[,.(`Hi 95`)]
            lower_list[[alg]] <- dt[,.(`Lo 95`)]
          }

          means_dt <- Reduce(cbind,means_list)
          upper_dt <- Reduce(cbind,upper_list)
          lower_dt <- Reduce(cbind,lower_list)

          w <-  reactiveVariables$Forecasts$ENSEMBLE[['Weights']] 
          pf <- as.matrix(replace(means_dt, is.na(means_dt), 1)) %*% w
          lb <- as.matrix(replace(lower_dt, is.na(lower_dt), means_dt)) %*% w
          ub <- as.matrix(replace(upper_dt, is.na(upper_dt), means_dt)) %*% w
          ensemble_dt <- data.table('Point Forecast' = pf[,1],
                                    'Lo 95' = lb[,1],
                                    'Hi 95' = ub[,1])
          forecasts_ahead$ENSEMBLE[['Forecast']] <- ensemble_dt
        }
        
        ### CLOSING                                                                 ####
        reactiveVariables$Forecasts_ahead <- forecasts_ahead[models_to_forecast]
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
            tags$h4(names(chart_lines[x]))
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
              hc_add_series(chart_lines[[x]], type = "arearange",
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
    do.call(what = shiny::tabsetPanel, 
            args = gap %>% append(list(type = "tabs", id   = "forecasting_charts")))
  })
}
  