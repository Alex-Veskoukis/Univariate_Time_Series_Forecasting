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
             # tags$div(class="loader") ,
             HTML("<div class='lds-facebook'><div></div><div></div><div></div></div>"),
             tags$div(h4(strong(paste0(text)))))
           ,
           tags$head(tags$style(type="text/css", 
                                "
.lds-facebook {
  display: inline-block;
  position: relative;
  width: 80px;
  height: 80px;
}
.lds-facebook div {
  display: inline-block;
  position: absolute;
  left: 8px;
  width: 16px;
  background: #fff;
  animation: lds-facebook 1.2s cubic-bezier(0, 0.5, 0.5, 1) infinite;
}
.lds-facebook div:nth-child(1) {
  left: 8px;
  animation-delay: -0.24s;
}
.lds-facebook div:nth-child(2) {
  left: 32px;
  animation-delay: -0.12s;
}
.lds-facebook div:nth-child(3) {
  left: 56px;
  animation-delay: 0;
}
@keyframes lds-facebook {
  0% {
    top: 8px;
    height: 64px;
  }
  50%, 100% {
    top: 24px;
    height: 32px;
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
crossValidationProphetExcludingInitial <- function(dt, horizon, window, initial){
  freq_int <- findfrequency(dt)
  valid_frequency <-  c( 1, 7, 12, 4, 365)
  names(valid_frequency) <-  c('day', 'week', 'month', 'quarter', 'year')
  check <- data.table(valid_frequency = valid_frequency, freq_int = freq_int, name = names(valid_frequency))
  check[,diff := abs(valid_frequency - freq_int)]
  freq <- check[which.min(diff),name]
  initial <- head(dt,initial)
  rest <- dt[(initial+1):nrow(dt)]
  buckets <- ceiling((nrow(rest) - horizon)/window)+1
  buckets
  trainlists <- list()
  evaluationlists <- list()
  restlist <- list()
  trainlists[[1]] <- rest[1:window]
  restlist[[1]] <- rest[(window+1):nrow(rest)]
  evaluationlists[[1]] <- rest[(window + 1):(window + horizon)]
  
  i <- 2
  while(T){
    if(nrow(restlist[[i-1]]) >= window + horizon){
      dt1 <- rbind(trainlists[[i-1]], restlist[[i-1]][1:window,])
      dt2 <- restlist[[i-1]][-(1:window)]
      dt3 <- dt2[1:horizon]
      trainlists[[i]] <- dt1
      restlist[[i]] <- dt2
      evaluationlists[[i]] <- dt3
    }else{
      break;
    } 
    i <- i + 1
  }
  
  results <- expand.grid(Trainlist = c(1:length(trainlists)), Metric = c('MAE','RMSE','MAPE'), horizon = 1:horizon)
  setDT(results)
  
  for(j in  seq_along(trainlists)){
    message('Evaluating trainging list ', j)
    fit <- try(prophet(trainlists[[j]]))
    future <- make_future_dataframe(fit, periods = horizon,freq = freq)
    forecast <- predict(fit, future)
    forecastFit_dt <- as.data.table(forecast)
    forecastFit_dt <- tail(forecastFit_dt[,.(`Point Forecast` = yhat)], horizon)
    for(i in 1:horizon){
      results[Trainlist == j & horizon == i & Metric == 'MAE', 
              Value := mae(as.vector(evaluationlists[[j]]$y)[i], forecastFit_dt[, `Point Forecast`][i])]
      
      results[Trainlist == j & horizon == i & Metric == 'RMSE', 
              Value := rmse(as.vector(evaluationlists[[j]]$y)[i], forecastFit_dt[, `Point Forecast`][i])]
      
      results[Trainlist == j & horizon == i & Metric == 'MAPE', 
              Value := mape(as.vector(evaluationlists[[j]]$y)[i], forecastFit_dt[, `Point Forecast`][i])]
    }
  }
  final_results <- results[, mean(Value), by = .(Metric, horizon)]
  final_results
  final_results <- dcast(final_results, Metric ~ paste0('h=', horizon), value.var = 'V1')
  return(final_results)
}
crossValidationProphet <- function(dt, horizon, window, initial){
  freq_int <- findfrequency(dt)
  valid_frequency <-  c( 1, 7, 12, 4, 365)
  names(valid_frequency) <-  c('day', 'week', 'month', 'quarter', 'year')
  check <- data.table(valid_frequency = valid_frequency, freq_int = freq_int, name = names(valid_frequency))
  check[,diff := abs(valid_frequency - freq_int)]
  freq <- check[which.min(diff),name]
  initial_dt <- head(dt,initial)
  rest <- dt[(initial+1):nrow(dt)]
  buckets <- ceiling((nrow(rest) - horizon)/window)+1
  trainlists <- list()
  evaluationlists <- list()
  restlist <- list()
  trainlists[[1]] <- initial_dt
  restlist[[1]] <- rest
  evaluationlists[[1]] <- rest[1:horizon]
  i <- 2
  while(T){
    if(nrow(restlist[[i-1]]) >= window + horizon){
      dt1 <- rbind(trainlists[[i-1]], restlist[[i-1]][1:window,])
      dt2 <- restlist[[i-1]][-(1:window)]
      dt3 <- dt2[1:horizon]
      trainlists[[i]] <- dt1
      restlist[[i]] <- dt2
      evaluationlists[[i]] <- dt3
    }else{
      break;
    } 
    i <- i + 1
  }
  results <- expand.grid(Trainlist = c(1:length(trainlists)), Metric = c('MAE','RMSE','MAPE'), horizon = 1:horizon)
  setDT(results)
  for(j in  seq_along(trainlists)){
    message('Evaluating trainging list ', j)
    fit <- try(prophet(trainlists[[j]]))
    future <- make_future_dataframe(fit, periods = horizon,freq = freq)
    forecast <- predict(fit, future)
    forecastFit_dt <- as.data.table(forecast)
    forecastFit_dt <- tail(forecastFit_dt[,.(`Point Forecast` = yhat)], horizon)
    for(i in 1:horizon){
      results[Trainlist == j & horizon == i & Metric == 'MAE', 
              Value := mae(as.vector(evaluationlists[[j]]$y)[i], forecastFit_dt[, `Point Forecast`][i])]
      
      results[Trainlist == j & horizon == i & Metric == 'RMSE', 
              Value := rmse(as.vector(evaluationlists[[j]]$y)[i], forecastFit_dt[, `Point Forecast`][i])]
      
      results[Trainlist == j & horizon == i & Metric == 'MAPE', 
              Value := mape(as.vector(evaluationlists[[j]]$y)[i], forecastFit_dt[, `Point Forecast`][i])]
    }
  }
  final_results <- results[, mean(Value), by = .(Metric, horizon)]
  final_results
  final_results <- dcast(final_results, Metric ~ paste0('h=', horizon), value.var = 'V1')
  return(final_results)
}
time_series_cv <- function (y, forecastfunction, h = 1, window = NULL, xreg = NULL, initial = 0, ...){
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  if (initial >= n) 
    stop("initial period too long")
  tsp(e) <- tsp(y)
  if (!is.null(xreg)) {
    xreg <- ts(as.matrix(xreg))
    if (NROW(xreg) != length(y)){
      stop("xreg must be of the same size as y")
    }
    tsp(xreg) <- tsp(y)
  }
  if (is.null(window)){ 
    indx <- seq(1 + initial, n - 1L)
  }else{
    indx <- seq(window + initial, n - 1L, by = 1L)
  }
  for (i in indx) {
    y_subset <- subset(y, start = 1, end = i)
    if (is.null(xreg)) {
      fc <- try(suppressWarnings(forecastfunction(y_subset, h = h, ...)), silent = TRUE)
    } else {
      xreg_subset <- as.matrix(subset(xreg, start = ifelse(is.null(window), 
                                                           1L, ifelse(i - window >= 0L, i - window + 1L, 
                                                                      stop("small window")))))
      fc <- try(suppressWarnings(forecastfunction(y_subset, 
                                                  h = h, xreg = xreg_subset, ...)), silent = TRUE)
    }
    if (!is.element("try-error", class(fc))) {
      e[i, ] <- y[i + (1:h)] - fc$mean
    }
  }
  if (h == 1) {
    return(e[, 1L])
  }
  else {
    colnames(e) <- paste("h=", 1:h, sep = "")
    return(e)
  }
}

style <- "
.highcharts-plot-band-label{ color:#ffffff;}
.panel {background-color:#34495e !important;}
body {background-color:#34495e}
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
}"

myToastOptions <- list(
  positionClass = "toast-top-right",
  progressBar = FALSE,
  timeOut = 3000,
  closeButton = TRUE,
  
  # same as defaults
  newestOnTop = TRUE,
  preventDuplicates = FALSE,
  showDuration = 300,
  hideDuration = 1000,
  extendedTimeOut = 1000,
  showEasing = "linear",
  hideEasing = "linear",
  showMethod = "fadeIn",
  hideMethod = "fadeOut"
)


forecast.ARIMA <-function(x,h,...){ 
  fit = auto.arima(x,...)
  forecast(fit,h)
}
forecast.THETA <- function(y, model, opt.method, s, h, ...){
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
         'Dynamic Optimised Theta Model' = dotm(y=y, opt.method=opt.method, s=ss, h=h,...), 
         'Dynamic Standard Theta Model' = dstm(y=y, opt.method=opt.method, s=ss, h=h, ...), 
         'Optimised Theta Model' = otm(y=y, opt.method=opt.method, s=ss, h=h, ...),
         'Standard Theta Model' = stm(y=y, opt.method=opt.method, s=ss, h=h, ...),
         "Standard Theta Method (STheta)" = stheta(y=y,  s=ss, h=h, ...))
}
forecast.NNETAR <-function(x,h, ...){ 
  fit = nnetar(y = x, ...)
  forecast(fit,h)
}
forecast.TBATS <-function(x,h, ...){ 
  fit = tbats(x, ...)
  forecast(fit,h)
}
forecast.ETS <-function(x,h, ... ){ 
  fit = ets(x, ...)
  forecast(fit,h)
}
forcast.prophet <- function(y, h, freq,...){
  dt <- data.table(ds=seq.Date(from = Sys.Date(),
                               length.out = length(y),
                               by = freq),
                   y = y)
  fit <- try(prophet(df = dt, ...))
  future <- make_future_dataframe(fit, 
                                  periods = h,
                                  freq = freq,
                                  include_history = F)
  forecast <- predict(fit, future)
  forecast
}

time_series_cv2 <- function(y, forecastfunction, h = 1, window = NULL, initial = 0, ...){
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  if (initial >= n) 
    stop("initial period too long")
  if (!is.null(window) && window >= n-1 - initial) 
    stop("window period too long")
  if(h >= n-2 - initial)
    stop("horizon too long")
  tsp(e) <- tsp(y)
  if (is.null(window)){
    indx <- seq(1 + initial, n - 1L) 
  } else {
    indx <- seq(from = window + initial, to = n - 1L, by = 1L)
  }
  for (i in indx) {
    if(is.null(window)){
      proper_start <- 1 
    } else if (i - window >= 0L) {
      proper_start <- i - window + 1L 
    } else {
      stop("small window")
    }
    y_subset <- subset(y, start = proper_start, end = i)
    fc <- try(suppressWarnings(forecastfunction(y_subset,h = h,...)), silent = TRUE)
    if (!is.element("try-error", class(fc))) {
      e[i, ] <- y[i + (1:h)] - fc$mean
    }
  }
  if (h == 1) {
    return(e[, 1L])
  } else {
    colnames(e) <- paste("h=", 1:h, sep = "")
    return(e)
  }
}
time_series_cv2_par <- function(y, forecastfunction, h = 1, window = NULL, initial = 0, ...){
  # max(1,detectCores()-1)
  cl <- makeCluster(1)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  if (initial >= n) 
    stop("initial period too long")
  if (!is.null(window) && window >= n-1 - initial) 
    stop("window period too long")
  if(h >= n-2 - initial)
    stop("horizon too long")
  
  if (is.null(window)){
    indx <- seq(1 + initial, n - 1L) 
  } else {
    indx <- seq(from = window + initial, to = n - 1L, by = 1L)
  }
  
  error_fun <- function(i, y = y, window = window, forecastfunction = forecastfunction, h = h, ...){
    if(is.null(window)){
      proper_start <- 1 
    } else if (i - window >= 0L) {
      proper_start <- i - window + 1L 
    } else {
      stop("small window")
    }
    y_subset <- forecast:::subset.ts(y, start = proper_start, end = i)
    fc <- try(suppressWarnings(forecastfunction(y_subset,h = h, ...)), silent = TRUE)
    if (!is.element("try-error", class(fc))) {
      y[i + (1:h)] - fc$mean
    }
  }
  e <- foreach(i = indx, .combine = 'c', .packages = c('forecast','forecTheta'))%dopar% {
    error_fun(i=i, y = y, window = NULL, forecastfunction = forecastfunction, h=h, ...)
  }
  
  length(e) <- suppressWarnings(prod(dim(matrix(e ,nrow = n, ncol=h, byrow = T))))
  final_result <- matrix(data = e, nrow = n, ncol=h, byrow = T)
  if (h == 1) {
    return(final_result[, 1L])
  } else {
    colnames(final_result) <- paste("h=", 1:h, sep = "")
    return(final_result)
  }
}


time_series_cv_prophet_par <- function(y, forecastfunction, h = 1, window = NULL, initial = 0, freq = freq, ...){
  # max(1,detectCores()-1)
  cl <- makeCluster(1)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  y <- as.ts(y)
  n <- length(y)
  if (initial >= n) 
    stop("initial period too long")
  if (!is.null(window) && window >= n-1 - initial) 
    stop("window period too long")
  if(h >=    n-2 - initial)
    stop("horizon too long")
  error_fun_prophet <- function(i, y = y, window = window, forecastfunction = forecastfunction, h = h, freq = freq, ...){
    if(is.null(window)){
      proper_start <- 1 
    } else if (i - window >= 0L) {
      proper_start <- i - window + 1L 
    } else {
      stop("small window")
    }
    y_subset <- forecast:::subset.ts(y, start = proper_start, end = i)
    fc <- try(suppressWarnings(forecastfunction(y = y_subset,h = h,freq = freq,...)), silent = TRUE)
    if (!is.element("try-error", class(fc))) {
      y[i + (1:h)] - fc$yhat
    }
  }
  e <- foreach(i = indx, .combine = 'c', .packages = c('forecast','prophet','data.table'),.verbose = T)%dopar% {
    error_fun_prophet(i=i, y = y, window = NULL, forecastfunction = forecastfunction, h=h, freq = freq,...)
  }
  
  length(e) <- suppressWarnings(prod(dim(matrix(e ,nrow = n, ncol=h, byrow = T))))
  final_result <- matrix(data = e, nrow = n, ncol=h, byrow = T)
  if (h == 1) {
    return(final_result[, 1L])
  } else {
    colnames(final_result) <- paste("h=", 1:h, sep = "")
    return(final_result)
  }
}



