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

crossValidationProphet <- function(dt, horizon, window, initial){
  freq_int <- findfrequency(dt)
  valid_frequency <-  c( 1, 7, 12, 4, 365)
  names(valid_frequency) <-  c('day', 'week', 'month', 'quarter', 'year')
  check <- data.table(valid_frequency = valid_frequency, freq_int = freq_int, name = names(valid_frequency))
  check[,diff := abs(valid_frequency - freq_int)]
  freq <- check[which.min(diff),name]
  
  
  initial <- head(dt,initial)
  rest <- dt[(floor(nrow(dt)/2)+1):nrow(dt)]
  buckets <- ceiling((nrow(rest) - horizon)/window)+1
  buckets
  window <- 5
  trainlists <- list()
  evaluationlists <- list()
  restlist <- list()
  trainlists[[1]] <- initial
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