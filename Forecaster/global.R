

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
library(foreach)
library(doParallel)
library(smooth)

Modal <- function(text){
  tags$div(id='modal1',
           modalDialog(
             size="l",
             easyClose = FALSE,
             footer = NULL,
             fade=F,
             HTML("<div class='lds-facebook'><div></div><div></div><div></div></div>"),
             tags$div(h4(strong(paste0(text))))))
}#modal


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





