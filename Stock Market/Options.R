#!/usr/bin/Rscript

library(quantmod)
library(tidyverse)

symbols <- stockSymbols()
symbols <- symbols$Symbol

finalData <- data.frame()
stockChanges <- data.frame()

for(i in 1:length(symbols)) {
  # PULL HISTORICAL DATA
  histData <- tryCatch({getSymbols(symbols[i],
                                   auto.assign = FALSE,
                                   from = Sys.Date()-35)}, 
                       error=function(e){})
  
  if(anyNA(histData)){
    histData <- data.frame()
  }
  
  #print(paste(symbols[i], nrow(histData)))
  if(!is.null(histData)){
    if(nrow(histData) > 20) {
      
      # FIND % CHANGE, MAX AND MIN FOR EACH TIME RANGE ############################################
      histData <- as.data.frame(histData)
      dStart <- nrow(histData) - 1
      wStart <- nrow(histData) - 5
      mStart <- nrow(histData) - 20
      end <- nrow(histData)
      
      # Current Price
      currPrice <- round(histData[end, 4],2) 
      
      # Max and Min Prices
      dMax <- round(max(histData[dStart:end,2]),2)
      dMin <- round(min(histData[dStart:end,2]),2)
      wMax <- round(max(histData[wStart:end,2]),2)
      wMin <- round(min(histData[wStart:end,2]),2)
      mMax <- round(max(histData[mStart:end,2]),2)
      mMin <- round(min(histData[mStart:end,2]),2)
      
      # Bool - If price spike/drop greater than/less than targets, mark as T/F
      dCall <- ifelse(((histData[dStart,4] - histData[end,4]) / histData[dStart,4]) >= 0.1, TRUE, FALSE)
      dPut <- ifelse(((histData[dStart,4] - histData[end,4]) / histData[dStart,4]) <= -0.1, TRUE, FALSE)
      wCall <- ifelse(((histData[wStart,4] - histData[end,4]) / histData[wStart,4]) >= 0.15, TRUE, FALSE)
      wPut <- ifelse(((histData[wStart,4] - histData[end,4]) / histData[wStart,4]) <= -0.15, TRUE, FALSE)
      mCall <- ifelse(((histData[mStart,4] - histData[end,4]) / histData[mStart,4]) >= 0.25, TRUE, FALSE)
      mPut <- ifelse(((histData[mStart,4] - histData[end,4]) / histData[mStart,4]) <= -0.25, TRUE, FALSE)
      
      print(paste(nrow(finalData), symbols[i], dCall, dPut, wCall, wPut, mCall, mPut))
      
      # Check to see if any come out TRUE
      if(dCall | dPut | wCall | wPut | mCall | mPut) {
        
        # Pull All Options
        stockOptions <- tryCatch({getOptionChain(symbols[i], NULL)}, 
                                 error=function(e){})
        
        if(!is.null(stockOptions)) {
          if(length(stockOptions) > 0) {
            
            # CHECK FOR CALLS AND PUT ALL CALLS IN DF
            if(!is.null(stockOptions[[1]]$calls)){
              
              calls <- data.frame()
              for(j in 1:length(stockOptions)) {
                stockOptions2 <- stockOptions[[j]]$calls
                
                if(!is.null(stockOptions2)){
                  calls <- bind_rows(calls, stockOptions2)
                }
              }
              
              # Add Metadata
              if(!is.null(calls) & nrow(calls)>0) {
                calls$Ticker <- symbols[i]
                date <- gsub("[^0-9.-]", "", rownames(calls))
                calls$date <- as.Date(substr(date, 1, 6), format = "%y%m%d")
                calls <- calls %>%
                  filter(Ask < 0.51 & Ask > 0.001) %>%
                  filter(Last < 0.51 & Last > 0.001)
                
                # filter data and add day/week/month data
                if(dCall & nrow(calls)>0){
                  dCallOptions <- calls %>%
                    filter(date > Sys.Date() + 30) %>%
                    filter(Strike < dMax)
                  
                  if(nrow(dCallOptions) > 0){
                    dCallOptions <- dCallOptions %>%
                      mutate(Range = "day", RangeMax = dMax, RangeMin = dMin, Type = "Call") %>%
                      select(Ticker, date, Strike, Last, Bid, Ask, Range, RangeMax, RangeMin, Type)
                    finalData <- bind_rows(finalData, dCallOptions)
                  }
                }
                if(wCall & nrow(calls)>0) {
                  wCallOptions <- calls %>%
                    filter(date > Sys.Date() + 60) %>%
                    filter(Strike < wMax)
                  
                  if(nrow(wCallOptions) > 0){
                    wCallOptions <- wCallOptions %>%
                      mutate(Range = "week", RangeMax = wMax, RangeMin = wMin, Type = "Call") %>%
                      select(Ticker, date, Strike, Last, Bid, Ask, Range, RangeMax, RangeMin, Type)
                    finalData <- bind_rows(finalData, wCallOptions)
                  }
                }
                if(mCall & nrow(calls)>0) {
                  mCallOptions <- calls %>%
                    filter(date > Sys.Date() + 90) %>%
                    filter(Strike < mMax)
                  
                  if(nrow(mCallOptions) > 0){
                    mCallOptions <- mCallOptions %>%
                      mutate(Range = "month", RangeMax = mMax, RangeMin = mMin, Type = "Call") %>%
                      select(Ticker, date, Strike, Last, Bid, Ask, Range, RangeMax, RangeMin,Type)
                    finalData <- bind_rows(finalData, mCallOptions)
                  }
                }
              }
            }
            
            # PUTS ################################################################################
            
            if(!is.null(stockOptions[[1]]$puts)){
              puts <- data.frame()
              for(j in 1:length(stockOptions)) {
                stockOptions2 <- stockOptions[[j]]$puts
                
                if(!is.null(stockOptions2)){
                  puts <- bind_rows(puts, stockOptions2)
                }
              }
              if(!is.null(puts) & nrow(puts)>0) {
                puts$Ticker <- symbols[i]
                date <- gsub("[^0-9.-]", "", rownames(puts))
                puts$date <- as.Date(substr(date, 1, 6), format = "%y%m%d")
                puts <- puts %>%
                  filter(Ask < 0.51 & Ask > 0.001) %>%
                  filter(Last < 0.51 & Last > 0.001)
                
                if(dPut & nrow(puts)>0){
                  dPutOptions <- puts %>%
                    filter(date > Sys.Date() + 30) %>%
                    filter(Strike > dMin)
                  
                  if(nrow(dPutOptions) > 0){
                    dPutOptions <- dPutOptions %>%
                      mutate(Range = "day", RangeMax = dMax, RangeMin = dMin, Type = "Put") %>%
                      select(Ticker, date, Strike, Last, Bid, Ask, Range, RangeMax, RangeMin, Type)
                    finalData <- bind_rows(finalData, dPutOptions)
                  }
                }
                if(wPut & nrow(puts)>0) {
                  wPutOptions <- puts %>%
                    filter(date > Sys.Date() + 60) %>%
                    filter(Strike > wMin)
                  
                  if(nrow(wPutOptions) > 0){
                    wPutOptions <- wPutOptions %>%
                      mutate(Range = "week", RangeMax = wMax, RangeMin = wMin, Type = "Put") %>%
                      select(Ticker, date, Strike, Last, Bid, Ask, Range, RangeMax, RangeMin, Type)
                    finalData <- bind_rows(finalData, wPutOptions)
                  }
                }
                if(mPut & nrow(puts)>0) {
                  mPutOptions <- puts %>%
                    filter(date > Sys.Date() + 90) %>%
                    filter(Strike > mMin)
                  
                  if(nrow(mPutOptions) > 0){
                    mPutOptions <- mPutOptions %>%
                      mutate(Range = "month", RangeMax = mMax, RangeMin = mMin, Type = "Put") %>%
                      select(Ticker, date, Strike, Last, Bid, Ask, Range, RangeMax, RangeMin, Type)
                    finalData <- bind_rows(finalData, mPutOptions)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
