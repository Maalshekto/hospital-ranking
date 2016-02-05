## 
## best.R 
##
## This file contains 1 function that will process the outcome-of-care-measures.csv file and 
## return the name of the best Hospital for a given state according to the lowest 
## 30 Days Mortality from a specific outcome.
## 
## Author   - Thomas MARTIN
## Date     - 05/02/2016
## Email    - tmartin@live.fr
## Version  - 1.0
##

## 
## best
##
## This function return the name of the best Hospital for a given state and a given outcome.
## The funcion do : 
## 1 - read outcome data
## 2 - check the validity of arguments
## 3 - subset the data to obtain only data from specific state and outcome.
## 4 - order data and return the name of best Hospital.
## 



best <- function(state, outcome) {
  ## Read outcome data
  out_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!state %in% unique(out_data["State"])$State) {
    stop("invalid state")
  }
 
  ## if entered outcome is correct, obtain the concerned colomn name. 
  switch(outcome,
      "heart attack"= { outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"},
      "heart failure" = { outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"},
      "pneumonia" = { outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"},
      stop("invalid outcome"))
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  ## Force numeric value in 
  options(warn=-1)
  out_data[, outcome] <- as.numeric(out_data[, outcome]) 
  options(warn=0)
  
  ## Remove NA value and subset the data to get only useful data.
  out_data_clean <- out_data[ , c("Hospital.Name","State", outcome)][!is.na(out_data[,outcome]),]
  
  ## Subset to specific state data.
  out_state <- out_data_clean[out_data_clean$State == state,]
  
  ## Order lowest rate to highest rate of mortality from specific outcome
  ## and the alphabetic order of the name of Hospital then return the first
  ## Hospital name.
  
  out_state[order(out_state[,outcome], out_state$Hospital.Name), ][1,"Hospital.Name"]
}