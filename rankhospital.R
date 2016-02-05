rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  out_data <- read.csv("C:/Users/Thomas MARTIN/Desktop/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  expected_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% unique(out_data["State"])$State) {
    stop("invalid state")
  }
 
  if (!outcome %in% expected_outcomes) {
    stop("invalid outcome")
  }
  else {
    switch(outcome,
      "heart attack"= { outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"},
      "heart failure" = { outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"},
      "pneumonia" = { outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"},
      stop("Something wrong"))
    
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Remove NA value in ...
  options(warn=-1)
  out_data[, outcome] <- as.numeric(out_data[, outcome]) 
  options(warn=0)
  
  out_data_clean <- out_data[ , c("Hospital.Name","State", outcome)][!is.na(out_data[,outcome]),]
  
  out_state <- out_data_clean[out_data_clean$State == state,]
  ordered_out_state <- out_state[order(out_state[,outcome], out_state$Hospital.Name), ]
  if (num == "best") {
    result  <- ordered_out_state[1,"Hospital.Name"] 
  }
  else if (num == "worst") {
    result <- ordered_out_state[nrow(out_state),"Hospital.Name"] 
  }
  else {
    result <- ordered_out_state[num,"Hospital.Name"]
  }
  result
}