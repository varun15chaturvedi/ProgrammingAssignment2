setwd("C://Users//varun//Downloads")
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  ## Check that state and outcome are valid
  unique_state<- unique(outcome_data$State)
  if(!(state %in% unique_state)){
    stop("invalid state")
  }
  outcomes<- c("heart attack","heart failure","pneumonia")
  if(!(outcome %in% outcomes)){
    stop("invalid outcome")
  }
  
  
  ## Return hospital name in that state with lowest 30-day death
  patt = "^Hospital.30.Day.Death..Mortality"
  filter(outcome_data,outcome_data$State==state,)->temp
  if (outcome =="heart attack" ) {
    patt_out = "attack"
  }else if(outcome=="heart failure"){
    patt_out = "failure"
  }else{
    patt_out = "pneumonia"
  }
  
  temp%>%select(Hospital.Name,ends_with(patt_out))->temp
  temp%>%select(Hospital.Name,rate= grep(pattern =patt ,names(temp),ignore.case = T))->final_data
  ## Return hospital name in that state with the given rank
  final_data$rate = as.numeric(final_data$rate)
  final_data = final_data[which(!is.na(final_data$rate)),]
  rank_vector= final_data$rate
  final_data$Rank<-  rank(rank_vector,na.last = TRUE,ties.method ="max" )
  ## 30-day death rate
  exit<- F
  
  if(num == "worst"){
    num = max(final_data$Rank)
  }else if (num == "best"){
    num = min(final_data$Rank)
  }else if (num > max(final_data$Rank)){
    exit = T
  }else{
    num = num
  }
  
  
  if (exit == F){
  temp_data <- filter(.data = final_data,Rank ==num)
  order(temp_data)
  hname<- as.character(temp_data[1,1])
  }else {
    hname<- NA
  }
  #return(temp_data)
  return(hname)
}