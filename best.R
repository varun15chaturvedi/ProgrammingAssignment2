setwd("C://Users//varun//Downloads")
library(dplyr)
best <- function(state, outcome) {
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
  
  
  ## rate
  final_data$rate = as.numeric(final_data$rate)
  final_data = final_data[which(!is.na(final_data$rate)),]
  rank_vector= final_data$rate
  final_data$rank<-  rank(rank_vector,ties.method = "max")
  hname <- filter(final_data,final_data$rank ==1)[1]
  return(as.character(hname))
}




#a<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#
#b<-split(filter(a,a$State=="NY"),a$Hospital.Name,drop = TRUE)
#outcome<- "pneumonia"
#patt= paste("^Hospital.30.Day.Death..Mortality",sep = "")
#patt
#filter(a,a$State=="MD")%>%select(contains(a,outcome,ignore.case = T))->temp
#temp%>%select(grep(pattern =patt ,names(temp),ignore.case = T))
#str(contain)
