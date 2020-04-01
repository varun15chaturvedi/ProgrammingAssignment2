setwd("C://Users//varun//Downloads")
library(dplyr)
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  ## Check that state and outcome are valid
  outcomes<- c("heart attack","heart failure","pneumonia")
  if(!(outcome %in% outcomes)){
    stop("invalid outcome")
  }
  
  
  ## Return hospital name in that state with lowest 30-day death
  patt = "^Hospital.30.Day.Death..Mortality"
  outcome_data->temp
  if (outcome =="heart attack" ) {
    patt_out = "attack"
  }else if(outcome=="heart failure"){
    patt_out = "failure"
  }else{
    patt_out = "pneumonia"
  }
  
  temp%>%select(Hospital.Name,State,ends_with(patt_out))->temp
  temp%>%select(Hospital.Name,State,rate= grep(pattern =patt ,names(temp),ignore.case = T))->final_data
  final_data<- group_by(.data = final_data,State)
  final_data$rank <- rep(NA,nrow(final_data))
  unique_state<-unique(final_data$State)
  for(i in unique_state){
    temp_col<- final_data[final_data$State == i,2:4]
    temp_col$rate = as.numeric(temp_col$rate)
    rank_vector= temp_col$rate
    temp_col$rank<-  rank(rank_vector,na.last = TRUE,ties.method ="max" )
    final_data[final_data$State == i,2:4]<-temp_col
    final_data = final_data[which(!is.na(final_data$rate)),]
  }
  #final_data_temp<- split(final_data,final_data$State,drop = T)
  ## Return hospital name in that state with the given rank
  #final_list<- lapply(final_data_temp,as.numeric,final_data_temp$rate)
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  if(num == "worst"){
    num = max(final_data$Rank)
  }else if (num == "best"){
    num = min(final_data$Rank)
  }else if (num > max(final_data$Rank)){
    exit = T
  }else{
    num = num
  }
  hname_data<- data.frame(hospital= as.character(rep(NA,length(unique_state))),State = unique_state,row.names = unique_state,stringsAsFactors = F)
  hname_temp<-vector("character",length = 0)
  for (i in unique_state){
    temp_data<- final_data[final_data$State == i,]
    r_data<- temp_data[temp_data$rank == num,]
    hname<- r_data[1,1]
    #hname_data[hname_data$State == i,1]<-r_data[1,1]
    hname_temp<- c(hname_temp,hname)
  }
  hname_data$hospital = hname_temp
  ## (abbreviated) state name
  arrange(hname_data,hname_data$State)
  return(hname_data)
}
