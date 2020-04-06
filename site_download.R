download<- function(filename){
  
  library(rvest)
  library(selectr)
  library(httr)
  library(XML)
  library(stringi)
  # setting up path
  path<- getwd()
  path_list<- strsplit(path,split ="/")
  path<-paste(path_list[[1]][1],path_list[[1]][2],path_list[[1]][3],sep = "\\")
  setwd(path)
  if(!dir.exists("365 DS Course")){
    dir.create("365 DS Course")  
  }
  setwd("365 DS Course")
  ## reading URL`s and downloading data.`
  read_links<- read.csv(filename,stringsAsFactors = F,header = T)
  links<- read_links[,2]
  names<- read_links[,1]
  setwd("Power BI")
  for(i in seq_along(links)){
    link<-sub(".bin",".mp4",links[i] )
    name<- paste(names[i],".mp4",sep = "")
    download.file(link,destfile=name,method ="curl" )
  }
}
# copy and paste in console and press enter
#setwd("C:/Users/varun/365 DS Course")
#download('SQL_tableau_python_link.csv')