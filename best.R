best<-function(state,outcome){
  out<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  if(outcome=="heart attack")
  {
    col<-11
  }
  else if(outcome=="heart failure")
  {
    col<-17
  }
  else{
    col<-23
  }
  small<-9999
  for(i in 1:nrow(out))
  {
    if(out[i,7]==state && !out[i,col]=="Not Available"){
      temp<-as.numeric(out[i,col])
      temp2<- small-temp
      if(temp2>0){
      small<-as.numeric(out[i,col])
      }
    }
  }
  result<-vector(mode="character",length=0)
  for(i in 1:nrow(out)){
    if(out[i,7]==state && out[i,col]!="Not Available"){
    if(as.numeric(out[i,col])==small){
      result<-c(result,out[i,2])
    }
    }
  }
  if(length(result)>1){
    result<-sort(result)
  }
  print(result[1])
}