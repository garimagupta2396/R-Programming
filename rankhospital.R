rankhospital<-function(state,outcome,num){
  out<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  s<-split(out,out$State)
  a<-s[[state]]
  if(outcome=="heart attack"){
    col<-11
  }
  else if(outcome=="heart failure"){
    col<-17
  }
  else{
    col<-23
  }
  
  row<-vector(mode="logical",length=0)
  for(i in 1:nrow(a)){
    if(a[i,col]=="Not Available"){
      row<-c(row,FALSE)
    }
    else{
      row<-c(row,TRUE)
    }
  }
  a<-a[row,]
  a<-a[order(a[[2]]),]
  a[[col]]<-as.numeric(a[[col]])
  a<-a[order(a[[col]]),]
  print(a[,c(2,col)])
  if(num=="best"){
    result<-a[1,2]
  }
  else if(num=="worst"){
    result<-a[nrow(a),2]
  }
  else if( num > nrow(a)){
    print("NA")
  }
  else {
    result<-a[num,2]
  }
  print(result)
}