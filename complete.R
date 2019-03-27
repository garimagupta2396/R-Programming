complete<-function(directory,id){
  answer<-matrix(nrow=length(id),ncol=2)
  a<-1
  for(i in id){
    if(nchar(i)==1){
      name<-c("00",i,".csv")
      file_name<-paste(name,collapse="")
    }
    
    else if(nchar(i)==2){
      name<-c("0",i,".csv")
      file_name<-paste(name,collapse="")
    }
    
    else{
      name<-c(i,".csv")
      file_name<-paste(name,collapse="")
    }
    f<-read.csv(file_name)
    norv<-0
    for(r in 2:nrow(f)){
      b<-!(is.na(f[r,2])) && (!(is.na(f[r,3]))) && !(is.na(f[r,1]))
      if(b) {
        norv<-norv+1
      }
    }
    answer[a,1]<-i
    answer[a,2]<-norv
    a<-a+1;
  }
  answer
}