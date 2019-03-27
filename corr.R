corr<-function(directory,threshold){
  answer<-vector(mode="numeric",length=0)
  for(i in 1:332){
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
    if(norv>threshold){
      sums<-f[2:nrow(f),2]
      sumn<-f[2:nrow(f),3]
      result<-cor(sums,sumn,use='complete.obs')
      answer<-c(answer,result)
    }
  }
  answer
}