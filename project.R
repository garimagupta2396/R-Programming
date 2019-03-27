pollutantmean<-function(directory,pollutant,id){
  sum<-vector(mode="numeric", length=0)
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
  sulphate<-c("sulphate")
  nitrate<-c("nitrate")
  if(identical(sulphate,pollutant)){
    col<-2
  }
  if(identical(nitrate,pollutant)){
    col<-3
  }
  sum<-c(sum,f[2:nrow(f),col])
  }
  mean_final<-mean(sum,na.rm=TRUE)
  mean_final
}