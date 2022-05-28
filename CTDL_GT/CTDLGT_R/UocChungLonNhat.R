ucln<-function(a,b){
  if(b==0){
    return(a)}
  else if (a==0){
    return(b)}
  ucln(b,a%%b)
}

a<-17
b<-90
cat('Uoc chung lon nhat cua',a,'va',b,'la:',ucln(a,b))
