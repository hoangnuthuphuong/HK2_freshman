ucln<-function(a,b){
  if(b==0){
    return(a)}
  else if (a==0){
    return(b)}
  ucln(b,a%%b)
}
a<-readline("Nhap a:")
a<-as.integer(a)
b<-readline("Nhap b:")
b<-as.integer(b)
cat('Uoc chung lon nhat cua',a,'va',b,'la:',ucln(a,b))
