thn <- function(n, A, B, C){
  if(n == 1){
    cat("Di chuyen dia 1 tu", A, "sang", B,'\n')
    return()}
  thn(n-1, A, B, C)  
  cat("Di chuyen dia", n, "tu", A, "sang", B,'\n')
  thn(n-1, B, C, A)
}
n<-readline("Nhap n:")
n<-as.integer(n)
thn(n, "A", "B", "C")
cat("Hoan thanh")
