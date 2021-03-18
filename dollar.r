dollar <- function(x, digits = 2, format = "f", ...) {
  if(x>=0){
    paste0("$", formatC(x, format = format, big.mark = ",", digits = digits, ...))
  }else{
    paste0("-$", formatC(abs(x), format = format, big.mark = ",", digits = digits, ...))
  }  
}
