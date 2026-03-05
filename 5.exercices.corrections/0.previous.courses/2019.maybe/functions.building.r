
sum.2.pos.number <- function(x, y){
  if(x > 0 & y > 0){
  z <- x + y
  return(z)
  }else{
    stop('at least one value is negative')
  }
}

{
  a <- 3
  b <- -2
  
  c <- sum.2.pos.number(a, b)
  print(c)
  
  d <- a + as.numeric(c)
  print(d)
}
