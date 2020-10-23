### function power it

power.it <- function(a, b){
  if(!is.numeric(a) | !is.numeric(b)){
    stop('one argument at least is not a number')
  }
  if(b < 1){
    warning('computing a root')
  }
  return(a ^ b)
}

power.it(3, .5)

# ratio as percentage #####

ratio.as.percent <- function(a, b){
  my.percentage <- round((a/b) * 100, 0)
  return(paste(my.percentage, '%'))
}

