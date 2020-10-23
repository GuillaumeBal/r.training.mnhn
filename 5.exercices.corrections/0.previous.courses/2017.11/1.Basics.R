

# some math ========================================================
x <- 2 # define x

x + 2
x

# looking at help =============================================================
help(rep)
??'glm'

# Exercises: random numbers drawn from a uniform distribution:
?runif

y <- 
  runif(n = 5, 
        min = 0, 
        max = 1)

y

??mean
mean(x = y)

??"standard deviation"
sd(x = y)

help.search("normal distribution")
rnorm(n = 3, 
      mean = 0, 
      sd = 1)
rnorm(n = 3)

rnorm(n = 3, 
      mean = 20, 
      sd = c(1, 2))

rnorm()
