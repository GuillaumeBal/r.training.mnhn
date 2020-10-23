for(q in 1:6){
  print(q)
  #Sys.sleep(2)
}
q

# vectorization vs loops ============================================

vec1 <- rep(10, 10)

vec2 <- rnorm(10)

# create a new vector as a sum, vectorized
vec3.a <- vec1 + vec2

# saun with a loop
vec3.b <- rep(NA, length(vec1))
for(i in 1:length(vec1)){
  vec3.b[i] <- vec1[i] + vec2[i]
}
