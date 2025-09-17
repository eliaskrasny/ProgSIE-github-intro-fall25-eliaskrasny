#1
x <- 5
typeof(x)

#2
x<- "a"
typeof(x)

#9
x <- c(2, 2, 4, 5, 7, 10, 11, 9, 10)
print(x[5])

#13
x<- c(1,2,3,4,5)
for(i in x){
  print(i)
}

x <- 1:5
result <- numeric(length(x))  # Pre-allocate a vector to store results


for (i in seq_along(x)) {
  result[i] <- x[i]^2
}


result
