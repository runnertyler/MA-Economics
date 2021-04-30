fib <- c(1, 2)
while (max(fib) < 4000000){
  len <- length(fib)
  fib <- c(fib, fib[len - 1] + fib[len])
}
sum <- sum(fib[fib %% 2 == 0])
print(sum)