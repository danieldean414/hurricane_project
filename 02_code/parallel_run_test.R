
library(parallel)

detectCores()

# Just testing example `parallel` code:

# load parallel package
require(parallel)
# define function to test whether an number is prime
is_prime <- function(num)
{
  # if input equals 2 or 3, then we know it's prime
  if(num == 2 | num == 3) 
    return(TRUE)
  # if input equals 1, then we know it's not prime
  if(num == 1) 
    return(FALSE)
  
  # else if num is greater than 2
  # and divisible by 2, then can't be even
  if(num %% 2 == 0) 
    return(FALSE)
  
  # else use algorithm to figure out
  # what factors, if any, input has
  
  # get square root of num, rounded down
  root <- floor(sqrt(num))
  
  # try to divide each odd number up to root
  # into num; if any leave a remainder of zero,
  # then we know num is not prime
  for(elt in seq(5,root))
  {
    if (num %% elt == 0)
      return(FALSE)
    
  }
  # otherwise, num has no divisors except 1 and itself
  # thus, num must be prime
  return(TRUE)
  
}
# get random sample of 1 million integers from integers between 1 and 
# 10 million
# set seed so the random sample will be the same every time
set.seed(2)
sample_numbers <- sample(10000000, 1000000)
# do a couple checks of function
is_prime(17) # 17 is prime
is_prime(323) # 323 = 17 * 19; not prime
# create cluster object
cl <- makeCluster(3)
# test each number in sample_numbers for primality
results <- parSapply(cl , sample_numbers , is_prime)
# close
stopCluster(cl)


# Difference made by # cores:

start <- proc.time()
results <- sapply(sample_numbers , is_prime)
end <- proc.time()
print(end - start) # 41.39

start <- proc.time()
cl <- makeCluster(2)
results <- parSapply(cl , sample_numbers , is_prime)
stopCluster(cl)
end <- proc.time()
print(end - start) # 24.29 

start <- proc.time()
cl <- makeCluster(3)
results <- parSapply(cl , sample_numbers , is_prime)
stopCluster(cl)
end <- proc.time()
print(end - start) # 17.13 


start <- proc.time()
cl <- makeCluster(4)
results <- parSapply(cl , sample_numbers , is_prime)
stopCluster(cl)
end <- proc.time()
print(end - start) # 14.00 


start <- proc.time()
cl <- makeCluster(5)
results <- parSapply(cl , sample_numbers , is_prime)
stopCluster(cl)
end <- proc.time()
print(end - start) # 13.46 


start <- proc.time()
cl <- makeCluster(6)
results <- parSapply(cl , sample_numbers , is_prime)
stopCluster(cl)
end <- proc.time()
print(end - start) # 17.86 (?!) <- oh, laptop had gone into battery-saver mode
                    # 12.92 on a rerun


start <- proc.time()
cl <- makeCluster(7)
results <- parSapply(cl , sample_numbers , is_prime)
stopCluster(cl)
end <- proc.time()
print(end - start) # 12.79


start <- proc.time()
cl <- makeCluster(8)
results <- parSapply(cl , sample_numbers , is_prime)
stopCluster(cl)
end <- proc.time()
print(end - start) # 12.04


cores <- c(1,2,3,4,5,6, 7, 8)
times <- c(41.39, 24.29, 17.13, 14.00, 13.46, 12.92, 12.79, 12.0)

ggplot() + aes(x = cores, y = times) + geom_smooth() + geom_point()

  # looks like diminishing returns after ~5 cores here

# So, if a storm took e.g. 3 minutes to run w/ one core,
  # theoretically could get to ~1 minute with parallel processing

  # so for ~10-20 storms (let's say 15) over 25 years = 375 storms = ~19 hr 
    # could get down to ~6 hr

  # vs more like 1/10 with SUMMIT method


### 
# Thinking throgh logistics; so this applies an arbitrary function to every row/column
  # I wonder if it would be feasible to do this inside a nested tibble?
    # i.e. have storm events as the rows?