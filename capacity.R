days <- 40
t <- 1:days
r = 0.2
i0 <- 1
testprop <- 0.5
capacity <- 5000
positive_ratio = 0.05

## just say only a proportion of I will go get a test
Itest <- i0*exp(r*t)*testprop


## To get a positive % daily, we need to test Itest/prob people
people_testing <- Itest/positive_ratio

## But we also have a bunch of S that wants to be tested (aka the negatives)
## Assuming it is a constant per day


backlog <- function(x,cap=5000){
  q <- 0
  if(x>cap){q <- x-cap}
  return(q)
}

pending <- sapply(people_testing,function(x)backlog(x,cap=capacity))

print(pending)

## how long will it take to finish testing everyone?

print(max(pending)/capacity)
extradays <- ceiling(max(pending)/capacity)

extradat <- data.frame(day = 1:(extradays+days), count = c(pending,rep(NA,extradays)))

for(i in 1:extradays){
  extradat$count[days+i] <- max(pending) - i*capacity
}


