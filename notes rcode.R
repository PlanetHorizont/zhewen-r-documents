library(quantreg)
data(AirPassengers)
x <- as.numeric(time(AirPassengers))
y <- as.numeric(AirPassengers)
medianreg <- rq( y ~ x, tau=0.5 )
plot(x,y)
lines(x, fitted(medianreg))
fit <- lm(y~x)
abline(fit,col="red")


sel=c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE) 
      x = x[sel]
      y = log(y[sel])
      medianreg <- rq( y ~ x, tau=0.5 )
      fit <- lm(y ~x)
      plot(x,y)
      lines(x, fitted(medianreg)) 
      abline(fit,col="red")
      
      y[1]=3.5
      medianreg <- rq( y ~ x, tau=0.5 ) 
      fit <- lm(y ~x)
      plot(x,y)
      lines(x, fitted(medianreg)) 
      abline(fit,col="red")
      
      y <- as.numeric(AirPassengers)
      y = log(y[sel])
      x[1]=1935
      medianreg <- rq( y ~ x, tau=0.5 ) 
      fit <- lm(y ~x)
      plot(x,y)
      lines(x, fitted(medianreg)) 
      abline(fit,col="red")
      
      
library(MASS)
medianreg <- rq( y ~ x, tau=0.5 )
plot(x,y)
lines(x, fitted(medianreg))
abline(lmsreg(y ~ x),lwd=2,col="red",lty=2)
abline(ltsreg(y ~ x),lwd=2,col="green",lty=3)


n =500; B <- 10000; 
X = rnorm(n,mean=0) 
TH = median(X) 
THboot <- numeric(B) 
for (b in 1:B){
  xboot = sample(X,n,replace=TRUE); 
  THboot[b] = median(xboot);}

1/(2*sqrt(n)*dnorm(0)) #Asymptotic var ## [1] 0.05604991
sd(THboot) # " Bootstrap
## [1] 0.04428533