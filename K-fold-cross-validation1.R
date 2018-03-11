err1 <-c()
err2 <-c()
for(J in 1:50){
  ##Generate the data as in the Part 10 code.##
  f = function(x) (1 + 10*x - 5*x^2)*sin(10*x)
  n = 1e3
  k <- 5
  nn <- n*(1-1/k)
  x = runif(n)
  x=sort(x)
  y = f(x) + rnorm(n)
  h <- seq(0.01, 1, 0.01)
  ##Choose the h that has the smallest predict error##
  RSS <- c()
  RSS1 <- c()
  for(b in 1: length(h)){
    Rss <- c()
    Rss1 <- c()
    for(i in 1: k){
      kk<-n/k
      qw<-seq(i, 1000, by=k)
      num <- rep(c(1:k), n/k)
      xtrain <- x[-qw] 
      ytrain <- y[-qw]
      xvalid <- x[qw] 
      yvalid <- y[qw]
      fit11 = ksmooth(xtrain, ytrain, 'normal', bandwidth = h[b], x.points=xvalid)
      nnn <- sort(sample(n, n/k, replace=FALSE))
      xtrain1 <- x[-nnn] 
      ytrain1 <- y[-nnn]
      xvalid1 <- x[nnn] 
      yvalid1 <- y[nnn]
      fit22 = ksmooth(xtrain1, ytrain1, 'normal', bandwidth = h[b], x.points=xvalid1)
      yvalidhat <- fit11$y
      yvalidhat1 <- fit22$y
      Rss[i] <- sum((yvalid - yvalidhat)^2)/kk
      Rss1[i] <- sum((yvalid1 - yvalidhat1)^2)/kk
    }
    RSS[b] <- mean(Rss)
    RSS1[b] <- mean(Rss1)
  }
  hbest <- h[which.min(RSS)]
  hbest1 <- h[which.min(RSS1)]
  
  ##Test##
  ##Generate 500 new data to test our method.##
  f = function(x) (1 + 10*x - 5*x^2)*sin(10*x)
  nvalid = 500
  xtest = runif(nvalid)
  xtest = sort(xtest)
  ytest = f(xtest) + rnorm(nvalid)
  fit1 = ksmooth(x, y, 'normal', bandwidth = hbest, x.points=xtest)
  fit2 = ksmooth(x, y, 'normal', bandwidth = hbest1, x.points=xtest)
  err1[J] <- sum((ytest - fit1$y)^2)/nvalid
  err2[J] <- sum((ytest - fit2$y)^2)/nvalid
}
er1 <- mean(err1)
er2 <- mean(err2)
rratesmall <- length(which(err1<err2))/50
rratebig <- length(which(err1>err2))/50
cat("The mean err for K-fold CV is", er1)
cat("The mean err for variant method is", er2)
cat("The rate that the mean err for K-fold CV is smaller is ", rratesmall)
cat("The rate that the mean err for variant method is smaller is ", rratebig)
##We test the method by the data generated as in the Part 10 code. We simulate
##for 50 times. For each time, we choose two h values by k-fold CV method and variant 
##method respectively, and then we use the chosen values of h to fit the model.

##At last, among the 50 simulations, we get the mean prediction errors for k-fold CV method and variant 
##method respectively, and we can see that the mean prediction error for k-fold CV method 
##is about 1.02, and the mean prediction error for variant method is about 1.04.
##Then k-fold CV method has smaller prediction error than variant method.

##Among the 50 simulations, rate that the mean err for K-fold CV is smaller is about 0.6, and
##the rate that the mean err for variant method is smaller is about 0.3. So k-fold CV method is 
##better than variant method.

