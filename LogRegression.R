z.h <- function(theta,x){
  return(1/(1+exp(1)^((-1)*(theta%*%t(x)))))
}

grad <- function(x,y,theta){
  num_train = dim(x)[1]
  pred <- z.h(theta,x)
  dif <- ((pred - t(y)) %*% x)
  return(dif)
}

grad.Desc <- function(y){
  Theta = matrix(c(.5,.5,.5,.5),nrow=1,ncol=4,byrow=TRUE)

  maxiter <- 60

  #y <- get.Key(name)
  x <- prep.Data()
  #cost <- 0
  m = dim(x)[1]
  alpha <- .01
  for(i in 1:maxiter){
    Theta <- Theta - alpha/m*(grad(x,y,Theta))
    #cost <- c(cost,get.Cost(x,y,Theta))
    }
  #plot(cost)
  return(Theta)
}

get.Cost<- function(x,y,theta){
  cost <- (1/150)*(-1*sum(t(y)*log(z.h(theta,x))-(t(1-y))*(log(1-z.h(theta,x)))))
  return(cost)
}

prep.Data <- function(){
  x <- iris
  x$Species <- NULL
  z <- matrix(as.numeric(unlist(x)),nrow=150,ncol=4)
  z.scaled <- z
  z.scaled[,1] <- (z[,1] - mean(z[,1]))/sd(z[,1])
  z.scaled[,2] <- (z[,2] - mean(z[,2]))/sd(z[,2])
  z.scaled[,3] <- (z[,3] - mean(z[,3]))/sd(z[,3])
  z.scaled[,4] <- (z[,4] - mean(z[,4]))/sd(z[,4])
  return(z.scaled)
}

get.Key <- function(name){
  x <- iris
  key <- iris$Species
  x$Species <- NULL
  num_data <- dim(x)[1]
  y <- matrix(0,num_data,1)
  for(i in 1:num_data){
    if(key[i] == name){
      y[i] = 1
    }
    else{
      y[i] = 0
    }
  }
  return(y)
}

train.Models <- function(){
  setosa.y <- get.Key("setosa")
  versicolor.y <- get.Key("versicolor")
  virinica.y <- get.Key("virginica")
  
  setosa.theta <- grad.Desc(setosa.y)
  versicolor.theta <- grad.Desc(versicolor.y)
  virinica.theta <- grad.Desc(virinica.y)
  
  print(setosa.theta)
  print(versicolor.theta)
  print(virinica.theta)
}
