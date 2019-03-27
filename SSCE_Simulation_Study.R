library(MASS)

#needed if using multiple cores
library(parallel)

###Simulation scenarios 

##Scenario 1
set.seed(151)

 M <- 500
 n <- 250
 p <- 10
 
X <- lapply(1:M, function(i) matrix(rnorm(n*p, mean = 1, sd = 2), n, p))
 
p.lin <- lapply(1:M, function(i) 0.2*X[[i]][,1] - 2*X[[i]][,2] + X[[i]][,5] - X[[i]][,6] + X[[i]][,7] - X[[i]][,8])
 
P <- lapply(1:M, function(i) exp(p.lin[[i]])/(1+exp(p.lin[[i]])))
 
A <- lapply(1:M, function(i) rbinom(n, 1, P[[i]]))
 
o.lin <- lapply(1:M, function(i) 2*X[[i]][,1] + 0.2*X[[i]][,2] + 5*X[[i]][,3] + 5*X[[i]][,4])
 
Y <- lapply(1:M, function(i) A[[i]] + o.lin[[i]] + rnorm(n, mean=0, sd=2))
 
set.seed(511)
  
X2 <- lapply(1:M, function(i) matrix(rnorm(n*p, mean = 1, sd = 2), n, p))
 
 p.lin2 <- lapply(1:M, function(i) 0.2*X2[[i]][,1] - 2*X2[[i]][,2] + X2[[i]][,5] - X2[[i]][,6] + X2[[i]][,7] - X2[[i]][,8])
 
 P2 <- lapply(1:M, function(i) exp(p.lin2[[i]])/(1+exp(p.lin2[[i]])))
 
 A2 <- lapply(1:M, function(i) rbinom(n, 1, P2[[i]]))
 
 o.lin2 <- lapply(1:M, function(i) 2*X2[[i]][,1] + 0.2*X2[[i]][,2] + 5*X2[[i]][,3] + 5*X2[[i]][,4])
 
Y2 <- lapply(1:M, function(i) A2[[i]] + o.lin2[[i]] + rnorm(n, mean=0, sd=2))
 
padd <- 490
 set.seed(122)
 Xadd <- lapply(1:M, function(i) matrix(rnorm(n*padd, mean = 1, sd = 2), n, padd))
 
padd2 <- 40
 set.seed(212)
 Xadd2 <- lapply(1:M, function(i) matrix(rnorm(n*padd, mean = 1, sd = 2), n, padd2))


#Scenario 1, n=100, p=50
Xorig.a <- lapply(1:M, function(i) cbind(X[[i]], Xadd[[i]][,1:40])[1:100,])
Yorig.a <- lapply(1:M, function(i) Y[[i]][1:100])
Aorig.a <- lapply(1:M, function(i) A[[i]][1:100])

#Scenario 1, n=150, p=50
Xorig.b <- lapply(1:M, function(i) cbind(X[[i]], Xadd[[i]][,1:40])[1:150,])
Yorig.b <- lapply(1:M, function(i) Y[[i]][1:150])
Aorig.b <- lapply(1:M, function(i) A[[i]][1:150])

#Scenario 1, n=200, p=50
Xorig.c <- lapply(1:M, function(i) cbind(X[[i]], Xadd[[i]][,1:40])[1:200,])
Yorig.c <- lapply(1:M, function(i) Y[[i]][1:200])
Aorig.c <- lapply(1:M, function(i) A[[i]][1:200])

#Scenario 1, n=250, p=50
Xorig.d <- lapply(1:M, function(i) cbind(X[[i]], Xadd[[i]][,1:40]))
Yorig.d <- lapply(1:M, function(i) Y[[i]])
Aorig.d <- lapply(1:M, function(i) A[[i]])

#Scenario 1, n=500, p=50
Xorig.e <- lapply(1:M, function(i) rbind(cbind(X[[i]], Xadd[[i]][,1:40]),
	cbind(X2[[i]], Xadd2[[i]])))
Yorig.e <- lapply(1:M, function(i) c(Y[[i]],Y2[[i]]))
Aorig.e <- lapply(1:M, function(i) c(A[[i]],A2[[i]]))

#Scenario 1, n=250, p=100
Xorig.f <- lapply(1:M, function(i) cbind(X[[i]], Xadd[[i]][,1:90]))
Yorig.f <- lapply(1:M, function(i) Y[[i]])
Aorig.f <- lapply(1:M, function(i) A[[i]])

#Scenario 1, n=250, p=250
Xorig.g <- lapply(1:M, function(i) cbind(X[[i]], Xadd[[i]][,1:240]))
Yorig.g <- lapply(1:M, function(i) Y[[i]])
Aorig.g <- lapply(1:M, function(i) A[[i]])

#Scenario 1, n=250, p=500
Xorig.h <- lapply(1:M, function(i) cbind(X[[i]], Xadd[[i]][,1:490]))
Yorig.h <- lapply(1:M, function(i) Y[[i]])
Aorig.h <- lapply(1:M, function(i) A[[i]])


##Scenario 2
set.seed(151)
M <- 500
n <- 250
p <- 30

sigma <- matrix(0.5, nrow=30, ncol=30)
sigma[,21:30] <- 0
sigma[21:30,] <- 0
diag(sigma)<-1

X<-lapply(1:M, function(i) mvrnorm(n=250, mu=rep(0,30), Sigma=sigma))

p.lin <- lapply(1:M, function(i) -2*X[[i]][,1] - 2*X[[i]][,2] - 2*X[[i]][,3] + X[[i]][,4] + X[[i]][,5] + X[[i]][,6] + 2*X[[i]][,7] + X[[i]][,8] - 0.5*X[[i]][,9] - 0.2*X[[i]][,10] + 2*X[[i]][,13] - 2*X[[i]][,14] + 2*X[[i]][,15] - 2*X[[i]][,16])

P <- lapply(1:M, function(i) exp(p.lin[[i]])/(1+exp(p.lin[[i]])))

A <- lapply(1:M, function(i) rbinom(n, 1, P[[i]]))

o.lin <- lapply(1:M, function(i) 0.2*X[[i]][,1] + 0.2*X[[i]][,2] + 0.2*X[[i]][,3] + 0.2*X[[i]][,4] + 0.2*X[[i]][,5] - 
	0.5*X[[i]][,6] + 0.2*X[[i]][,7] + 0.2*X[[i]][,8] + 0.2*X[[i]][,9] + 0.2*X[[i]][,10] - 5*X[[i]][,11] - 5*X[[i]][,12])

Y <- lapply(1:M, function(i) A[[i]] + o.lin[[i]] + rnorm(n, mean=0, sd=2))

X2<-lapply(1:M, function(i) mvrnorm(n=250, mu=rep(0,30), Sigma=sigma))

p.lin2 <- lapply(1:M, function(i) -2*X2[[i]][,1] - 2*X2[[i]][,2] - 2*X2[[i]][,3] + X2[[i]][,4] + X2[[i]][,5] + X2[[i]][,6] + 2*X2[[i]][,7] + X2[[i]][,8] - 0.5*X2[[i]][,9] - 0.2*X2[[i]][,10] + 2*X2[[i]][,13] - 2*X2[[i]][,14] + 2*X2[[i]][,15] - 2*X2[[i]][,16])

P2 <- lapply(1:M, function(i) exp(p.lin2[[i]])/(1+exp(p.lin2[[i]])))

A2 <- lapply(1:M, function(i) rbinom(n, 1, P2[[i]]))

o.lin2 <- lapply(1:M, function(i) 0.2*X2[[i]][,1] + 0.2*X2[[i]][,2] + 0.2*X2[[i]][,3] + 0.2*X2[[i]][,4] + 0.2*X2[[i]][,5] - 
	0.5*X2[[i]][,6] + 0.2*X2[[i]][,7] + 0.2*X2[[i]][,8] + 0.2*X2[[i]][,9] + 0.2*X2[[i]][,10] - 5*X2[[i]][,11] - 5*X2[[i]][,12])

Y2 <- lapply(1:M, function(i) A2[[i]] + o.lin2[[i]] + rnorm(n, mean=0, sd=2))

#Scenario 3, n=100, p=30
Xorig.i <- lapply(1:M, function(i) X[[i]][1:100,])
Yorig.i <- lapply(1:M, function(i) Y[[i]][1:100])
Aorig.i <- lapply(1:M, function(i) A[[i]][1:100])

#Scenario 3, n=250, p=30
Xorig.j <- X
Yorig.j <- Y
Aorig.j <- A

#Scenario 3, n=500, p=30
Xorig.k <- lapply(1:M, function(i) rbind(X[[i]], X2[[i]]))
Yorig.k <- lapply(1:M, function(i) c(Y[[i]],Y2[[i]]))
Aorig.k <- lapply(1:M, function(i) c(A[[i]],A2[[i]]))




##Scenario 3

set.seed(151)
M <- 500
n <- 250
p <- 30

sigma <- matrix(0.5, nrow=30, ncol=30)
sigma[,21:30] <- 0
sigma[21:30,] <- 0
diag(sigma)<-1

X<-lapply(1:M, function(i) mvrnorm(n=250, mu=rep(0,30), Sigma=sigma))

p.lin <- lapply(1:M, function(i) -2*X[[i]][,1] - 2*X[[i]][,2] - 2*X[[i]][,3] + X[[i]][,4] + X[[i]][,5] + X[[i]][,6] + 2*X[[i]][,7] + X[[i]][,8] - 0.5*X[[i]][,9] - 0.2*X[[i]][,10] + 2*X[[i]][,13] - 2*X[[i]][,14] + 2*X[[i]][,15] - 2*X[[i]][,16])

P <- lapply(1:M, function(i) exp(p.lin[[i]])/(1+exp(p.lin[[i]])))

A <- lapply(1:M, function(i) rbinom(n, 1, P[[i]]))

o.lin <- lapply(1:M, function(i) 0.2*X[[i]][,1] + 0.2*X[[i]][,2] + 0.2*X[[i]][,3] + 0.2*X[[i]][,4] + 0.2*X[[i]][,5] - 
	0.5*X[[i]][,6] + 0.2*X[[i]][,7] + 0.2*X[[i]][,8] + 0.2*X[[i]][,9] + 0.2*X[[i]][,10] - 5*X[[i]][,11] - 5*X[[i]][,12])

Y <- lapply(1:M, function(i) A[[i]] + o.lin[[i]] + rnorm(n, mean=0, sd=2))

X2<-lapply(1:M, function(i) mvrnorm(n=250, mu=rep(0,30), Sigma=sigma))

p.lin2 <- lapply(1:M, function(i) -2*X2[[i]][,1] - 2*X2[[i]][,2] - 2*X2[[i]][,3] + X2[[i]][,4] + X2[[i]][,5] + X2[[i]][,6] + 2*X2[[i]][,7] + X2[[i]][,8] - 0.5*X2[[i]][,9] - 0.2*X2[[i]][,10] + 2*X2[[i]][,13] - 2*X2[[i]][,14] + 2*X2[[i]][,15] - 2*X2[[i]][,16])

P2 <- lapply(1:M, function(i) exp(p.lin2[[i]])/(1+exp(p.lin2[[i]])))

A2 <- lapply(1:M, function(i) rbinom(n, 1, P2[[i]]))

o.lin2 <- lapply(1:M, function(i) 0.2*X2[[i]][,1] + 0.2*X2[[i]][,2] + 0.2*X2[[i]][,3] + 0.2*X2[[i]][,4] + 0.2*X2[[i]][,5] - 
	0.5*X2[[i]][,6] + 0.2*X2[[i]][,7] + 0.2*X2[[i]][,8] + 0.2*X2[[i]][,9] + 0.2*X2[[i]][,10] - 5*X2[[i]][,11] - 5*X2[[i]][,12])

Y2 <- lapply(1:M, function(i) A2[[i]] + o.lin2[[i]] + rnorm(n, mean=0, sd=2))

#Scenario 3, n=100, p=30
Xorig.l <- lapply(1:M, function(i) X[[i]][1:100,])
Yorig.l <- lapply(1:M, function(i) Y[[i]][1:100])
Aorig.l <- lapply(1:M, function(i) A[[i]][1:100])

#Scenario 3, n=250, p=30
Xorig.m <- X
Yorig.m <- Y
Aorig.m <- A

#Scenario 3, n=500, p=30
Xorig.n <- lapply(1:M, function(i) rbind(X[[i]], X2[[i]]))
Yorig.n <- lapply(1:M, function(i) c(Y[[i]],Y2[[i]]))
Aorig.n <- lapply(1:M, function(i) c(A[[i]],A2[[i]]))


