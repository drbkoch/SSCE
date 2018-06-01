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


###Run methods on simulated data 

##Using Scenario 1a as an example (simply change Yorig.a, Xorig.a, and Aorig.a to Yorig.x, Xorig.x, and Aorig.x, respectively, to get results for a different scenario above)

#number of cores used per simulation
no.of.cores <- 24

#SSCE: 

#here is the code for this function:

library(truncnorm) 
library(MCMCpack) 
library(parcor)

#Xorig: nxp matrix of covariates 
#Yorig: nx1 vector of outcomes
#Aorig: nx1 binary vector indicating treatment assignment
#tau.2: shrinkage parameter (set to large value to remove shrinkage bias; default is 1000) 
#M: number of MCMC iterations
#burn: number of burn-in iterations
SSCE <- function(Xorig, Yorig, Aorig, tau.2 = 1000, M = 5000, burn = 0, Bilevel = TRUE){
	#this stores outcome coefficients at each MCMC iteration 
	beta.sv <- list()
	
	#this stores treatment coefficients at each MCMC iteration 
	gamma.sv <- list()
	
	#this stores pi_0 at each MCMC iteration
	pi_0.sv <- list()
	
	#this stores sigma.2 at each MCMC iteration
	sigma.2.sv <- list()

	#number of covariates
	p <- ncol(Xorig)

	#sample size
	n <- nrow(Xorig)

	#transform covariates to have mean zero and unit variance
	Xstd <- cbind(t(t(Xorig - matrix(rep(apply(Xorig, 2, mean), nrow(Xorig)), 
		nrow(Xorig),byrow=TRUE))*(1/apply(Xorig, 2, sd))), 1, Aorig)

	#initialize beta to zero
	beta <- rep(0, p+2)

	#initialize gamma to zero
	gamma <- rep(0, p+1)

	#initialize pi_0
	pi_0 <- 0.5

	#initialize sigma.2
	sigma.2 <- 1

	#these two variables are used in computations in the Gibbs sampler below
	Sigma <- 1/(n - 1 + 1/tau.2)
	D.tau.inv <-diag(rep(1/tau.2, p))

		#############################################################
		######This block is used to estimate parameters for Bilevel SSCE#############
		##################################################################

		#To use the lasso on the outcome model, we will transform the outcome so that
		#we do not need to estimate an intercept or the main effect of treatment 
		##Such a transformation is not necessary but allows for all lasso software to be used
		##in this situation: an alternative is to include the intercept and main effect of treatment
		##in the model and not penalize their coefficients (i.e., not shrink them toward zero)

		#this is used in the transformation of the outcome
		Anew <- ifelse(Aorig == 1, 1, -1)

		#mean of outcome among treated (used in the transformation of the outcome)
		meanYtrt <- mean(Yorig[Anew == 1])

		#mean of outcome among untreated (used in the transformation of the outcome)
		meanYcont <- mean(Yorig[Anew == -1])

		#mean of each covariate among the treated (used in the transformation of the outcome)
		meanXtrt <- unlist(lapply(1:ncol(Xorig), function(i) mean(Xorig[Anew==1,i]))) 

		#mean of each covariate among the untreated (used in the transformation of the outcome)
		meanXcont <- unlist(lapply(1:ncol(Xorig), function(i) mean(Xorig[Anew==-1,i]))) 
	
		#used in the transformation of the outcome
		stdx <- unlist(lapply(1:p, function(i) sd(Xorig[,i] - (1/2)*(Anew + 1)*meanXtrt[i] - (1/2)*(1 -			
			Anew)*meanXcont[i])))
	
		#create transformed covariate matrix so that covariates have mean zero and sd=1
		Xout <- matrix(
			unlist(lapply(1:p, function(i) (Xorig[,i] - (1/2)*(Anew + 1)*meanXtrt[i] - (1/2)*(1 - 	
			Anew)*meanXcont[i])/stdx[i])), n, p)	

		#center the outcomes so that there is no need to estimate the intercept and main effect of 			
		#treatment (i.e., estimates for these coefficients are exactly zero after this transformation)
		Ycent = (Yorig - (1/2)*(Anew + 1)*meanYtrt - (1/2)*(1 - Anew)*meanYcont)

		#fit the lasso to the (transformed) outcome model, which now has only p parameters
		##10 fold cv is used to choose tuning parameter lambda
		lasso.fit <- adalasso(Xout, Ycent, k = 10, intercept = FALSE)

		#get estimated lasso coefficients
		lasso.coef <- lasso.fit$coefficients.lasso

		#determine which covariate coefficients are non-zero
		nonzero.lasso.coef <- which(lasso.coef != 0)

		#find sigma.2.y_a.x for all covariates that have coefficients equal to zero according to lasso
		temp <-  (1/(n-length(nonzero.lasso.coef)))*sum((Yorig - Xstd[,c(nonzero.lasso.coef, p+1, p+2)]
			%*%(solve(t(Xstd[,c(nonzero.lasso.coef, p+1, p+2)])%*%Xstd[,c(nonzero.lasso.coef, p+1, p+2)])
			%*%t(Xstd[,c(nonzero.lasso.coef, p+1, p+2)])%*%Yorig))^2) 

		#set sigma.2.y_a.x to the value above for all covariates (we change the values below for covariates
		#that are non-zero)
		sigma.2.y_a.x <- rep(temp, p)

		#find sigma.2.y_a.x for all covariates that have non-zero coefficients according to the lasso
		temp <- unlist(lapply(nonzero.lasso.coef, function(g) 
			(1/(n-length(nonzero.lasso.coef)))*sum((Yorig - Xstd[,c(nonzero.lasso.coef, p+1, p+2)][,-g]
				%*%(solve(t(Xstd[,c(nonzero.lasso.coef, p+1, p+2)][,-g])%*%
				Xstd[,c(nonzero.lasso.coef, p+1, p+2)][,-g])%*%t(Xstd[,c(nonzero.lasso.coef, p+1, p+2)][,-g])
				%*%Yorig))^2)  ))
	
		#set sigma.2.y_a.x to the values above for the covariates with non-zero coefficients
		sigma.2.y_a.x[nonzero.lasso.coef] <- temp	
	
		#set values for the other parameters in Bilevel SSCE	
		sigma.2.a_x <- rep(1, p)	
		sigma.2.z_a.x <- rep(1, p)
		sigma.2.z_x <- rep(1, p)

		##################################################################
		######End of block used to estimate parameters for Bilevel SSCE#############
		##################################################################


	#############START GIBBS SAMPLER##################
	for(iter in 1:(burn+M)){
		#Draw Astar
		Astar <- ifelse(Aorig == 1, rtruncnorm(n=1,a = 0, b = Inf, mean = Xstd[,-(p+2)]%*%gamma, sd = 1), 
			rtruncnorm(n=1, a = -Inf, b = 0, mean = Xstd[,-(p+2)]%*%gamma, sd=1))

	for(g in 1:p){
		#mean of slab for outcome coefficient
		mu.g.out <-  Sigma*t(Xstd[,g])%*%(Yorig - Xstd[,-g]%*%beta[-g])
		#mean of slab for treatment coefficient
		mu.g.trt <-  Sigma*t(Xstd[,g])%*%(Astar - Xstd[,-c(g,p+2)]%*%gamma[-g])

		#draw conditional prob. coefficients are zero
		l.g <-  pi_0/(pi_0 + (1 - pi_0)*(tau.2*tau.2)^(-1/2)*
			(Sigma*Sigma)^(1/2)*exp(
			(1/2)*((1/sigma.2)*(Sigma^(1/2)*t(Xstd[,g])%*%(Yorig - Xstd[,-g]%*%beta[-g]))^2 + 		
    		(Sigma^(1/2)*t(Xstd[,g])%*%(Astar - Xstd[,-c(g,p+2)]%*%gamma[-g]))^2)))
		
		#draw indicators denoting which coefficients are zero/non-zero				
		zero.ind <- rbinom(1, 1, l.g)

		#get coefficient values for SSCE or for first level of Bilevel SSCE
		###############################################
		if(zero.ind == 1){
			beta[g] <- 0
			gamma[g] <- 0
		}else{
			temp <- rnorm(n = 2, mean = c(mu.g.out, mu.g.trt), 
			sd =  sqrt(c(sigma.2*Sigma, Sigma)))
	
			beta[g] <- temp[1]
		
			gamma[g] <- temp[2]
		}
		###############################################

		#This is for Bilevel SSCE only
		############################################
		if(Bilevel == TRUE){
			##For each non-zero coefficient according to SSCE, we find the change in MSE if
			##this coefficient was set to zero; we then set the coefficients to zero if doing so
			##improves MSE of the treatment effect estimator  
			if(beta[g] != 0){
			 	var_small <- sigma.2.y_a.x[g]/sigma.2.a_x[g]*1/n
    			var_big <- (sigma.2.y_a.x[g] - sigma.2.z_a.x[g]*beta[g]^2)/
    		 	 	(sigma.2.a_x[g] - sigma.2.z_x[g]*(gamma[g]/sqrt(2*pi))^2)/n
    			bias <- (beta[g]*(gamma[g]/sqrt(2*pi))*sigma.2.z_x[g])/sigma.2.a_x[g] 
    			MSE_change <- bias^2 + var_small - var_big 
				if(MSE_change < 0){
					beta[g] <- 0
					gamma[g] <- 0
				}
			}
		}
		#################################################
	}

	#draw the intercept for the outcome model
	beta[p+1] <- rnorm(1, mean = (1/n)*t(Xstd[,p+1])%*%(Yorig - Xstd[,-(p+1)]%*%beta[-(p+1)]), 
		sd  = sqrt(sigma.2/n))

	#draw the treatment effect for the outcome model
	beta[p+2] <- rnorm(1, mean = (1/sum(Aorig))*t(Xstd[,p+2])%*%(Yorig - Xstd[,-(p+2)]%*%beta[-(p+2)]), 
		sd  = sqrt(sigma.2/(sum(Aorig))))

	#draw the intercept for the treatment model
	gamma[p+1] <- rnorm(1, mean = (1/n)*t(Xstd[,p+1])%*%(Astar - Xstd[,-c(p+1,p+2)]%*%gamma[-(p+1)]), 
		sd  = sqrt(1/n))
		
	no.non.zero <- sum(ifelse(beta[1:p] == 0, 0, 1))

	#draw sigma.2 
	sigma.2 <- rinvgamma(1, shape = n/2 + (1/2)*no.non.zero + 0.1, 
		scale = (1/2)*(t(Yorig - Xstd%*%beta)%*%(Yorig - Xstd%*%beta) + 
		t(beta[1:p])%*%D.tau.inv%*%beta[1:p]) + 0.1)

	#draw p_0
	pi_0 <- rbeta(1, 1 + p - no.non.zero, 1 + no.non.zero)
	
	#store parameters for this iteration
	beta.sv[[iter]] <- beta
	gamma.sv[[iter]] <- gamma
	sigma.2.sv[[iter]] <- sigma.2
	pi_0.sv[[iter]] <- pi_0	
	}

	#matrix of parameters in outcome model, each row represents one draw from the posterior
	out.mat <-matrix(unlist(beta.sv), nrow = M + burn, ncol = p+2, 
		byrow=TRUE)[(burn+1):(M+burn),]

	#matrix of covariate coefficients in outcome model; each row represents one draw from the posterior
	out.cov.mat <- out.mat[,1:p]
	
	#estimated posterior distribution of intercept in outcome model
	out.intercept.post <- out.mat[,p+1]
	
	#estimated posterior distribution of treatment effect 
	trt.effect.post <- out.mat[,p+2]

	out.cov.mat2 <- ifelse(abs(out.cov.mat) <= 1e-10, 0, 1)

	#this gives covariate inclusion probabilities
	IP <- colMeans(out.cov.mat2)

	#this finds mean of each coefficient in outcome model
	out.cov.means <- colMeans(out.cov.mat)
	
	#this finds mean intercept in outcome model
	out.intercept.mean <- mean(out.intercept.post)
	
	#mean treatment effect
	mean.trt.effect <- mean(out.mat[,p+2])

	#find lower limit of 95% credible interval for the treatment effect
	lower.limit <- quantile(out.mat[,p+2], 0.025)

	#find upper limit of 95% credible interval for the treatment effect
	upper.limit <- quantile(out.mat[,p+2], 0.975)

	#matrix of parameters in treatment model, each row represents one draw from the posterior
	trt.mat <- matrix(unlist(gamma.sv), nrow = M + burn, ncol = p+1, 
		byrow=TRUE)[(burn+1):(M+burn),]

	#matrix of covariate coefficients in treatment model; each row represents one draw from the posterior
	trt.cov.mat <- trt.mat[,1:p]
	
	#estimated posterior distribution of intercept in treatment model
	trt.intercept.post <- trt.mat[,p+1]
	
	#mean intercept in treatment model
	trt.intercept.mean <- mean(trt.intercept.post)
	
	#mean of each covariate coefficient in treatment model
	trt.cov.means <- colMeans(trt.cov.mat)

	return(list(IP=IP, mean.trt.effect = mean.trt.effect, lower.limit=lower.limit, upper.limit=upper.limit,
		 trt.effect.post = trt.effect.post, out.cov.means = out.cov.means, 
		 trt.cov.means = trt.cov.means, out.cov.mat = out.cov.mat, 
		 trt.cov.mat = trt.cov.mat, out.intercept.mean = out.intercept.mean, 
		 trt.intercept.mean = trt.intercept.mean, out.intercept.post = out.intercept.post,
		 trt.intercept.post = trt.intercept.post))
}


SSCE.fit <- mclapply(1:M, function(i) SSCE(Xorig.a[[i]], Yorig.a[[i]], Aorig.a[[i]], M = 5000, burn = 1, Bilevel = FALSE), mc.cores = no.of.cores)

#inclusion probability
SSCE.IP <- lapply(1:M, function(i) SSCE.fit[[i]]$IP)

#average causal effect
SSCE.avg <- lapply(1:M, function(i) SSCE.fit[[i]]$mean.trt.effect)

#lower CL
SSCE.LCL <- lapply(1:M, function(i) SSCE.fit[[i]]$lower.limit)

#upper CL
SSCE.UCL <- lapply(1:M, function(i) SSCE.fit[[i]]$upper.limit)

#avg IP
colMeans(matrix(unlist(SSCE.IP), nrow = M, byrow = TRUE))

#bias
mean(unlist(SSCE.avg)-1)

#sd
sd(unlist(SSCE.avg))

#mse
mean((unlist(SSCE.avg)-1)^2)

#coverage
sum(unlist(lapply(1:M, function(i) ifelse(SSCE.LCL[[i]] < 1 & SSCE.UCL > 1, 1, 0))))/M


##BSSCE

BSSCE.fit <- mclapply(1:M, function(i) SSCE(Xorig.a[[i]], Yorig.a[[i]], Aorig.a[[i]], M = 5000, burn = 1, Bilevel = TRUE), mc.cores = no.of.cores)

#inclusion probability
BSSCE.IP <- lapply(1:M, function(i) BSSCE.fit[[i]]$IP)

#average causal effect
BSSCE.avg <- lapply(1:M, function(i) BSSCE.fit[[i]]$mean.trt.effect)

#lower CL
BSSCE.LCL <- lapply(1:M, function(i) BSSCE.fit[[i]]$lower.limit)

#upper CL
BSSCE.UCL <- lapply(1:M, function(i) BSSCE.fit[[i]]$upper.limit)

#avg IP
colMeans(matrix(unlist(BSSCE.IP), nrow = M, byrow = TRUE))

#bias
mean(unlist(BSSCE.avg)-1)

#sd
sd(unlist(BSSCE.avg))

#mse
mean((unlist(BSSCE.avg)-1)^2)

#coverage
sum(unlist(lapply(1:M, function(i) ifelse(BSSCE.LCL[[i]] < 1 & BSSCE.UCL > 1, 1, 0))))/M


##BAC
library(bacr)

p <- ncol(Xorig.a[[1]])


Z <- lapply(1:M, function(i) as.data.frame(cbind(Yorig.a[[i]], Aorig.a[[i]], Xorig.a[[i]])))
	

result <- mclapply(1:M, function(i) bac(data=Z[[i]], exposure = "V2", outcome="V1", confounders=paste("V", 3:(p+2), sep=""), familyX="binomial", familyY="gaussian", interactors=NULL,num_its=5000,burnM=1, burnB=1,thin=1),mc.cores=no.of.cores)

res <- lapply(1:M, function(i) mean(result[[i]]$ACE))

mean(unlist(res)-1)	
sd(unlist(res)-1)	
mean((unlist(res)-1)^2)	

cov <- lapply(1:M, function(i) ifelse(
	quantile(result[[i]]$ACE, 0.025) < 1 & quantile(result[[i]]$ACE, 0.975) > 1, 1, 0))

sum(unlist(cov))/M

probs <- lapply(1:M, function(i) 
	summary(result[[i]])$PIP
	)
	
Reduce("+", probs)/M

##BSSL

#code to run BSSL
BSSL <- function(Xorig, Yorig, Aorig, M, burn){
	M = M + burn
	tau.g.out.2 <- 1000
	tau.g.trt.2 <- 1000

	beta.sv <- list()
	delta.sv <- list()
	sigma.2.sv <- list()
	p <- ncol(Xorig)
	n <- nrow(Xorig)

	#transform covariates to have mean zero and unit variance
	Xstd <- cbind(t(t(Xorig - matrix(rep(apply(Xorig, 2, mean), nrow(Xorig)), 
		nrow(Xorig),byrow=TRUE))*(1/apply(Xorig, 2, sd))), 1, Aorig)

	#initialize beta
	#beta <- as.numeric(coef(lm(Yorig ~ Xstd - 1)))
	beta <- rep(0, p+2)

	#initialize pi_0
	pi_0 <- 0.5

	#set Sigma.g.out (n and tau.g.out.2 are fixed)
	Sigma.g.out <- 1/(n - 1 + 1/tau.g.out.2)

	#initialize Sigma.g.trt
	#Sigma.g.trt <-  1/(n - 1 + 1/tau.g.trt.2)

	D.tau.inv <-diag(rep(1/tau.g.out.2, p))

	#initialize sigma.2
	sigma.2 <- 1

	#############START GIBBS SAMPLER##################
	for(iter in 1:M){
		for(g in 1:p){
		mu.g.out <-  Sigma.g.out*t(Xstd[,g])%*%(Yorig - Xstd[,-g]%*%beta[-g])
	
		#draw conditional prob. coefficients are zero
		l.g <-  pi_0/(pi_0 + (1 - pi_0)*(tau.g.out.2)^(-1/2)*
				(Sigma.g.out)^(1/2)*exp(
				(1/2)*((1/sigma.2)*(Sigma.g.out^(1/2)*t(Xstd[,g])%*%(Yorig - Xstd[,-g]%*%beta[-g]))^2)))
		
		zero.ind <- rbinom(1, 1, l.g)

		if(zero.ind == 1){
			beta[g] <- 0
		}else{
			beta[g] <- rnorm(n = 1, mean = mu.g.out, 
				sd =  	sqrt(sigma.2*Sigma.g.out))
		}
	
	}

		beta[p+1] <- rnorm(1, mean = (1/n)*t(Xstd[,p+1])%*%(Yorig - Xstd[,-(p+1)]%*%beta[-(p+1)]), 
			sd  = sqrt(sigma.2/n))

		beta[p+2] <- rnorm(1, mean = (1/sum(Aorig))*t(Xstd[,p+2])%*%(Yorig - Xstd[,-(p+2)]%*%beta[-(p	+2)]), 
			sd  = sqrt(sigma.2/(sum(Aorig))))

		
		no.non.zero <- sum(ifelse(beta[1:p] == 0, 0, 1))

		sigma.2 <- rinvgamma(1, shape = n/2 + (1/2)*no.non.zero + 0.1, 
			scale = (1/2)*(t(Yorig - Xstd%*%beta)%*%(Yorig - Xstd%*%beta) + t(beta[1:p])%*%D.tau.inv%*	%beta[1:p]) + 0.1)

		pi_0 <- rbeta(1, 1 + p - no.non.zero, 1 + no.non.zero)
	
		if(iter > burn){
			beta.sv[[iter-burn]] <- beta
		}
		
	}

	mat <-matrix(unlist(beta.sv), nrow = M-burn, ncol = p+2, byrow=TRUE)

	mat2 <- ifelse(abs(mat) <= 1e-10, 0, 1)

	IP <- colMeans(mat2)

	means <- colMeans(mat)

	lower.limit <- quantile(mat[,p+2], 0.025)

	upper.limit <- quantile(mat[,p+2], 0.975)


	return(list(IP=IP, means = means, lower.limit=lower.limit, upper.limit=upper.limit))

}

BSSL.fit <- mclapply(1:M, function(i) BSSL(Xorig.a[[i]], Yorig.a[[i]], Aorig.a[[i]], 5000, 1), mc.cores=no.of.cores)

#inclusion probability
BSSL.IP <- lapply(1:M, function(i) BSSL.fit[[i]]$IP[1:p])

#average causal effect
BSSL.avg <- lapply(1:M, function(i) BSSL.fit[[i]]$means[p+2])

#lower CL
BSSL.LCL <- lapply(1:M, function(i) BSSL.fit[[i]]$lower.limit)

#upper CL
BSSL.UCL <- lapply(1:M, function(i) BSSL.fit[[i]]$upper.limit)

#avg IP
colMeans(matrix(unlist(BSSL.IP), nrow = M, byrow = TRUE))

#bias
mean(unlist(BSSL.avg)-1)

#sd
sd(unlist(BSSL.avg))

#mse
mean((unlist(BSSL.avg)-1)^2)

#coverage
sum(unlist(lapply(1:M, function(i) ifelse(BSSL.LCL[[i]] < 1 & BSSL.UCL > 1, 1, 0))))/M


