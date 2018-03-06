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

####Simulated example####

#set seed
set.seed(111)

#set sample size
n <- 250

#set number of covariates
p <- 10
 
#generate nxp matrix of covariates 
X <- matrix(rnorm(n*p, mean = 1, sd = 1), n, p)
 
#generate linear predictor for the treatment model 
trt.lin <- 0.2*X[,1] - 2*X[,2] + X[,5] - X[,6] + X[,7] - X[,8]
 
#generate probability of receiving treatment
P <- exp(trt.lin)/(1+exp(trt.lin))
 
#generate treatment assignment binary indicators  
A <- rbinom(n, 1, P)
 
#generate linear predictor for the outcome model
out.lin <- 2*X[,1] + 0.2*X[,2] + 5*X[,3] + 5*X[,4]
 
#generate outcomes with treatment effect equal to 1
Y <- A + out.lin + rnorm(n, mean=0, sd=1)
 
##use Spike and Slab Causal Estimator (SSCE)
fit.SSCE <- SSCE(X, Y, A, tau.2 = 1000, M = 5000, burn = 0, Bilevel = FALSE)

##use Bilevel Spike and Slab Causal Estimator (BSSCE)
fit.BSSCE <- SSCE(X, Y, A, tau.2 = 1000, M = 5000, burn = 0, Bilevel = TRUE)

##get mean treatment effect 
fit.SSCE$mean.trt.effect 		
fit.BSSCE$mean.trt.effect

##get a 95% CI lower bound for the treatment effect (2.5% quantile) 
##Note: other CI bounds can be found by working directly with posterior of treatment effect
fit.SSCE$lower.limit
fit.BSSCE$lower.limit

##get a 95% CI lower bound for the treatment effect (97.5% quantile) 
##Note: other CI bounds can be found by working directly with posterior of treatment effect
fit.SSCE$upper.limit
fit.BSSCE$upper.limit

##get histogram for the estimated posterior distribution of the treatment effect
hist(fit.SSCE$trt.effect.post)
hist(fit.BSSCE$trt.effect.post)

##get covariate inclusion probabilities 
fit.SSCE$IP
fit.BSSCE$IP

##get estimated means for each covariate coefficient in the outcome model
fit.SSCE$out.cov.means
fit.BSSCE$out.cov.means

##get estimated means for each covariate coefficient in the treatment model
fit.SSCE$trt.cov.means
fit.BSSCE$trt.cov.means

##get estimated mean outcome model intercept 
fit.SSCE$out.intercept.mean
fit.BSSCE$out.intercept.mean

##get estimated mean treatment model intercept 
fit.SSCE$trt.intercept.mean
fit.BSSCE$trt.intercept.mean

##get histogram for estimated posterior of outcome model intercept 
hist(fit.SSCE$out.intercept.post)
hist(fit.BSSCE$out.intercept.post)

##get histogram for estimated posterior of treatment model intercept 
hist(fit.SSCE$trt.intercept.post)
hist(fit.BSSCE$trt.intercept.post)

##get histogram for estimated posterior of coefficient for 2nd covariate in outcome model 
##Note: posterior distributions for all p covariates are available
hist(fit.SSCE$out.cov.mat[,2])
hist(fit.BSSCE$out.cov.mat[,2])

##get histogram for estimated posterior of coefficient for 5th covariate in treatment model 
##Note: posterior distributions for all p covariates are available
hist(fit.SSCE$trt.cov.mat[,5])
hist(fit.BSSCE$trt.cov.mat[,5])