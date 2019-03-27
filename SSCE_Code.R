library(msm)
library(mnormt)
library(mgcv)
library(MCMCpack)
library(parcor)

EmpBayes <- function(X, Y, A){

	d <- dim(X)
	
	p <- d[2]
	
	n <- d[1]
	
	eta <- solve(t(cbind(X,1))%*%cbind(X, 1))%*%t(cbind(X, 1))%*%A

	lasso.fit <- lapply(1:p, function(i) adalasso(X[,-i], X[,i], k = 10, intercept = FALSE, both = FALSE))

	lasso.coef <- lapply(1:p, function(i) lasso.fit[[i]]$coefficients.lasso)
	
	sigma2.x_xj <- unlist(lapply(1:p, function(i) 
		(t(X[,i] - X[,-i]%*%lasso.coef[[i]])%*%(X[,i] - X[,-i]%*%lasso.coef[[i]]))/(n-sum(ifelse(lasso.coef[[i]]==0, 0, 1))) )
		)
		
	lasso.fit <- lapply(1:p, function(i) adalasso(cbind(X[,-i], A), X[,i], k = 10, intercept = TRUE, both = FALSE))

	lasso.coef <- lapply(1:p, function(i) lasso.fit[[i]]$coefficients.lasso)
	
	sigma2.x_xj.A <- unlist(lapply(1:p, function(i) 
		(t(X[,i] - cbind(X[,-i], A)%*%lasso.coef[[i]] - lasso.fit[[i]]$intercept.lasso)%*%
			(X[,i] - cbind(X[,-i], A)%*%lasso.coef[[i]] - lasso.fit[[i]]$intercept.lasso))/
				(n-1-sum(ifelse(lasso.coef[[i]]==0, 0, 1))) )
		)
		
	lasso.fit <- adalasso(cbind(X, A), Y, k = 10, intercept = TRUE, both = FALSE)
	
	nonzero.coef <- which(lasso.fit$coefficients.lasso != 0)

	temp <- (t(Y - cbind(X, A)%*%lasso.fit$coefficients.lasso - lasso.fit$intercept.lasso)%*%
			(Y - cbind(X, A)%*%lasso.fit$coefficients.lasso - lasso.fit$intercept.lasso))/
				(n-1-sum(ifelse(lasso.fit$coefficients.lasso==0, 0, 1))) 

	sigma2.y_x.A  <- rep(temp, p+1)
	
	lasso.fit <- lapply(1:length(nonzero.coef), function(i) 
		adalasso(cbind(X[,-nonzero.coef[i]], A), Y, k = 10, intercept = TRUE, both = FALSE))


	temp <- unlist(lapply(1:length(nonzero.coef), function(i) 
		(t(Y - cbind(X[,-nonzero.coef[i]], A)%*%lasso.fit[[i]]$coefficients.lasso - lasso.fit[[i]]$intercept.lasso)%*%
			(Y - cbind(X[,-nonzero.coef[i]], A)%*%lasso.fit[[i]]$coefficients.lasso - lasso.fit[[i]]$intercept.lasso))/
				(n-1-sum(ifelse(lasso.fit[[i]]$coefficients.lasso==0, 0, 1)))
	))
	
	sigma2.y_x.A[nonzero.coef] <- temp
	
	lasso.fit <- adalasso(X, A, k = 10, intercept = TRUE, both = FALSE)
	
	nonzero.coef <- which(lasso.fit$coefficients.lasso != 0)

	temp <- (t(A - X%*%lasso.fit$coefficients.lasso - lasso.fit$intercept.lasso)%*%
			(A - X%*%lasso.fit$coefficients.lasso - lasso.fit$intercept.lasso))/
				(n-1-sum(ifelse(lasso.fit$coefficients.lasso==0, 0, 1))) 

	sigma2.A_x  <- rep(temp, p)
	
	lasso.fit <- lapply(1:length(nonzero.coef), function(i) 
		adalasso(X[,-nonzero.coef[i]], A, k = 10, intercept = TRUE, both = FALSE))

	temp <- unlist(lapply(1:length(nonzero.coef), function(i) 
		(t(A - X[,-nonzero.coef[i]]%*%lasso.fit[[i]]$coefficients.lasso - lasso.fit[[i]]$intercept.lasso)%*%
			(A - X[,-nonzero.coef[i]]%*%lasso.fit[[i]]$coefficients.lasso - lasso.fit[[i]]$intercept.lasso))/
				(n-1-sum(ifelse(lasso.fit[[i]]$coefficients.lasso==0, 0, 1)))
	))
	
	sigma2.A_x[nonzero.coef] <- temp
	
	return(list(eta, sigma2.x_xj, sigma2.x_xj.A, sigma2.y_x.A, sigma2.A_x))
}




BSSCE_EM_lambda2 = function(Y, X, eta, sigma.2.z_x, sigma.2.z_a.x, sigma.2.y_a.x, sigma.2.a_x, Bilevel=TRUE, num_update = 100, niter = 100, group_size, sigma2,a=1, b=1,
                           verbose = FALSE, delta=0.001, alpha=1e-1,
                           gamma=1e-1, pi_prior=TRUE, pi=0.5,option.update="global",option.weight.group=FALSE)
{
  ####################################
  # Create and Initialize parameters #
  ####################################
  n = length(Y)
  p = dim(X)[2]
  ngroup = length(group_size)
  # initialize parameters
  tau2 = rep(1, ngroup)
  sigma2 = 4
  lambda2 = 1
  matlambda2 = rep(1,ngroup)
  lambda2_path = rep(-1, num_update)
  matlambda2_path = matrix(-1,ncol=ngroup,nrow=num_update)
  l = rep(0, ngroup)
  beta = vector(mode='list', length=ngroup)
  for(i in 1:ngroup) beta[[i]]=rep(0, group_size[i])
  Z = rep(0, ngroup)
  
  trt.int <- 0
  out.int <- 0
  trt.eff <- 0

  ###############################
  # avoid duplicate computation #
  ###############################

	A <- Y[(n/2+1):n]
    
    Astar <- A
    
    N1 <- sum(ifelse(Astar==0,0,1))
    N0 <- length(A) - N1
    
    mu_a <- X[(n/2+1):n,seq(from=2,to=p,by=2)] %*% unlist(beta)[seq(from=2,to=p,by=2)]
    
  	# Draw latent variable z from its full conditional: z | \theta, y, X
  	Astar[A == 0] <- rtnorm(N0, mean = mu_a[A == 0] + trt.int, sd = 1, lower = -Inf, upper = 0)
  	Astar[A != 0] <- rtnorm(N1, mean = mu_a[A != 0] + trt.int, sd = 1, lower = 0, upper = Inf)
  
  	Y[(n/2+1):n] <- Astar
    
   # XtX = t(X) %*% X
    XktY = vector(mode = "list", length = ngroup)
    XktXk = vector(mode = "list", length = ngroup)
    XktXmk = vector(mode = "list", length = ngroup)
    begin_idx = 1
    for (i in 1:ngroup) {
        end_idx = begin_idx + group_size[i] - 1
        Xk = X[, begin_idx:end_idx]
        XktXk[[i]] = t(Xk) %*% Xk
        XktXmk[[i]] = t(Xk) %*% cbind(X[, -(begin_idx:end_idx)], 
        	c(rep(1,n/2),rep(0,n/2)), c(A,rep(0,n/2)),
        	c(rep(0,n/2),rep(1,n/2)))
        	
        begin_idx = end_idx + 1
    }




  #####################
  # The Gibbs Sampler #
  #####################

  for (update in 1:num_update) {
    # print(c("updadte=",update))
    coef = array(0, dim=c(p, niter))
    tau2_each_update = array(0, dim=c(ngroup, niter))



    for (iter in 1:niter)    {
    	
    	mu_a <- X[(n/2+1):n,seq(from=2,to=p,by=2)] %*% unlist(beta)[seq(from=2,to=p,by=2)]
    
  	# Draw latent variable z from its full conditional: z | \theta, y, X
  	Astar[A == 0] <- rtnorm(N0, mean = mu_a[A == 0] + trt.int, sd = 1, lower = -Inf, upper = 0)
  	Astar[A != 0] <- rtnorm(N1, mean = mu_a[A != 0] + trt.int, sd = 1, lower = 0, upper = Inf)
  
  	Y[(n/2+1):n] <- Astar
    YtY = t(Y) %*% Y
   # XtY = t(X) %*% Y
    
     begin_idx = 1
    for (i in 1:ngroup) {
        end_idx = begin_idx + group_size[i] - 1
        Xk = X[, begin_idx:end_idx]
        XktY[[i]] = t(Xk) %*% Y
        begin_idx = end_idx + 1
    }

      # print the current number of iteration
      if (verbose == TRUE) {print(iter)}

      # Update beta's
      for(i in 1:ngroup)
      {
        bmk = c()
        for(j in 1:ngroup)
        {
          if(j!=i) bmk = c(bmk, beta[[j]])
        }
        
        bmk[p-1] <- out.int
        bmk[p] <- trt.eff
        bmk[p+1] <- trt.int

        f1 = XktY[[i]] - XktXmk[[i]] %*% bmk
        f2 = XktXk[[i]]+1/tau2[i]*diag(nrow=group_size[i])
        f2_inverse = solve(f2)
        mu = f2_inverse %*% f1

		Sigma = 1/(n/2 - 1 + 1/tau2[i])
		
		mean.betaj <- mu[1]
		var.betaj <- sigma2*f2_inverse[1,1]
		
		
			mse_change_fncn <- function(b1){
			var_small <- sigma.2.y_a.x[i]/sigma.2.a_x[i]*1/(n/2)

			var_big <- (sigma.2.y_a.x[i] - sigma.2.z_a.x[i]*b1^2)/
			    (sigma.2.a_x[i] - sigma.2.z_x[i]*(eta[i])^2)/(n/2)
   
			bias <- (b1*eta[i]*sigma.2.z_x[i])/sigma.2.a_x[i]

			MSE_change <- bias^2 + var_small - var_big
			ifelse(MSE_change < 0, 0, 1)
			}

			t<-apply(X=matrix(seq(-2, 0, by=.001),nrow=2001,ncol=1), MARGIN=2,FUN=mse_change_fncn)

			if(length(which(t==0)) > 0){
			  	lower.lim <- seq(-2, 0, by=.001)[which(t == 0)[1]]
			  	upper.lim <- -1*lower.lim
				int1 <- pnorm(q = lower.lim, mean = mean.betaj, sd = sqrt(var.betaj))
				int2 <- pnorm(q = upper.lim, mean = mean.betaj, sd = sqrt(var.betaj))				
				
				area <- int2 - int1
			}else{
				area <- 0
			} 	
		
		newpiece <- 1 - area
			
        l[i] = pi/(pi + (1 - pi)*newpiece*(tau2[i]*tau2[i])^(-1/2)*
			(Sigma*Sigma)^(1/2)*exp(
			(1/2)*((1/sigma2)*(Sigma^(1/2)*t(X[1:(n/2),(2*i-1)])%*%(Y[1:(n/2)] - cbind(X[1:(n/2),seq(1,p,2)[-i]],1,A)%*%c(unlist(beta)[seq(1,p,2)][-i],out.int,trt.eff)))^2 + 		
    		(Sigma^(1/2)*t(X[1:(n/2),(2*i-1)])%*%(Astar - cbind(X[(n/2+1):n,seq(2,p,2)[-i]],1)%*%c(unlist(beta)[seq(2,p,2)][-i], trt.int)))^2)))
    		
    	        

        if(runif(1)<l[i])
        {
          beta[[i]] = rep(0, group_size[i])
          Z[i] = 0
        }else{

          beta[[i]] = rmnorm(1, mean=mu, varcov=c(sigma2,1)*f2_inverse)
          
                  
          Z[i] = 1
        }

		###############################################


			##For each non-zero coefficient according to SSCE, we find the change in MSE if
			##this coefficient was set to zero; we then set the coefficients to zero if doing so
			##improves MSE of the treatment effect estimator  
			if(beta[[i]][1] != 0){
			
				var_small <- sigma.2.y_a.x[i]/sigma.2.a_x[i]*1/(n/2)
				
    			var_big <- (sigma.2.y_a.x[i] - sigma.2.z_a.x[i]*beta[[i]][1]^2)/
    		 	 	(sigma.2.a_x[i] - sigma.2.z_x[i]*(eta[i]/1)^2)/(n/2)
    			 	 	
    			bias <- (beta[[i]][1]*(eta[i]/1)*sigma.2.z_x[i])/sigma.2.a_x[i] 
    			
    			MSE_change <- bias^2 + var_small - var_big 
    			
				if(MSE_change < 0){
					beta[[i]][1] <- 0
					beta[[i]][2] <- 0
					Z[i] = 0
				}

		}
		#################################################


      }
            
          #draw the intercept for the outcome model
		out.int <- rnorm(1, mean = (1/(n/2))*t(rep(1,n/2))%*%(Y[1:(n/2)] - cbind(X[1:(n/2),seq(1,p,2)],A)%*%c(unlist(beta)[seq(1,p,2)],trt.eff)), 
		sd  = sqrt(sigma2/(n/2)))


		trt.eff <- rnorm(1, mean = (1/sum(A))*t(A)%*%(Y[1:(n/2)] - cbind(X[1:(n/2),seq(1,p,2)],rep(1,n/2))%*%c(unlist(beta)[seq(1,p,2)], out.int)), 
		sd  = sqrt(sigma2/(sum(A))))
		
		
		#draw the intercept for the treatment model
		trt.int <- rnorm(1, mean = (1/(n/2))*t(rep(1,n/2))%*%(Y[(n/2+1):n] - 
		cbind(X[(n/2+1):n,seq(2,p,2)])%*%c(unlist(beta)[seq(2,p,2)])), 
		sd  = sqrt(1/(n/2)))

           
   
      # Update tau2's
      if (option.weight.group== FALSE){
        for(i in 1:ngroup)
        {
          if(Z[i]==0){tau2[i] = rgamma(1, shape=(group_size[i]+1)/2, rate=matlambda2[i]/2)}
          else{
          	tau2[i] = 1/rig(1, mean = sqrt(matlambda2[i]*
          		sigma2/(beta[[i]][1]^2 + sigma2*beta[[i]][2]^2)), scale = 1/(matlambda2[i]))          	}
        }
      }else{
        for(i in 1:ngroup)
        {
          if(Z[i]==0){tau2[i] = rgamma(1, shape=(group_size[i]+1)/2, rate=matlambda2[i]*(group_size[i])/2)}
          else{
          	tau2[i] = 1/rig(1, mean = sqrt(matlambda2[i] * 
                      group_size[i] * sigma2/(beta[[i]][1]^2 + sigma2*beta[[i]][2]^2)), 
                      scale = 1/(group_size[i] * matlambda2[i]))
          	
          	}

        }
      }
      tau2_each_update[,iter] = tau2

      # Update sigma2
      s=0
      for(i in 1:ngroup)
      {
        s = s + sum(beta[[i]][1]^2)/tau2[i]
      }
      beta_vec = c()
      for(j in 1:ngroup) beta_vec = c(beta_vec, beta[[j]])
      coef[,iter] = beta_vec
     
		sigma2 = rinvgamma(1, shape = n/4 + sum(Z)/2 + alpha, scale = (t(Y[1:(n/2)])%*%Y[1:(n/2)] - 2 * t(c(beta_vec[seq(1,p,2)], out.int, trt.eff)) %*% t(cbind(X[1:(n/2),seq(1,p,2)], 1, A))%*%Y[1:(n/2)] + t(c(beta_vec[seq(1,p,2)], out.int, trt.eff)) %*% 
            t(cbind(X[1:(n/2),seq(1,p,2)], 1, A))%*%cbind(X[1:(n/2),seq(1,p,2)], 1, A) %*% c(beta_vec[seq(1,p,2)], out.int, trt.eff) + s)/2 + gamma)
            
	     
      # Update pi
      if(pi_prior==TRUE){
        pi = rbeta(1, shape1=a+ngroup-sum(Z), shape2=b+sum(Z))
	}

    }


    # Update lambda
    tau2_mean = apply(tau2_each_update, 1, mean)

    matlambda2 = (group_size+1)/(tau2_mean*group_size)

    lambda2 = (p + ngroup) / sum(tau2_mean*group_size)


    if(option.update=="global") matlambda2 <- rep(lambda2,ngroup)
    if(option.weight.group==FALSE) matlambda2 <- rep((p + ngroup) / sum(tau2_mean),ngroup)
    matlambda2_path[update,] = matlambda2



  }

  # output the posterior mean and median as our estimator
  pos_mean = apply(coef, 1, mean)
  pos_median = apply(coef, 1, median)
  list(pos_mean = pos_mean, pos_median = pos_median, coef = coef, lambda2_path = matlambda2_path)
}


BSSCE <- function (Y, X) 
{
	
	n = length(Y)
	p = dim(X)[2]
	
	E.bayes <- EmpBayes(X[1:(n/2), seq(1, p, 2)], Y[1:(n/2)], Y[(n/2+1):n])
	
	eta <- as.numeric(t(E.bayes[[1]]))
	sigma.2.z_x <- E.bayes[[2]]
	sigma.2.z_a.x <- E.bayes[[3]]
	sigma.2.y_a.x <- E.bayes[[4]]
	sigma.2.a_x <- E.bayes[[5]]
	
	Bilevel=TRUE
	niter = 5100
 	burnin = 100 
	group_size = rep(2, p/2) 
	sigma2=1
	a = 1 
   	b = 1 
	num_update = 100 
	niter.update = 100 
	verbose = FALSE 
    alpha = 0.1
	gamma = 0.1
	pi_prior = TRUE 
	pi = 0.5
	update_tau = TRUE 
    option.weight.group = FALSE
	option.update = "global"
 	lambda2_update = NULL
   	ngroup = length(group_size)
    tau2 = rep(1, ngroup)
    sigma2 = 4
    l = rep(0, ngroup)
    beta = vector(mode = "list", length = ngroup)
    for (i in 1:ngroup) beta[[i]] = rep(0, group_size[i])
    Z = rep(0, ngroup)
        
    trt.int=0
    out.int=0
    trt.eff=0
    


	A <- Y[(n/2+1):n]
  
    
    if (update_tau == TRUE){
        fit_for_lambda2 = BSSCE_EM_lambda2(Y, X, eta, sigma.2.z_x, sigma.2.z_a.x, sigma.2.y_a.x,
        sigma.2.a_x, num_update = num_update, 
            niter = niter.update, group_size = group_size, sigma2=sigma2,option.update = option.update, 
            option.weight.group = option.weight.group)
           
            
        lambda2 = apply(fit_for_lambda2$lambda2_path, 2, tail, 1)
        
    }else{
        lambda2 <- lambda2_update
    }
    
    
    Astar <- A
    
    N1 <- sum(ifelse(Astar==0,0,1))
    N0 <- length(A) - N1
    
    mu_a <- X[(n/2+1):n,seq(from=2,to=p,by=2)] %*% unlist(beta)[seq(from=2,to=p,by=2)]
    
  	Astar[A == 0] <- rtnorm(N0, mean = mu_a[A == 0] + trt.int, sd = 1, lower = -Inf, upper = 0)
  	Astar[A != 0] <- rtnorm(N1, mean = mu_a[A != 0] + trt.int, sd = 1, lower = 0, upper = Inf)
  
  	Y[(n/2+1):n] <- Astar
    
    #XtX = t(X) %*% X
    XktY = vector(mode = "list", length = ngroup)
    XktXk = vector(mode = "list", length = ngroup)
    XktXmk = vector(mode = "list", length = ngroup)
    begin_idx = 1
    for (i in 1:ngroup) {
        end_idx = begin_idx + group_size[i] - 1
        Xk = X[, begin_idx:end_idx]
        XktXk[[i]] = t(Xk) %*% Xk
		XktXmk[[i]] = t(Xk) %*% cbind(X[, -(begin_idx:end_idx)], 
        	c(rep(1,n/2),rep(0,n/2)), c(A,rep(0,n/2)),
        	c(rep(0,n/2),rep(1,n/2)))
        	
        begin_idx = end_idx + 1
    }
    coef = array(0, dim = c(p+3, niter - burnin))
    coef_tau = array(0, dim = c(ngroup, niter))



    for (iter in 1:niter) {
    	
    	mu_a <- X[(n/2+1):n,seq(from=2,to=p,by=2)] %*% unlist(beta)[seq(from=2,to=p,by=2)]
    
  	# Draw latent variable z from its full conditional: z | \theta, y, X
  	Astar[A == 0] <- rtnorm(N0, mean = mu_a[A == 0] + trt.int, sd = 1, lower = -Inf, upper = 0)
  	Astar[A != 0] <- rtnorm(N1, mean = mu_a[A != 0] + trt.int, sd = 1, lower = 0, upper = Inf)
  
  	Y[(n/2+1):n] <- Astar
    	YtY = t(Y) %*% Y
    #XtY = t(X) %*% Y
    
     begin_idx = 1
    for (i in 1:ngroup) {
        end_idx = begin_idx + group_size[i] - 1
        Xk = X[, begin_idx:end_idx]
        XktY[[i]] = t(Xk) %*% Y
        begin_idx = end_idx + 1
    }
    
        if (verbose == TRUE) {
            print(iter)
        }
       # Update beta's
      for(i in 1:ngroup)
      {
        bmk = c()
        for(j in 1:ngroup)
        {
          if(j!=i) bmk = c(bmk, beta[[j]])
        }
        
        bmk[p-1] <- out.int
        bmk[p] <- trt.eff
        bmk[p+1] <- trt.int

        f1 = XktY[[i]] - XktXmk[[i]] %*% bmk
        f2 = XktXk[[i]]+1/tau2[i]*diag(nrow=group_size[i])
        f2_inverse = solve(f2)
        mu = f2_inverse %*% f1

		Sigma = 1/(n/2 - 1 + 1/tau2[i])
		
		mean.betaj <- mu[1]
		var.betaj <- sigma2*f2_inverse[1,1]
		
		
			mse_change_fncn <- function(b1){
			var_small <- sigma.2.y_a.x[i]/sigma.2.a_x[i]*1/(n/2)

			var_big <- (sigma.2.y_a.x[i] - sigma.2.z_a.x[i]*b1^2)/
			    (sigma.2.a_x[i] - sigma.2.z_x[i]*(eta[i])^2)/(n/2)
   
			bias <- (b1*eta[i]*sigma.2.z_x[i])/sigma.2.a_x[i]

			MSE_change <- bias^2 + var_small - var_big
			ifelse(MSE_change < 0, 0, 1)
			}

			t<-apply(X=matrix(seq(-2, 0, by=.001),nrow=2001,ncol=1), MARGIN=2,FUN=mse_change_fncn)

			if(length(which(t==0)) > 0){
			  	lower.lim <- seq(-2, 0, by=.001)[which(t == 0)[1]]
			  	upper.lim <- -1*lower.lim
				int1 <- pnorm(q = lower.lim, mean = mean.betaj, sd = sqrt(var.betaj))
				int2 <- pnorm(q = upper.lim, mean = mean.betaj, sd = sqrt(var.betaj))				
				
				area <- int2 - int1
			}else{
				area <- 0
			} 	
		
		newpiece <- 1 - area
			
        l[i] = pi/(pi + (1 - pi)*newpiece*(tau2[i]*tau2[i])^(-1/2)*
			(Sigma*Sigma)^(1/2)*exp(
			(1/2)*((1/sigma2)*(Sigma^(1/2)*t(X[1:(n/2),(2*i-1)])%*%(Y[1:(n/2)] - cbind(X[1:(n/2),seq(1,p,2)[-i]],1,A)%*%c(unlist(beta)[seq(1,p,2)][-i],out.int,trt.eff)))^2 + 		
    		(Sigma^(1/2)*t(X[1:(n/2),(2*i-1)])%*%(Astar - cbind(X[(n/2+1):n,seq(2,p,2)[-i]],1)%*%c(unlist(beta)[seq(2,p,2)][-i], trt.int)))^2)))
    		
    	
        if(runif(1)<l[i])
        {
          beta[[i]] = rep(0, group_size[i])
          Z[i] = 0
        }else{

          beta[[i]] = rmnorm(1, mean=mu, varcov=c(sigma2,1)*f2_inverse)
          
                  
          Z[i] = 1
        }

		###############################################


			##For each non-zero coefficient according to SSCE, we find the change in MSE if
			##this coefficient was set to zero; we then set the coefficients to zero if doing so
			##improves MSE of the treatment effect estimator  
			if(beta[[i]][1] != 0){
			
				var_small <- sigma.2.y_a.x[i]/sigma.2.a_x[i]*1/(n/2)
				
    			var_big <- (sigma.2.y_a.x[i] - sigma.2.z_a.x[i]*beta[[i]][1]^2)/
    		 	 	(sigma.2.a_x[i] - sigma.2.z_x[i]*(eta[i]/1)^2)/(n/2)
    			 	 	
    			bias <- (beta[[i]][1]*(eta[i]/1)*sigma.2.z_x[i])/sigma.2.a_x[i] 
    			
    			MSE_change <- bias^2 + var_small - var_big 
    			
				if(MSE_change < 0){
					beta[[i]][1] <- 0
					beta[[i]][2] <- 0
					Z[i] = 0
				}

		}
		#################################################


      }
        
        #draw the intercept for the outcome model
		out.int <- rnorm(1, mean = (1/(n/2))*t(rep(1,n/2))%*%(Y[1:(n/2)] - cbind(X[1:(n/2),seq(1,p,2)],A)%*%c(unlist(beta)[seq(1,p,2)],trt.eff)), 
		sd  = sqrt(sigma2/(n/2)))


		trt.eff <- rnorm(1, mean = (1/sum(A))*t(A)%*%(Y[1:(n/2)] - cbind(X[1:(n/2),seq(1,p,2)],rep(1,n/2))%*%c(unlist(beta)[seq(1,p,2)], out.int)), 
		sd  = sqrt(sigma2/(sum(A))))
		
		#draw the intercept for the treatment model
		trt.int <- rnorm(1, mean = (1/(n/2))*t(rep(1,n/2))%*%(Y[(n/2+1):n] - 
		cbind(X[(n/2+1):n,seq(2,p,2)])%*%c(unlist(beta)[seq(2,p,2)])), 
		sd  = sqrt(1/(n/2)))

        
        
        if (update_tau) {
            if (option.weight.group == FALSE) {
                for (i in 1:ngroup) {
                  if (Z[i] == 0) {
                    tau2[i] = rgamma(1, shape = (group_size[i] + 
                      1)/2, rate = lambda2[i]/2)
                  }
                  else {
                    tau2[i] = 1/rig(1, mean = sqrt(lambda2[i]*
          		sigma2/(beta[[i]][1]^2 + sigma2*beta[[i]][2]^2)), scale = 1/(lambda2[i]))
                      
                  }
                }
            }
            else {
                for (i in 1:ngroup) {
                  if (Z[i] == 0) {
                    tau2[i] = rgamma(1, shape = (group_size[i] + 
                      1)/2, rate = lambda2[i] * (group_size[i])/2)
                  }
                  else {
                    tau2[i] = 1/rig(1, mean = sqrt(lambda2[i] * 
                      group_size[i] * sigma2/(beta[[i]][1]^2 + sigma2*beta[[i]][2]^2)), 
                      scale = 1/(group_size[i] * lambda2[i]))
                     
                  }
                  coef_tau[i, iter] = tau2[i]
                }
            }
        }
        s = 0
        for (i in 1:ngroup) {
            s = s + sum(beta[[i]][1]^2)/tau2[i]
        }
        beta_vec = c()
        for (j in 1:ngroup) beta_vec = c(beta_vec, beta[[j]])
        if (iter > burnin) 
            coef[, iter - burnin] = c(beta_vec, out.int,trt.eff,trt.int)
           
		sigma2 = rinvgamma(1, shape = n/4 + sum(Z)/2 + alpha, scale = (t(Y[1:(n/2)])%*%Y[1:(n/2)] - 2 * t(c(beta_vec[seq(1,p,2)], out.int, trt.eff)) %*% t(cbind(X[1:(n/2),seq(1,p,2)], 1, A))%*%Y[1:(n/2)] + t(c(beta_vec[seq(1,p,2)], out.int, trt.eff)) %*% 
            t(cbind(X[1:(n/2),seq(1,p,2)], 1, A))%*%cbind(X[1:(n/2),seq(1,p,2)], 1, A) %*% c(beta_vec[seq(1,p,2)], out.int, trt.eff) + s)/2 + gamma)
            
   
        if (pi_prior == TRUE) 
                pi = rbeta(1, shape1=a+ngroup-sum(Z), shape2=b+sum(Z))

    }
    pos_mean = apply(coef, 1, mean)
    pos_median = apply(coef, 1, median)
    list(pos_mean = pos_mean, pos_median = pos_median, coef = coef)
}
