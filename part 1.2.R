trueA <- 5       #define a variable "trueA" equal to 5
trueB <- 0       #define a variable "trueB" equal to 0
trueSd <- 10     #define a variable "trueSd" equal to 10
sampleSize <- 31 #make the sample size equal to 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)   #make the values of x equal from -15 to 15(e.g. -15,-14,-13...,13,14,15), there are totally 31 values of x, whihc equals to the sample size.

# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)  #make a linear relationship between the independent variable x and dependent variable y. So we take a linear model and add some noise.

plot(x,y,main="Test Data")  ##plot a graph which y corresponding to x.

likelihood <- function(param){   #create a function called likelihoood with vector 'param' as parameter
  a = param[1]     # define a equal to the 1st value of vector param
  b = param[2]     # define b equal to the 2nd value of vector param
  sd = param[3]    # define sd equal to the 3rd value of vector param
  
  pred = a*x + b  #create dependent value 'pred' according to ax + b 
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)  #look up the probability densities (using dnorm) for y, with 'pred' as mean and sd as standard deviation
  sumll = sum(singlelikelihoods)  #sum all of the 31 values, calculate the probability
  
  return(sumll)   #return the sumll
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}  #create a function "slopevalues" with parameter x; return likelihood(take vector (x,trueB,trueSd) as parameter)
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )   # create a sequence from 3 to 7(flit by 0.5) to the function 'slopevalue', can get 81 numbers
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")  #plot the Likelihood for a range of parameter values of the slope parameter(value from 3 to 7 as x axis and return value as y axis)


# Prior distribution
prior <- function(param){  #create a function called prior with vector 'param' as parameter
  a = param[1]     # define a equal to the 1st value of vector param
  b = param[2]     # define b equal to the 2nd value of vector param
  sd = param[3]    # define sd equal to the 3rd value of vector param
  aprior = dunif(a, min=0, max=10, log = T)  # make a prior (uniform) distribution of a
  bprior = dnorm(b, sd = 5, log = T) # make a prior (normal) distribution of b
  sdprior = dunif(sd, min=0, max=30, log = T) # make a prior (uniform) distribution of sd
  return(aprior+bprior+sdprior)  #retunr the sum of aprior, bprior and sdprior to get the prior distribution for all, which is a log value.
  
}


posterior <- function(param){  #create a function called posterior with vector 'param' as parameter
  return (likelihood(param) + prior(param))   #retunr the sum of likelihood and prior to get the actual quantity the MCMC, which is a log value.
  
}

proposalfunction <- function(param){  #create a function called proposalfunction with vector 'param' as parameter
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
  #retunr the random normal distributionof 3 values, with mean = param and sd = (0.1,0.5,0.3).
}

run_metropolis_MCMC <- function(startvalue, iterations){  #create a function called run_metropolis_MCMC with 'startvalue' and 'iterations' as parameters
  chain = array(dim = c(iterations+1,3))  # make chain be an array with (iterations+1) rows and 3 columns
  chain[1,] = startvalue   # make the first row of chain equal to startvalue
  
  for (i in 1:iterations){  # repeat iterations times
    proposal = proposalfunction(chain[i,])  # make a vector 'proposal' equals to vector in chain's row i, following the proposalfunction 
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))  # let the probability of jumping to new vector equal to the e^(posterior(proposal) - posterior(chain[i,]))
    
    if (runif(1) < probab){  # decide whether to jump by compare with a random value from 0 to 1
      chain[i+1,] = proposal  # let chain's (i+1) row equal to the proposal
    }else{  #if  runif(1) >= probab, do not jump
      chain[i+1,] = chain[i,]  # let chain's (i+1) row equal to rom i
    }
  } 
  return(chain)  #return chain
}

startvalue = c(4,0,10)  #let startvalue be a vector equal to (4,0,10)
chain = run_metropolis_MCMC(startvalue, 10000)  #let chain equal to the function run_metropolis_MCMC with parameters: startvalue = startvalue and iterations = 10000

burnIn = 5000  #make burnIn equal to 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))  #calculate acceptance starting from 5001 row


par(mfrow = c(2,3))  #display the graph in 2X3(2 rows with 3 graphs in each)
hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line" )  # plot a histogram of posterior of a(number in chain's 1st column) from chain's 5001 row
abline(v = mean(chain[-(1:burnIn),1]))  #add a line to show the mean of a from 5001 row
abline(v = trueA, col="red" )  #add a line to show the value of trueA

hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")  # plot a histogram of posterior of b(number in chain's 2nd column) from chain's 5001 row
abline(v = mean(chain[-(1:burnIn),2]))  #add a line to show the mean of b from 5001 row
abline(v = trueB, col="red" )  #add a line to show the value of trueB

hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")  # plot a histogram of posterior of sd(number in chain's 3rd column) from chain's 5001 row
abline(v = mean(chain[-(1:burnIn),3]) )  #add a line to show the mean of sd from 5001 row
abline(v = trueSd, col="red" )   #add a line to show the value of trueSd

plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )  # plot chain value of a from 5001 row
abline(h = trueA, col="red" )  #add a horizontal line = trueA
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )  # plot chain value of b from 5001 row
abline(h = trueB, col="red" )  #add a horizontal line = trueB
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )  # plot chain value of sd from 5001 row
abline(h = trueSd, col="red" )  #add a horizontal line = trueSd

# for comparison:
summary(lm(y~x))  # summary the linear relation between x and y
