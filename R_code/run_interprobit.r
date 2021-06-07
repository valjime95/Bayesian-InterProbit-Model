
source("../interprobit_functions/rinterprobit.R")
# source("../interprobit_functions/rinterprobit_truncated.r")
source("../Functions/function_matrix_W.R") #bayesm already called in this

data_or = read.table("../Data/sim_data.dat",header=TRUE) #data already simulated

data = data_or
X <- data[,3:4]
y <- data[,2]

#-----------------------------------------------------
# extract data for testing
# data simulated such that beta1=beta2=1.0, sigma2=4.0, rho=0.5
X = as.matrix(X[1:500,]) #change the number of observations
y = as.matrix(y[1:500])

#-----------------------------------------------------
k <- ncol(X)
n <- nrow(X)

# Construction of W (circular
W = W_circular(n)

# initial values
burn = 25001
R = 30000

out=rinterprobit(Data=list(y=y,X=X,W=W),
                 Prior=list(betabar=rep(0,ncol(X)),A=diag(rep(.01,ncol(X))),s0=5,q0=10),
                 Mcmc=list(R=R,keep=1))

beta1 = out$betadraw[burn:R,1]
beta2 = out$betadraw[burn:R,2]
sigma = out$sigma2draw[burn:R]
rho = out$rhodraw[burn:R]

plot(out$betadraw[,1])
hist(beta1)
mean(beta1)
median(beta1)
sd(beta1)

plot(out$betadraw[,2])
hist(beta2)
mean(beta2)
median(beta2)
sd(beta2)

plot(out$sigma)
hist(sigma)
mean(sigma)
median(sigma)
sd(sigma)

plot(out$rho)
hist(rho)
mean(rho)
median(rho)
sd(rho)

