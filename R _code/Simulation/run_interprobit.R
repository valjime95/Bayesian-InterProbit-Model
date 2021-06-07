library(bayesm)

#source("r_interprobit_truncated/rinterprobit2.r")
source("rinterprobit.r")
data_or = read.table("./Data/sim_data.dat",header=TRUE) #data already simulated

data = data_or
X <- data[,3:4]
y <- data[,2]

#-----------------------------------------------------
# extract data for testing
# data simulated such that beta1=beta2=1.0, sigma2=4.0, rho=0.5
X = as.matrix(X[1:500,])
y = as.matrix(y[1:500])

#-----------------------------------------------------
k <- ncol(X)
n <- nrow(X)
# # Construction of W

iota = array(0,n)+1
W=array(0,dim=c(n,n))
for (i in 2:n-1) {W[i,i+1]=1; W[i,i-1]=1}
W[1,2]=1; W[1,n]=1
W[n,n-1]=1; W[n,1]=1
rsum=W%*%iota
W=W/as.vector(rsum)


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

