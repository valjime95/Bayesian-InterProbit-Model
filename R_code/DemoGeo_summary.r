# library(xtable) # tables to Latex

source("../Functions/function_matrix_W.R")
source("../Functions/functions_model.R")
source("../interprobit_functions/rinterprobit_demogeo.R")


# Data prep
data <- read.table("../Data/Interdependent.dat",header=TRUE)
data <- data[1:666,]
d <- norm_var(data)
dummy <- dummy_col(data)

name <- append(names,group_names(dummy))

X <- cbind(d$data[,5:10],d$lat,d$long,dummy)
y <- data$y
n <- nrow(X)
intercept <- array(0,n)+1
X <- cbind(intercept,X)
X <- as.matrix(X)
y <- as.matrix(y)

# W matrix
W <- W_matrix('zip_code',data,normalizada = TRUE, lambda = TRUE)$W

# MODEL
Data <- list(y=y,X=X,W=W) 
Prior <- list(betabar=rep(0,ncol(X)),A=diag(rep(.01,ncol(X))),s0=5,q0=10) #*
Mcmc <- list(R=20000,keep=1)
out_zip <- rinterprobit(Data=Data,Prior=Prior,Mcmc=Mcmc)

# Model Verification
out_burn = burn_data(out_zip,'zip',dummy,R=20000,burn=19001,name=name)

ls_param = c('theta','rho','sigma2','llike')

# PARAMETERS DATA FRAME
mean_params = func_outcomes2(ls_md = list(out_burn),ls_par = ls_param,func=mean,mod_names=c('zip'))
mean_beta = func_outcomes2(ls_md = list(out_burn),ls_par = c('beta'),func=mean,mod_names=c('zip'))
df = round(rbind(mean_beta,mean_params),6)