setwd("/Users/valeriajimeno/Desktop/Tesis_R") #*
library(xtable) # tables to Latex
source('functions_model.R')
source('rinterprobit_demogeo.R')

# Data prep
data=read.table("Interdependent.dat",header=TRUE)
data <- data[1:666,]
d <-norm_var(data)
dummy = dummy_col(data)

name <- append(names,group_names(dummy))

X=cbind(d$data[,5:10],d$lat,d$long,dummy)
y=data$y
n=nrow(X)
iota=array(0,n)+1
X=cbind(iota,X)
X=as.matrix(X)
y=as.matrix(y)

# W matrix
load("/Users/valeriajimeno/Desktop/Tesis_R/W_matrix_666.RData")
oldtau = abs(rnorm(1,0,.5)) # prior τ>0
oldphi = exp(oldtau)/(exp(oldtau)+1) # φ
W =(oldphi*W_zip)+((1-oldphi)*W_dem)

# MODEL
Data=list(y=y,X=X,W=W) #*
Prior=list(betabar=rep(0,ncol(X)),A=diag(rep(.001,ncol(X))),
           s0=5,q0=100,
           oldtau=oldtau,oldphi=oldphi) #*
Mcmc=list(R=200,keep=1)
out=rinterprobit_demogeo(Type='demo_geo',Data=Data,Prior=Prior,Mcmc=Mcmc)
# save(out_dist,file = './new_outputs/dist_output_new_005_0.1.RData')

# Model Verification
out_burn = burn_data(out,'demo_geo',dummy,R=200,burn=50,name=name)

ls_param = c('theta','rho','sigma2','llike','tau','phi')

# PARAMETERS DATA FRAME
mean_params = func_outcomes2(ls_md = list(out_burn),ls_par = ls_param,func=mean,mod_names=c('demo_geo'))
mean_beta = func_outcomes2(ls_md = list(out_burn),ls_par = c('beta'),func=mean,mod_names=c('demo_geo'))
df = round(rbind(mean_beta,mean_params),6)
df
logMargDenNR(out$llike)

acceptance_rate(out$rho_udraw,out$rho_alphadraw)
acceptance_rate(out$tau_udraw,out$tau_alphadraw)

# #PROBIT
# Data_probit=list(y=y,X=X)
# Prior_probit =list(betabar=rep(0,ncol(X)),A=diag(rep(.01,ncol(X))))
# out_probit=rbprobitGibbs(Data_probit,Prior_probit,Mcmc)
# out_probit = out_probit$betadraw[2501:7000,]


