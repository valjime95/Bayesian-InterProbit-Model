
rinterprobit_demogeo=
function(Type,Data,Prior,Mcmc){
  # '''Runs the Bayesian Probit with Spatial and Demographic Dependencies Model''' using
  #  Gibbs within Metropolis Hasting algorithm.
  
  # Receives:
  # ** Type of model: 
  #   * demo: weight matrix W is based on demographic groups
  #   * zip: W is based on zip codes (geo)
  #   * dist: W is based on distance between consumers (geo)
  #   * demo_geo: W combines the demo and geo matrices on one
  # **  Data: list with the X,y and W matrices.
  # **  Prior: list with the hyperparameters.
  # **  Mcmc: list with R, the number of iterations and keep, parameter for slim.
  
# Data, Priors and MCMC parameters
X=Data$X
y=Data$y
W=Data$W

betabar=Prior$betabar
A=Prior$A
s0=Prior$s0
q0=Prior$q0

R=Mcmc$R
keep=Mcmc$keep
nxvar=ncol(X)
n=nrow(X)

# MCMC draws
oldbeta=matrix(double(nxvar),ncol=1)
oldtheta=matrix(double(n),ncol=1)
oldz=matrix(double(n),ncol=1) 

betadraw=matrix(double(floor(R/keep)*nxvar),ncol=nxvar)
thetadraw=matrix(double(floor(R/keep)*n),ncol=n)
sigma2draw=double(floor(R/keep))
rhodraw=double(floor(R/keep))
llike=double(floor(R/keep))

# DRAWS TO CHECK HOW IS PERFORMING THE M-H ACCEPTANCE RATE
rho_alphadraw=double(floor(R/keep))
rho_udraw=double(floor(R/keep))
# Tunning Parameter for rho M-H
# c = 1
# count_rho = 0
# rate_rho = 1
# i = 0

if (Type =='demo_geo'){
  
  #PRIORS
  oldtau=Prior$oldtau #*
  oldphi=Prior$oldphi #*
  # DRAWS
  taudraw=double(floor(R/keep)) #* alpha to tau
  phidraw=double(floor(R/keep)) #*
  # M-H
  tau_alphadraw=double(floor(R/keep))
  tau_udraw=double(floor(R/keep))
}

oldrho=0.5
oldsigma2=4

# W values
lambda=eigen(W,only.values=TRUE)$values
lmin=min(lambda)
lmax=max(lambda)

# truncation points for draWs
        a=ifelse(y == 0,-100, 0)
        b=ifelse(y == 0, 0, 100)
# Epsilon Cov-var 
sigma=c(rep(1,n))
# Beta posterior parameters
root=chol(chol2inv(chol(crossprod(X)+A)))
Abetabar=crossprod(A,betabar)

###########################################################
#                                                         #
#                           MCMC                          #
#                                                         #
###########################################################

itime=proc.time()[3] #proc.time determines how much real and CPU time (in seconds) the currently running R process has already taken.
cat("MCMC Iteration (est time to end - min)",fill=TRUE)
flush.console()
for (j in 1:R) {


# Draw Z|β,θ,ρ,σ2,y -> truncate normal distribution
mu=(X%*%oldbeta)+oldtheta 
oldz=rtrun(mu,sigma,a,b) # rtrun draws from a truncated univariate normal distribution

# Draw β|θ,ρ,σ2,Z,y -> normal conjugate prior
oldbeta=breg(oldz-oldtheta,X,betabar,A)
# cov = crossprod(root,root)
# betatilde = cov%*%(crossprod(X,y)+Abetabar)
# oldbeta = betatilde+t(root)%*%rnorm(length(betatilde))

# Draw θ| β,ρ,σ2,Z,y -> normal multivariate with μ = A^{-1}b & σ2 = A^{-1}, A = (X'X + T) & b = (X'(z-θ) + betabar T)
B=diag(n)-oldrho*W
omega=chol2inv(chol(diag(n)+crossprod(B)/oldsigma2))
nu=omega%*%(oldz-(X%*%oldbeta))
oldtheta=nu+t(chol(omega))%*%rnorm(n)

# Draw σ2| β,θ,ρ,Z,y -> Inverse Gamma conjugate prior 
# rossi
aa=.5*(s0+n)
bb=1/(.5*(q0+crossprod(B%*%oldtheta)))
# bb=1/(.5*((s0*q0)+crossprod(B%*%oldtheta)))
oldsigma2=1/rgamma(1,shape=aa,scale=bb)

# Draw ρ|β,θ,σ2,Z,y with Metropolis-Hastings
newrho = oldrho + rnorm(1,mean=0,sd=0.005) #*
# newrho=rnorm(1,mean=oldrho,sd=.1)

# Tunning Parameter
# if(j != 1){
  # if (rate_rho < .4){c = c/1.1}
  # else if (rate_rho > .6){c = c*1.1}}

# newrho = oldrho + (c*rnorm(1,mean=0,sd=1)) #*

if(newrho > 1./lmin & newrho < 1./lmax) {
# i = i+1
Aold=crossprod(diag(n)-oldrho*W)
Anew=crossprod(diag(n)-newrho*W)
alphan=(det(Anew)^.5)*exp(-1/(2*oldsigma2)*(crossprod(oldtheta,Anew)%*%oldtheta))
alphad=(det(Aold)^.5)*exp(-1/(2*oldsigma2)*(crossprod(oldtheta,Aold)%*%oldtheta))
# alphan=(det(Anew)^.5)*exp(-1/(2*oldsigma2)*crossprod(oldtheta,Anew)) #* like in the book
# alphad=(det(Aold)^.5)*exp(-1/(2*oldsigma2)*crossprod(oldtheta,Aold)) #* like in the book
rho_alpha=min(alphan/alphad,1) #*

# if(rho_alpha=="NaN") rho_alpha=-1
u_rho = runif(n=1,min=0, max=1)

if(u_rho < rho_alpha) {oldrho = newrho }
  # count_rho = count + 1}
  # rate_rho = count_rho/i
}

# Draw tau for demo_geo model with M-H
if (Type =='demo_geo'){
  # draw oldtau -> old phi -> W
  newtau = abs(oldtau + rnorm(1,mean=0,sd=0.005)) #*
  # newtau=rnorm(1,mean=oldtau,sd=.1)
  newphi = exp(newtau)/(exp(newtau)+1) #*
  W_n = (newphi*W_zip) + ((1-newphi)*W_dem) #*
  
  #Acceptance probability
  Bold=crossprod(diag(n)-oldrho*W) #*
  Bnew=crossprod(diag(n)-oldrho*W_n) #*
  tau_alpha_n=(det(Bnew)^.5)*exp(-1/(2*oldsigma2)*(crossprod(oldtheta,Bnew)%*%oldtheta))*exp(-1/(2*.01)*(newtau^2)) #*
  tau_alpha_p=(det(Bold)^.5)*exp(-1/(2*oldsigma2)*(crossprod(oldtheta,Bold)%*%oldtheta))*exp(-1/(2*.01)*(oldtau^2)) #*
  tau_alpha=min(tau_alpha_n/tau_alpha_p,1)
  
  if(tau_alpha=="NaN") tau_alpha=-1 #*
  u_tau = runif(n=1,min=0, max=1) #*
  if(u_tau < tau_alpha) { #*
    oldtau = newtau #*
    oldphi = newphi #*
    W = W_n }
}

# update screen
if(j%%100==0)
          {
           ctime=proc.time()[3]
           timetoend=((ctime-itime)/j)*(R-j)
           cat(" ",j," (",round(timetoend/60,1),")",fill=TRUE)
           flush.console()
           }

mkeep=j/keep #slim the chain
if(mkeep*keep == (floor(mkeep)*keep))
          {betadraw[mkeep,]=oldbeta
           thetadraw[mkeep,]=oldtheta
           sigma2draw[mkeep]=oldsigma2
           rhodraw[mkeep]=oldrho
           rho_udraw[mkeep]=u_rho
           rho_alphadraw[mkeep]=rho_alpha #*
           
           if (Type=='demo_geo'){
           taudraw[mkeep]=oldtau #*
           phidraw[mkeep]=oldphi #*
           tau_alphadraw[mkeep]=tau_alpha
           tau_udraw[mkeep]=u_tau }
           
	mu=X%*%oldbeta+oldtheta
	llike[mkeep]=sum(log(pnorm(mu)*y + (1-pnorm(mu))*(1-y)+.00001))
	            }
}

ctime=proc.time()[3]
cat(" Total Time Elapsed: ",round((ctime-itime)/60,2),fill=TRUE)

if(Type=='demo_geo'){
list(betadraw=betadraw,sigma2draw=sigma2draw,thetadraw=thetadraw,rhodraw=rhodraw,
     taudraw=taudraw,phidraw=phidraw,
     rho_alphadraw=rho_alphadraw,tau_alphadraw=tau_alphadraw,
     rho_udraw=rho_udraw,tau_udraw=tau_udraw,
     llike=llike)
  }
else{
  list(betadraw=betadraw,sigma2draw=sigma2draw,thetadraw=thetadraw,rhodraw=rhodraw,
       rho_alphadraw=rho_alphadraw, rho_udraw=rho_udraw,
       llike=llike)
}

}

