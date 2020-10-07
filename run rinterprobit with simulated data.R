# library(pastecs)
library(bayesm)
library(ggplot2)
library(bayesplot)
# setwd("/Users/valeriajimeno/Desktop/Tesis_R/Modelo data simulada")

source("../rinterprobit_demogeo.R")
# source("../rinterprobit_rossi.R")
# source("/Users/valeriajimeno/Desktop/rinterprobit.R")

data = read.table("simulated interdep data.dat",header=TRUE) #data already simulated
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
R=10000
Mcmc = list(R=R,keep=1)
burn=2500

out=rinterprobit_demogeo(Type = 'new',
                         Data=list(y=y,X=X,W=W),
                         Prior=list(betabar=rep(0,ncol(X)),A=diag(rep(.0001,ncol(X))),s0=5,q0=10),
                         Mcmc=list(R=20000,keep=1))
out_500_datasim = out
save(out_500_datasim,file = 'out_500_datsim.RData')
# # estadisticas(out_50_rossi_simdata,5000,1000)
# # estadisticas = function(out,R,burn){
# # 
# #   j = burn + 1
# #   beta1 = out$betadraw[,1][j:R]
# #   beta2 = out$betadraw[,2][j:R]
# #   sigma2draw = out$sigma2draw[j:R]
# #   rhodraw = out$rhodraw[j:R]
# #   d = data.frame(beta1,beta2,rhodraw,sigma2draw)
# #   print(summary(d))
# #   print(cat(sd(beta1),sd(beta2),sd(rhodraw),sd(sigma2draw)))
# # }



# # ----------P R O B I T  G I B B S-----------------

data_probit = list(y=y,X=X)
prior_probit = list(betabar=rep(0,ncol(X)),A=diag(rep(.01,ncol(X))))
probit = rbprobitGibbs(data_probit,prior_probit,Mcmc)

beta_probit = data.frame(beta1 = probit$betadraw[,1],
                  beta2 = probit$betadraw[,2])

beta = data.frame(beta1 = out$betadraw[,1],
                 beta2 = out$betadraw[,2])

color_scheme_set("blue")

p1 <- mcmc_trace(beta,n_warmup = 1000,
                 facet_args = list(ncol = 1, strip.position = "left"))
# # beta1_pro = probit$betadraw[-(1:burn_in),1]
# # beta2_pro = probit$betadraw[-(1:burn_in),2]
p1

plot_title <- ggtitle(expression('Cadena de Markov de '~beta))
ggplot(beta,aes(x = 1:R,y = beta1))+
  geom_rect(aes(xmin = 0, xmax = burn, ymin = -Inf, ymax = Inf),
            fill = "gray97")+
  geom_line(color = '#b3cde0')+
  geom_line(aes(y = cumsum(beta1)/1:R), color = '#7b8e9c',size = .75)+
  geom_line(aes(y = 1), color = '#7693a8',size = .75)+
  scale_x_continuous(name="Iteraciones", limits=c(0,R))+
  plot_title +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

plot_title <- ggtitle(expression('Cadena de Markov de '~beta))
ggplot(beta,aes(x = 1:R,y = beta2))+
  geom_rect(aes(xmin = 0, xmax = burn, ymin = -Inf, ymax = Inf),
            fill = "gray97")+
  geom_line(color = '#b3cde0')+
  geom_line(aes(y = cumsum(beta2)/1:R), color = '#7b8e9c',size = .75)+
  geom_line(aes(y = 1), color = '#7693a8',size = .75)+
  scale_x_continuous(name="Iteraciones", limits=c(0,R))+
  plot_title +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


# # https://journal.r-project.org/archive/2013/RJ-2013-013/RJ-2013-013.pdf
# # https://cran.r-project.org/web/packages/spatialprobit/spatialprobit.pdf
# # https://rdrr.io/cran/spatialprobit/src/R/semprobit.R
# # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0219212#pone-0219212-t002
