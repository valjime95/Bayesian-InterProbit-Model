# -*- coding: utf-8 -*-

import pandas as pd 
import numpy as np
from scipy.stats import truncnorm
from scipy.stats import invgamma
from scipy.linalg import cholesky as chol
from scipy.linalg import inv

data = pd.read_csv('datos_autos.csv')
data.rename(columns = {'Unnamed: 0' : 'id'}, inplace= True)
n = len(data)

#___________-MODELO-_________________
# Se normalizan las variables

data['price'] = data['price']/1000
data['option'] = data['option']/100 
data['age'] = data['age']/100
data['income'] = data['income']/10000
data['ethnic'] = data['ethnic']/100
data['education'] = data['education']/100
lon =data['long']+100
lat =data['lat']-30

# Juntamos las variables  price, option, age, income, ethnic y education en una nueva variable X
iota = [1]* n
X = data.iloc[:,5:11]
X['lat'] = lat
X['lon'] = lon
y = data['y']

iota = [1]* n
iota = pd.DataFrame(iota)
X = pd.concat([iota, X], axis=1)
k = len(X.columns)

#Convertimos en matrices a X, y.
X = X.values
y = y.values

#Como guarde W_zip

W = pd.read_csv('W_zip.csv')
W = W.iloc[:,1:]
W = W.values

rsum  = W.dot(iota)
W = W/rsum

#__________________Parametros de MCMC______________________________
R = 100 #numero de iteraciones
keep = 1
###########################################################
#                                                         #
              #Distribuciones Iniciales# 
#                                                         #
############################################################

# 1. Inicializamos los valores.

# beta

T = np.zeros((k,k))
np.fill_diagonal(T, 0.1) # es un array de ceros y elementos en la diagonal igual a .01 de (9X9) vector de ceros de (kx1)

betabar = np.zeros((1,k)) # vector de ceros de (1 x k)
oldbeta = np.zeros((k,1)) # matriz de (k x 1)

# theta #debe ser en form de DF
oldtheta = np.zeros((n,1))  # matriz de ceros (n x 1)

# Z #debe ser en form de DF
oldz = np.zeros((n,1)) # matriz de (n x 1)

# 2. Draws: Creamos los esqueletos de los sorteos de cada parametro.

in_row =int(np.floor(R/keep)) 

#_____beta draw______
betadraw = np.zeros((in_row,k)) #matriz de (100 x 9)

#_____theta draw______
thetadraw = np.zeros((in_row,n)) #matriz de (100 x 857)

# #_____sigma2,rho, alpha, llike draws______
sigmadraw = rhodraw = alphadraw = llike = [0]* in_row # Vector de cero de tama (1xR)

###########################################################
#                                                         #
            #Distribuciones Finales Condicionales# 
#                                                         #
###########################################################
            # Z #
# Z tiene una distribución normal truncada con mu = Xbeta+ theta y sigma2 = 1
sigma = np.ones((1,n)) #vector de unos de tamaño (n)
# Truncation Points
# a = lower bound | b = upper bound
# Si y_i = 1 -> va a estar truncada a la izquierda en 0, i.e., a = 0 y b = 100 
# Si y_i = 0 -> va a estar truncada a la derecha en 0, i.e., a = -100 y b = 0
a = data['y'].apply(lambda y: -100 if y == 0 else 0)
b = data['y'].apply(lambda y: 0 if y == 0 else 100)
def trunc_rvs(mu,a,b,n): #*
	i = 0 
	l = []
	while i < n:
		l.append(truncnorm.rvs(a[i] - mu[i],b[i] - mu[i], size=1)[0])
		i += 1
	return np.array(l)

							# theta #
# --------------------  ¿?
# HACER CON GRID SEARCHING 
oldrho=0.5 #[0.5,0.6,0.7]
oldsigma2=4
# --------------------

###########################################################
#                                                         #
            			# MCMC # 
#                                                         #
###########################################################
            # Draw Z #
mu = X.dot(oldbeta) + oldtheta
oldz = trunc_rvs(mu,a,b,n)

            # Draw beta #
# beta tiene una distribución final condicional proporcional a una densidad normal mult con mu = A^{-1}b y sigma2 = A^{-1}, con A = (X'X + T) y b = (X'(z-theta) + betabar T)

# A = (X.T @ X) + T
# b = X.T.dot((oldz - oldtheta)) + (betabar.dot(T))

            # Draw θ #
# θ| β,ρ,σ2,Z,y tiene una distribución final condicional proporcional a una densidad normal mult con μ = A^{-1}b y σ2 = A^{-1}, con A = (X'X + T) y b = (X'(z-θ) + betabar T)

B = np.identity(n)-(oldrho * W)
R = chol((B.T.dot(B))/oldsigma2, lower=False)
omega = inv(R.T.dot(R))
nu = omega.dot(oldz - (X.dot(oldbeta)))
#oldtheta = nu + (chol(omega, lower = False).T) # %*%rnorm(n)

            # Draw σ2 #
v0 = 5
s0 = .1
aa = .5*(v0 + n)
Btheta = B.dot(oldtheta)
bb = (v0*s0) + (Btheta.T.dot(Btheta))
# oldsigma2 = invgamma.rvs(a = aa,scale = bb, size = 1)
print(B.dot(oldtheta))




  # oldtheta=nu+t(chol(omega))%*%rnorm(n) # cada vez se genera una nueva
  
  # # draw oldsigma2
  # aa=.5*(s0+n)
  # bb=1/(.5*(q0+crossprod(B%*%oldtheta)))
  # oldsigma2=1/rgamma(1,shape=aa,scale=bb)





























