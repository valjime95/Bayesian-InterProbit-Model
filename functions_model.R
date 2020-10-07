library(fastDummies)
library(rlist)
library(tidyverse)
source('function_matrix_W.R')


# DATA PREP
dummy_col <- function(data){
  # dummy columns of the demo groups.
  # Eliminate the groups that have more than 60% of observations with y = 1.
  
  demo_group <- demo_groups(data)
  datos_y <- data.frame(cbind(data$y,demo_group))
  group <- ifelse(datos_y[,1]==1,'no available',demo_group)
  
  dummy <- dummy_columns(group)
  dummy <- dummy[,2:(ncol(dummy)-1)]
  return(dummy)
}

norm_var <- function(data){
  # Normalize the variables.
  # Initial points: lat = -30, long = 100
  
  data$price <- data$price/1000
  data$option <- data$option/100
  data$age <- data$age/10
  data$income <- data$income/10000
  data$ethnic <- data$ethnic/100
  data$education <- data$education/100
  long <- data$long+100
  lat <- data$lat-30
  
  list(data=data,long=long,lat=lat)
}


# EVALUATION OF THE MODEL
group_names <- function(dummy){
  # Return a vector with the group of the dummy variables
  
  name_vector <- vector(mode="character")
  for (i in 1:ncol(dummy)) {
    names <- paste('group',i,sep = ' ')
    name_vector <- append(name_vector,names) }
  return(name_vector) }

names <- c("iota","price","option","age","income","ethnic","education","lat",'long')

burn_data <- function(out,Type,dummy,R,burn,name){
  # Return the list of the parameters clean 
  # (i.e. without the burn data)
  
  b <- burn+1
  left <- b:R
  
  theta <- out$thetadraw[left]
  beta <- data.frame(out$betadraw[left,])
  # Names with dummy or without dummy variables
  if (ncol(beta)==9){colnames(beta) <- names}
  else{colnames(beta) <- name}
  
  rho <- out$rhodraw[left]
  sigma2 <- out$sigma2draw[left]
  llike <- out$llike[left]
  
  if (Type == 'demo_geo'){
    tau <- out$taudraw[left]
    phi <- out$phidraw[left]
    
    list(beta=beta,theta=theta,rho=rho,
         sigma2=sigma2,
         tau=tau,phi=phi,
         llike=llike)}
  
  else{list(beta=beta,theta=theta,rho=rho,
         sigma2=sigma2,llike=llike)}
  
  # scatter3D(beta$lat,beta$long,theta,phi = 0,
  #           pch = 20, cex = 2, ticktype = "detailed")
}

acceptance_rate<- function(u,alpha){
  # Calculates the acceptance rate of the M-H 
  # and plot the performance during the iteration process
  x = data.frame(u=u,alpha=alpha)
  y=ifelse(u<alpha,1,0)
  rate = sum(y)/length(y)
  
  # PLOT
  
  df= x %>% arrange(alpha)
  plot = ggplot(df,aes(x = 1:length(u),y = alpha))+geom_line()+
    xlab('Iteraciones') + ylab('alpha')+
    ggtitle('Probabilidad de aceptación de ...')

  list(plot=plot,rate=rate)
}

# Models Statistics as DF.

func_outcomes2 = function(ls_md,ls_par,func,mod_names){
  #
  l = length(ls_md)
  l2 = length(ls_par)
  
  if ('beta' %in% ls_par){
    df_beta <- data.frame(matrix(0,28,l),row.names = name)
    colnames(df_beta) <- mod_names
    for (i in 1:l){
      df_beta[i] = unname(sapply(data.frame(ls_md[[i]]['beta']),func))}
    return(df_beta)
  }
  
  else{
    df_param <- data.frame(matrix(0,l2,l),row.names = ls_par)
    colnames(df_param) <- mod_names
    for(p in ls_par){
      for (i in 1:l){ df_param[p,i] <- unname(sapply(ls_md[[i]][p],func))}}
    return(df_param)
  }
}

# func_outcomes = function(ls_md,parameter,func){
#   #  Returns a vector with the func applied in the parameter list.
#   l = length(ls_md)
#   v <- vector('list',length = l)
#   for (i in 1:l){ v[[i]] <- unname(unlist(ls_md[[i]][parameter])) }
#   return(unlist(map(v,func)))
# }
# 
# df_outcomes <- function(ls_md,ls_par,func,names){
#   #  Returns a data frame of the define function (func)
#   #  of all the  parameters listed in ls_par for all the models.
#   df <- data.frame(matrix(0,length(ls_md),length(ls_par)),
#                    row.names = names)
#   for (i in 1:4){df[i] <- func_outcomes(ls_md,ls_par[i],func)}
#   names(df) <- ls_par
#   
#   cat("\nSummary of Posterior Marginal Distributions \n ")
#   return(df)
# }
