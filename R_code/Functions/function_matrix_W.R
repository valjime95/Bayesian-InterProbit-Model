library(argosfilter)
library(bayesm)

#__________________Matriz de pesos W______________________________

# Se van a crear funciones con las definiciones de matrices que se requiera, en este caso se tienen 4 tipos: 'zip_code', 'distance', 'demo', 'demo_zip' 
# 1. 'zip_code': Matriz W en donde si tienen el mismo zip code entonces son vecinos.
W_zip = function(W, n, base){
  for (i in 1:n){for (j in 1:n){
    if (base$zip[i]== base$zip[j]) {W[i,j] = 1}
    else {W[i,j]=0}
  }}
  return(W)
}
# 2. 'distance': Matriz W en donde en donde se mide la distancia geográfica y si se cumple que tienen la distancia minima requerida entonces son vecinos.
# GEOCALC.

W_geocalc = function(lat_i, lon_i, lat_j, lon_j){
  EARTH_R = 6372.8
  lat_i = radian(lat_i)
  lon_i = radian(lon_i)
  lat_j = radian(lat_j)
  lon_j = radian(lon_j)
  dlon = lon_i - lon_j
  y = sqrt((cos(lat_j) * sin(dlon)) ** 2 +(cos(lat_i) * sin(lat_j) - sin(lat_i) * cos(lat_j) * cos(dlon)) ** 2)
  x = sin(lat_i) * sin(lat_j) + cos(lat_i) * cos(lat_j) * cos(dlon)
  c = atan2(y, x)
  return (EARTH_R * c)
}

W_distance <- function(W,n,base){
  for (i in 1:n){ for (j in 1:n){
    x = c(base$lat[i], base$long[i])
    y = c(base$lat[j], base$long[j])
    W[i,j] = 1/exp(W_geocalc(x[1],x[2],y[1],y[2]))
  }}
  return(W)
  }

# 3. 'demo': Matriz W en donde en donde si pertenecen al mismo grupo geográfico entonces son vecinos.
# 3.1 Se dividen las variables en los grupos demográficos: 3 grupos de edad, 3 de sueldo anual, 2 de educación y 2 de etnia.

W_g3dem = function(base, f){
  x = unlist(base[f], use.names = FALSE)
  t = c(quantile(x , .3333),quantile(x , .6666),max(x))
  l = substring(f,1,1)
  group = cut(x,breaks = c(-1,t[1],t[2],t[3]),
               labels = c(paste(l,1, sep = ''),paste(l,2, sep = ''),paste(l,3, sep = '')))
  return(group)
}

demo_groups = function(base){
  # Education group  with 3 levels ( primary, secondary and college)
  edu_g3 <- W_g3dem(base,'education')
  # Education group  with 2 levels ( basic & beyond college)
  edu_g2 <- replace(edu_g3,edu_g3 == 'e2','e1')
  eth_group <- ifelse(base$ethnic>27,1,0) #grupo de ethnic
  d_group <- paste(W_g3dem(base ,'age'),W_g3dem(base,'income'),edu_g2,eth_group, sep = '_')
  return(d_group)
}

W_demo = function(W,n,d_group){
  for (i in 1:n){ for (j in 1:n){
    if (d_group[i]== d_group[j]) {W[i,j] = 1}
    else {W[i,j]=0}
  }}
  return(W)
}

# W_demogeo = function(Wg,Wd){
#   oldtau = rnorm(1,0,.01) # prior τ
#   oldphi = exp(oldtau)/(exp(oldtau)+1) # φ
#   W = (oldphi*Wg) + ((1-oldphi)*Wd)
# }

W_matrix = function(tipo,base,normalizada = TRUE, lambda = TRUE) { #tipos: 'zip_code', 'distance', 'demo'
  n = nrow(base)
  W = array(0,dim = c(n,n))
  if (tipo == 'zip_code'){W = W_zip(W,n,base)}
  else if (tipo == 'distance'){W = W_distance(W,n,base)}
  else if (tipo == 'demo'){W = W_demo(W,n,demo_groups(base))}
  
  diag(W) <- 0
  W <- as.matrix(W)
  # normalizamos la matriz por renglones
  if (normalizada == TRUE){
  iota = array(0,n)+1
  rsum = W%*%iota
  W = W/as.vector(rsum)
  W[is.nan(W)] <- 0}
  # calculamos los eigenvalores de la matriz
  if (lambda == TRUE){
    lambda = eigen(W, only.values = TRUE)$values }
  
  list(W = W,lambda = lambda)
}

W_circular = function(n){
  iota = array(0,n)+1
  W=array(0,dim=c(n,n))
  for (i in 2:n-1) {W[i,i+1]=1; W[i,i-1]=1}
  W[1,2]=1; W[1,n]=1
  W[n,n-1]=1; W[n,1]=1
  rsum=W%*%iota
  W=W/as.vector(rsum)

}
