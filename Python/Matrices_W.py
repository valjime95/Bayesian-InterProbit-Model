import pandas as pd 
import numpy as np


data = pd.read_csv('datos_autos.csv')
data.rename(columns = {'Unnamed: 0' : 'id'}, inplace= True)
n = len(data)

data['price'] = data['price']/1000
data['option'] = data['option']/100 
data['age'] = data['age']/100
data['income'] = data['income']/10000
data['ethnic'] = data['ethnic']/100
data['education'] = data['education']/100
lon =data['long']+100
lat =data['lat']-30

iota = [1]* n
X = data.iloc[:,5:11]
X['lat'] = lat
X['lon'] = lon
y = data['y']

iota = [1]* n
iota = pd.DataFrame(iota)
X = pd.concat([iota, X], axis=1)
k = len(X.columns)

#__________________Matriz de pesos W______________________________

# Se van a crear funciones con las definiciones de matrices que se requiera, en este caso se tienen 5 tipos: 'zip_code', 'distance', 'demo', 'demo_zip' y 'demo_dist'.

W =np.zeros((n,n))

# 1. 'zip_code': Matriz W en donde si tienen el mismo zip code entonces son vecinos.

def W_zip(W,base, n):
	i= 0
	while i < n:
		j = 0
		while j < n:
			if base['zip'][i] == base['zip'][j]:
				W[i][j] = 1	
			else:
				W[i][j] = 0
			j+=1
		i+=1
	return W #W.values

# 2. 'distance': Matriz W en donde en donde se mide la distancia geográfica y si se cumple que tienen la distancia minima requerida entonces son vecinos.

# GEOCALC.

def W_geocalc(lat_i, lon_i, lat_j, lon_j):
    EARTH_R = 6372.8
    lat_i = np.radians(lat_i)
    lon_i = np.radians(lon_i)
    lat_j = np.radians(lat_j)
    lon_j = np.radians(lon_j)
    dlon = lon_i - lon_j
    y = np.sqrt((np.cos(lat_j) * np.sin(dlon)) ** 2 +
        (np.cos(lat_i) * np.sin(lat_j) - np.sin(lat_i) *
         np.cos(lat_j) * np.cos(dlon)) ** 2)
    x = np.sin(lat_i) * np.sin(lat_j) + \
        np.cos(lat_i) * np.cos(lat_j) * np.cos(dlon)
    c = np.arctan2(y, x)

    return EARTH_R * c

def W_distance(W,base,n):
	i = 0
	while i < n:
		j = 0
		while j < n:
			x = (data['lat'][i],data['long'][i])
			y = (data['lat'][j],data['long'][j])
			W[i][j] = 1/np.exp(W_geocalc(x[0],x[1],y[0],y[1]))
			j += 1
		i += 1
	return W #W.values

# np.savetxt('W_dist2.csv', W, delimiter=',')

# 3. 'demo': Matriz W en donde en donde si pertenecen al mismo grupo geográfico entonces son vecinos.
# 3.1 Se dividen las variables en los grupos demográficos: 3 grupos de edad, 3 de sueldo anual, 2 de etnia y 2 de educación.

def demo_group(base,*args):
	df = pd.DataFrame()
	for i in args:
		tercil1 = base[i].quantile(.3333)
		tercil2 = base[i].quantile(.6666)

		f = i[0]
		groups = lambda x: '{}1'.format(f) if x <= tercil1 else '{}2'.format(f)\
											if x <= tercil2 else '{}3'.format(f)
		name = '{}_group'.format(i)									
		df[name] = base[i].apply(groups)

	group = df[df.columns].apply(lambda x: '_'.join(x), axis = 1 )
	return group

demo_group = demo_group(data,*['age','income'])

def W_demo(W,lis,n):
	i= 0
	while i < n:
		j = 0
		while j < n:
			if lis[i] == lis[j]:
				W[i][j] = 1	
			else:
				W[i][j] = 0
			j+=1
		i+=1
	return W #W.values

# np.savetxt('W_demo.csv', W, delimiter=',')

# 4. 'demo_zip': Matriz W en donde se juntan los criterios de las matrices 'zip_code' y 'demo'.
# 5. 'demo_dist': Matriz W en donde se juntan los criterios de las matrices 'dist' y 'demo'.

# Función final de matriz W, en donde se normaliza la matriz por renglones.

def matriz_W(tipo, base, n): #tipos: 'zip_code', 'distance', 'demo', 'demo_geo'
	if tipo == 'zip_code':
		x = W_zip(W,base, n)
	elif tipo == 'distance':
		x = W_distance(W,base, n)
	elif tipo == 'demo':
		x = W_demo(W,lis, n)
	elif tipo == 'demo_zip':
		x = W_demo_zip(W,base, n)
	elif tipo == 'demo_dist':
		x = W_demo_dist(w,base, n)
	
	# Normalizamos la matriz
	x = x.values
	rsum  = W.dot([1]* n) 
	W = W/rsum

	return W

# W = pd.read_csv('W_zip.csv')
# W = W.iloc[:,1:]
# W = W.values

# rsum  = W.dot([1]* n)
# W = W/rsum
# print(rsum)
