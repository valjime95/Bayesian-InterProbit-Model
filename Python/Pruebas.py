import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt
import folium
import os

#PROBIT
#https://jbhender.github.io/Stats506/F18/GP/Group14.html

# print(os.getcwd())
data = pd.read_csv('datos_autos.csv')
data.rename(columns = {'Unnamed: 0' : 'id'}, inplace= True)
n = len(data)

# Análisis
# No se tienen variables categóricas, ni se tienen datos faltantes
# print(data.dtypes)
#print(data.describe(include = 'all').transpose())

# funcion que filtra la base por y y agrega una columna con condiones dependiendo del precio.
def filt(base_y, y_number):
	filtered = base_y[(base_y['y'] == y_number)]
	if y_number == 1:
		filtered['price_segun_y'] = np.where(filtered['price'] < 0, 'Más barato japones', 'Más caro japones')
	elif y_number == 0:
		filtered['price_segun_y'] = np.where(filtered['price'] < 0, 'Más caro japones', 'Más barato japones')

	print(filtered.head(5))

# funcion que cuenta los autos japonenes mas caros que loss baratos.
def contador(base):
	caro_j,barato_j = 0,0

	for i in range(len(base)):
		if ((base['y'][i] == 1 and base['price'][i] < 0 ) or (base['y'][i] == 0  and base['price'][i] < 0 )):
			barato_j += 1
		elif ((base['y'][i] == 1 and base['price'][i] > 0 ) or (base['y'][i] == 0  and base['price'][i] > 0 )):
			caro_j += 1
	print('Existen {} casos en el que el coche japones es más barato'.format(barato_j))
	print('Existen {} casos en el que el coche japones es más caro'.format(caro_j))


def contador_2(base):
	caro_1, caro_0, barato_1, barato_0 = 0,0,0,0
	for i in range(len(base)):
		if (base['y'][i] == 1 and base['price'][i] < 0 ):
			barato_1 += 1 # compro auto japones y le salio más barato
		elif (base['y'][i] == 0  and base['price'][i] < 0 ):
			caro_0 += 1 # compro auto no japones y le salio más caro
		elif (base['y'][i] == 1 and base['price'][i] > 0 ):
			caro_1 += 1 # compro auto japones y le salio más caro
		elif (base['y'][i] == 0  and base['price'][i] > 0 ):
			barato_0 += 1 # compro auto no japones y le salio más barato
	# print('De los {} individuos:'.format(len(data)))
	# print('{} eligieron comprar el auto japones y les salio más barato que en promedio uno no japones'.format(barato_1))
	# print('{} eligieron comprar el auto japones y les salio más caro que en promedio uno no japones'.format(caro_1))
	# print('{} eligieron comprar el auto no japones y les salio más barato que en promedio uno japones'.format(barato_0))
	# print('{} eligieron comprar el auto no japones y les salio más caro que en promedio uno japones'.format(caro_0))
	return(caro_1, caro_0, barato_1, barato_0)

#caro_1, caro_0, barato_1, barato_0 = contador_2(data)

# GRAFICO: DESICION DE COMPRA, BARATO VS CARO.
# labels = ['Japones', 'No japones']
# #tipo = [japones, no_japones]
# caro = [caro_1, caro_0]
# barato = [barato_1, barato_0]

# x = np.arange(len(labels)) # the label locations 
# width = 0.35  # the width of the bars

# fig, ax = plt.subplots()
# rects1 = ax.bar(x - width/2, caro, width, label='Más costosos', color = ['navy'])
# rects2 = ax.bar(x + width/2, barato, width, label='Más baratos', color = ['royalblue'])

# # Add some text for labels, title and custom x-axis tick labels, etc.
# ax.set_ylabel('Individuos')
# ax.set_title('Decisión de comprar: \n  Auto japones vs. Auto no japones')
# ax.set_xticks(x)
# ax.set_xticklabels(labels)
# ax.legend() #Place a legend on the axes.

# def autolabel(rects):
#     """Attach a text label above each bar in *rects*, displaying its height."""
#     for rect in rects:
#         height = rect.get_height()
#         ax.annotate('{}'.format(height),
#                     xy=(rect.get_x() + rect.get_width() / 2, height),
#                     xytext=(0, 0), 
#                     textcoords="offset points",
#                     ha='center', va='bottom')


# autolabel(rects1)
# autolabel(rects2)

# fig.tight_layout()

# plt.show()

#___________-INCOME-_________________-

#print(data.describe(include = 'all')['income'])
plt.hist(data['income'])
# plt.show()

#___________-boxplot-_________________-
# for i in data.columns:
# 	data.boxplot(column = i)
# 	plt.show()
#___________-MAPS-_________________


zip_coord= data[['zip','lat','long']].sort_values('zip')
zip_coord.drop_duplicates(subset='zip', inplace=True)


m = folium.Map(location =[34.156113, -118.131943])
folium.Marker([zip_coord['lat'][0],zip_coord['long'][0]]).add_to(m)

for lat, lon, name in zip(zip_coord['lat'],zip_coord['long'],zip_coord['zip']):
	folium.Marker(location = [lat,lon], popup = name).add_to(m)
#m.save('mapita.html')
