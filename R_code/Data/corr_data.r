# install.packages('CorrToolBox')
library(ltm)
library(corrplot)
# correlation variables
data = read.table("Interdependent.dat", header = TRUE)
head(data)

# .----------------data histograms-------------------------
# par(mfrow = c(1,1))
# hist(data$zip, prob = TRUE, )
# lines(density(data$zip), col = "red", lwd = 2)
# hist(data$lat, prob = TRUE)
# lines(density(data$lat), col = "red", lwd = 2)
# hist(data$long)
# hist(data$y)
# hist(data$price)
# hist(data$option)
# hist(data$age)
# hist(data$income)
# hist(data$ethnic)
# hist(data$education)

# --------------correlation variables-----------------------
# CHANGE IN LATEX.
correlation <- cor(data[2:10]) # pearson correlation only for the cute plot
colors = c('#70284E','#973468','#BE4082','#CC659B','#DA8AB4','#D88AB8','#E6AFCF',
           '#DFD8F3','#C0B2E6','#9484D8','#745FCA','#563EB8','#453291','#33266A')

# tikz(file = '/Users/valeriajimeno/Desktop/Escri-tesis/Tesis/sections/corrplot.tex', width = 5.5, height = 5.5)
corrplot(correlation, method = 'number',number.cex = 0.9,type='lower',tl.col="black", col=colors)
# dev.off()


# variables type
# cathegorical:
#   *nominal ----> zip, y(dic)
# Numerical:
#   *continous ---> lat, long, price, option, income, ethnic, education
#   * discrete ---> age


# point biserial : Computes the point-biserial correlation between a dichotomous and a continuous variable.
# dic ----> y
# cont ---> lat, long, price, option, income, ethnic, education
biserial.cor(data$lat, data$y,level = 1)
biserial.cor(data$long, data$y,level = 1)
biserial.cor(data$price, data$y,level = 1)
biserial.cor(data$option, data$y,level = 1)
biserial.cor(data$age, data$y,level = 1)
biserial.cor(data$income, data$y,level = 1)
biserial.cor(data$ethnic, data$y,level = 1)
biserial.cor(data$education, data$y,level = 1)


