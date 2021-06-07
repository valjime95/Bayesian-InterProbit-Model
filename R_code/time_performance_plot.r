library(tikzDevice)
iteraciones = c(1000,5000,10000,15000,20000)
time_100 = c(0.08,0.42,0.82,1.47,1.97) # en minutos
time_300 = c(2.1,8.9,21.5,33.7,45.6) 
time_600 = c(15.44,97.8,179.5,264.1,350.9) 
muestra = c(100,300,600)

cols = c('#6c4684','#535871','#93a5b4')
tikz(file = '/Users/valeriajimeno/Git/Tesis/Latex/sections/time_performance.tex', width = 5, height=6)
plot(time_600, xaxt = 'n',type='l',col=cols[1], ylim = c(0,360), 
     ylab = 'Tiempo (minutos)', xlab = 'Iteraciones', lwd=3)
axis(1,at =1:5 ,labels = iteraciones)
lines(time_300, lwd=3,col = cols[2])
lines(time_100, lwd=3,col = cols[3])
legend(1,350,
       c('m=600','m=300','m=100'),
       fill=c(cols[1],cols[2],cols[3]), cex = 0.7)
dev.off()
