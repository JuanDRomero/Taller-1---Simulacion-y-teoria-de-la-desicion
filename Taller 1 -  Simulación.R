##Taller #1: Simulaci�n y teoria de la decis�on
#Presentado por: Juan David Romero y Diego Andres Riveros


# 1� PUNTO ----------------------------------------------------------------

#Al comparar los caracteres morfom�tricos de una especie de langostinos de 
#gran importancia econ�mica en Argentina se observaron diferencias de tama�o 
#entre macho y hembra. En la siguiente tabla se presentan los datos 
#(Largo total mm) de las morfometr�as de machos y hembras obtenidos en diferentes 
#recolectas (Ruiz & Mendia, 2008). 

  #a. Haga un histograma con cinco clases y determine la distribuci�n de los 
  #   datos para cada sexo. Explique acerca de la distribuci�n del tama�o para 
  #   cada g�nero. 
  #b. Hallar el promedio y la desviaci�n est�ndar para cada sexo. �Qu� puede 
  #   concluir? 
  #c. Halle un intervalo para la media del Largo total por sexo con un nivel de 
  #   confianza del 97%. Provea la inter-pretaci�n respectiva. 
  #d. Construya un Boxplot por sexo e interpr�telo.
  #e. Compruebe a normalidad de los datos.

#Datos de langostinos hembras
hembras<-c(183.2,182.5,166.8,184.1,190.0,196.3,183.0,178.1,193.3,
           204.3,193.2,187.3,176.5,180.4,185.8,179.0,184.3,189.3,
           188.3,189.2,195.5,186.8,189.1,202.4,202.2,203.1,210.8);hembras 

#Datos de langostinos machos
machos<-c(140.9,173.9,118.9,121.7,177.4,140.0,173.8,154.8,192.7, 
          154.5,177.5,134.4,109.2,153.4,175.0,150.7,138.7,169.8,
          203.3,136.7,153.9,163.0,165.3,176.7,137.7,126.7,150.0);machos

nHembras = length(hembras)
nMachos = length(machos)

# 1� PUNTO, ITEM A --------------------------------------------------------

#Parcelar las graficas
par(mfrow=c(1,2))

#Histograma de 5 clases para macho y hembra
#Histograma de 5 clases para macho y hembra
hist(hembras,breaks = 5,main="Hembras",col="#F19C89",
     col.main="#520707",col.sub="white",col.lab="#520707",fg="black", xlab = "longitud", , ylab = "Numero de langostinos")
hist(machos,breaks = 5,main="Machos",col="#F19C89",
     col.main="#520707",col.sub="white",col.lab="#520707",fg="black", xlab = "longitud", , ylab = "Numero de langostinos")


#Con la libreria "moments", se saca la variable de asimetria y el coeficiente 
#de curtosis para hembras y machos



skewness(machos)
kurtosis(machos)

#El problema nos presenta una distribucion normal tanto para hembras, como machos
# debido a las caracteristicas de sus datos ya que son continuos y Mediante el 
# coeficiente de simetria para los  datos de ambos sexos nos arrojan la sigueinte 
#informaci�n de su comportamiento.

skewness(hembras)
kurtosis(hembras)

# Para las hembras, nos da un valor de 0.2056 en la variable de asimetria, lo 
# que indica que tiene una simetria positiva y con el coeficiente Courtisis que 
# nos da 2.9457, nos muestra una distribuci�n leptocurtica.


skewness(machos)
kurtosis(machos)

# Para las hembras, nos da un valor de 0.0447 en la variable de asimetria, lo 
# que indica que tiene una simetria positiva y con el coeficiente Courtisis que 
# nos da 2.4257, nos muestra una distribuci�n leptocurtica.


# 1� PUNTO, ITEM B ------------------------------------------------------

#Hembras

mediaHembras=mean(hembras);mediaHembras
desviacionHembras<-sd(hembras);desviacionHembras

#Estos datos estadisticos nos indican que el tama�o promedio de los langostinos
#hembras es de 189.066 y su desviaci�n estandar de 9.9831 nos muestra la 
#dispersion de los datos con respecto a su media, en este caso, nos da a conocer
#que los datos no se encuentran tan dispersos entre s�.


#Machos
desviacionMachos<-sd(machos);desviacionMachos
mediaMachos=mean(machos);mediaMachos

#Estos datos estadisticos nos indican que el tama�o promedio de los langostinos
#machos es de 154.4667 y su desviaci�n estandar de 23.0642 nos muestra la 
#dispersion de los datos con respecto a su media, en este caso, nos da a conocer
#que los datos se encuentran un poco dispersos entre s�.


# 1� PUNTO, ITEM C --------------------------------------------------------

#Confianza asignada
confianza = 0.97

#Se buscan sus dos colas, dejando un 5% en la parte inferior y superior.
z = qnorm(0.5+confianza/2)

#intervalo de confianza HEMBRAS
idcH = c(mediaHembras-z*desviacionHembras/nHembras^.5, 
        mediaHembras+z*desviacionHembras/nHembras^.5);idcH

#El intervalo de confianza para las HEMBRAS VA DESDE 184.6608 A 193.1725
#dandonos a entender que cualquier longitud fuera de estos margenes es poco 
#probable que suceda ya que hay un 97% de confianza que el dato este entre los
#valores


#intervalo de confianza MACHOS
idcM = c(mediaMachos-z*desviacionMachos/nMachos^.5, 
         mediaMachos+z*desviacionMachos/nMachos^.5);idcM

#El intervalo de confianza para las MACHOS VA DESDE 144.8341 A 164.0992
#dandonos a entender que cualquier longitud fuera de estos margenes es poco 
#probable que suceda ya que hay un 97% de confianza que el dato este entre los
#valores
  
# 1� PUNTO, ITEM D ------------------------------------------------------
par(mfrow=c(1,2))

#BOXPLOT MACHOS
boxplot(machos, horizontal = FALSE,   
        main = "LANGOSTINOS MACHOS", ylab = "longitud", col="#F6A0A0")
stripchart(machos, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = "#F54B4B")

#BOXPLOT hembras
boxplot(hembras, horizontal = FALSE,   
        main = "LANGOSTINOS HEMBRAS", ylab = "longitud", col="#F6A0A0")
stripchart(hembras, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col="#F54B4B")



# 1� PUNTO, ITEM E --------------------------------------------------------



# 2� PUNTO ----------------------------------------------------------------

# En un restaurante de la ciudad se sabe que la probabilidad de que se reciba 
#un billete de $50.000 falso es de 0.015. Si se sabe que en una semana se 
#reciben pagos con 900 billetes de $50.000, halle la probabilidad de que: 
  #a. A lo sumo 25 billetes sean falsos.
  #b. La cantidad de billetes falsos est� entre 20 y 30.
  #c. M�s de 10 sean falsos.


# X = numero de billetes falsos
n = 900
p = 0.015
q = 1-p

plot(0:30, dbinom(0:30, n, p),main = "Distribucion Binomial",xlab = "Numero de billetes",ylab = "Probabilidad",pch=16,col="red",
     col.main="blue",col.sub="white",col.lab="orange",fg="purple")

# 2� PUNTO, ITEM A --------------------------------------------------------

# P(X1<=25)

x1 = 25
prob1 = pbinom(x1, n, p, lower.tail = T);prob1

#La probabilidad de que a lo sumo 25 billetes sean falsos es de 0.9985


# 2� PUNTO, ITEM B --------------------------------------------------------

# P(X>=20) y P(X<=30)

x2 = 20
x3 = 30

prob2 = pbinom(x2, n, p, lower.tail = F)
prob3 = pbinom(x3, n, p, lower.tail = F)

probTotal2 = prob2-prob3;

#La probabilidad de que 20 a 30 de billetes sean falsos  es de 0.0339


# 2� PUNTO, ITEM C --------------------------------------------------------

# P(X>10)

x4 = 10

probTotal3 = 1 - pbinom(x4, n, p);probTotal3

#La probabilidad de que hayan m�s de 10 billetes sean falsos es de 0.7907


# 3� PUNTO ----------------------------------------------------------------

#Seg�n un estudio del Departamento Nacional de Estad�stica -DANE-, la vida 
#media para el quinquenio de 2010 a 2015 de los habitantes de Colombia es 76 
#a�os, con una varianza de 25. Se pretende hacer un estudio con el objetivo de 
#extrapolar los resultados anteriores a una peque�a ciudad de 100.000 habitantes, 
#considerando que el tiempo de sobrevida es normal.
  #a. �Cu�ntos de los habitantes de la peque�a ciudad superar�n previsiblemente los 92 a�os?
  #b. �Cu�ntos vivir�n menos de 55 a�os o m�s de 75 a�os?
  
