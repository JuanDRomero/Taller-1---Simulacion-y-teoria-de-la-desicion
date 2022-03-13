##Taller #1: Simulación y teoria de la decisíon
#Presentado por: Jhonatan Romero y Diego Riveros


# 1° PUNTO ----------------------------------------------------------------

#Al comparar los caracteres morfométricos de una especie de langostinos de 
#gran importancia económica en Argentina se observaron diferencias de tamaño 
#entre macho y hembra. En la siguiente tabla se presentan los datos 
#(Largo total mm) de las morfometrías de machos y hembras obtenidos en diferentes 
#recolectas (Ruiz & Mendia, 2008). 

  #a. Haga un histograma con cinco clases y determine la distribución de los 
  #   datos para cada sexo. Explique acerca de la distribución del tamaño para 
  #   cada género. 
  #b. Hallar el promedio y la desviación estándar para cada sexo. ¿Qué puede 
  #   concluir? 
  #c. Halle un intervalo para la media del Largo total por sexo con un nivel de 
  #   confianza del 97%. Provea la inter-pretación respectiva. 
  #d. Construya un Boxplot por sexo e interprételo.
  #e. Compruebe a normalidad de los datos.

#Datos de langostinos hembras
hembras<-c(183.2,182.5,166.8,184.1,190.0,196.3,183.0,178.1,193.3,
           204.3,193.2,187.3,176.5,180.4,185.8,179.0,184.3,189.3,
           188.3,189.2,195.5,186.8,189.1,202.4,202.2,203.1,210.8);hembras 

#Datos de langostinos machos
machos<-c(140.9,173.9,118.9,121.7,177.4,140.0,173.8,154.8,192.7, 
          154.5,177.5,134.4,109.2,153.4,175.0,150.7,138.7,169.8,
          203.3,136.7,153.9,163.0,165.3,176.7,137.7,126.7,150.0);machos



# 1° PUNTO, ITEM A --------------------------------------------------------

#Parcelar las graficas
par(mfrow=c(1,2))

#Histograma de 5 clases para macho y hembra
hist(hembras,breaks = 5,main="Hembras")
hist(machos,breaks = 5,main="Machos")


#Con la libreria "moments", se saca la variable de asimetria y el coeficiente 
#de curtosis para hembras y machos



skewness(machos)
kurtosis(machos)

#El problema nos presenta una distribucion normal tanto para hembras, como machos
# debido a las caracteristicas de sus datos ya que son continuos y Mediante el 
# coeficiente de simetria para los  datos de ambos sexos nos arrojan la sigueinte 
#información de su comportamiento.

skewness(hembras)
kurtosis(hembras)

# Para las hembras, nos da un valor de 0.2056 en la variable de asimetria, lo 
# que indica que tiene una simetria positiva y con el coeficiente Courtisis que 
# nos da 2.9457, nos muestra una distribución leptocurtica.


skewness(machos)
kurtosis(machos)

# Para las hembras, nos da un valor de 0.0447 en la variable de asimetria, lo 
# que indica que tiene una simetria positiva y con el coeficiente Courtisis que 
# nos da 2.4257, nos muestra una distribución leptocurtica.


# 1° PUNTO, ITEM B ------------------------------------------------------

#Hembras

mediaHembras=mean(hembras);mediaHembras
desviacionHembras<-sd(hembras);desviacionHembras

#Estos datos estadisticos nos indican que el tamaño promedio de los langostinos
#hembras es de 189.066 y su desviación estandar de 9.9831 nos muestra la 
#dispersion de los datos con respecto a su media, en este caso, nos da a conocer
#que los datos no se encuentran tan dispersos entre sí.


#Machos
desviacionMachos<-sd(machos);desviacionMachos
mediaMachos=mean(machos);mediaMachos

#Estos datos estadisticos nos indican que el tamaño promedio de los langostinos
#machos es de 154.4667 y su desviación estandar de 23.0642 nos muestra la 
#dispersion de los datos con respecto a su media, en este caso, nos da a conocer
#que los datos se encuentran un poco dispersos entre sí.


# 1° PUNTO, ITEM C --------------------------------------------------------


# 1° PUNTO, ITEM D ------------------------------------------------------


# 1° PUNTO, ITEM E --------------------------------------------------------



# 2° PUNTO ----------------------------------------------------------------

# En un restaurante de la ciudad se sabe que la probabilidad de que se reciba 
#un billete de $50.000 falso es de 0.015. Si se sabe que en una semana se 
#reciben pagos con 900 billetes de $50.000, halle la probabilidad de que: 
  #a. A lo sumo 25 billetes sean falsos.
  #b. La cantidad de billetes falsos esté entre 20 y 30.
  #c. Más de 10 sean falsos.


# X = numero de billetes falsos
n = 900
p = 0.015
q = 1-p


# 2° PUNTO, ITEM A --------------------------------------------------------

# P(X1<=25)

x1 = 25
prob1 = pbinom(x1, n, p, lower.tail = T);prob1

#La probabilidad de que a lo sumo 25 billetes sean falsos es de 0.9985


# 2° PUNTO, ITEM B --------------------------------------------------------

# P(X>=20) y P(X<=30)

x2 = 20
x3 = 30

prob2 = pbinom(x2, n, p, lower.tail = F)
prob3 = pbinom(x3, n, p, lower.tail = F)

probTotal2 = prob2-prob3;

#La probabilidad de que 20 a 30 de billetes sean falsos  es de 0.0339


# 2° PUNTO, ITEM C --------------------------------------------------------

# P(X>10)

x4 = 10

probTotal3 = 1 - pbinom(x4, n, p);probTotal




# 3° PUNTO ----------------------------------------------------------------

#Según un estudio del Departamento Nacional de Estadística -DANE-, la vida 
#media para el quinquenio de 2010 a 2015 de los habitantes de Colombia es 76 
#años, con una varianza de 25. Se pretende hacer un estudio con el objetivo de 
#extrapolar los resultados anteriores a una pequeña ciudad de 100.000 habitantes, 
#considerando que el tiempo de sobrevida es normal.
  #a. ¿Cuántos de los habitantes de la pequeña ciudad superarán previsiblemente los 92 años?
  #b. ¿Cuántos vivirán menos de 55 años o más de 75 años?
  
