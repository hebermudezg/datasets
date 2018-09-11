
# Leyendo la base de datos-------------------------------------
library(readxl)
Datosssm <- read_excel(file.choose())
#View(Tabla_Pequena_Filtrada)

## Dimension de la base de datos........................................... 
dim(Datosssm)


## Verifiquemos los datos faltantes........................................
apply(Datosssm, 2, function(x) sum(is.na(x)))



## Escojamos las variables que esten completas.........................
Datosssm2 <- Datosssm[,colSums(is.na(Datosssm))==0] 
apply(Datosssm2, 2, function(x) sum(is.na(x)))


## Dataframe con datos completos.....................................
Datosssm2 <-  as.data.frame(Datosssm2)
str(Datosssm2)
apply(Datosssm2, 2, function(x) class(x))





## Hagamos modelos con variables dummy........................................................
## Preparemos las variables dummy

#Datosssm2$P6020 <- factor(Datosssm2$P6020, levels = c("2"))     ## sexo 
Datosssm2$P5502 <- factor(Datosssm2$P5502)   ## autualmente .. 3 viuda, 5 soltera
#Datosssm2$P6081 <- factor(Datosssm2$P6081)  # padre vive en el hogar
#Datosssm2$P6083 <- factor(Datosssm2$P6083) # la madre vive en este hogar
Datosssm2$P6080 <- factor(Datosssm2$P6080) # a que cultura pertenece
Datosssm2$P1896 <- factor(Datosssm2$P1896) # satisfaccion con el ingreso
Datosssm2$P1897 <- factor(Datosssm2$P1897) # satisfaccion con la salud
Datosssm2$P1898 <- factor(Datosssm2$P1898) # satisfecho con el nivel de seguridad
Datosssm2$P1899 <- factor(Datosssm2$P1899) # satisfecho con el trabajo
Datosssm2$P1901 <- factor(Datosssm2$P1901) # feliz el dia de ayer
Datosssm2$P1902 <- factor(Datosssm2$P1902) # tranquilo el dia de ayer
Datosssm2$P1903 <- factor(Datosssm2$P1903) # preocupado el dia de ayer
Datosssm2$P1904 <- factor(Datosssm2$P1904) # triste el dia de ayer
Datosssm2$P1905 <- factor(Datosssm2$P1905) # las cosas que hace en su vida valen la pena?

Datosssm2$P1910 <- factor(Datosssm2$P1910) # utiliza _____ computador de escritorio (en cualquier lugar)?
Datosssm2$P1911 <- factor(Datosssm2$P1911) # utiliza _____ portatil (en cualquier lugar)?
Datosssm2$P1912 <- factor(Datosssm2$P1912) # utiliza _____ tablet (en cualquier lugar)?
Datosssm2$P1084 <- factor(Datosssm2$P1084) # utiliza _____ intenet ?
Datosssm2$P1083S3 <- factor(Datosssm2$P1083S3) # utiliza _____ redes sociales ?
Datosssm2$P1082 <- factor(Datosssm2$P1082) # utiliza _____ celular ?
#Datosssm2$P769 <- factor(Datosssm2$P769) # ¿con que frecuencia utiliza ____ teléfono celular?
#Datosssm2$P804 <- factor(Datosssm2$P804) # radio

# Variable respuesta

Datosssm2$P1895 <- factor(Datosssm2$P1895) # radio

boxplot(Datosssm2$P1895, main = "Diagrama de caja de Nivel de satisfacción de las madres solteras")
h <- table(Datosssm2$P1895)
barplot(h, main = "Distribución del nivel de satisfación de las madres solteras")

vect <-  as.matrix(Datosssm2$P1895)


barplot(Datosssm2$P1895, main="Car Distribution", 
        xlab="Number of Gears")




## Hagamos nuestro primer modelo con lm (minimos cuadrados)
mod0 <- lm(P1895~P6040+P5502+P6080+P1896+P1897+P1898+P1899+P1901+P1902+P1903+P1904+P1905+N_HIJOS+N_NIETOS+P1910+P1911+P1912+P1084+P1083S3+P1082, Datosssm2)
summary(mod0)


## vamos la matriz de diseño 
model.matrix(mod0)


#-------Paso1) prueba marginal bondad de ajuste variable explicativa (P1895) nivel de satisfacción  ---------------


hist(Datosssm2$P1895)

barplot(Datosssm2$P1895)

densidad <- density(Datosssm2$P1895)
plot(densidad)
p<-fitDist(Datosssm2$P1895,type="realplus")    # prueba de bondad de ajuste 
p$fits   #  para ver las mejores distrinbuciones ajustadas 

## histograma de la variable respuesta (distribucion de la variable respuesta)
with(Datosssm2, hist(P1895))

hist(Datosssm2$P1895,freq=T, col="lightcyan",
     main="Nivel de satisfacción en la vida de las madres solteras ",xlab="",ylab="Densidad")




###---------------------------- paso2) definir el horizonte del modelo -----------------------------------

FORM1 <- as.formula("~(N_HIJOS+N_NIETOS+P6040)^2+P6040+P5502+P6080+P1896+P1897+P1898+P1899+P1901+P1902+P1903+P1904+P1905+N_HIJOS+N_NIETOS+P1910+P1911+P1912+P1084+P1083S3+P1082")   # definimos todas las variables solas e interacciones entre ellas




### ---------------------------Haciendo el primer modelo con gamlss---------------------------------


library(gamlss)
mod1 <- gamlss(P1895~P6040+P5502+P6080+P1896+P1897+P1898+P1899+P1901+P1902+P1903+P1904+P1905+N_HIJOS+N_NIETOS+P1910+P1911+P1912+P1084+P1083S3+P1082,
               data = Datosssm2)
summary(mod1)
Rsq(mod1)



#-------------------------------------------------Paso3 ) seleccion de variables stepGAICAll.A---------------------------------------------



# Hagamos un proceso de seleccion de variables usando stepgaicall

mod2<- stepGAIC(mod1, scope=list(lower=~1, upper=FORM1), direction = 'both')



mod2$anova
summary(mod2)
Rsq(mod2)

round(coef(mod2),2) 


# Varibles seleccionadas..

#Step:  AIC= 3884.46 

#P1895 ~ P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + N_HIJOS:N_NIETOS





############################ Hagamos un modelo multinomial###################################
#############################################################################################
#############################################################################################

Mydata <-  Datosssm2[, c("P1895", "P1896", "P1897", "P1898", "P1901", "P1902", "P1905", "N_HIJOS", "N_NIETOS", "P1084", "P1083S3")]
plot(Mydata)


library(gamlss)
library(MASS)

require(nnet)
modmultinom<- multinom(P1895 ~ P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + P1084 + P1083S3 + N_HIJOS:N_NIETOS,
                  data=Datosssm2, trace=FALSE)



summary(modmultinom)

length(modcf$fitted.values)
length(Datosssm2$P1895)


pre<-predict(modmultinom, newdata = Datosssm2[,c("P1896","P1897","P1898","P1901","P1902","P1905","N_HIJOS","N_NIETOS", "P1084", "P1083S3")])
length(pre)



plot(Datosssm2$P1895, pre,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(Datosssm2$P1895, pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))





a <- Datosssm2[,c("P1896","P1897","P1898","P1901","P1902","P1905","N_HIJOS","N_NIETOS", "P1084", "P1083S3")]
dim(a)

valoresingresados <- data.frame(
  P1896 = as.factor(c(10)),
  P1897 = as.factor(c(10)),
  P1898 = as.factor(c(10)),
  P1901 = as.factor(c(10)),
  P1902 = as.factor(c(10)),
  P1905 = as.factor(c(10)),
  N_HIJOS = 1,
  N_NIETOS =1)

satisfaccion_calculada2 <- predict(modmultinom, newdata = valoresingresados)
satisfaccion_calculada2


class(valoresingresados)




tabladeresultados <- cbind(pre,Datosssm2$P1895)
View(tabladeresultados)

contador <- 0
for (i in 1:nrow(tabladeresultados)){
  if (pre[i]==Datosssm2$P1895[i]){
    contador = contador + 1
    
  }
    
  
}

aciertos <- contador/nrow(tabladeresultados)



pre[c(5:10)]
Datosssm2$P1895[0:10]


# comprobacion del modelo

obs<-Datosssm2$P1895
pre<-predict(modcf, type="class")
cont<-0

for (i in 1:length(Datosssm2)){
  if (pre[i]==obs[i])
    cont=cont+1
  else
    cont=cont
}
cont
tocc<- cont/length(Datosssm2)
tocc



## seleccionn de variables con el modelo multinom








###-----------------------------------------Logit multinom----------------------------

library(gml)

regresion2 = glm(P1895 ~ P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + N_HIJOS:N_NIETOS, family =multinom(), data = Datosssm2)
summary(regresion2)








###------------------------------





