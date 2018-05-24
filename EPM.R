
#---------------------------------------  Cargando la Generacion2 de datos -------------------------------

library(readxl)
Generacion2 <- read_excel("C:/Users/Esteban/Desktop/Poster/Generacion2.xlsm",sheet = "Hoja1")
Generacion2 <- as.data.frame(Generacion2[22:42,]) 

# View(Generacion2)
dim(Generacion2)  # para mirar la dimension de la Generacion2 

apply(Generacion2,2,function(x) sum(is.na(x)))  # verifiquemos no tener datos Na


#------------------------------------------- Cargando Generacion2 desde internet ---------------


url <-'https://raw.githubusercontent.com/hebermudezg/Generacion2dedatos/master/GeneracionEPM.txt'
Generacion2 <-read.table(file=url,header=T, sep = ";")



#---------------------------------------- Diagrama de disperción -------------------------

pairs(Generacion2)


#---------------------------------------------- Modelo con lm -----------------------------


mod0 <- lm(Generacion ~ Esta1 + Esta2 + ONI_Media, Generacion2)
summary(mod0)
plot(mod0) 



#--------------------------------------------- Graficos descriptivos ---------------------------

# Grafico de dispercion por variable explicativa 

par(mfrow = c(2, 2), mar = c(5, 4, 2, 2))
plot(y=Generacion2$Generacion, x=Generacion2$Esta1)
plot(y=Generacion2$Generacion, x=Generacion2$Esta2)
plot(y=Generacion2$Generacion, x=Generacion2$ONI_Media)



############################### Pasos a seguir segun freddy Hernandez Barajas (Profesor) ############################



#----------------------------Paso1) prueba marginal bondad de ajuste variable explicativa (y) Generación [Gwh] ---------------


hist(Generacion2$Generacion)
densidad <- density(Generacion2$Generacion)
plot(densidad)
p<-fitDist(Generacion2$Generacion,type="realplus")    # prueba de bondad de ajuste 
p$fits   #  para ver las mejores distrinbuciones ajustadas 



#---------------------------- paso2) definir el horizonte del modelo -----------------------------------


FORM2 <- as.formula("~(Esta1 + Esta2 +ONI_Media)^2 + I(Esta1^2) + I(Esta2^2) + I(ONI_Media^2) + I(Esta1^3) + I(Esta2^3) + I(ONI_Media^3)")



# ---------------------------- Gamlss Modelos Ajustados Iniciales  ----------------------------------------

#install.packages(gamlss)

library(gamlss)
mod1 <- gamlss(Generacion ~Esta1 + Esta2 +ONI_Media, family=WEI3, data = Generacion2)
mod2 <- gamlss(Generacion ~Esta1 + Esta2 +ONI_Media, family=GA, data = Generacion2)
mod3 <- gamlss(Generacion ~Esta1 + Esta2 +ONI_Media, family=GG, data = Generacion2)
mod4 <- gamlss(Generacion ~Esta1 + Esta2 +ONI_Media, family=BCCGo,data = Generacion2)

# resumen de los mejores modelos

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)


round(coef(mod1),5)  # WEI3
round(coef(mod2),5)  # GA
round(coef(mod3),5)  # GG
round(coef(mod4),5)  # BCCGo


Rsq(mod1)
Rsq(mod2)
Rsq(mod3)
Rsq(mod4)

fitted(mod1, "sigma")[1]
fitted(mod2, "sigma")[1]
fitted(mod3, "sigma")[1]
fitted(mod4, "sigma")[1]


a <- GAIC(mod1, mod2, mod3, mod4)

b <- c("WEI3", "GG", "BCCGo", "GA" )

cbind(a,b)  # redimiento de lo smodelos con AIC 





#-------------------------------------------------Paso3 ) seleccion de variables stepGAICAll.A---------------------------------------------

modWEI3 <- stepGAICAll.A(mod1, scope=list(lower=~1, upper=FORM2)) #, k=log(21))
modGA <- stepGAICAll.A(mod2, scope=list(lower=~1, upper=FORM2)) #,  k=log(21))
modGG <- stepGAICAll.A(mod3, scope=list(lower=~1, upper=FORM2)) #, k=log(21))
modBCCGo <- stepGAICAll.A(mod4, scope=list(lower=~1, upper= FORM2)) #, k=log(21))

summary(modWEI3)
GAIC(modWEI3, modGA , modGG, modBCCGo, k=log(21))   # cirterio bayesiano 

Rsq(modWEI3)
Rsq(modGA)
Rsq(modGG)
Rsq(modBCCGo)

plot(modWEI3)

wp(modWEI3)
title(main="Gráfico de gusano el para modelo WEI3")

pdf(file = "Graficogusano1.pdf", height = 6, width = 8)
wp(modWEI3)
dev.off()

plot(modGA)
wp(modGA)

plot(modGG)
wp(modGG)


plot(modBCCGo)
wp(modBCCGo)

par(mfrow = c(2, 2))
wp(modBCCGo)
wp(modWEI3)
wp(modGG)
wp(modGA)

help(wp)


#---------------------------------paso4)  Revisemos los residuales de los modelos ------------------------------

plot(modBCCGo, parameter=par(mfrow=c(2,2), mar=par("mar")+c(0,1,0,0),
                             col.axis = "blue4", col ="blue4", col.main="blue4",
                             col.lab ="blue4", pch = "+", cex =.45,
                             cex.lab = 1.2, cex.axis=1, cex.main=1.2))
plot(modWEI3)
plot(modGG)
plot(modGA)


term.plot(modBCCGo, data = Generacion2)





#---------------------------------- COdigo DIagrama de dispercion




pairs(Generacion2)
#?Mejorando el grafico
panel.reg <- function (x, y)
{
  points(x, y, pch=20)
  abline(lm(y ~ x), lwd=2, col='dodgerblue2')
}
# Funcion para crear el histograma
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="dodgerblue2", ...)
}
# Funcion para obtener la correlacion
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

pdf(file = "descriptivo22.pdf", height = 8, width = 9)
pairs(Generacion2,
      upper.panel = panel.reg,
      diag.panel = panel.hist,
      lower.panel = panel.cor)
dev.off()



#--------------------------- Pruebas de bondad de ajuste --------------------------------

pdf(file = "bondad1.pdf", height = 8, width = 9)
  
par(mfrow = c(2, 2))
histDist(Generacion2$Generacion,family = WEI3 ,main='Distribución WEI3',
         xlab='Generación', ylab='Densidad', 
         line.col='red', line.wd=3, line.ty = 1, labels = "hola perra")
histDist(Generacion2$Generacion,family = GA ,main='Distribución GA',
         xlab='Generación', ylab='Densidad', 
         line.col='red', line.wd=3, line.ty = 1)
histDist(Generacion2$Generacion,family = GG ,main='Distribución GG',
         xlab='Generación', ylab='Densidad', 
         line.col='red', line.wd=3, line.ty = 1)
histDist(Generacion2$Generacion,family = BCCGo,main='Distribución BCCGo',
         xlab='Generación', ylab='Densidad', 
         line.col='red', line.wd=3, line.ty = 1)
dev.off()
  
  
  

pairs(Generacion2,
      upper.panel = panel.reg,
      diag.panel = panel.hist,
      lower.panel = panel.cor)


  
  
  
