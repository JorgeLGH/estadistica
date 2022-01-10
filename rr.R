Examen<-data.frame("Growth"=c(3.9, 3.6, 3.1, 4.3, 5.2, 4.8, 7.9, 7.3, 8.1,6.3, 7.1, 6.8, 4.5, 4.1, 3.9, 9.1, 8.7, 9.5), 
           "Gender"=c("F","F","F","F","F","F","F","F","F","M","M","M","M","M","M","M","M","M"),
           "Conc"= c("Low","Low", "Low","Medium","Medium","Medium","High","High","High","High","High","High",
                     "Medium", "Medium", "Low","High","High","High"))
Examen
attach(Examen)
av1<-aov(Growth~Gender*Conc)
summary.aov(av1)
#Se observa que tanto el género, como la concentración tienen un efecto significativo sobre el creciemiento
#de los individuos; sin embargo, la interacción de ambas no lo es
TukeyHSD(av1)
#una prueba de tukey sirve para ver si dos promedios varían significativamente, más no para ver si es
#significativo el efecto de las interacciones; no es necesario para ver efectos significativos, pero ayuda
#a entender las diferencias entre las combinaciones de tratamientos
ex1<-ddply(Examen, .(Gender), summarise,
             m= mean(Growth),
             N= length(Growth),
             sde= sd(Growth),
             se= sde/sqrt(N))
ex1
ggplot(ex1, aes(x= Gender, y=m, fill=Gender)) +
  geom_bar(stat="identity") +
  geom_errorbar( aes(ymin= m-se, ymax= m+se))+ theme(    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))
  
ex2<-ddply(Examen, .(Conc), summarise,
           m= mean(Growth),
           N= length(Growth),
           sde= sd(Growth),
           se= sde/sqrt(N))
ex2
ggplot(ex2, aes(x= Conc, y=m, fill=Conc)) +
  geom_bar(stat="identity") + #make the bar itself
  geom_errorbar( aes(ymin= m-se, ymax= m+se))+ theme(    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))
detach(Examen)

layout(matrix(c(1,2,3,4),2,2))


layout(matrix(c(1,1),1,1))

attach(iris)                
iris
av2<-aov(Petal.Length~Sepal.Length)
plot(av2)
summary.aov(av2)
descdist(iris$Petal.Length, discrete = F, boot = 5000)
f1<-fitdist(cancer$death, "weibull")
summary(f1)
f2<-glm(Petal.Length~Sepal.Length)
summary(f2)    
plot(f2)
#tras probar con varios métodos, se observa que la longitud del sépalo tiene un efecto significativo
#sobre la longitud de los pétalos

ggplot(iris, aes(x= Sepal.Length, y=Petal.Length, fill=Species)) +
  geom_bar(stat="identity") + theme(    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))
  
ggplot(iris, aes(x= Sepal.Length, y=Petal.Length, group=Species, colour=Species))+ 
  geom_point(shape=7)+
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE)+ theme(    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))

av2.1<-aov(Petal.Length~Species*Sepal.Length)
plot(av2.1)
summary(av2.1)
pp1<-glm(Petal.Length~Species*Sepal.Length, family = gaussian)
summary(pp1)
#Parece ser que solo hay un efeco significativo con las especies versicolor y virginica
#cuandp estas interactúan junto con Sepal.length, por separado no tienen un efecto al parecer
detach(iris)

attach(InsectSprays)
InsectSprays
ins1$count<- as.character(ins1$count)
ins1<- InsectSprays [which (InsectSprays$spray== c("A", "C")), ] 
ins1
avv<-aov(data=ins1,count~spray)
avv
plot(avv)
#parece que una anova se ajusta más o menos bien 
summary(avv)
av22<-glm(count~spray, family = gaussian, data = ins1)
summary(av22)
#se puede observar que los spray son significativamente diferentes; C es menor respecto
#a count cuando se compara con el A
ob30<-ddply(ins1, .(spray), summarise,
           m= mean(count),
           N= length(count),
           sde= sd(count),
           se= sde/sqrt(N))
ob30
ggplot(ob30, aes(x=spray, y=m, fill=spray)) +
  geom_bar(stat="identity") +
  ylab("Mean count")+
  geom_errorbar( aes(ymin= m-se, ymax= m+se)) + theme(    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))

