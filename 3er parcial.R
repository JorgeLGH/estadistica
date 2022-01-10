###ANCOVA
ipomopsis

anc1<-lm(Fruit~Grazing*Root, data=ipomopsis)
anc1
anova(anc1)
anc2<- lm(Fruit~Root*factor(Grazing),data= ipomopsis)
anc2
summary(anc2)
summary(anc1)

Anova(anc1, type= "III")


plot(anc2)
plot(anc1)

ggplot(ipomopsis, aes(x=Root, y=Fruit, colour=Grazing, shape=Grazing)) + geom_point(shape=7) +
  geom_smooth(method=lm,  
              se=FALSE, 
              fullrange=TRUE)+scale_colour_manual(values=c("blue","purple"))+scale_shape_manual(values=c(16,22))



names(cells)
head(cells)
attach(cells)
cells 
cel1<- aov(cells~smoker*age*sex*weight, data = cells)
cel1
hist(cells)
summary.aov(cel1)
layout(matrix(c(1,2,3,4),2,2))
plot(cel1)
descdist(cells$cells, discrete = TRUE, boot = 500) 
fit.poiss<- fitdist(cells$cells, "pois")
fit.poiss
plot(fit.poiss)

fit1<- glm(cells~smoker*age*sex*weight,data = cells, family = quasipoisson())
summary(fit1) 
fit2<- update(fit1, ~. *smoker*age*weight*sex,family= quasipoisson, data= cells)
summary(fit2)
