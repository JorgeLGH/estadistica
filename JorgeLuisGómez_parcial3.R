attach(species)#attach the data frame to avoid writing more
hist(Species)#check if it resembles normal distribution
an1<- aov(Species~pH*Biomass)#did an ANOVA 
an1
summary.aov(an1)#check for the effect of the variables on our response
is.factor(pH)#check if it is a factor or not
is.factor(Biomass)
layout(matrix(c(1,2,3,4),2,2))#show more graphs at the same time
plot(an1)#the assumptions for the anova seem to be actually true, therefore, the analysis will continue
#based on the summary, we notice both variables have a significant effect on the response, however, taking into account
#both of the variables, they do not have a significant effect when joined
anc1<-lm(Species~pH*Biomass, data=ipomopsis)
anc1
summary(anc1)
pp1<-plot(anc1)
pp1 #just making sure i'm on the right with the lines above
ggplot(species, aes(x=Biomass, y=Species, colour=pH, shape=pH)) + geom_point(shape=7) +
  geom_smooth(method=lm,  
              se=FALSE, 
              fullrange=TRUE) + theme(    
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))#plot for how does it looks 
detach(species)



attach(cancer)
cancer
an2<- aov(death~treatment*status, data=cancer)
an2
plot(an2)
summary.aov(an2)
#after taking into account the lines before, it seems as if the assumptions for the anova are violated, therefore can't
#continue using this analysis
descdist(cancer$death, discrete = TRUE, boot = 500) #after checking, seems as the repetitions 
#show as if the Poisson distribution is the most approxiamte distribution. nonetheless, the negative binomial
#could possibly be also a good answer; will use poisson due to mpre concentration of predicted data 
fit.poiss<- fitdist(cancer$death, "pois")
fit.poiss
summary(fit.poiss)
plot(fit.poiss)#the lines above and this one show how the data adapts to a poisson distribution
#it doesn't fully adapt to the distribution, quasi will be used
fit1<- glm(death~treatment*status, family = quasipoisson())
summary(fit1)
plot(fit1)
#the data shows to be more adequately adapted to the distribution
#doesn't seem to show a significan effect on any of the treatments, it would mean that the treatment
#doesn't have an effect, no matter which of them it is.
#just to make sure, will now do the same as above but with negative binomial
fit.ngt<- fitdist(cancer$death, "nbinom")
fit.ngt
summary(fit.ngt)
plot(fit.ngt) #seems to adjust way better than poisson
fit2<-glm.nb(formula = death ~ treatment * status, init.theta = 1.032713156, 
    link = log)
summary(fit2)
fit3<- update(fit2, ~. *treatment*status)
summary(fit3)
plot(fit3)
#now it looks more as a correct distribution, and it shows as if the treatment C does have
#a significant effect on the response

obje<-ddply(cancer, .(treatment), summarise,
            Mean_deaths= mean(death),
            N= length(death),
            sde= sd(death),
            se= sde/sqrt(N))
obje
graph_2<- ggplot(obje, aes(x= treatment, y=Mean_deaths, fill=treatment)) +
  geom_bar(stat="identity") + #make the bar itself
  geom_errorbar( aes(ymin= Mean_deaths-se, ymax= Mean_deaths+se))+ theme(    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")) #mestablish the order you want your bars
graph_2
