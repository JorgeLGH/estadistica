seeds
attach(seeds)
names(seeds)
sed1<- subset(seeds, insect_population== c("0", "1", "2", "3")) #use the fuction subset to determine 
#I only wish for insect_population to work with 0-3, created an object
sed1 #run object to verify
names(seeds)[names(seeds) == "seeds"] <- "seedsss" #try to rename the column of seeds, to other thing,
#as it affects what i'm writting
names(seeds)
sed2<-aov(seedsss~insect_population*plant_population*plant_size*fruits)#made an anova to check the 
#the interactions of the variables, searching for a significative p value
sed2
summary(sed2)#check for significant values, the estimated effects are not balanced, as it has many variables
#less would be ideal
layout(matrix(c(1,2,3,4),2,2))+ plot(sed2) #plot the summary
#####Bonus?????######this next part is only to plot the "significant" interactions#########
sed3<-ddply(seeds, .(plant_population), summarise,
            mean_seeds= mean(seedsss),
            Nseeds= length(seedsss),
            sd= sd(seedsss),
            ste= sd(seedsss)/sqrt(Nseeds),
            CIseeds= qnorm(.95)/sqrt(Nseeds),
            varianceseeds= var(seedsss))
sed3
dap4<-ggplot(sed3, aes(x=plant_population, y=seedsss, fill=plant_population)) +
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin= mean_seeds-ste, ymax= mean_seeds+ste))+
  theme(   
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))
dap4
detach(seeds)

attach(regression)
reg1<- lm(growth~tannin) #line to determine my x and y, meaning how does the type of tannin impacts
#the growth 
reg1
reg2<-summary.lm(reg1)# this will give me the details about the prediction of how will the growth
#change based on the tannin
reg3<-scatter.smooth(x=regression$tannin, y=regression$growth) #codes for the plot, shows how
#the higher the tannin, the lower the growth, it's negative
reg3
detach(regression)

attach(poly)
CrossTable(treatment, response, chisq = TRUE)
pol1<- aov(response~treatment)#again, check interactions
pol1
summary(pol1)
#it can be noticed with the function summary, that the treatment has an actual significative effect 
#on the response, the value of p clearly demonstrates it so
detach(poly)
