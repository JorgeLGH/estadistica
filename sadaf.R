#####revisi?n examen
attach(seeds)
#separar con corchetes
[ , ] #primer valor para renglones y el otro para columnas
[1:20, ] #separates from 1-20 rows and all columns
[1:20, 3] #separates from 1-20 rows and the third column
[, 1:4] #every row but from 1-4 columns
#separate with "which"
rev1<- seeds [-which (seeds$insect_population== "4"), ] 
rev1
#make the anova
rev2<- aov(seeds~insect_population*plant_population, data = seeds)
rev2
summary.aov(rev2)#the degrees of freedom are not correct
rev1$insect_population<- as.character(rev1$insect_population)#make the 0 as character
rev1$plant_population<- as.character(rev1$plant_population)#make the 0 as character
rev2<- aov(seeds~insect_population*plant_population, rev1)
summary.aov(rev2)
####plot
plot(rev2)#direct
summary1<- summarySE(rev1, measurevar = "seedsss", 
                     groupvars = c("insect_population", "plant_population"), conf.interval = .95, na.rm = TRUE)
summary1 #this of a summary of the new object, will later plot
plot(summary1)#basic plot
plot2<-ggplot(summary1, aes(x= plant_population, y=seedsss, fill=insect_population)) +
  geom_bar(stat="identity") + #make the bar itself
  geom_errorbar( aes(ymin= seedsss-se, ymax= seedsss+se)) + #make the graph's thing for error
  geom_point(stat = "identity", shape= 21, colour ="blue", size=2)+
  scale_fill_manual(values= c("0"="yellow", "1"="green", "2"="pink", "3"="red"))+#manually fill with colour vector
  scale_x_discrete(limits=c("0","1","2", "3"))+
  ylab("Seeds")+ #name the "y"
  xlab("Plant population")+ theme(    #name "x"
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))
plot2



##########second part
attach(regression)
rev3<- lm(growth~tannin)
rev3
summary.lm(rev3)
plot(rev3)
scatter.smooth(x=regression$tannin, y=regression$growth)
rev4<-ggplot(regression, aes(x=tannin, y=growth))+
  geom_smooth(method="lm", se=FALSE)+
  geom_point()+
  stat_smooth(method = lm,se=FALSE, colour="red", lty=5, formula = y ~ poly(x, 2))
rev4

CrossTable(tannin, growth)

