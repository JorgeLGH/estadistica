#Libraries used
library(rgl)
library(car)
library(plot3D)
library(plyr)
library(vegan)
library(sm)
library(ggplot2)
library(PerformanceAnalytics)
library(corrgram)
library(Hmisc)
library(descr)
library(gmodels)
library(tidyverse)
library(psych)
library(Rmisc)
library(pastecs)
library(ggpubr)
library(graphics)
library(gplots)
library(latexpdf)
library(fitdistrplus)
library(ggpubr)
library(stats19)
library(stats)
library(cluster)
library(gclus)
library(gplots)
# tic tac toe symbol is for commentary
#R reconoce 5 objetos: character, numérico(número real), integral, complejo, lógico(flaso,verdadero)
##symbol for marking everything yo the right belongs to te object "<-"
#if we run the program and the symbol in the window below, it means something has gone wrong
obj1<-52 #object made with number
obj2<-"hola" #has to be WEith the "" for text to be read
obj3<-1:100 #the double point marks a sequence
obj4<- c(1, 14, 0, 9)
#for us to make sure an object was made, we must evaluate, for this we go to print and mark the object we select
print(obj3)
obj3
obj2
#create a vector: must be of the same class, only numerical, logic, etc. we write class()
class(obj3)
class(obj2)
class(obj1)
#explicit coercion: coerce a vector to be read as something that it isn't: example: int to numeric
#as.numeric
#as.Character
#as.logical
#go to help bar and search for the "as.whatever"
as.integer(obj1)
as.integer(obj2)
as.numeric(obj3)
as.integer(obj4)
#data base in r is called data frames, import it
#head() headline
##names() name columns
#tail() last data
#write name for full data frame
#use excell to import database
#the use depends on the quantity of the data
#quickr is a good site for investigation and practice
names(ChickWeight)
var(weight)
library(help = "datasets")
head(iris)
tail(iris)
iris
names(iris)
head(UKLungDeaths)
tail(UKLungDeaths)
names(UKLungdeaths)
names(USArrests)
tail(USArrests)
USArrests
head(USArrests)
#install libraries
#check for the name of the library to install, can be many
#load library with command below
library(vegan)
library(vegan)
library(vegan)
iris
head(USAccDeaths)
USAccDeaths

obj5<- c(25, 12, 7, 4, 6, 6, 2, 1, 0, 2)
class(obj5)
max(obj5)
min(obj5)
sum(obj5)
mean(obj5)
median(obj5)
range(obj5)
var(obj5)
hist(obj5)

library(plyr)
library(plyr)

#check for help of the packages in the help section and type the name of the package

ChickWeight
names(ChickWeight)

#check for names to know the name of the columns
#the function "attach" if for R to know we are talking about the same data frame, 
#talks about the same dataframe for the following code lines
#detach to stop using the dataframe
attach(ChickWeight)
detach(ChickWeight)
#use ddply to split frame data and apply functions
#check function for print
diet_weight<- ddply(ChickWeight, .(Diet), summarise, #has to be with .() to be read
                    mean_weight= mean(weight), #created mean_weight for it to be an object
                    N= length(weight), #check for number of chickens
                    sd= sd(weight)) #have to check everything
diet_weight
print(diet_weight)
library(vegan)
library(plyr)
USAccDeaths
number_deaths<- ddply(USAccDeaths, .(Jan), summarise,
                      mean_deaths= mean(Jan),
                      N= length(Jan))
number_deaths<- ddply(USAccDeaths, .(1973), summarise,
                      mean_deaths_year= mean(1973),
                      Nyear= length(1973))
number_deaths
print(number_deaths)
number_deaths
number_deaths
names(USAccDeaths)
names(USArrests)
USArrests

number_murder<- ddply(USArrests, .(Murder), summarise,
                      mean_deaths= mean(Murder),
                      N= length(Murder))
number_murder
class(number_murder)

names(obj5)
ChickWeight
USAccDeaths

#realizar un histograma 
hist(weight)#funcion para histograma
attach (ChickWeight)
hist(weight)#se cambia el color o lo que tu quieras 
hist(weight, col= "blue")#sali? del color deseado
hist(weight, col= "blue",ylab= "Frecuencia", xlab= "Peso", main="Primera grafica")#si funcionaaaa
#ejercicio de hacerlo con la base de datos que nos de Etzel 
hist(weight, col= "green4", main= "Primer tarea", ylab= "Frecuencia", xlab="Peso")
detach(ChickWeight)

hist(USAccDeaths)
attach(USAccDeaths)
hist(1978)
detach(USAccDeaths)

library(vegan)
library(plyr)
hist(USAccDeaths, col = "blue", ylab = "frecuencia", xlab = "muertes", main = "muertes")
boxplot(USAccDeaths)
attach(ChickWeight)
densityplot(weight, .col(Diet))#plot density by group of diet, how?
ChickWeight
library(sm)

sm.density.compare(weight, Diet, filled.contour(.5))
library(ggplot2)

qplot(x= weight, fill=Diet, geom = "density", data = ChickWeight)
qplot(Chickweight,x= weight, fill=Diet, geom_boxplot(Diet))
geom_histogram(x= ChickWeight$weight, fill= ChickWeight$Diet, data = ChickWeight)

detach(ChickWeight)

attach(ChickWeight)

qplot(x=weight, fill= Diet)
qplot(boxplot(x= weight, y= Diet))
qplot(x= weight, fill= Diet)

ggplot(ChickWeight, aes(x=weight, fill=Diet)) +
  geom_histogram(binwidth=1, alpha=1)

ggplot(geom_histogram(ChickWeight))      
ggplot(ChickWeight, aes(x=weight, fill=Diet)) + geom_histogram(binwidth=1)
#we have to make an object to be clear
try<- ggplot(ChickWeight, aes(x=weight, fill= Diet)) +
  geom_boxplot()


ggplot(ChickWeight, aes(x=weight)) + geom_boxplot(ChickWeight)
bye<-ggplot(ChickWeight, aes(x=weight, y=Diet))

ggplot(ChickWeight, aes(x=Diet, y=weight, fill=Diet)) + #for it to work, we have to check 
  #for what we have in x and y IMPORTANT
  geom_boxplot(width=.8, alpha=.8)
#########################################################################################################3
detach(ChickWeight)


diet_weight<- ddply(ChickWeight, .(Diet), summarise, #has to be with .() to be read
                    mean_weight= mean(weight), #created mean_weight for it to be an object
                    N= length(weight), #check for number of chickens
                    sd= sd(weight)) #have to check everything

diet_weight
hist(weight, col = "blue", border = "pink")

#graph bar with standard deviation and stuff
attach(ChickWeight, fill= "weight")
ggplot(diet_weight, aes(x= mean_weight, y= Diet, fill= mean_weight)) +
  geom_bar(position = position_dodge(width = .5)) +
  geom_errorbar(aes())

diet_weight
names(diet_weight)





ggplot(tgc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=len-se, ymax=len+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

ggplot(ChickWeight, aes(x=weight, fill=Diet)) + geom_histogram(binwidth=1, colour="blue", fill="green")
#this ~~~~ is for a formula, it is the equivalent, it is for the variation in our response
boxplot(x=weight)
boxplot(x=weight, col = "green")
boxplot(weight~Diet, col= c("blue","green", "yellow", "brown"))
boxplot(weight~Diet, col= c("blue","green", "yellow", "brown"), horizontal = TRUE, border="magenta", lty=2)
ggplot(ChickWeight, aes(x=Diet, y=weight, fill=Diet)) +  
  geom_boxplot(width=.8, alpha=.8, fill="blue")

ggplot(ChickWeight, aes(x=Diet, y=weight, fill=Diet)) +  
  geom_boxplot(width=.8, alpha=.8)

ggplot(ChickWeight, aes(x=Diet, y=weight, fill=Diet)) +  
  geom_boxplot(width=.8, alpha=.8, fill="blue", colour="red")

ggplot(ChickWeight, aes(x=Diet, y=weight, fill=Diet)) +  
  geom_boxplot(width=.8, alpha=.8, colour="red", fill= c("red", "green", "magenta", "brown"), lty=2) + theme_classic(1)

ggplot(ChickWeight, aes(x=Diet, y=weight, fill=Diet)) +  
  geom_boxplot(width=.8, alpha=.8, colour="red", fill= c("red", "green", "magenta", "brown"), lty=2) + theme( 
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "black"))
names(ChickWeight)
attach(ChickWeight)
detach(ChickWeight)
#test null hypothesis
#m->mean chicken weight
#one population
t.test(weight, mu= 0.8183)# the closer the p value goes to 1, then it is true the null hypothesis, 
#if it goes below .05, then the alternative hypothesis will be mosy likely true

#two populations, check for m1=m2
#wanna use ChickWeight, must separate by diet. funcionts: which and subset()
dt1<- ChickWeight[which(ChickWeight$Diet=='1'),]
dt1
dt2<- ChickWeight[which(ChickWeight$Diet=='2'),]
dt2
dt3<- ChickWeight[which(ChickWeight$Diet=='3'),]
dt3
dt4<- ChickWeight[which(ChickWeight$Diet=='4'),]
dt4
t.test(ChickWeight, c(dt1,dt2))#error
t.test(dt1&dt2, data=Diet)#error
t.test(formula= dt1~dt2)
t.test(ChickWeight~dt1,dt2)
t.test(ChickWeight, dt1&dt2)
t.test(dt1, dt2, paired = TRUE)
#####correct way down below
tt1<-t.test(dt1$weight, dt2$weight)#as valor in p is less than .05, then it is significative
tt1
chisq.test(dt1$weight, dt2$weight)
#using subset
dt11<- subset(ChickWeight, Diet==1)
dt11

####three populations test, makes it bigger for the chance of commiting a type 1 error
t.test(dt1$weight, dt2$weight, dt3$weight)



#ANOVA test in r
#aov(y~A+B+A*B, data=dataframe)
ChickWeight
attach(Libro1.1.2)
detach(Libro1.1.2)
Libro1.1
Libro1.1.2
Libro1.1.2<-read.csv(file = "C:/Users/fotgo/OneDrive/Documentos/Libro1.1.2.csv")
hist(Libro1.1.2)
class(Libro1.1.2)
title(Libro1.1.2)
head(Libro1.1.2)
names(Libro1.1.2)
#correct way of using aov
anx<- aov(ansiedad~edad+sexo+edad*sexo,
               data=Libro1.1.2)
anx
summary(Libro1.1.2)
summary(anx)
summary.aov(anx)
#either of the last works just fine

plot(anx)
#it shows all 4 graphs, but to show everything we use function layout
layout(matrix(c(1,2,3,4),2,2))+ plot(anx)
#to restore of modify how many of the pannels show, we use layout function again
layout(matrix(c(1),1,1))+ plot(anx)
layout(matrix(c(1,2,3,4,5,6),3,2))+plot(anx)
#the residual vs. fitted must have a straight line for it to be fine, meaning there's a correct distribution, 
#we want every dot appears randomly across everything, means we have variance homogeneity, therefore the residuals 
#and ______ are correct, if the data in x has a cone like figure, means the variance doesn't have homogeneity
#the second graph, q-q, must have most of the dots aligned within the line for it to confirm the above, 
#means the distribution of the model's residuals, shows which observation doesn't fit the distribution
#scale location, same as the first, basically
#residuals vs. leverage, it tells us the "weight" of the outliers, it would tilt de graph if the outliers 
#are too heavy, there are outliers that are taking the values of the means and variances, we want to get rid of 
#them, the ones that appear with a number are the ones that we don't want
#meaning it would show if an outlier may influence the data, data that tilts must corroborate with a boxplot
#
# the homogenity of the variances can be out of the desired state depending on the size of the sample
#when the dots on the second graph has inclination,but not bent, means it has a correct distribution but has a certain shape
#if the variables are independetn, the first graph will show the homogenity of the variance, must be there



#post hoc tests
TukeyHSD(anx)
plot(TukeyHSD(anx))
#check the table to interpretate
library(help="datasets")
detach(ChickWeight)  
summary(anx)
summary(Libro1.1.2)

#more functions for AOV
summary.lm(anx) #stands for lineal model
#summary.lm has imprtant the adjusted R-squared and other things that are important
#the adjusted R-squared is a vlaue that shows how good the model is, meaning how well the models fits
#the lower the value it means we are not showing the variation of variation response
#can only go between 0-1
#almost never we go above the .8, it would be weird


#now show graphically 
layout(matrix(c(1,2,3,4),2,2))
plot(anx)
TukeyHSD(anx)
#graph tukey with:
plot(TukeyHSD(anx))
#graph the ones that are significant only:
obje<-ddply(Libro1.1.2, .(edad), summarise,
            ms= mean(ansiedad),
            N= length(ansiedad),
            sde= sd(ansiedad),
            se= sde/sqrt(N))
obje



graph_edad<- ggplot(obje, aes(x= edad, y=ms, fill=edad)) +
  geom_bar(stat="identity") + #make the bar itself
  geom_errorbar( aes(ymin= ms-se, ymax= ms+se)) + #make the graph's thing for error
  scale_fill_manual(values= c("yellow", "green", "pink"))+ #manually fill with colour vector
  scale_x_discrete(limits=c("n","j","a")) #establish the order you want your bars

graph_edad + annotate("text", x=1, y=6.2, label="a") + #function for text in bars
   annotate("text", x=2, y=5, label="b")+
   annotate("text", x=3, y=9, label="c")+
  ylab("Ansiedad")+ #name the "y"
  xlab("Edad")+ theme(    #name "x"
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))

#Dunnet test is compare the levels against a null/control
#unlike tukey which compares everything against everything

ob2<-ddply(Libro1.1.2, .(sexo), summarise,
           mss= mean(ansiedad),
           Nn= length(ansiedad),
           sdee= sd(ansiedad),
           see= sdee/sqrt(Nn))
ob2
graph_sexo<- ggplot(ob2, aes(x= sexo, y=mss, fill= sexo)) +
  geom_bar(stat="identity") + 
  geom_errorbar( aes(ymin= mss-see, ymax= mss+see)) + 
  scale_fill_manual(values= c("blue","brown")) 
graph_sexo + annotate("text", x=1.5, y=7.5, label="*", size= 8) + 
  ylab("Ansiedad")+ #name the "y"
  xlab("Sexo")+ theme(    #name "x"
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))


pd <- position_dodge(0.1)
ob3<-ddply(Libro1.1.2, .("sexo", "edad"), summarise,
           mss= mean(ansiedad),
           Nn= length(ansiedad),
           sdee= sd(ansiedad),
           see= sdee/sqrt(Nn))
ob3
graph_ob3<-ggplot(ob3, aes(x=edad, y=sexo, fill=edad)) + #doesn?t work at all
  geom_errorbar(aes(ymin=mss-see, ymax=mss+see), width=.1) +
  geom_line() +
  geom_point()
graph_ob3
#make a summary with everything you need:
#use summarySE


#regresi?n lineal
library(rgl)
library(car)
library(plot3D)
library(plyr)
library(vegan)
library(sm)
library(ggplot2)
library(PerformanceAnalytics)
library(corrgram)
library(Hmisc)
library(descr)
library(gmodels)
library(tidyverse)
library(psych)
library(Rmisc)
library(pastecs)
library(ggpubr)
library(graphics)
library(gplots)
library(latexpdf)

head(Orange)
Orange
attach(Orange)
lm1<-lm(age~circumference)
lm1
summary(lm1)
summary.lm(lm1)
#to make the plot for this object:
#we use plot, but the x is for the variable, in y the response
plot(circumference, age) +
  abline(a=16.6, b=7.816)
ggplot(Orange, aes(x=circumference, y=age, group=Tree, colour=Tree))+ #x= circumference as it is 
  #our variable, y=age as we determine the age based on the circumference
  geom_smooth(method= "lm", se=FALSE,lty=5)+ #geon_smooth it gives the visual representation of the line
  geom_point()+ theme(   #geom_point shows the points in the graph
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))

lm2<-lm(age~circumference+Tree)
lm2
summary.lm(lm2)
geom_lin

diminish

attach(diminish)
lm3<-lm(yv~xv)
lm3
summary.lm(lm3)
plot(xv,yv)
fig1<-ggplot(diminish, aes(x=xv, y=yv))+
  geom_smooth(method="lm", se=FALSE)+
  geom_point()+
  stat_smooth(method = lm,se=FALSE, colour="red", lty=5, formula = y ~ poly(x, 2))#this line is
#for the curve
fig1

#now with polynomial adjust
#the adjust rises the r-squared value
#we select which adjust we choose by comparing both models with an anova
cuad1<- lm(yv~poly(xv, 2))
cuad2<- lm(yv~ xv + I(xv^2))
cuad1
cuad2
summary(cuad1)
summary(cuad2)
anova(lm3, cuad1)

detach(diminish)


########################################################### make 3D plots
mtcars
names(mtcars)
head(mtcars)
row.names(mtcars)
attach(mtcars)
lmcar<- lm(mpg~wt+hp)
lmcar
summary.lm(lmcar)
plot(lmcar)
scatter3D(x=wt, y=mpg, z=hp)

scatter3d(x=wt, y=mpg, z=hp)
############################################################check for the installing of the libraries
#correlations nigga
library(corrgram)
library(Hmisc)
corrgram(mtcars)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
        ewef upper.panel = panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Car Mileage Data in PC2/PC1 Order")
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")
detach(mtcars)
######################################################
attach(iris)
names(iris)
iris
corrgram(iris, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)
corrgram(iris, main= "yaaas")
library(PerformanceAnalytics)
install.packages("PerformanceAnalytics")
rcorr(as.matrix(iris))
typeof(iris)
iris
corrgram(iris, order = TRUE, lower.panel= panel.ellipse,
         upper.panel = panel.pie, text.panel = panel.txt)
chart.Correlation(mtcars, histogram = TRUE, pch=19)
chart.Correlation(iris, histogram = TRUE) #why doesn't it work???
#######################################################3333

corrgram(ChickWeight, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pie, text.panel=panel.txt,
         diag.panel=panel.minmax)
#############################################################
#contingency analysis
#check for independence between categories
#always do with x^2
encuestas

attach(encuestas)
detach(encuestas)
CrossTable(encuestas$gender, encuestas$ed2, chisq = TRUE)


#tryout
ed2<- subset(encuestas, education==c("primaria", "secundaria", "bachillerato"), select =gender:books_year)
ed2
CrossTable(ed2$gender:ed2$education,ed2$books_year, chisq = TRUE)
#########################################################

library(latexpdf)
li
tinytex::install_tinytex()

layout(matrix(c(1,1),1,1))
######################################################################
###ANCOVA
ipomopsis
#covariable is for roots
#response is the fruit
#ANCOVA can be made with lm or anova
#first the factor, then the covariable
anc1<-lm(Fruit~Grazing*Root, data=ipomopsis)
anc1
anova(anc1)
anc2<- lm(Fruit~Root*factor(Grazing),data= ipomopsis)
anc2
summary(anc2)
summary(anc1)#use this one 

Anova(anc1, type= "III")

#need to learn how to read
plot(anc2)
plot(anc1)
#NORMAL Q-Q, LAS ESQUINAS SE VAN A SALIR, NO VAN SOBRE LA LINEA 
#SI LOS SUPUESTOS NO SE CUMPLEN, SE PUEDE UTILIZAR OTRO TIPO DE ANALISIS. 
##GRAFICA. 
#plot in the way i want it to
ggplot(ipomopsis, aes(x=Root, y=Fruit, colour=Grazing, shape=Grazing)) + geom_point(shape=7) +
  geom_smooth(method=lm,  
              se=FALSE, 
              fullrange=TRUE)+scale_colour_manual(values=c("blue","purple"))+scale_shape_manual(values=c(16,22))



#################################################################
#Modelos lineales generalizados
#generalizados porque son generalidad de todo lo que hemos visto antes
#variables que afectan variable de respuesta
#generalized linear models GLMs
#make something like an anova, explain
#can do even without the residuals fitting a normal distribution
#####Error structure: aleatory component that accounts the distribution of the errors, can adjust with different 
#families like, normal, binomial, poisson, gamma, binomial negative, etc
##### lineal predictor: systematic compound, the model compares each observed value with the expected and, the 
#expected values is obtained by transforming the value of the linear predictor. compares the observed with the predicted, 
##### Link function: function that links the aleatory component and the systematic component. links the 
#expected value of y wit the predictors (factors) of the model
### are many link functions depending on the error structure, search for canonical link depending on the error's family

###Examples
names(cells)
head(cells)
attach(cells)
cells #with the 0 values, shows it won't have a normal distribution
#anova, is wrong, but still do
cel1<- aov(cells~smoker*age*sex*weight, data = cells)
cel1
hist(cells)
summary.aov(cel1)
layout(matrix(c(1,2,3,4),2,2))
plot(cel1) #the assumptions are not true, therefore can't be trusted
#will now make the correct thing with GLM
#how to know which distribution? with library fitdistrplus
#will use function descdist
descdist(cells$cells, discrete = TRUE, boot = 500) #the name of the dataframe and the variable of 
#response ($), we tell if its variable is discrete or not with the TRUE or FALSE, boot=bootstrap and 
#recounts everuthing to obtain error
#kurtosis is the height of the distribution, the skewness shows how does the distribution goes to a side or another
#the legend will show by closeness to which distribution model it better fits
#to modify the distribution, we use the function fitdist
fit.poiss<- fitdist(cells$cells, "pois")
fit.poiss
plot(fit.poiss)
#has an option of saying its a quasi-whatever
fit1<- glm(cells~smoker*age*sex*weight,data = cells, family = quasipoisson())
summary(fit1) #NA appears when there are not enough of that combination, if that occurs, we take out 
#that interaction, this case is the 4 interactions
#to avpid writting everything again, we make a new object with update function
fit2<- update(fit1, ~. *smoker*age*weight*sex,family= quasipoisson, data= cells)
summary(fit2)
#########################################
#supuestos de anova y todo lo dem?s
#Errores tienen distribuci?n gaussina
#homegeidad de varianzas
#errores y la variable de respuesta son independientes


anx<- aov(ansiedad~edad+sexo+edad*sexo,
          data=Libro1.1.2)
summary.aov(anx)

