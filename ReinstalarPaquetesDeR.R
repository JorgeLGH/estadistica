paquetes_instalados<-installed.packages()

# Con la siguiente instrucción se guarda el objeto paquetes_instalados


saveRDS(paquetes_instalados,"C:/Users/fotgo/OneDrive/PaquetesQueTengo_Instalados_Roberto.rds")

# Con el nombre pongan la dirección en donde guardara ese documento, es importante que
# pongan la extensión rds
# En mi caso lo guardo en mi Desktop (RECUERDEN QUE EL CARACTER ~ ES UNA FORMA BREVE DE ESCRIBIR HOME)
# O pongan setwd("DIRECCIÓN DONDE LO QUIEREN GUARDAR") y sólo pongan el nombre


##########################
# 1. Quiten Rstudio y vayan a la página para descargar R (4.0)  https://cloud.r-project.org
# 2 Instalen R
# 2. Vuelvan a abrir RStudio
##############


paquetes_instalados<-readRDS("C:/Users/fotgo/OneDrive/PaquetesQueTengo_Instalados_Roberto.rds")

# Y sólo pongan 

install.packages(paquetes_instalados[,1])

#Listo!

