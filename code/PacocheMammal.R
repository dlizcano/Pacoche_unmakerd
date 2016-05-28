

# library(xtable)
library(lubridate)
library(dplyr)
library(maptools)
# Load 'rgdal' package, which is used to read/write shapefiles and rasters
library(rgdal)
source("C:/code/ULEAM/Infor_Caract/pacoche/code/TEAM_code.R")
source("C:/code/ULEAM/Infor_Caract/pacoche/code/calendar.R")


#############################
### antes de leer cambiar manualmente en editor de texto:
## enero, diciembre, abril, agosto  POR:  jan, dec, apr, aug
pacoche.raw<-read.csv("C:/code/ULEAM/Infor_Caract/pacoche/data/CT-RVP-2014e.csv") # ojo falta la camara 3-12



#############################
#### date fix
#############################

# unique(year(pacoche.raw$camera_trap_start_time))
pacoche.raw$photo_date2<-as.Date(as.character(pacoche.raw$photo_date), "%Y-%m-%d")

pacoche.raw$Sampling.Period<-2014 ##### ad the samplin period for 
pacoche.raw$binomial<-paste(pacoche.raw$genus, pacoche.raw$specise, sep = " ")

pacoche.raw$camera_trap_start_date<-as.Date(substr(as.character(pacoche.raw$camera_trap_start_time), start=1, stop=11), "%d-%b-%Y")
pacoche.raw$camera_trap_end_date<-as.Date(substr(as.character(pacoche.raw$camera_trap_end_time), start=1, stop=11), "%d-%b-%Y")

#################################
# Fix 2013 problem
# A camera has date wrong
# CT-RVP-2-13  and  CT-RVP-1-3
# Fix start date manually
################################
# identify the problem
# index_problem_2011<-which(pacoche.raw$camera_trap_start_date == "2013-11-11")
# problem_2011<-pacoche.raw[index_problem_2011,]
# unique(problem_2011$camera_trap)


# # fix 2-9 Time difference of 1124 days
index_2_9<-which(pacoche.raw$camera_trap == "CT-RVP-2-9")
cam_2_9<-pacoche.raw[index_2_9, ] # make data frame using the cam
pacoche.raw<-pacoche.raw[-index_2_9 ,] # remove the cam
#cam_2_9$photo_date2<-cam_2_13$photo_date2 + 1124
cam_2_9$camera_trap_start_date<-as.Date("2014-12-08", format="%Y-%m-%d")
cam_2_9$camera_trap_end_date<-as.Date("2015-02-04", format="%Y-%m-%d")



# # fix 2-13 Time difference of 1124 days
index_2_13<-which(pacoche.raw$camera_trap == "CT-RVP-2-13")
cam_2_13<-pacoche.raw[index_2_13, ] # make data frame using the cam
pacoche.raw<-pacoche.raw[-index_2_13 ,] # remove the cam
cam_2_13$photo_date2<-cam_2_13$photo_date2 + 1124
cam_2_13$camera_trap_start_date <-cam_2_13$camera_trap_start_date+1124
cam_2_13$camera_trap_end_date   <-cam_2_13$camera_trap_end_date+1124

# # fix 2-4 Time difference of start_date
index_2_4<-which(pacoche.raw$camera_trap == "CT-RVP-2-4")
cam_2_4<-pacoche.raw[index_2_4, ] # make data frame using the cam
pacoche.raw<-pacoche.raw[-index_2_4 ,] # remove the cam
cam_2_4$camera_trap_start_date<-as.Date("2014-12-08", format="%Y-%m-%d")
cam_2_4$camera_trap_end_date<-as.Date("2015-02-03", format="%Y-%m-%d")


# # fix 1-3 Time difference of  457 days
index_1_3<-which(pacoche.raw$camera_trap == "CT-RVP-1-3")
cam_1_3<-pacoche.raw[index_1_3, ] # make data frame using the cam
pacoche.raw<-pacoche.raw[-index_1_3 ,] # remove the cam
cam_1_3$camera_trap_end_date<-as.Date("2014-12-01", format="%Y-%m-%d")
cam_1_3$camera_trap_start_date<-as.Date("2014-10-06", format="%Y-%m-%d")
cam_1_3$photo_date2<-cam_1_3$photo_date2 + 457


# # fix 1-4 Time difference of start_date
index_1_4<-which(pacoche.raw$camera_trap == "CT-RVP-1-4")
cam_1_4<-pacoche.raw[index_1_4, ] # make data frame using the cam
pacoche.raw<-pacoche.raw[-index_1_4 ,] # remove the cam
cam_1_4<-cam_1_4[-c(1:3),] # borra 3 primeros porblematic 2011
cam_1_4$camera_trap_start_date<-as.Date("2014-10-06", format="%Y-%m-%d")
cam_1_4$camera_trap_end_date<-as.Date("2014-12-01", format="%Y-%m-%d")

## fix 1-1 Time difference of 114 days days
index_1_1<-which(pacoche.raw$camera_trap == "CT-RVP-1-1")
cam_1_1<-pacoche.raw[index_1_1, ] # make data frame using the cam
pacoche.raw<-pacoche.raw[-index_1_1 ,] # remove the cam
cam_1_1$camera_trap_start_date<-as.Date("2014-10-06", format="%Y-%m-%d")
cam_1_1$camera_trap_end_date<-as.Date("2014-12-01", format="%Y-%m-%d")
cam_1_1$photo_date2<-cam_1_1$photo_date2 + 118


# ############ fix NA problem
# naproblem<-which(is.na(pacoche.raw$camera_trap_start_date))
# cam_na<-pacoche.raw[naproblem, ] # make data frame using the cam
# pacoche.raw<-pacoche.raw[-naproblem ,] # remove the cam
# cam_na$camera_trap_start_date<- as.Date("2014-12-08", format="%Y-%m-%d") 


#### Add corrected
pacoche.raw<-rbind(pacoche.raw, cam_1_3, cam_1_4, cam_1_1,
                   cam_2_13, cam_2_4, cam_2_9)
                   # cam_na )


# pacoche.raw<-rbind(pacoche.raw, cam_1_10)
# pacoche.raw<-rbind(pacoche.raw, cam_3_10)
# pacoche.raw<-rbind(pacoche.raw, cam_3_07)

#  problematic ?
# which(pacoche.raw$photo_date2 == "2011-11-11")
# which(pacoche.raw$camera_trap == "CT-PNM-1-10")




########## extract yr and month
pacoche.raw$year<-year(pacoche.raw$photo_date2)
pacoche.raw$month<-month(pacoche.raw$photo_date2)





####################################
# make photo calendar type
####################################

# f.calendar.yr(dataset = pacoche.raw, yr_toplot = 1)
# f.calendar.yr(dataset = pacoche.raw, yr_toplot = 2)

#  f.calendar.yr(dataset = pacoche.raw, yr_toplot = 1)
#  f.calendar.yr(dataset = pacoche.raw, yr_toplot = 2)

# Especies registradas

library(xtable)
library(dplyr)


####################### eliminar aca el array 4 FALTA ####################
mat.per.sp<-f.matrix.creator2(data = pacoche.raw,year = 2014) ## cchk si deja 2015 out

sp.names<-names(mat.per.sp) # species names

# counting how many (total) records per species by all days
cont.per.sp<-data.frame(row.names = sp.names)
row.per.sp<-as.data.frame(matrix(nrow = length(sp.names), ncol=c(63))) # 
col.per.sp<-as.data.frame(matrix(nrow = length(sp.names), ncol=c(194)))
rownames(row.per.sp)<-sp.names
rownames(col.per.sp)<-sp.names

for (i in 1:length(mat.per.sp)){
  ############### chequear porque da diferente!!!!!!
  cont.per.sp[i,1]<-sum(apply(as.data.frame(mat.per.sp [[i]]),FUN=sum,na.rm=T, MARGIN = 1))
  
  row.per.sp[i,]<-apply(mat.per.sp[[i]],1, function(x) sum(x, na.rm=T))
  # row.per.sp[i,which(row.per.sp[i,]>0)]<-1 # convert to presence absence  1 and 0
  
  col.per.sp[i,]<-apply(mat.per.sp[[i]],2, function(x) sum(x, na.rm=T))
  # col.per.sp[i,which(col.per.sp[i,]>0)]<-1 # convert to presence absence  1 and 0
  
}

# colnames(row.per.sp)<-rep
# cont.per.sp$especie<-rownames(cont.per.sp)
# colnames(cont.per.sp)<-c("Numero_de_fotos","especie")
#xtable(arrange(df = cont.per.sp, desc(Numero_de_fotos)))
cont.per.sp
library(knitr)
kable(cont.per.sp, format = "html", caption = "Table 1. Eventos por especie")
xtable(cont.per.sp)
############################################################
## Distribucion posterior de la riqueza de especies
############################################################

# Riqueza de especies y acumulación, modelando la ocurrencia y la detectabilidad. 
# Este análisis sigue el método de Dorazio et al. (2006).

library("R2OpenBUGS")
source("C:/code/ULEAM/Infor_Caract/pacoche/code/MultiSpeciesSiteOcc.R")

# library(dplyr)
# colapse mat in: sp by row site by column

# Chk names in sp.names
row.per.sp<-row.per.sp[-40,] # elimina Carduelis siemiradzkii
row.per.sp<-row.per.sp[-39,] # elimina Damophila julie
row.per.sp<-row.per.sp[-38,] # elimina Momotus momota
row.per.sp<-row.per.sp[-36,] # elimina Columbina cruziana
row.per.sp<-row.per.sp[-35,] # elimina Pipistrellus pipistrellus
row.per.sp<-row.per.sp[-33,] # elimina Gallus gallus 
row.per.sp<-row.per.sp[-31,] # elimina Buteogallus urubitinga 
row.per.sp<-row.per.sp[-30,] # elimina Coragyps atratus 
row.per.sp<-row.per.sp[-28,] # elimina Zenaida auriculata
# row.per.sp<-row.per.sp[-26,] # elimina Damophila julie 
row.per.sp<-row.per.sp[-22,] # elimina Ortalis vetula
row.per.sp<-row.per.sp[-14,] # elimina Ortalis erythroptera
row.per.sp<-row.per.sp[-11,] # elimina Homo sapiens
row.per.sp<-row.per.sp[-10,] # elimina Leptotila verreauxi
row.per.sp<-row.per.sp[-08,] # elimina Tinamus major
row.per.sp<-row.per.sp[-01,] # elimina 1era especie vacia

######### CHK names
rownames(row.per.sp)


# Numero de especies observadas
length(row.per.sp[,1])


X1 = as.matrix(row.per.sp) # col.per.sp por dias y row.per.sp por sitios (camaras)
nrepls = 90 #dias 
#######################################################################
especies = MultiSpeciesSiteOcc(nrepls = nrepls, X = X1) ##### J U S T   W A I T
#######################################################################

 # summary(especies$fit$sims.matrix)

alpha.post = especies$fit$sims.matrix[,"alpha"]
sigmaU.post = especies$fit$sims.matrix[,"sigma.u"]
N.post = especies$fit$sims.matrix[,"N"]

nsites = 60 
cum_sp<-CumNumSpeciesPresent(nsites, alpha.post, sigmaU.post, N.post)

#histogram of posteriors
hist(especies$fit$sims.matrix[,"N"],breaks = c(24:52), xlab="Number of mammal species", ylab="Relative frecuency", main="")
abline(v=length(row.per.sp[,1]),col="blue", lty = 2) # -> lines.histogram(*) observadas
abline(v=median(especies$fit$sims.matrix[,"N"]),col="red", lty = 2) # -> esperadas por detectabilidad



mean(especies$fit$sims.matrix[,"N"])
median(especies$fit$sims.matrix[,"N"])


#############################################################
#### over park limit


library(maptools)
library(rgdal) 
library(spatstat)

pacochelimit <- readShapePoly(fn="C:\\Users\\Diego\\Documents\\CodigoR\\ULEAM\\Infor_Caract\\shp\\pacoche",
                              IDvar="boundary")
geo <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #def cord
proj4string(pacochelimit)<-geo # set cords



################################### 
### get just cameras

just.cam1<-select(pacoche.raw, camera_trap, latitude, longitude)
just.cams<-distinct(just.cam1)

coordinates(just.cams) <- ~longitude+latitude #make spatial data frame
proj4string(just.cams)<-geo # set cords

############ over
cam_in_out<-over(x = just.cams, y = pacochelimit)
# add camera name to table
cam_in_out$camera_trap<-just.cams$camera_trap

index.in<-which(cam_in_out$boundary == "protected_area")
index.out<-which(is.na(cam_in_out$boundary))

just.in<-just.cams[index.in,]
just.out<-just.cams[index.out,]

##### cuantas camaras in
length(index.in)

##### cuantas camaras out
length(index.out)

######## plot to check
plot(just.out, col="red" )
plot(just.in,  add=T)
plot(pacochelimit, add=T)


plot(pacochelimit)
plot(just.in, col="red", add=T)
text(just.in, labels=just.out$camera_trap, cex = .5)




############# select by vector

pacoche.in<-pacoche.raw[is.element(pacoche.raw$camera_trap, just.in$camera_trap),]
pacoche.out<-pacoche.raw[is.element(pacoche.raw$camera_trap, just.out$camera_trap),]


##########################################
################ make mat per species
##########################################

mat.per.sp.in<-f.matrix.creator2(data = pacoche.in,year = 2014) ## cchk si deja 2015 out
mat.per.sp.out<-f.matrix.creator2(data = pacoche.out,year = 2014) ## cchk si deja 2015 out

sp.names.in<-names(mat.per.sp.in) # species names
sp.names.out<-names(mat.per.sp.out) # species names



################ Count ################ 
################ in IN
# counting how many (total) records per species by all days
dim(mat.per.sp.in[[5]])

cont.per.sp.in<-data.frame(row.names = sp.names.in)
row.per.sp.in<-as.data.frame(matrix(nrow = length(sp.names.in), ncol=c(46)))
col.per.sp.in<-as.data.frame(matrix(nrow = length(sp.names.in), ncol=c(194)))
rownames(row.per.sp.in)<-sp.names.in
rownames(col.per.sp.in)<-sp.names.in

for (i in 1:length(mat.per.sp.in)){
  cont.per.sp.in[i,1]<-sum(apply(as.data.frame(mat.per.sp.in [[i]]),FUN=sum,na.rm=T, MARGIN = 1))
  
  row.per.sp.in[i,]<-apply(mat.per.sp.in[[i]],1, function(x) sum(x, na.rm=T))
  # row.per.sp[i,which(row.per.sp[i,]>0)]<-1 # convert to presence absence  1 and 0
  
  col.per.sp.in[i,]<-apply(mat.per.sp.in[[i]],2, function(x) sum(x, na.rm=T))
  # col.per.sp[i,which(col.per.sp[i,]>0)]<-1 # convert to presence absence  1 and 0
  
}

row.per.sp.in<-row.per.sp.in[-35,] # elimina Carduelis siemiradzkii
row.per.sp.in<-row.per.sp.in[-34,] # elimina Damophila julie
row.per.sp.in<-row.per.sp.in[-33,] # elimina Momotus momota
#row.per.sp.in<-row.per.sp[-32,] # elimina Gallus gallus
# row.per.sp.in<-row.per.sp[-33,] # elimina Buteogallus urubitinga 
row.per.sp.in<-row.per.sp.in[-31,] # elimina Columbina cruziana 
row.per.sp.in<-row.per.sp.in[-30,] # elimina Gallus gallus 
row.per.sp.in<-row.per.sp.in[-29,] # elimina Coragyps atratus 
row.per.sp.in<-row.per.sp.in[-27,] # elimina Zenaida auriculata
#row.per.sp.in<-row.per.sp[-25,] # elimina Damophila julie 
row.per.sp.in<-row.per.sp.in[-20,] # elimina Ortalis vetula
row.per.sp.in<-row.per.sp.in[-13,] # elimina Ortalis erythroptera
row.per.sp.in<-row.per.sp.in[-11,] # elimina Homo sapiens
row.per.sp.in<-row.per.sp.in[-10,] # elimina Leptotila verreauxi
row.per.sp.in<-row.per.sp.in[-08,] # elimina Tinamus major
row.per.sp.in<-row.per.sp.in[-01,] # elimina 1era especie vacia

######### CHK names
rownames(row.per.sp.in)

#################### #################### #################### 
#################### Acumula in 

# get dimensions
dim(mat.per.sp.in[[5]])

X2 = as.matrix(row.per.sp.in) # col.per.sp por dias y row.per.sp por sitios (camaras)
nrepls = 90 #dias 
especies.in = MultiSpeciesSiteOcc(nrepls, X2)

# summary(especies$fit$sims.matrix)

alpha.post = especies.in$fit$sims.matrix[,"alpha"]
sigmaU.post = especies.in$fit$sims.matrix[,"sigma.u"]
N.post = especies.in$fit$sims.matrix[,"N"]

nsites = 46 
cum_sp.in<-CumNumSpeciesPresent(nsites, alpha.post, sigmaU.post, N.post)

#histogram of posteriors
hist(especies.in$fit$sims.matrix[,"N"],breaks = c(24:40), xlab="Number of mammal species", ylab="Relative frecuency", main="")
abline(v=length(row.per.sp.in[,1]),col="blue", lty = 2) # -> lines.histogram(*) observadas
abline(v=median(especies.in$fit$sims.matrix[,"N"]),col="red", lty = 2) # -> esperadas por detectabilidad


################ Count ################ 
################ in OUT
# counting how many (total) records per species by all days

# get dimensions
dim(mat.per.sp.out[[2]])

cont.per.sp.out<-data.frame(row.names = sp.names.out)
row.per.sp.out<-as.data.frame(matrix(nrow = length(sp.names.out), ncol=c(17))) 
col.per.sp.out<-as.data.frame(matrix(nrow = length(sp.names.out), ncol=c(187)))
rownames(row.per.sp.out)<-sp.names.out
rownames(col.per.sp.out)<-sp.names.out

for (i in 1:length(mat.per.sp.out)){
  cont.per.sp.out[i,1]<-sum(apply(as.data.frame(mat.per.sp.out [[i]]),FUN=sum,na.rm=T, MARGIN = 1))
  
  row.per.sp.out[i,]<-apply(mat.per.sp.out[[i]],1, function(x) sum(x, na.rm=T))
  # row.per.sp[i,which(row.per.sp[i,]>0)]<-1 # convert to presence absence  1 and 0
  
  col.per.sp.out[i,]<-apply(mat.per.sp.out[[i]],2, function(x) sum(x, na.rm=T))
  # col.per.sp[i,which(col.per.sp[i,]>0)]<-1 # convert to presence absence  1 and 0
  
}


# row.per.sp.out<-row.per.sp[-35,] # elimina Carduelis siemiradzkii
# row.per.sp.out<-row.per.sp[-34,] # elimina Damophila julie
# row.per.sp.out<-row.per.sp[-33,] # elimina Momotus momota
# row.per.sp.out<-row.per.sp[-32,] # elimina Gallus gallus
# row.per.sp.out<-row.per.sp[-33,] # elimina Buteogallus urubitinga 
# row.per.sp.out<-row.per.sp[-31,] # elimina Columbina cruziana 
# row.per.sp.out<-row.per.sp[-30,] # elimina Gallus gallus 
# row.per.sp.out<-row.per.sp[-29,] # elimina Coragyps atratus 
# row.per.sp.out<-row.per.sp[-27,] # elimina Zenaida auriculata
row.per.sp.out<-row.per.sp.out[-25,] # elimina Pipistrellus pipistrellus 
row.per.sp.out<-row.per.sp.out[-22,] # elimina Ortalis vetula
row.per.sp.out<-row.per.sp.out[-17,] # elimina Homo sapiens
row.per.sp.out<-row.per.sp.out[-15,] # elimina Gallus gallus
row.per.sp.out<-row.per.sp.out[-13,] # elimina Buteogallus urubitinga
row.per.sp.out<-row.per.sp.out[-11,] # elimina Leptotila verreauxi
row.per.sp.out<-row.per.sp.out[-08,] # elimina Tinamus major
row.per.sp.out<-row.per.sp.out[-04,] # elimina Ortalis erythroptera
row.per.sp.out<-row.per.sp.out[-01,] # elimina 1era especie vacia

# CHK rownames(row.per.sp.out)

######### CHK names
rownames(row.per.sp.out)
length(rownames(row.per.sp.out))
length(rownames(row.per.sp.in))
#################### #################### #################### 
#################### Acumula OUT 


# get dimensions
dim(mat.per.sp.out[[5]])

X3 = as.matrix(row.per.sp.out) # col.per.sp por dias y row.per.sp por sitios (camaras)
nrepls = 187 #dias 
especies.out = MultiSpeciesSiteOcc(nrepls, X3)


# summary(especies$fit$sims.matrix)

alpha.post = especies.out$fit$sims.matrix[,"alpha"]
sigmaU.post = especies.out$fit$sims.matrix[,"sigma.u"]
N.post = especies.out$fit$sims.matrix[,"N"]

nsites = 17 
cum_sp.out<-CumNumSpeciesPresent(nsites, alpha.post, sigmaU.post, N.post)

#histogram of posteriors
hist(especies.out$fit$sims.matrix[,"N"],breaks = c(20:46), xlab="Number of mammal species", ylab="Relative frecuency", main="")
abline(v=length(row.per.sp.out[,1]),col="blue", lty = 2) # -> lines.histogram(*) observadas
abline(v=median(especies.out$fit$sims.matrix[,"N"]),col="red", lty = 2) # -> esperadas por detectabilidad



###########################################################
####################### fi  IN and OUT ####################


par(mfrow=c(2, 1))

#histogram of posteriors
hist(especies.in$fit$sims.matrix[,"N"],breaks = c(20:46), xlab="", ylab="Frecuencia relativa", main="dentro del RVSMCP")
abline(v=length(row.per.sp.in[,1]),col="blue", lty = 2) # -> lines.histogram(*) observadas
abline(v=median(especies.in$fit$sims.matrix[,"N"]),col="red", lty = 2) # -> esperadas por detectabilidad

#histogram of posteriors
hist(especies.out$fit$sims.matrix[,"N"],breaks = c(20:46), xlab="Número de especies de mamíferos", ylab="Frecuencia relativa", main="fuera del RVSMCP")
abline(v=length(row.per.sp.out[,1]),col="blue", lty = 2) # -> lines.histogram(*) observadas
abline(v=median(especies.out$fit$sims.matrix[,"N"]),col="red", lty = 2) # -> esperadas por detectabilidad





  


################## Taxatize
library(taxize)

############# mammal in
taxonomia.in<-matrix(nrow = length(sp.names.in),ncol = 5)
colnames(taxonomia.in)= c("db","query", "Class", "family", "order")
rownames(taxonomia.in)<-sp.names.in
taxonomia.in<-as.data.frame(taxonomia.in)
for (i in 2:length(sp.names.in)) { 
  taxonomia.in[i,]<-as.vector(tax_name(query = sp.names.in[i], get = c("Class", "family", "order"), db = "itis"))
}

############# mammal out
taxonomia.out<-matrix(nrow = length(sp.names.out),ncol = 5)
colnames(taxonomia.out)= c("db","query", "Class", "family", "order")
rownames(taxonomia.out)<-sp.names.out
taxonomia.out<-as.data.frame(taxonomia.out)
for (i in 2:length(sp.names.out)) { 
  taxonomia.out[i,]<-as.vector(tax_name(query = sp.names.out[i], get = c("Class", "family", "order"), db = "ncbi"))
}



################ Delete in in
row.per.sp.in<-row.per.sp[-39,] # elimina Momotus momota
row.per.sp<-row.per.sp[-37,] # elimina Columbina cruziana
row.per.sp<-row.per.sp[-36,] # elimina Pipistrellus pipistrellus
row.per.sp<-row.per.sp[-33,] # elimina Gallus gallus
row.per.sp<-row.per.sp[-31,] # elimina Buteogallus urubitinga 
row.per.sp<-row.per.sp[-28,] # elimina Carduelis siemiradzkii
row.per.sp<-row.per.sp[-27,] # elimina Damophila julie 
row.per.sp<-row.per.sp[-23,] # elimina Ortalis vetula
row.per.sp<-row.per.sp[-14,] # elimina Ortalis erythroptera
row.per.sp<-row.per.sp[-11,] # elimina Homo sapiens
row.per.sp<-row.per.sp[-10,] # elimina Leptotila verreauxi
row.per.sp<-row.per.sp[-08,] # elimina Tinamus major
row.per.sp<-row.per.sp[-01,] # elimina 1era especie vacia



##########################################
#########  predict 
##########################################

#  covariates of detection and occupancy in that order.
# fm8 <- occu(~ basal_a ~ elev, sp_UMF)

chisq <- function(fm) {
  umf <- getData(fm)
  y <- getY(umf)
  y[y>1] <- 1
  sr <- fm@sitesRemoved
  if(length(sr)>0)
    y <- y[-sr,,drop=FALSE]
  fv <- fitted(fm, na.rm=TRUE)
  y[is.na(fv)] <- NA
  sum((y-fv)^2/(fv*(1-fv)), na.rm=TRUE)
}

(pb <- parboot(fm8, statistic=chisq, nsim=500, report=5))
plot(pb)
re <- ranef(fm8)
EBUP <- bup(re, stat="mode")
CI <- confint(re, level=0.9)
sum(bup(re, stat="mode"))

backTransform(fm8)

# estimate detection effect at obsvars= 20
(lc <- linearComb(fm8['det'],c(1,20)))
(btlc <- backTransform(lc))
confint(btlc, level = 0.9)

# estimate occupancy effect at obsvars= 1000
(lc <- linearComb(fm8['state'],c(1,100)))
(btlc <- backTransform(lc))
confint(btlc, level = 0.9)


confint(fm8, type='det', method = 'normal')
confint(fm8, type='det', method = 'profile')

newdata <- data.frame(basal_a=seq(0, 20, length=50), elev=seq(0, 666, length=50) ) # 
predict(fm8, type="state", newdata=newdata)
predict(fm8, type="det", newdata=newdata)



