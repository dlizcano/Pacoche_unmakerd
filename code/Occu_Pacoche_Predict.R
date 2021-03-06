
---
  title: Informe Final del Proyecto "Caracterización ecológica de la fauna mediana y grande en áreas protegidas de la provincia de Manabí" \break http://faunamanabi.github.io/ \break Modelos de Ocupación, bajo el modelo estático (MacKenzie et al. 2002) para para las especies de mamíferos en el Parque Nacional Machalilla
author: Departamento Central de Investigaciones (DCI), \break Universidad Laica Eloy
Alfaro de Manabi (ULEAM) \break Diego J. Lizcano \break Laura Cervera \break Violeta Pares 
date: "Septiembre, 2015"
output:
  pdf_document:
  highlight: tango
number_sections: yes
toc: yes
toc_depth: 3
html_document:
  toc: yes
word_document:
  highlight: kate
license: CC
box: grey
bibliography: C:/code/ULEAM/Infor_Caract/citas_simula.bib

---
  
  
  \newcommand{\HRule}[1]{\hfill \rule{1\linewidth}{#1}} % Horizontal rule at the bottom of the page, adjust width here
    
    %%%%%% Logo
    \HRule{1pt} 
    
    \vspace{0.25in}
    \centerline {
      \includegraphics[width=2in]{C:/code/ULEAM/Infor_Caract/machalilla/img/ULEAM_DCI.png}
    }
    \vspace{0.1in}
    
    
    \HRule{1pt} 
    
    
    ```{r setup, include=FALSE}
    # cache all... if problem... delete cache folder
    knitr::opts_chunk$set(cache=TRUE)
    ```
    
    # La ocupación de hábitat
    
    Obtener datos para estudios de poblaciones animales, es costoso y dispendioso, y no siempre se puede medir la densidad poblacional o parámetros demográficos como natalidad o mortalidad [@Morrison2002]. Estimar adecuadamente la densidad poblacional requiere de un elevado número de registros, con los costos económicos y logísticos que esto conlleva [@Morrison2006]. Es por eso que la estimación de la ocupación de hábitat ($\psi$) es una buena herramienta para estudiar poblaciones, ya que es un fiel reflejo de otros parámetros poblacionales importantes como la abundancia y la densidad poblacional [@MacKenzie2002@MacKenzie2006]. Sin embargo y debido a que la detectabilidad (_p_) en animales silvestres no es completa o perfecta, el uso de los datos crudos genera subestimaciones de la ocupación del hábitat. Pero, con el empleo de muestreos repetidos, es posible generar estimaciones de la detectabilidad y, con esta estimación, obtener valores no sesgados de la ocupación del hábitat. La incorporación de covariables permite explicar mejor la heterogeneidad de la relación entre la probabilidad de detección y el hábitat, pero estas relaciones deben ser establecidas para poder interpretar correctamente los resultados de los modelos de hábitat [@Gu2004].
    
    Los métodos de análisis de ocupación son una familia de modelos que permiten realizar inferencias acerca de los efectos de variables continuas y categóricas sobre la ocupación del hábitat [@Bailey2013@Iknayan2014]. Además, si los muestreos se realizan a través de períodos largos y sucesivos de tiempo, también es posible estimar tasas de extinción y recolonización, que son útiles en estudios de metapoblaciones. Este es un campo de gran desarrollo en bioestadística que ha producido una gran explosión de estudios que usan la ocupación teniendo en cuenta la detectabilidad [@Royle2008; @Royle2012].  
    
    # Duración del muestreo
    
    Las trampas cámara permanecieron activas desde final de septiembre 2014 hasta comienzos de marzo 2015. La siguiente figura ilustra el periodo de actividad de cada trampa cámara, sus fechas de instalación, fechas de retirada, y su funcionamiento diario. 
    
    
    ```{r calendar1,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE, error=FALSE, message=FALSE, fig.height=11,fig.width=8}
    
    library(xtable)
    library(lubridate)
    library(dplyr)
    library(maptools)
    # Load 'rgdal' package, which is used to read/write shapefiles and rasters
    library(rgdal)
    source("C:/code/ULEAM/Infor_Caract/code/TEAM_code.R")
    source("C:/code/ULEAM/Infor_Caract/code/calendar.R")
    
    machalilla.raw<-read.csv("C:/code/ULEAM/Infor_Caract/Data/CT-PNM-2014.csv") # ojo falta la camara 3-12
    
    #############################
    #### date fix
    #############################
    
    # unique(year(machalilla.raw$camera_trap_start_time))
    machalilla.raw$photo_date2<-as.Date(as.character(machalilla.raw$photo_date), "%d-%b-%Y")
    
    machalilla.raw$Sampling.Period<-2014
    machalilla.raw$binomial<-paste(machalilla.raw$genus, machalilla.raw$specise, sep = " ")
    
    #############################
    # translate months
    #############################
    # 
    # meses<-as.data.frame(t(matrix(c("ago","aug","dic","dec","ene","jan","abr","apr"),nrow = 2,ncol = 4)))
    # machalilla.raw$camera_trap_start_time2<-NA
    # machalilla.raw$camera_trap_end_time2<-NA
    # for (i in 1:4){  
    # # get month
    #   chkmes<-substr(as.character(machalilla.raw$photo_date), start=4, stop=6)
    #   chkmes2<-substr(as.character(machalilla.raw$camera_trap_start_time), start=4, stop=6)
    #   chkmes3<-substr(as.character(machalilla.raw$camera_trap_end_time), start=4, stop=6)
    #   # chane month
    #   agoind<-which(chkmes == meses[i,1])
    #   agoind2<-which(chkmes2 == meses[i,1])
    #   agoind3<-which(chkmes3 == meses[i,1])
    #   # machalilla.raw$photo_date[agoind]
    #   machalilla.raw$photo_date2[agoind]<-as.Date(gsub(as.character(meses[i,1]), 
    #                                                      as.character(meses[i,2]), 
    #                                                      machalilla.raw$photo_date[agoind]), "%d-%b-%Y")
    #   
    #   machalilla.raw$camera_trap_start_time2[agoind2]<-as.Date(gsub(as.character(meses[i,1]), 
    #                                                    as.character(meses[i,2]), 
    #                                                    machalilla.raw$camera_trap_start_time[agoind2]), "%d-%b-%Y")
    #   
    #   machalilla.raw$camera_trap_end_time2[agoind3]<-as.Date(gsub(as.character(meses[i,1]), 
    #                                                                as.character(meses[i,2]), 
    #                                                                machalilla.raw$camera_trap_end_time[agoind3]), "%d-%b-%Y")
    # }
    # 
    # # tochk
    # # unique(machalilla.raw$photo_date2)
    
    
    machalilla.raw$camera_trap_start_date<-as.Date(substr(as.character(machalilla.raw$camera_trap_start_time), start=1, stop=11), "%d-%b-%Y")
    machalilla.raw$camera_trap_end_date<-as.Date(substr(as.character(machalilla.raw$camera_trap_end_time), start=1, stop=11), "%d-%b-%Y")
    
    #################################
    # Fix 2011 problem
    # Two cameras have date wrong
    # CT-PNM-1-7  and  CT-PNM-3-10
    # Fix start date manually
    ################################
    # identify the problem
    # index_problem_2011<-which(machalilla.raw$camera_trap_start_date == "2011-11-11")
    # problem_2011<-machalilla.raw[index_problem_2011,]
    # unique(problem_2011$camera_trap)
    
    
    
    # fix 1-7 just year
    index_1_07<-which(machalilla.raw$camera_trap == "CT-PNM-1-7")
    cam_1_07<-machalilla.raw[index_1_07, ] # make data frame using the cam
    machalilla.raw<-machalilla.raw[-index_1_07 ,] # remove the cam
    cam_1_07$camera_trap_start_date<-as.Date("2014-09-23", format="%Y-%m-%d")
    cam_1_07<-cam_1_07[-1,] # borra el primero porblematic 2011
    cam_1_07$photo_type[1]<-"Start" # fix start
    cam_1_07$camera_trap_end_date<-as.Date("2014-11-06", format="%Y-%m-%d")
    
    # fix 1-10  # add 31 days since data setup pickup en data 12 and beyond
    index_1_10<-which(machalilla.raw$camera_trap == "CT-PNM-1-10")
    cam_1_10<-machalilla.raw[index_1_10, ]
    machalilla.raw<-machalilla.raw[-index_1_10 ,]
    
    cam_1_10$camera_trap_start_date<-as.Date("2014-09-23", format="%Y-%m-%d") 
    cam_1_10$camera_trap_end_date<-as.Date("2014-11-05", format="%Y-%m-%d")
    cam_1_10$photo_date2[c(12:875)]<-cam_1_10$photo_date2[c(12:875)] +31
    # delete problematic data
    cam_1_10<-cam_1_10[-24,]
    cam_1_10<-cam_1_10[-25,]
    
    # fix 3-10  # restar 30 dias
    index_3_10<-which(machalilla.raw$camera_trap == "CT-PNM-3-10")
    cam_3_10<-machalilla.raw[index_3_10, ]
    machalilla.raw<-machalilla.raw[-index_3_10 ,]
    
    cam_3_10$camera_trap_start_date<-as.Date("2015-01-27", format="%Y-%m-%d") 
    cam_3_10$camera_trap_end_date<-as.Date("2015-03-15", format="%Y-%m-%d")
    # borra los primeros 30 problematicos con fecha 2011
    cam_3_10<-cam_3_10[-c(1:30),]
    cam_3_10$photo_date2<-cam_3_10$photo_date2  - 30
    
    
    # fix 3-07 # add difference of 365 days
    index_3_07<-which(machalilla.raw$camera_trap == "CT-PNM-3-07")
    cam_3_07<-machalilla.raw[index_3_07, ]
    
    machalilla.raw<-machalilla.raw[- index_3_07 ,]
    cam_3_07$camera_trap_start_date<-as.Date("2015-01-27", format="%Y-%m-%d")
    cam_3_07$camera_trap_end_date<-as.Date("2015-03-11", format="%Y-%m-%d")
    cam_3_07$photo_date2<- cam_3_07$photo_date2 + 365
    
    ### remove from machalilla.raw
    # machalilla.raw<-machalilla.raw[-index_1_07 ,]
    # machalilla.raw<-machalilla.raw[-index_3_10 ,]
    # machalilla.raw<-machalilla.raw[-index_3_07 ,]
    
    #### Add corrected
    machalilla.raw<-rbind(machalilla.raw, cam_1_07, cam_1_10, cam_3_10, cam_3_07)
    # machalilla.raw<-rbind(machalilla.raw, cam_1_10)
    # machalilla.raw<-rbind(machalilla.raw, cam_3_10)
    # machalilla.raw<-rbind(machalilla.raw, cam_3_07)
    
    #  problematic ?
    # which(machalilla.raw$photo_date2 == "2011-11-11")
    # which(machalilla.raw$camera_trap == "CT-PNM-1-10")
    
    
    ########## extract yr and month
    machalilla.raw$year<-year(machalilla.raw$photo_date2)
    machalilla.raw$month<-month(machalilla.raw$photo_date2)
    # problem?
    # which(machalilla.raw$year == "2011")
    
    
    ####################################
    # make photo calendar type
    ####################################
    
    f.calendar.yr(dataset = machalilla.raw, yr_toplot = 1)
    # f.calendar.yr(dataset = machalilla.raw, yr_toplot = 2)
    
    
    ```
    
    
    ```{r calendar2,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE, error=FALSE, message=FALSE, fig.height=11,fig.width=8}
    f.calendar.yr(dataset = machalilla.raw, yr_toplot = 2)
    ```
    
    # Especies registradas
    
    Las especies registradas en el Parque Nacional Machalilla fueron 36
    
    
    ```{r sps,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    library(xtable)
    library(dplyr)
    mat.per.sp<-f.matrix.creator2(data = machalilla.raw,year = 2014)
    sp.names<-names(mat.per.sp) # species names
    
    # counting how many (total) records per species by all days
    cont.per.sp<-data.frame(row.names = sp.names)
    row.per.sp<-as.data.frame(matrix(nrow = length(sp.names), ncol=c(59)))
    col.per.sp<-as.data.frame(matrix(nrow = length(sp.names), ncol=c(161)))
    rownames(row.per.sp)<-sp.names
    rownames(col.per.sp)<-sp.names
    
    for (i in 1:length(mat.per.sp)){
      cont.per.sp[i,1]<-sum(apply(as.data.frame(mat.per.sp [[i]]),FUN=sum,na.rm=T, MARGIN = 1))
      
      row.per.sp[i,]<-apply(mat.per.sp[[i]],1, function(x) sum(x, na.rm=T))
      # row.per.sp[i,which(row.per.sp[i,]>0)]<-1 # convert to presence absence  1 and 0
      col.per.sp[i,]<-apply(mat.per.sp[[i]],2, function(x) sum(x, na.rm=T))
      
    }
    
    cont.per.sp$especie<-rownames(cont.per.sp)
    colnames(cont.per.sp)<-c("Numero_de_registros","especie")
    xtable(arrange(df = cont.per.sp, desc(Numero_de_registros)))
    ```
    
    
    ## Distribucion posterior de la riqueza de especies
    
    Riqueza de especies y acumulación, modelando la ocurrencia y la detectabilidad. Este análisis sigue el método de Dorazio et al. [-@Dorazio2006].
    
    ```{r richnessmacha,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,fig.height=5,fig.width=8,message=FALSE}
    
    source("C:/code/ULEAM/Infor_Caract/code/MultiSpeciesSiteOcc.R")
    
    # library(dplyr)
    # colapse mat in: sp by row site by column
    
    row.per.sp<-row.per.sp[-1,] # elimina especie vacia del comienzo
    
    X1 = as.matrix(row.per.sp) # col.per.sp por dias y row.per.sp por sitios (camaras)
    nrepls = 90 #dias 
    especies = MultiSpeciesSiteOcc(nrepls, X1)
    
    # summary(especies$fit$sims.matrix)
    
    alpha.post = especies$fit$sims.matrix[,"alpha"]
    sigmaU.post = especies$fit$sims.matrix[,"sigma.u"]
    N.post = especies$fit$sims.matrix[,"N"]
    
    nsites = 59 
    cum_sp<-CumNumSpeciesPresent(nsites, alpha.post, sigmaU.post, N.post)
    
    #histogram of posteriors
    hist(especies$fit$sims.matrix[,"N"],breaks = c(35:80), xlab="Number of mammal species", ylab="Relative frecuency", main="")
    abline(v=38,col="blue", lty = 2) # -> lines.histogram(*)
    
    
    
    
    
    ```
    
    La media de la distribucion posterior es `r mean(especies$fit$sims.matrix[,"N"])`. Mientras que la mediana `r median(especies$fit$sims.matrix[,"N"])`. 
    
    
    # Covariables 
    
    Inicialmente se probó con cinco covariables para ajustar los modelos de ocupación. Estas cinco covariables fueron: Altitud (elev), Pendiente (slope) y Distancia a la carretera pavimentada (dis_rd) como covariables geográficas y altura del dosel, cobertura del dosel y área basal como covariables que se midieron momento de retirar las cámaras. Estas medidas se tomaron usando la metodología del cuadrante centrado en un punto. 
    
    La Altitud se obtuvo de una imagen SRTM del repositorio de [CGIAR](http://srtm.csi.cgiar.org/). La pendiente se infirió a partir de la altitud y la distancia a las carreteras se obtuvo de un mapa del Ministerio del Medio Ambiente del Ecuador.
    
    
    
    ```{r covs,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE, error=FALSE, message=FALSE, fig.height=10,fig.width=8}
    library(raster)
    library(rgdal)
    library(dismo)
    library(biomod2)
    library(spatstat)
    library(sp)
    library(dplyr)
    library(maptools)
    
    machalilla.raw<-read.csv("C:/code/ULEAM/Infor_Caract/Data/CT-PNM-2014.csv") # ojo falta la camara 3-12
    
    long<-unique(machalilla.raw$longitude)
    lati<-unique(machalilla.raw$latitude)
    centercoord<-c(mean(subset(long, long<=1)),mean(unique(subset(lati, lati<=1))))
    coordsubset<-subset(machalilla.raw,select = c(camera_trap,longitude,latitude,first_name_set_camera))
    
    #################################
    # get elevation
    ################################
    
    # elevation<-getData('SRTM',lon=centercoord[1], lat=centercoord[2])
    
    # read elevation from disk
    elevation<- raster("C:/code/ULEAM/Infor_Caract/srtm_20_13.tif")
    
    cam.cords<-as.data.frame(distinct(coordsubset))
    coordinates(cam.cords) <- ~longitude+latitude #make sppatial data fram
    geo <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #def cord
    proj4string(cam.cords)<-geo # set cords
    
    e<-extent (-80.9, -80.5, -1.75,-1.35)
    elevation.crop<-crop(elevation, e)
    
    plot(elevation.crop)
    plot(cam.cords, add=T, col="red")
    title(main="Altitud", sub="en color rojo se muestra donde se instalaron las camaras")
    
    slope<-terrain(elevation.crop, opt='slope', unit='degrees', neighbors=4)
    plot(slope)
    plot(cam.cords, add=T, col="red")
    title(main="Pendiente", sub="en color rojo se muestra donde se instalaron las camaras")
    
    cam.cords.sp<-SpatialPoints(cam.cords)
    proj4string(cam.cords.sp)<-geo 
    # etract values
    elev.ovr <- extract(elevation.crop, cam.cords, method='bilinear')
    slope.ovr <- extract(slope, cam.cords, method='bilinear')
    
    # add to table
    cam.cords$elev<-elev.ovr
    cam.cords$slope<-slope.ovr
    
    ############################
    # road
    ############################
    
    roadpol <- readShapeSpatial("C:/Users/CaracterizaciónD/Documents/GitHub/machalilla/Machalilla_paper/shp/machalilla_roadsclip.shp")
    names(roadpol)<-"dist_rd"
    proj4string(roadpol)<-geo
    dist_rd<-over(x = cam.cords, y = roadpol)
    # add to table
    cam.cords$dist_rd<-as.numeric(dist_rd[,1])
    
    roadpol.ow<-as(as(roadpol, "SpatialPolygons"), "owin") # make owin
    cam.and.covs<-as.data.frame(cam.cords)
    
    plot(roadpol, col=topo.colors(65))
    plot(cam.cords, add=T, col="red")
    title(main="Distancia a las carreteras", sub="en color rojo se muestra donde se instalaron las camaras")
    
    ##################################
    ## Estructura Veget
    ##################################
    
    est.veget<-read.csv("C:/code/ULEAM/Infor_Caract/machalilla/data/estructVeget.csv") # read table
    
    ##################################
    ########## scale ?
    ##################################
    
    
    
    cam.and.covs<-cbind(cam.and.covs,est.veget) #Paste covs
    
    
    ```
    
    
    ```{r functions,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,results='asis', error=FALSE, message=FALSE}
    
    f.sp.occu.plot.mat <- function(sp_number){
      ########################
      ### shrink to 15
      ########################
      
      library(unmarked)
      
      sp15<-f.shrink.matrix.to15(matrix = mat.per.sp[[sp_number]])
      
      
      ########################
      ### make unmarked object 
      ########################
      
      sp_UMF <- unmarkedFrameOccu(sp15)
      
      plot(sp_UMF, panels=1)
      # title(main=as.character(sp.names[sp_number]))
      
    }
    
    ##########################################
    #########  Models 
    ##########################################
    
    f.sp.occu.models <- function(sp_number){
      ########################
      ### shrink to 15
      ########################
      
      library(unmarked)
      
      sp15<-f.shrink.matrix.to15(matrix = mat.per.sp[[sp_number]])
      
      
      ########################
      ### make unmarked object 
      ########################
      
      sp_UMF <- unmarkedFrameOccu(sp15)
      
      # plot(sp_UMF, panels=1)
      # title(main=as.character(sp.names[sp_number]))
      
      # add some  covariates
      siteCovs(sp_UMF) <- cam.and.covs
      
      #######################
      ## occu models 
      #######################
      
      #  covariates of detection and occupancy in that order.
      fm0 <- occu(~ 1 ~ 1, sp_UMF) 
      fm1 <- occu(~ 1 ~ elev, sp_UMF)
      fm2 <- occu(~ 1 ~ slope, sp_UMF)
      fm3 <- occu(~ 1 ~ dist_rd, sp_UMF)
      fm4 <- occu(~ elev ~ elev, sp_UMF)
      fm5 <- occu(~ elev ~ slope, sp_UMF)
      fm6 <- occu(~ elev ~ dist_rd, sp_UMF)
      fm7 <- occu(~ basal_a ~ 1, sp_UMF)
      fm8 <- occu(~ basal_a ~ elev, sp_UMF)
      fm9 <- occu(~ basal_a ~ canopy_c, sp_UMF)
      fm10 <- occu(~ 1 ~ canopy_c, sp_UMF)
      
      # put the names of each model
      models <- fitList(
        'p(.)psi(.)' = fm0,
        'p(.)psi(elev)' = fm1,
        'p(.)psi(slope)' = fm2,
        'p(.)psi(dist_rd)' = fm3,
        'p(elev)psi(elev)' = fm4,
        'p(elev)psi(slope)' = fm5,
        'p(elev)psi(dist_rd)' = fm6,
        'p(basal_a)psi(.)' = fm7,
        'p(basal_a)psi(elev)' = fm8,
        'p(basal_a)psi(canopy_c)' = fm9,
        'p(.)psi(canopy_c)' = fm10
      )
      
      ms <- modSel(models)
      # (ms)
      
      #This part store some models coeficients in a table (mat_models) to compare on screen
      ms_AIC_models<-as.data.frame(ms@ Full[1], row.names = NULL) #store model name
      modelo<-paste("_", as.character(as.character(sp.names[sp_number])), 
                    "_", " models", sep="") # fix model name addin species
      ma_nPars<-as.data.frame(ms@Full$nPars) #store parameter number
      ms_AIC_values<- as.data.frame(ms@Full$AIC) #store AIC values
      ms_AIC_delta<- as.data.frame(ms@Full$delta) #store AIC delta values
      ms_AIC_AICwt<- as.data.frame(ms@Full$AICwt) #store AIC wt values
      ms_AIC_cumultw<-as.data.frame(ms@Full$cumltvWt) #store model name
      ms_m<-as.data.frame(row.names(ms_AIC_models)) #store m number
      ms_formula<- as.data.frame(ms@Full$formula) #store model formula
      mat_models <- cbind(ms_AIC_models, ma_nPars, ms_AIC_values, ms_AIC_delta, ms_AIC_AICwt, ms_AIC_cumultw) #paste in matrix
      colnames(mat_models)<-c(modelo, "nPars",'AIC', "delta", "AICwt", "cumltvWt") # change row names
      
      ##Print los 7 primeros modelos
      xtable(mat_models)
      # print(spname)
      # print (mat_models[c(1:7),])
      # as.character(sp.names[sp_number])
      
    }
    
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
    
    
    
    ```
    
    
    #Algebra del modelo de ocupación
    
    
    Cada especie tiene una historia de detección que se usó para modelar la ocupación. Esta se calculó teniendo en cuenta que cada sitio tiene también su propia historia de detección. La historia de detección puede ser representada como una ecuación matemática intuitiva, donde la especie objetivo fue detectada como una secuencia de unos y ceros de cada día de detección en cada cámara, con uno si se tomó una foto ese día y cero si no se tomó foto. Ese sitio (cámara) estará ocupado ($\psi$), con una probabilidad que se calcula de su historia de detección de la siguiente forma para un sitio con una historia de detección 1001:
      
      $$Pr(H_{i} = 1001) =\psi * p_{1} (1-p_{2}) (1-p_{3}) p_{4}$$
      
      
      Mientras que un sitio que tuvo una historia de detección donde no se registró la especie podría ser un sitio que no está ocupado por la especie (1-$\psi$) o estar ocupado pero la especie nunca detectada lo cual sería:
      
      $$Pr(H_{i} = 0000) =\psi * (1-p_{1}) (1-p_{2}) (1-p_{3}) (1-p_{4}) \textup{ or } \psi \prod_{1}^{4} (1-p_{j}) + (1-\psi)$$
      
      Matemáticamente podríamos combinar todas las historias de detección en un modelo de máxima verosimilitud como: 
      
      $$L(\psi, p\mid H_{1}...H_{x+1}) = \prod_{1}^{x+1} \textup{Pr} (H_{i})$$
      
      Estos modelos pueden incorporar covariables que interactúan con la ocupación y la probabilidad de ocupación y pueden ser resueltos con la ayuda del paquete unmarked del lenguaje estadístico R.
    
    
    #Modelos de ocupacion por especie
    ## La Cabra (_Capra aegagrus_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp3,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 3)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps3,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE, error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=3]))
    f.sp.occu.models(sp_number = 3)
    ```
    
    ## El Cabeza de Mate (_Eira barbara_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp6,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE, error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 6)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps6,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=6]))
    f.sp.occu.models(sp_number = 6)
    ```
    
    ## El tigrillo (_Leopardus wiedii_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp9,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 9)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps9,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=9]))
    f.sp.occu.models(sp_number = 9)
    ```
    
    
    ## La vaca (_Bos primigenius_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp10,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 10)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps10,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=10]))
    f.sp.occu.models(sp_number = 10)
    ```
    
    ## El Perro domestico (_Canis lupus_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp2,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 2)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps2,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=2]))
    f.sp.occu.models(sp_number = 2)
    ```
    
    ## El Oso Hormiero (_Tamandua mexicana_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp5,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 5)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps5,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=5]))
    f.sp.occu.models(sp_number = 5)
    ```
    
    ## El Venado (_Odocoileus virginianus_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp8,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 8)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps8,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=8]))
    f.sp.occu.models(sp_number = 8)
    ```
    
    ## El Conejo (_Sylvilagus brasiliensis_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp11,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 11)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps11,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=11]))
    f.sp.occu.models(sp_number = 11)
    ```
    
    
    
    ## El Caballo (_Equus ferus_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp12,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 12)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps12,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=12]))
    f.sp.occu.models(sp_number = 12)
    ```
    
    
    ## La Guanta (_Cuniculus paca_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp13,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 13)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps13,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=13]))
    f.sp.occu.models(sp_number = 13)
    ```
    
    
    ## La Guatusa (_Dasyprocta punctata_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp19,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 19)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps19,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=19]))
    f.sp.occu.models(sp_number = 19)
    ```
    
    ## El Pecari (_Pecari tajacu_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp20,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 20)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps20,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=20]))
    f.sp.occu.models(sp_number = 20)
    ```
    
    ## El Armadillo (_Dasypus novemcinctus_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp22,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE, error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 22)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps22,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE, error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=22]))
    f.sp.occu.models(sp_number = 22)
    ```
    
    
    ## La Ardilla de Guayaquil (_Sciurus stramineus_)
    ### Matriz de datos colapsada a 15 dias
    
    ```{r mat_sp23,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE, error=FALSE, message=FALSE, fig.height=4,fig.width=7}
    f.sp.occu.plot.mat(sp_number = 23)
    ```
    
    ### Selección de Modelos
    ```{r modeo_sps23,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE, error=FALSE, message=FALSE, results='asis'}
    print(as.character(sp.names[sp_number=23]))
    f.sp.occu.models(sp_number = 23)
    ```
    
    
    # Predict part
    ## Function
    
    ```{r predict, cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE,results='asis', error=FALSE, message=FALSE}
    
    
    
    
    ```
    
    
    
    
    
    
    
    # Codigo en R
    ## Función para calendario
    ```{r calendarfunction, eval=TRUE, echo=TRUE, comment="", highlight=TRUE, prompt=FALSE, tidy=TRUE}
    print(f.calendar.yr)
    
    ```
    
    ## Función para análisis de ocupación
    ```{r occufunction, eval=TRUE, echo=TRUE, comment="", highlight=TRUE, prompt=FALSE, tidy=TRUE}
    print(f.sp.occu.models)
    
    ```
    
    
    ## Función para análisis multiespecie riqueza y acumulacion
    Riqueza de especies y acumulación, modelando la ocurrencia y la detectabilidad. Este codigo es adaptado del método de Dorazio et al. (2006).
    ```{r multispoccu, eval=TRUE, echo=TRUE, comment="", highlight=TRUE, prompt=FALSE, tidy=TRUE}
    print(MultiSpeciesSiteOcc)
    
    ```
    
    
    ## Información de sesión en R
    Y por último se proporciona la información de nuestra sesión en R para poder hacer "proper reproducible research".
    ```{r session_info, comment="", highlight=TRUE, prompt=FALSE, tidy=TRUE}
    sessionInfo()
    ```
    
    ## References
    
    