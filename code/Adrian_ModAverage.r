
##you must install packages unmarked and AICcmodavg for these examples
##they are both available on the Comprehensive R Archive Network (CRAN)

##usually, import data with read.table( )
bandurria<-read.csv("data/bandurria1.csv")
##load the bullfrog occupancy data from the AICcmodavg package
library(AICcmodavg)
library(unmarked)



#to simplify use, create separate data sets
##detection data
counts <- bandurria


##site covariates (Inventadas)
site.covs <- data.frame(site.cov=c(1,2,3,4,5,5,4,3,2,1,1,2,3,4,5))
obs.Covs<- bandurria * rnorm(1,0.5,0.05) # inventada y al azar

##load unmarked package
library(unmarked)

##assemble in unmarkedFramePCount
bandurria.data <- unmarkedFramePCount(y = counts,
                                siteCovs = site.covs,
                                obsCovs = list(obs.error=obs.Covs))
##look at data
bandurria.data
##summary of formatted data set
summary(bandurria.data)

##run models
##note the double formula -- first elements are for p, the second for N
##constant detectability and N

m0 <- pcount(~ 1 ~ 1, data = bandurria.data, K=150)
m0
##detectability varies with survey, but N is constant
m1 <- pcount(~ site.cov ~ 1, data = bandurria.data, K=150)
m1
##detectability varies with survey, 
##and occupancy varies with survey
m2 <- pcount(~ site.cov ~ site.cov, data = bandurria.data, K=150)
m2
##constant detectability, but occupancy varies with reed presence
m3 <- pcount(~ obs.error ~ site.cov, data = bandurria.data, K=150)
m3

##do model selection and test goodness of fit
##using functions in AICcmodavg package

models <- fitList(
  'p(.)Abundance(.)' = m0,
  'p(site.cov)Abundance(.)' = m1,
  'p(site.cov)Abundance(site.cov)' = m2,
  'p(obs.error)Abundance(site.cov)' = m3)
  
  
modSel(models)

##set up candidate model list
Cands <- list(m0, m1, m2, m3) # eliminar modelos que no se quieran promediar
##assign meaningful names to each model
Model.names <- c("null", "'p(site.cov)Abundance(.)", # aca tambien eliminar para no promediar
                 "'p(site.cov)Abundance(site.cov)", "p(obs.error)Abundance(site.cov)")
##do model selection based on AICc 
aictab(cand.set = Cands, modnames = Model.names)
##possible to output tables in LaTeX format, see ?xtable.aictab

##evidence ratio between top-ranked model vs second-ranked model
evidence(aic.table = aictab(cand.set = Cands, modnames = Model.names))

##a single model has all the weight, but lets do multimodel inference on the 
##parameters appearing in top models
##model-averaged estimate 
modavg(cand.set = Cands, modnames = Model.names,
       parm = "site.cov", parm.type = "lambda") # promedio de los 4 modelos con la covariable de sitio

modavg(cand.set = Cands, modnames = Model.names,
       parm = "obs.error", parm.type = "detect") # promedio de los 4 modelos con la covariable de observacion

#### predecir abundancia con el modelo promedio y la covariable site.cov de 1 a 15
output <- modavgPred(cand.set = Cands, modnames = Model.names, newdata =
                       data.frame(site.cov = seq(from = 1, to = 15, by = 0.1)),
                                parm.type = "lambda",
                                type = "response")

