## ----style, echo = FALSE, results = 'asis'------------------------------------
  # BiocStyle::markdown()

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ----setup,  eval=TRUE, echo=FALSE, include=FALSE-----------------------------
library(bmstdr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

knitr::opts_chunk$set(eval = F)
longrun <- FALSE
tabs <- lapply(list.files(system.file('txttables', package = 'bmstdr'), full.names = TRUE), dget)

# fns <- list.files(system.file('txttables', package = 'bmstdr')) 
# how the table data and file names match 
table1 <- tabs[[1]]
table2 <- tabs[[4]]
table3 <- tabs[[5]]
table4 <- tabs[[6]]
table5 <- tabs[[7]]
table6 <- tabs[[8]]
table7 <- tabs[[9]]
table8 <- tabs[[10]]
table9 <- tabs[[11]]
table10 <- tabs[[2]]
table11 <- tabs[[3]]

tablepath <- system.file('last3tables', package = 'bmstdr')
print(tablepath)

table4.1 <- dget(file=paste0(tablepath, "/table4.1.txt"))
table4.2 <- dget(file=paste0(tablepath, "/table4.2.txt"))
table4.3 <- dget(file=paste0(tablepath, "/table4.3.txt"))

## -----------------------------------------------------------------------------
#  M1 <- Bspatial(formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial, mchoice=T)

## -----------------------------------------------------------------------------
#  M2 <- Bspatial(model="spat", formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial,
#        coordtype="utm", coords=4:5, phi=0.4, mchoice=T)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  asave <- phichoice_sp()
#  print(asave)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  M3 <- Bspatial(package="spBayes", formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial,
#                 coordtype="utm", coords=4:5, prior.phi=c(0.005, 2), mchoice=T)
#  M4 <- Bspatial(package="stan", formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial,
#                 coordtype="utm", coords=4:5,phi=0.4, mchoice=T)
#  M5  <- Bspatial(package="inla",formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial,
#                 coordtype="utm", coords=4:5, mchoice=T)

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  a3 <- Bmchoice(case="MC.sigma2.unknown", y=ydata)
#  # Now organize the all the results for forming Table 1.
#  a5 <- rep(NA, 11)
#  a5[c(1, 3, 5, 7, 9:11)] <- unlist(M5$mchoice)
#  table1 <- cbind.data.frame(unlist(a3), M1$mchoice, M2$mchoice, M3$mchoice, M4$mchoice,  a5)
#  colnames(table1) <- paste("M", 0:5, sep="")

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  Bmchoice(case="MC.sigma2.unknown", y=ydata).

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  s <- c(8,11,12,14,18,21,24,28)
#  f1 <- yo3~xmaxtemp+xwdsp+xrh
#  M1.v <-  Bspatial(package="none", model="lm", formula=f1,
#                    data=nyspatial, validrows=s)
#  M2.v <- Bspatial(package="none", model="spat", formula=f1,
#          data=nyspatial,   coordtype="utm", coords=4:5,phi=0.4,  validrows=s)
#  M3.v <-  Bspatial(package="spBayes", prior.phi=c(0.005, 2), formula=f1,
#          data=nyspatial,   coordtype="utm", coords=4:5, validrows=s)
#  M4.v  <- Bspatial(package="stan",formula=f1,
#      data=nyspatial,   coordtype="utm", coords=4:5,phi=0.4 , validrows=s)
#  M5.v  <- Bspatial(package="inla", formula=f1, data=nyspatial,
#          coordtype="utm", coords=4:5, validrows=s)

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
set.seed(44)
x <- runif(n=28)
u <- order(x)
s1 <- u[1:7]
s2 <- u[8:14]
s3 <- u[15:21]
s4 <- u[22:28]

## ----valplot1, echo=T, eval=FALSE, message=FALSE, results='hide', fig.cap="Prediction against observation plot with the prediction intervals included. The `in/out' symbol in the plot indicates whether or not a prediction interval incudes the 45 degree line."----
#  M2.v3 <- Bspatial(model="spat", formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial,
#                 coordtype="utm", coords=4:5, validrows= s3, phi=0.4, verbose = FALSE)

## ----echo=T, eval=FALSE, message=FALSE, results='hide'------------------------
#  names(M2.v3)
#  psums <- get_validation_summaries(M2.v3$valpreds)
#  names(psums)
#  a <- obs_v_pred_plot(yobs=M2.v3$yobs_preds$yo3, predsums=psums, segments=F, summarystat = "mean" )

## ----echo=TRUE, eval=FALSE, message=FALSE, results='hide'---------------------
#  f2 <- y8hrmax~xmaxtemp+xwdsp+xrh
#  M1 <- Bsptime(model="lm", formula=f2, data=nysptime, scale.transform = "SQRT", N=5000)
#  M2 <- Bsptime(model="separable", formula=f2, data=nysptime, scale.transform = "SQRT",
#                coordtype="utm", coords=4:5,  N=5000)

## ----echo=TRUE, eval=FALSE, message=FALSE, results='hide'---------------------
#  a <- residuals(M1, numbers=list(sn=28, tn=62))

## ----ptime, echo=T, eval=T, fig.cap="Weekly Covid-19 death rate per 100,000"----
engdeaths$covidrate <- 100000*engdeaths$covid/engdeaths$popn
ptime <- ggplot(data=engdeaths,  aes(x=factor(Weeknumber), y=covidrate)) +
  geom_boxplot() +
  labs(x = "Week", y = "Death rate per 100,000")  +
  stat_summary(fun=median, geom="line", aes(group=1, col="red")) +
  theme(legend.position = "none")
ptime

## ----echo=T, eval=T, message=FALSE, results='hide'----------------------------
Ncar <- 50000
burn.in.car <- 10000
thin <- 10

## ----echo=TRUE, eval=FALSE, message=FALSE, results='hide'---------------------
#  f1 <- noofhighweeks ~ jsa + log10(houseprice) + log(popdensity) + sqrt(no2)

## ----echo=T, eval=FALSE, message=FALSE, results='hide'------------------------
#  M1 <- Bcartime(formula=f1,   data=engtotals, family="binomial",
#  trials=engtotals$nweek, N=Ncar, burn.in=burn.in.car, thin=thin)

## ----echo=T, eval=FALSE, message=FALSE, results='hide'------------------------
#  M1.leroux <- Bcartime(formula=f1, data=engtotals, scol="spaceid",
#  model="leroux", W=Weng, family="binomial", trials=engtotals$nweek,
#  N=Ncar, burn.in=burn.in.car, thin=thin)

## ----echo=T, eval=FALSE, message=FALSE, results='hide'------------------------
#  M1.bym <- Bcartime(formula=f1, data=engtotals,
#  scol="spaceid", model="bym", W=Weng, family="binomial",
#  trials=engtotals$nweek, N=Ncar, burn.in=burn.in.car, thin=thin)

## ----echo=T, eval=FALSE, message=FALSE, results='hide'------------------------
#  M1.inla.bym <- Bcartime(formula=f1, data=engtotals, scol ="spaceid",
#  model=c("bym"),  W=Weng, family="binomial", trials=engtotals$nweek,
#  package="inla", N=Ncar, burn.in=burn.in.car, thin=thin)

## ----echo=T, eval=FALSE, message=FALSE, results='hide'------------------------
#  a <- rbind(M1$mchoice, M1.leroux$mchoice, M1.bym$mchoice)
#  a <- a[, -(5:6)]
#  a <- a[, c(2, 1, 4, 3)]
#  b <- M1.inla.bym$mchoice[1:4]
#  a <- rbind(a, b)
#  rownames(a) <- c("Independent", "Leroux", "BYM", "INLA-BYM")
#  colnames(a) <- c("pDIC", "DIC", "pWAIC",  "WAIC")
#  table4.1 <- a

## ----echo=T, eval=FALSE-------------------------------------------------------
#  f2 <-  covid ~ offset(logEdeaths) + jsa + log10(houseprice) + log(popdensity) + sqrt(no2)

## ----echo=T, eval=FALSE, message=FALSE, results='hide'------------------------
#  M2 <- Bcartime(formula=f2, data=engtotals, family="poisson",
#                N=Ncar, burn.in=burn.in.car, thin=thin)
#  
#  M2.leroux <- Bcartime(formula=f2, data=engtotals,
#              scol="spaceid",  model="leroux",  family="poisson", W=Weng,
#              N=Ncar, burn.in=burn.in.car, thin=thin)
#  
#  M2.bym <- Bcartime(formula=f2, data=engtotals,
#                     scol="spaceid",  model="bym",  family="poisson", W=Weng,
#                     N=Ncar, burn.in=burn.in.car, thin=thin)
#  
#  M2.inla.bym <- Bcartime(formula=f2, data=engtotals, scol ="spaceid",
#                          model=c("bym"), family="poisson",
#                          W=Weng, offsetcol="logEdeaths", link="log",
#                            package="inla", N=Ncar, burn.in = burn.in.car, thin=thin)
#  

## ----echo=FALSE, eval=FALSE, message=FALSE, results='hide'--------------------
#  a <- rbind(M2$mchoice, M2.leroux$mchoice, M2.bym$mchoice)
#  a <- a[, -(5:6)]
#  a <- a[, c(2, 1, 4, 3)]
#  b <- M2.inla.bym$mchoice[1:4]
#  a <- rbind(a, b)
#  rownames(a) <- c("Independent", "Leroux", "BYM",   "INLA-BYM")
#  colnames(a) <- c("pDIC", "DIC", "pWAIC",  "WAIC")
#  table4.2 <- a
#  dput(table4.2, file=paste0(tablepath, "/table4.2.txt"))

## ----echo=T, eval=FALSE-------------------------------------------------------
#  f3 <-  sqrt(no2) ~  jsa + log10(houseprice) + log(popdensity)

## ----echo=T, eval=FALSE, message=FALSE, results='hide'------------------------
#  M3 <- Bcartime(formula=f3, data=engtotals, family="gaussian",
#                 N=Ncar, burn.in=burn.in.car, thin=thin)
#  
#  M3.leroux <- Bcartime(formula=f3, data=engtotals,
#                        scol="spaceid",  model="leroux",  family="gaussian", W=Weng,
#                        N=Ncar, burn.in=burn.in.car, thin=thin)
#  
#  
#  M3.inla.bym <- Bcartime(formula=f3, data=engtotals, scol ="spaceid",
#                          model=c("bym"), family="gaussian",
#                          W=Weng,  package="inla", N=Ncar, burn.in =burn.in.car, thin=thin)
#  

## ----echo=T, eval=FALSE, message=FALSE, results='hide'------------------------
#  a <- rbind(M3$mchoice, M3.leroux$mchoice)
#  a <- a[, -(5:6)]
#  a <- a[, c(2, 1, 4, 3)]
#  b <- M3.inla.bym$mchoice[1:4]
#  a <- rbind(a, b)
#  rownames(a) <- c("Independent", "Leroux",  "INLA-BYM")
#  colnames(a) <- c("pDIC", "DIC", "pWAIC",  "WAIC")
#  table4.3 <- a
#  dput(table4.3, file=paste0(tablepath, "/table4.3.txt"))

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  nweek <- rep(1, nrow(engdeaths))

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  f1 <- highdeathsmr ~  jsa + log10(houseprice) + log(popdensity)

## ----echo=T, eval=FALSE-------------------------------------------------------
#  M1st <- Bcartime(formula=f1, data=engdeaths, scol=scol, tcol=tcol, trials=nweek,
#  W=Weng, model="linear", family="binomial", package="CARBayesST",  N=Ncar,
#  burn.in=burn.in.car, thin=thin)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  f2 <-  covid ~ offset(logEdeaths) + jsa + log10(houseprice) + log(popdensity) + n0 + n1 + n2 + n3

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  M2st <- Bcartime(formula=f2, data=engdeaths, scol=scol, tcol=tcol,  W=Weng,
#  model="ar", family="poisson", package="CARBayesST", N=Ncar, burn.in=burn.in.car, thin=thin)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  vs <- sample(nrow(engdeaths), 0.1*nrow(engdeaths))

## ----obsvpredplot-m2st, echo=TRUE, eval=FALSE, message=FALSE, results='hide',  fig.show='hide'----
#  f20 <-  covid ~ offset(logEdeaths) + jsa + log10(houseprice) + log(popdensity) + n0
#  model <- c("bym", "ar1")
#  f2inla <-  covid ~  jsa + log10(houseprice) + log(popdensity) + n0
#  set.seed(5)
#  vs <- sample(nrow(engdeaths), 0.1*nrow(engdeaths))
#  M2st_ar2.0 <- Bcartime(formula=f20, data=engdeaths, scol="spaceid", tcol= "Weeknumber",
#                         W=Weng, model="ar", AR=2, family="poisson", package="CARBayesST",
#                         N=Ncar, burn.in=burn.in.car, thin=thin,
#                         validrows=vs, verbose=F)
#  M2stinla.0  <- Bcartime(data=engdeaths, formula=f2inla, W=Weng, scol ="spaceid", tcol="Weeknumber",
#                          offsetcol="logEdeaths",  model=model,  link="log", family="poisson", package="inla", validrow=vs, N=N, burn.in=0)
#  yobspred <- M2st_ar2.0$yobs_preds
#  names(yobspred)
#  yobs <- yobspred$covid
#  predsums <- get_validation_summaries(t(M2st_ar2.0$valpreds))
#  dim(predsums)
#  b <- obs_v_pred_plot(yobs, predsums, segments=T)
#  names(M2stinla.0)
#  inlapredsums <- get_validation_summaries(t(M2stinla.0$valpreds))
#  dim(inlapredsums)
#  a <- obs_v_pred_plot(yobs, inlapredsums, segments=T)
#  inlavalid <- a$pwithseg
#  ar2valid <- b$pwithseg
#  library(ggpubr)
#  ggarrange(ar2valid, inlavalid, common.legend = TRUE, legend = "top", nrow = 2, ncol = 1)
#  figpath <- system.file("figures", package = "bmstdr")
#  ggsave(filename = paste0(figpath, "/figure11.png"))

## ----obsvpredplot, echo=FALSE, eval=TRUE, fig.cap="Predictions with 95% limits against observations for two models: AR (2) on the left panel and INLA on the right panel", fig.width=1.2----
figpath <- system.file("figures", package = "bmstdr") 
knitr::include_graphics(paste0(figpath, "/figure11.png"))

## ----echo=T, eval=FALSE-------------------------------------------------------
#  M3st <- Bcartime(formula=f3, data=engdeaths, scol=scol, tcol=tcol,
#  W=Weng, model="ar", family="gaussian", package="CARBayesST",
#  N=Ncar, burn.in=burn.in.car, thin=thin)

