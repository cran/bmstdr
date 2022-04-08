## ----style, echo = FALSE, results = 'asis'------------------------------------
  BiocStyle::markdown()

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  # tidy.opts = list(width.cutoff = 60), 
  # tidy = TRUE, 
   collapse = TRUE,
   class.source="watch-out",
   comment = "#>")

## ----setup,  eval=TRUE, echo=FALSE, include=FALSE-----------------------------
library(bmstdr)
library(ggplot2)
require(ggsn)
library(tidyr)
library(huxtable)
library(RColorBrewer)
library(akima)
knitr::opts_chunk$set(eval = F)


tabs <- lapply(list.files(system.file('txttables', package = 'bmstdr'), full.names = TRUE), dget)
# print(tabs)

tablepath <- system.file('last3tables', package = 'bmstdr')
print(tablepath)

table4.1 <- dget(file=paste0(tablepath, "/table4.1.txt"))
table4.2 <- dget(file=paste0(tablepath, "/table4.2.txt"))
table4.3 <- dget(file=paste0(tablepath, "/table4.3.txt"))
print(table4.1)
print(table4.2)
print(table4.3)


figpath <- system.file("figs", package = "bmstdr") 
print(figpath)

table1 <-  tabs[[1]]
table10 <- tabs[[2]]
table11 <- tabs[[3]]
table2 <-  tabs[[4]]
table3 <-  tabs[[5]]
table4 <- tabs[[6]]
table5 <- tabs[[7]]
table6 <- tabs[[8]]
table7 <- tabs[[9]]
table8 <- tabs[[10]]
table9 <- tabs[[11]]
colpalette <- c("dodgerblue4",  "dodgerblue2",  "firebrick2",   "firebrick4",   "purple")     

## ----vsites3, echo=FALSE, eval=TRUE, fig.cap="25 fitted sites and 3 validation sites (numbered) in  New York", fig.width=6, fig.height=5----
nymap <- map_data(database="state",regions="new york")
s <- c(1, 5, 10)
fcoords <- nyspatial[-s, c("Longitude", "Latitude")]
vcoords <- nyspatial[s,  c("Longitude", "Latitude")]
library(tidyr)
label <- tibble(
   long = -76.1,
   lat = 41.5,
   label = "25 fitted (circles) & 3  \n  validation (numbered) sites"
)

vsites3 <- ggplot() +
   geom_polygon(data=nymap, aes(x=long, y=lat, group=group),
                color="black", size = 0.6, fill=NA) +
   geom_point(data =fcoords, aes(x=Longitude,y=Latitude)) +
   geom_text(data=vcoords, aes(x=Longitude,y=Latitude, label=s), col=4) +
   labs( title= "28 air pollution monitoring sites in New York", x="Longitude", y = "Latitude") +
   geom_text(aes(label=label, x=long, y=lat), data = label, vjust = "top", hjust = "right")  +
   # geom_rect(mapping=aes(xmin=-80.2, xmax=-77.3, ymin=41, ymax=41.6), color="black", fill=NA) + 
   geom_rect(mapping=aes(xmin=-78.7, xmax=-75.8, ymin=41, ymax=41.6), color="black", fill=NA) + 
   ggsn::scalebar(data =nymap, dist = 100, location = "bottomleft", transform=T, dist_unit = "km",
                  st.dist = .05, st.size = 5, height = .06, st.bottom=T, model="WGS84") +
   ggsn::north(data=nymap, location="topleft", symbol=12) 
vsites3

## ----chunk1, echo=TRUE, eval=FALSE--------------------------------------------
#  M1 <- Bspatial(formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial, mchoice=T)

## ----m2spatial, echo=TRUE, eval=FALSE-----------------------------------------
#  M2 <- Bspatial(model="spat", formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial,
#        coordtype="utm", coords=4:5, phi=0.4, mchoice=T)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  asave <- phichoice_sp(formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial, coordtype="utm", coords=4:5, phis=seq(from=0.1, to=1, by=0.1), scale.transform="NONE", s=c(8,11,12,14,18,21,24,28), N=2000, burn.in=1000)
#  asave

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  M3 <- Bspatial(package="spBayes", formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial,
#                 coordtype="utm", coords=4:5, prior.phi=c(0.005, 2), mchoice=T)
#  M4 <- Bspatial(package="stan", formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial,
#                 coordtype="utm", coords=4:5,phi=0.4, mchoice=T)
#  M5  <- Bspatial(package="inla",formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial,
#                 coordtype="utm", coords=4:5, mchoice=T)

## ---- echo=FALSE, eval=FALSE--------------------------------------------------
#  a3 <- Bmchoice(case="MC.sigma2.unknown", y=ydata)
#  # Now organize the all the results for forming Table 1.
#  a5 <- rep(NA, 11)
#  a5[c(1, 3, 5, 7, 9:11)] <- unlist(M5$mchoice)
#  table1 <- cbind.data.frame(unlist(a3), M1$mchoice, M2$mchoice, M3$mchoice, M4$mchoice,  a5)
#  colnames(table1) <- paste("M", 0:5, sep="")
#  round(table1,  2)
#  dput(table1, file=paste0(path, "table1.txt"))

## ----tab, echo=FALSE, eval=TRUE-----------------------------------------------
   table1 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen")) %>%
   add_colnames("Criteria") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>% 
   set_caption('(#tab:mchoicenyspatial) Model choice criteria for various models fitted to the nyspatial data set.')


## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  Bmchoice(case="MC.sigma2.unknown", y=ydata).

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
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

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
   table2[-4, ] %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(3)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen", "purple")) %>%
   add_colnames("Criteria") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red")%>% 
   set_caption('(#tab:mvalidnyspatial) Model validation statistics for the five models fitted to the nyspatial data set.')

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
set.seed(44)
x <- runif(n=28)
u <- order(x)
s1 <- u[1:7]
s2 <- u[8:14]
s3 <- u[15:21]
s4 <- u[22:28]

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
   table3 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(3)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen", "purple")) %>%
   add_colnames("Criteria") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>%
   set_caption('(#tab:m2-4-fold-validnyspatial) 4-fold cross-validation statistics for model M2 fitted to the nyspatial data set.')


## ----valplot1, echo=T, eval=T, message=FALSE, results='hide', fig.cap="Prediction against observation plot with the prediction intervals included. The `in/out' symbol in the plot indicates whether or not a prediction interval includes the 45 degree line.", fig.height=5, fig.width=6----
M2.v3 <- Bspatial(model="spat", formula=yo3~xmaxtemp+xwdsp+xrh, data=nyspatial, 
               coordtype="utm", coords=4:5, validrows= s3, phi=0.4, verbose = FALSE)

## ---- echo=TRUE, eval=FALSE, message=FALSE, results='hide'--------------------
#  names(M2.v3)
#  psums <- get_validation_summaries(M2.v3$valpreds)
#  names(psums)
#  a <- obs_v_pred_plot(yobs=M2.v3$yobs_preds$yo3, predsums=psums, segments=FALSE, summarystat = "mean" )

## ---- echo=TRUE, eval=TRUE, message=FALSE, results='hide'---------------------
f2 <- y8hrmax~xmaxtemp+xwdsp+xrh
M1 <- Bsptime(model="lm", formula=f2, data=nysptime, scale.transform = "SQRT")
M2 <- Bsptime(model="separable", formula=f2, data=nysptime, scale.transform = "SQRT",
              coordtype="utm", coords=4:5)

## ----residM2, echo=TRUE, eval=TRUE, fig.width=7, fig.height=5, fig.cap="A multiple time series plot of residuals"----
a <- residuals(M2)

## ---- echo=TRUE, eval=FALSE, message=FALSE, results='hide'--------------------
#  a <- residuals(M1, numbers=list(sn=28, tn=62))

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  M2$phi.s; M2$phi.t

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  asave <- phichoicep(formula=y8hrmax ~ xmaxtemp+xwdsp+xrh, data=nysptime,
#                  coordtype="utm", coords=4:5, scale.transform = "SQRT",  phis=c(0.001,  0.005, 0.025, 0.125, 0.625), phit=c(0.05, 0.25, 1.25, 6.25),
#  valids=c(8,11,12,14,18,21,24,28), N=2000, burn.in=1000)

## ----valplot21, echo=TRUE, eval=TRUE, message=FALSE, results='hide', fig.show='hide'----
valids <-  c(1, 5, 10)
vrows <-  which(nysptime$s.index%in% valids)
M2.1 <- Bsptime(model="separable",  formula=f2, data=nysptime, 
             validrows=vrows, coordtype="utm", coords=4:5,
             phi.s=0.005, phi.t=0.05, scale.transform = "SQRT")

## ---- echo=TRUE, eval=TRUE, message=FALSE-------------------------------------
summary(M2.1)

## ---- echo=TRUE, eval=TRUE,message=FALSE, results='hide'----------------------
M3 <- Bsptime(package="spTimer", formula=f2, data=nysptime, n.report=5, 
              coordtype="utm", coords=4:5, scale.transform = "SQRT")

## ---- echo=T, eval=T----------------------------------------------------------
set.seed(44)
tn <- 62
sn <- 28
valids <- c(1, 5, 10)
validt <- sort(sample(1:tn, size=31))
vrows <- getvalidrows(sn=sn, tn=tn, valids=valids, validt=validt)

## ---- echo=TRUE, eval=TRUE, message=FALSE, results='hide', fig.show='hide'----
M31 <- Bsptime(package="spTimer",formula=f2, data=nysptime, 
               coordtype="utm", coords=4:5,
               validrows=vrows, model="GP", report=5, 
               mchoice=F, scale.transform = "NONE")

## ---- echo=T, eval=T----------------------------------------------------------
modfit <- M31$fit
fitall <- data.frame(modfit$fitted)
fitall$s.index <- rep(1:sn, each=tn)
library(spTimer)
vdat <- spT.subset(data=nysptime, var.name=c("s.index"), s=valids)
fitvalid <- spT.subset(data=fitall, var.name=c("s.index"), s=valids)
fitvalid$low <- fitvalid$Mean - 1.96 * fitvalid$SD
fitvalid$up <- fitvalid$Mean + 1.96 * fitvalid$SD
fitvalid$yobs <- vdat$y8hrmax
yobs <- matrix(fitvalid$yobs, byrow=T, ncol=tn)
y.valids.low <- matrix(fitvalid$low, byrow=T, ncol=tn)
y.valids.med <- matrix(fitvalid$Mean, byrow=T, ncol=tn)
y.valids.up <- matrix(fitvalid$up, byrow=T, ncol=tn)

## ----valid3sites, echo=T, eval=T, fig.height=8, fig.width=7, fig.cap="Time series of observed and predicted values at three sites."----
p1 <- fig11.13.plot(yobs[1, ], y.valids.low[1, ], y.valids.med[1, ], 
                    y.valids.up[1, ], misst=validt)
p1 <- p1 + ggtitle("Validation for Site 1")
p2 <- fig11.13.plot(yobs[2, ], y.valids.low[2, ], y.valids.med[2, ], 
                    y.valids.up[2, ], misst=validt)
p2 <- p2 + ggtitle("Validation for Site 5")
p3 <- fig11.13.plot(yobs[3, ], y.valids.low[3, ], y.valids.med[3, ], 
                    y.valids.up[3, ], misst=validt)
p3 <- p3 + ggtitle("Validation for Site 10")
library(ggpubr)
ggarrange(p1, p2, p3, common.legend = TRUE, legend = "top", nrow = 3, ncol = 1)

## ---- echo=T, eval=T----------------------------------------------------------
sitemeans <- function(a, sn, tn=62) { 
   u <- matrix(a, nrow=sn, ncol=tn, byrow=T)
   b <- apply(u, 1, mean)
   as.vector(b)
}

## ---- echo=T, eval=T, message=FALSE, results='hide'---------------------------
post <- M3$fit
gpred <- predict(post, newdata=gridnysptime, newcoords=~Longitude+Latitude)
u <- gpred$pred.samples
v <- apply(u, 2, sitemeans, sn=100)
a <- get_parameter_estimates(t(v)) 
b <- data.frame(gridnyspatial[, 1:5], a) 

## ---- echo=TRUE, eval=TRUE, message=FALSE, results='hide'---------------------
meanmat <- post$op
sig2eps <-  post$sig2ep
sige <- sqrt(sig2eps)
itmax <- ncol(meanmat)
nT <- nrow(nysptime)
sigemat <- matrix(rep(sige, each=nT), byrow=F, ncol=itmax)
a <- matrix(rnorm(nT*itmax), nrow=nT, ncol=itmax)
ypreds <- meanmat + a * sigemat
ypreds <-  (ypreds)^2
v <- apply(ypreds, 2, sitemeans, sn=28)
a <- get_parameter_estimates(t(v)) 
fits <- data.frame(nyspatial[, 1:5], a)

## ---- echo=TRUE, eval=TRUE, message=FALSE, results='hide'---------------------
b <- rbind(b, fits)

## ---- echo=TRUE, eval=TRUE, message=FALSE, results='hide'---------------------
coord <- nyspatial[, c("Longitude","Latitude")]
library(akima)
xo <- seq(from=min(coord$Longitude)-0.5, to = max(coord$Longitude)+0.8, length=200)
yo <- seq(from=min(coord$Latitude)-0.25, to = max(coord$Latitude)+0.8, length=200)
surf <- interp(b$Longitude, b$Latitude, b$mean,  xo=xo, yo=yo)
v <- fnc.delete.map.XYZ(xyz=surf)

interp1 <- data.frame(long = v$x, v$z )
names(interp1)[1:length(v$y)+1] <- v$y
library(tidyr)
interp1 <- gather(interp1,key = lat,value =Predicted,-long,convert = TRUE)
library(ggplot2)
nymap <- map_data(database="state",regions="new york")
mappath <- cbind(nymap$long, nymap$lat)
zr <- range(interp1$Predicted, na.rm=T)
P <- ggplot() +  
geom_raster(data=interp1, aes(x = long, y = lat,fill = Predicted)) +
geom_polygon(data=nymap, aes(x=long, y=lat, group=group), color="black", size = 0.6, fill=NA) + 
geom_point(data=coord, aes(x=Longitude,y=Latitude))  +
stat_contour(data=na.omit(interp1), aes(x = long, y = lat,z = Predicted), colour = "black", binwidth =2) +
scale_fill_gradientn(colours=colpalette, na.value="gray95", limits=zr) +
theme(axis.text = element_blank(), axis.ticks = element_blank()) +
ggsn::scalebar(data =interp1, dist = 100, location = "bottomleft", transform=T, dist_unit = "km", st.dist = .05, st.size = 5, height = .06, st.bottom=T, model="WGS84") +
ggsn::north(data=interp1, location="topleft", symbol=12) +
labs(x="Longitude", y = "Latitude", size=2.5) 

## ---- echo=F, eval=T, fig.height=6, fig.width=8, fig.cap="Predicted map of average ozone air pollution in New York"----
P

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  M1.c <- Bsptime(model="lm", formula=f2, data=nysptime,
#            scale.transform = "SQRT", mchoice=T)
#  M2.c <- Bsptime(model="separable",  formula=f2, data=nysptime,
#            coordtype="utm", coords=4:5, phi.s=0.005, phi.t=0.05,
#            scale.transform = "SQRT", mchoice=T)
#  M3.c <- Bsptime(package="spTimer", model="GP",
#          formula=f2, data=nysptime, coordtype="utm",
#          coords=4:5, scale.transform = "SQRT",
#          mchoice=T, N=5000)
#  M4.c <- Bsptime(package="stan",formula=f2, data=nysptime,
#          coordtype="utm", coords=4:5, scale.transform = "SQRT",
#          N=1500, burn.in=500, mchoice=T, verbose = F)

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
   colnames(table4) <- c("M1", "M2", "M3", "M4")
   table4 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen", "purple")) %>%
   add_colnames(c("Criteria")) %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>% 
   set_caption('(#tab:mchoice-m1-m4) Model choice  criteria for the four spatio-temporal models M1 to M4.')

## ---- echo=TRUE, eval=FALSE, message=FALSE, results='hide', fig.show='hide'----
#  M5 <- Bsptime(package="spTimer", model="AR", formula=f2, data=nysptime,
#                  coordtype="utm", coords=4:5, scale.transform = "SQRT",
#                  mchoice=T,  validrows = vrows)
#  

## ---- echo=TRUE, eval=FALSE, message=FALSE, results='hide'--------------------
#  M6 <- Bsptime(package="inla", model="AR", formula=f2, data=nysptime,
#          coordtype="utm", coords=4:5, scale.transform = "SQRT",
#          mchoice=T, validrows=vrows)

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
   table5 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen")) %>%
   add_colnames("Criteria") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>%
   set_caption('(#tab:mcvm5m6) Model choice and validation statistics for the two AR models M5 and M6.')

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
   table6 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(3)     %>%
   map_text_color(by_cols("black", "darkred", "blue", "darkgreen", "purple", "darkred", "blue", "darkgreen", "purple")) %>%
   add_colnames("Criteria") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>% 
   set_caption('(#tab:mparm-m5-m6) Parameter estimates for the two AR models M5 (first four columns) and M6 (last four columns).')

## ---- echo=TRUE, eval=TRUE, message=FALSE, results='hide'---------------------
library(spTDyn)
f3 <- y8hrmax~ xmaxtemp + sp(xmaxtemp)+ tp(xwdsp) + xrh
M7 <- Bsptime(package="sptDyn", model="GP", formula=f3, data=nysptime, 
      coordtype="utm", coords=4:5, scale.transform = "SQRT", n.report=2)

## ----speffects,  echo=TRUE, eval=TRUE, message=FALSE,  fig.cap="Spatial effects of maximum temperature from model M7", fig.height=5, fig.width=7----
out <- M7$fit
dim(out$betasp)
a <- out$betasp
u <- c(t(out$betasp))
sn <- nrow(a)
itmax <- ncol(a)
v <- rep(1:sn, each=itmax)
d <- data.frame(site=as.factor(v), sp = u)
p <- ggplot(data=d, aes(x=site, y=sp)) + 
   geom_boxplot(outlier.colour="black", outlier.shape=1,
                outlier.size=0.5) +
   geom_abline(intercept=0, slope=0, color="blue") + 
   labs(title= "Spatial effects of maximum temperature", x="Site", y = "Effects", size=2.5) 
p 

## ----temporaleffects, echo=TRUE, eval=TRUE, message=FALSE, fig.cap="Temporal effects  of wind speed from model M7", fig.height=5, fig.width=7----
b <- out$betatp
tn <- nrow(b)
itmax <- ncol(b)
tids <- 1:tn 
stat <- apply(b[tids,], 1, quantile, prob=c(0.025,0.5,0.975))
tstat <- data.frame(tids, t(stat))
dimnames(tstat)[[2]] <- c("Days", "low", "median", "up")
# head(tstat)
yr <- c(min(c(stat)),max(c(stat)))
p <- ggplot(data=tstat, aes(x=Days, y=median)) + 
   geom_point(size=3) + 
   ylim(yr) + 
   geom_segment(data=tstat, aes(x=Days, y=median, xend=Days, yend=low), linetype=1) +
   geom_segment(data=tstat, aes(x=Days, y=median, xend=Days, yend=up), linetype=1) +
   geom_abline(intercept=0, slope=0, col="blue") +
   labs(title="Temporal effects of wind speed", x="Days", y="Temporal effects") 
p 

## ----echo=TRUE, eval=TRUE, message=FALSE, results='hide'----------------------
M8 <- Bsptime(package="spBayes",  formula=f2, data=nysptime, 
              prior.sigma2=c(2, 25),
              prior.tau2 =c(2, 25),
              prior.sigma.eta =c(2, 0.001),
              coordtype="utm", 
              coords=4:5, scale.transform = "SQRT", 
              mchoice=T,  N=5000,  n.report=200)

## ---- echo=TRUE, eval=TRUE, message=FALSE, results='hide'---------------------
modfit <- M8$fit
N <- 5000
burn.in <- 1000
tn <- 62
quant95 <- function(x){
   quantile(x, prob=c(0.025, 0.5, 0.975))
}
theta <- apply(modfit$p.theta.samples[burn.in:N,], 2, quant95)
sigma.sq <- theta[,grep("sigma.sq", colnames(theta))]
tau.sq <- theta[,grep("tau.sq", colnames(theta))]
phi <- theta[,grep("phi", colnames(theta))]

## ---- echo=FALSE, eval=TRUE, message=FALSE, results='hide'--------------------
adat <- data.frame(x=1:tn, low=sigma.sq[1, ], med=sigma.sq[2, ], up=sigma.sq[3, ])
# head(adat)

psigma <- ggplot() + 
   geom_point(data = adat, aes(x =x, y = med, shape=19), shape=19, col="blue", size = 2) + 
   geom_ribbon(data = adat, aes(x =x, ymin =low, ymax = up), alpha = 0.2, color = "grey50") +
   theme(legend.position ="none") + 
   labs(y = "sigma2", x = "Days") 
# psigma

adat <- data.frame(x=1:tn, low=tau.sq[1, ], med=tau.sq[2, ], up=tau.sq[3, ])
# head(adat)

ptau <- ggplot() + 
   geom_point(data = adat, aes(x =x, y = med, shape=19), shape=19, col="blue", size = 2) + 
   geom_ribbon(data = adat, aes(x =x, ymin =low, ymax = up), alpha = 0.2, color = "grey50") +
   theme(legend.position ="none") + 
   labs(y = "tau2", x = "Days") 
# ptau

adat <- data.frame(x=1:tn, low=3/phi[3, ], med=3/phi[2, ], up=3/phi[1, ])
# head(adat)

prange <- ggplot() + 
   geom_point(data = adat, aes(x =x, y = med, shape=19), shape=19, col="blue", size = 2) + 
   geom_ribbon(data = adat, aes(x =x, ymin =low, ymax = up), alpha = 0.2, color = "grey50") +
   theme(legend.position ="none") + 
   labs(y = "Range", x = "Days") 
# prange

## ----spBayesplots, echo=FALSE, eval=TRUE, fig.cap="Parameter estimates for the spBayes fitted spatio-temporal model M8", fig.height=10, fig.width=7----
ggarrange(psigma, ptau, prange, common.legend = TRUE, legend = "none", nrow = 3, ncol = 1)

## ---- echo=TRUE, eval=FALSE, message=FALSE, results='hide'--------------------
#  M9 <-  Bsptime(package="spTimer", model="GPP", g_size=5,
#                 formula=f2, data=nysptime, n.report=5,
#                 coordtype="utm", coords=4:5, scale.transform = "SQRT")

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
   table7 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen")) %>%
   add_colnames("Criteria") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>% 
   set_caption('(#tab:modv-m1-m9) Model choice and validation statistics for the eight models.')

## ----atllocationsdeep, echo=T, eval=T, fig.height=5, fig.width = 7, fig.cap="Locations of moving Argo floats in the deep ocean in 2003."----
atlmap <- map_data("world", xlim=c(-70, 10), ylim=c(15, 65))
atlmap <- atlmap[atlmap$long < 5, ]
atlmap <- atlmap[atlmap$long > -70, ]
atlmap <- atlmap[atlmap$lat < 65, ]
atlmap <- atlmap[atlmap$lat > 10, ]

argo <- argo_floats_atlantic_2003

deep <- argo[argo$depth==3, ]
deep$month <- factor(deep$month)

p <- ggplot() +
  geom_polygon(data=atlmap, aes(x=long, y=lat, group=group),
               color="black", size = 0.6, fill=NA) +
  geom_point(data =deep, aes(x=lon, y=lat, colour=month), size=1) +
  labs( title= "Argo float locations in deep ocean in 2003", x="Longitude", y = "Latitude") +
  ggsn::scalebar(data =atlmap, dist = 1000, location = "bottomleft", transform=T, dist_unit = "km",
                 st.dist = .05, st.size =5, height = .05, st.bottom=T, model="WGS84") +
  ggsn::north(data=atlmap, location="topright", symbol=12)
p

## ----deep, echo=F, eval=T-----------------------------------------------------

deep <- argo_floats_atlantic_2003[argo_floats_atlantic_2003$depth==3, ]
deep$x2inter <- deep$xinter*deep$xinter
deep$month <- factor(deep$month)

deep$lat2 <- (deep$lat)^2
deep$sin1 <- round(sin(deep$time*2*pi/365), 4)
deep$cos1 <- round(cos(deep$time*2*pi/365), 4)
deep$sin2 <- round(sin(deep$time*4*pi/365), 4)
deep$cos2 <- round(cos(deep$time*4*pi/365), 4)

#head(deep)
## scaling variables 
deep[, c( "xlat2", "xsin1", "xcos1", "xsin2", "xcos2")] <- 
  scale(deep[,c("lat2", "sin1", "cos1", "sin2", "cos2")])

f2 <- temp ~ xlon + xlat + xlat2+ xinter + x2inter 

## ----deepmodfitting, echo=TRUE, eval=FALSE, results='hide', message=FALSE-----
#  Natl <- 110
#  Nburn <- 10
#  options(warn = -1)
#  M2atl <- Bmoving_sptime(formula=f2, data = deep, coordtype="lonlat",
#    coords = 1:2, N=Natl, burn.in=Nburn, validrows =NULL, mchoice =F)

## ----processdeep, echo=FALSE, eval=FALSE, message=FALSE-----------------------
#  library(rstan)
#  listofdraws <- rstan::extract(M2atl$fit)
#  # names(listofdraws)
#  # dim(listofdraws$xbmodel)
#  # dim(listofdraws$bigS)
#  dat <- M2atl$datatostan
#  # names(dat)
#  ## Extract the diagonal elements of all St
#  v <- numeric()
#  x <- numeric()
#  m <- length(dat$nts)
#  for (i in 1:m) {
#    k <- dat$nts[i]
#    a1 <- 1:k^2
#    a2 <- seq(from=1, to =k^2, length=k)
#    b1 <- rep(0, k^2)
#    b1[which(a1==a2)] <- 1
#    cbind(a1, b1)
#    v <- c(v, a1)
#    x <- c(x, b1) ## indicates if the corresponding index is a diagonal
#  }
#  
#  # b <- cbind(v, x)
#  # b[1:20, ]
#  # u <- which(x>0)
#  # length(u)
#  
#  varsamps <- listofdraws$bigS[, which(x>0)]/365
#  # dim(varsamps)
#  xbeta <- listofdraws$xbmodel
#  # dim(xbeta)
#  sdsamps <- sqrt(varsamps)
#  ntot <- nrow(xbeta) * ncol(xbeta)
#  zsamp <- matrix(rnorm(ntot), nrow=nrow(xbeta), ncol=ncol(xbeta))
#  # dim(zsamp)
#  zsamp <- xbeta + zsamp * sdsamps
#  ansummary <- get_parameter_estimates(zsamp)
#  # head(ansummary)
#  # summary(ansummary$mean)
#  # summary(ansummary$sd)
#  # summary(ansummary$low)
#  # summary(ansummary$up)
#  
#  pdata <- cbind(deep, ansummary)
#  
#  atlmap <- map_data("world", xlim=c(-70, 10), ylim=c(15, 65))
#  #head(atlmap)
#  atlmap <- atlmap[atlmap$long < 5, ]
#  atlmap <- atlmap[atlmap$long > -70, ]
#  atlmap <- atlmap[atlmap$lat < 65, ]
#  atlmap <- atlmap[atlmap$lat > 10, ]
#  
#  xo <- seq(from=-70, to = 5,  length=200)
#  yo <- seq(from= 10, to = 65, length=200)
#  
#  ## Temperature
#  surf <- interp(pdata$lon, pdata$lat, pdata$mean,  xo=xo, yo=yo)
#  #names(surf)
#  surf <- list(x=surf$x, y=surf$y, z=surf$z)
#  interp1 <- data.frame(long = surf$x, surf$z )
#  names(interp1)[1:length(surf$y)+1] <- surf$y
#  interp1 <- gather(interp1,key = lat,value =Temp,-long,convert = TRUE)
#  
#  #head(interp1)
#  #summary(interp1$Temp)
#  pcolors <- topo.colors(5)
#  patldeep <- ggplot() +
#    geom_raster(data=interp1, aes(x = long, y = lat,fill = Temp)) +
#    # scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
#    scale_fill_gradientn(colours=pcolors) +
#    geom_contour(data=interp1, aes(x = long, y = lat,z = Temp)) +
#    geom_polygon(data=atlmap, aes(x=long, y=lat, group=group),
#                 color="black", size = 0.6, fill=NA) +
#      geom_point(data =deep, aes(x=lon, y=lat, colour=month), size=1) +
#    labs( title= "Annual temperature in deep ocean in 2003", x="Longitude", y = "Latitude") +
#    ggsn::scalebar(data =atlmap, dist = 1000, location = "bottomleft", transform=T, dist_unit = "km",
#                   st.dist = .05, st.size =5, height = .05, st.bottom=T, model="WGS84") +
#    ggsn::north(data=atlmap, location="topright", symbol=12)
#  
#  # patldeep
#  
#  
#  ## sd of temperature
#  
#  surf <- interp(pdata$lon, pdata$lat, pdata$sd,  xo=xo, yo=yo)
#  #names(surf)
#  surf <- list(x=surf$x, y=surf$y, z=surf$z)
#  interp1 <- data.frame(long = surf$x, surf$z )
#  names(interp1)[1:length(surf$y)+1] <- surf$y
#  interp1 <- gather(interp1,key = lat,value =sd,-long,convert = TRUE)
#  
#  # head(interp1)
#  #summary(interp1$sd)
#  
#  patlsd <- ggplot() +
#    geom_raster(data=interp1, aes(x = long, y = lat,fill = sd)) +
#    # scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
#    scale_fill_gradientn(colours=pcolors) +
#    geom_contour(data=interp1, aes(x = long, y = lat,z = sd)) +
#    geom_polygon(data=atlmap, aes(x=long, y=lat, group=group),
#                 color="black", size = 0.6, fill=NA) +
#    geom_point(data =deep, aes(x=lon, y=lat, colour=month), size=1) +
#    labs( title= "sd of annual temperature in deep ocean in 2003", x="Longitude", y = "Latitude") +
#    ggsn::scalebar(data = atlmap, dist = 1000, location = "bottomleft", transform=T, dist_unit = "km",
#                   st.dist = .05, st.size =5, height = .05, st.bottom=T, model="WGS84") +
#    ggsn::north(data=atlmap, location="topright", symbol=12)
#  

## ----oceanplot, eval=TRUE, echo=FALSE, fig.height=6, fig.cap="A map of predicted temperatures in the   deep ocean in 2003", out.width = '90%'----
knitr::include_graphics(paste0(figpath, "/temp_deep.png"))

## ----ptime, echo=T, eval=T, fig.width=7, fig.height=5,  fig.cap="Weekly Covid-19 death rate per 100,000"----
engdeaths$covidrate <- 100000*engdeaths$covid/engdeaths$popn
ptime <- ggplot(data=engdeaths,  aes(x=factor(Weeknumber), y=covidrate)) +
  geom_boxplot() +
  labs(x = "Week", y = "Death rate per 100,000")  +
  stat_summary(fun=median, geom="line", aes(group=1, col="red")) +
  theme(legend.position = "none")
ptime

## ---- echo=T, eval=T, message=FALSE, results='hide'---------------------------
Ncar <- 50000
burn.in.car <- 10000
thin <- 10

## ---- echo=TRUE, eval=FALSE, message=FALSE, results='hide'--------------------
#  f1 <- noofhighweeks ~ jsa + log10(houseprice) + log(popdensity) + sqrt(no2)

## ---- echo=T, eval=FALSE, message=FALSE, results='hide'-----------------------
#  M1 <- Bcartime(formula=f1,   data=engtotals, family="binomial",
#  trials=engtotals$nweek, N=Ncar, burn.in=burn.in.car, thin=thin)

## ---- echo=T, eval=FALSE, message=FALSE, results='hide'-----------------------
#  M1.leroux <- Bcartime(formula=f1, data=engtotals, scol="spaceid",
#  model="leroux", W=Weng, family="binomial", trials=engtotals$nweek,
#  N=Ncar, burn.in=burn.in.car, thin=thin)

## ---- echo=T, eval=FALSE, message=FALSE, results='hide'-----------------------
#  M1.bym <- Bcartime(formula=f1, data=engtotals,
#  scol="spaceid", model="bym", W=Weng, family="binomial",
#  trials=engtotals$nweek, N=Ncar, burn.in=burn.in.car, thin=thin)

## ---- echo=T, eval=FALSE, message=FALSE, results='hide'-----------------------
#  M1.inla.bym <- Bcartime(formula=f1, data=engtotals, scol ="spaceid",
#  model=c("bym"),  W=Weng, family="binomial", trials=engtotals$nweek,
#  package="inla", N=Ncar, burn.in=burn.in.car, thin=thin)

## ---- echo=T, eval=FALSE, message=FALSE, results='hide'-----------------------
#  a <- rbind(M1$mchoice, M1.leroux$mchoice, M1.bym$mchoice)
#  a <- a[, -(5:6)]
#  a <- a[, c(2, 1, 4, 3)]
#  b <- M1.inla.bym$mchoice[1:4]
#  a <- rbind(a, b)
#  rownames(a) <- c("Independent", "Leroux", "BYM", "INLA-BYM")
#  colnames(a) <- c("pDIC", "DIC", "pWAIC",  "WAIC")
#  table4.1 <- a
#  dput(table4.1, file=paste0(tablepath, "/table4.1.txt"))

## ----m1mchoice, echo=FALSE, eval=TRUE-----------------------------------------

#table4.1 <- structure(c(4.97364951758527, 85.0582651584891, 87.0579313931239, 
#76.379205946683, 1503.99901271082, 1352.37719804419, 1353.60166910473, 
#1348.37610850059, 6.24416032366254, 52.3598308212364, 53.3895453655056, 
#49.2665262573736, 1505.39538129241, 1330.11305694479, 1330.71580863181, 
#1330.37517336979), .Dim = c(4L, 4L), .Dimnames = list(c("Independent", 
#"Leroux", "BYM", "INLA-BYM"), c("pDIC", "DIC", "pWAIC", "WAIC"
#)))

 table4.1 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen", "purple")) %>%
   add_colnames("Model") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>% 
   set_caption('(#tab:logistic) Comparison of logistic regession models for static areal data')

## ---- echo=T, eval=FALSE------------------------------------------------------
#  f2 <-  covid ~ offset(logEdeaths) + jsa + log10(houseprice) + log(popdensity) + sqrt(no2)

## ---- echo=T, eval=FALSE, message=FALSE, results='hide'-----------------------
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

## ---- echo=FALSE, eval=FALSE, message=FALSE, results='hide'-------------------
#  a <- rbind(M2$mchoice, M2.leroux$mchoice, M2.bym$mchoice)
#  a <- a[, -(5:6)]
#  a <- a[, c(2, 1, 4, 3)]
#  b <- M2.inla.bym$mchoice[1:4]
#  a <- rbind(a, b)
#  rownames(a) <- c("Independent", "Leroux", "BYM",   "INLA-BYM")
#  colnames(a) <- c("pDIC", "DIC", "pWAIC",  "WAIC")
#  table4.2 <- a
#  dput(table4.2, file=paste0(tablepath, "/table4.2.txt"))

## ----m2mchoice, echo=FALSE, eval=TRUE-----------------------------------------

table4.2 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen")) %>%
   add_colnames("Model") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>%
   set_caption('(#tab:poisson) Comparison of disease mapping models for Covid-19 mortality')

## ---- echo=T, eval=FALSE------------------------------------------------------
#  f3 <-  sqrt(no2) ~  jsa + log10(houseprice) + log(popdensity)

## ---- echo=T, eval=FALSE, message=FALSE, results='hide'-----------------------
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

## ---- echo=T, eval=FALSE, message=FALSE, results='hide'-----------------------
#  a <- rbind(M3$mchoice, M3.leroux$mchoice)
#  a <- a[, -(5:6)]
#  a <- a[, c(2, 1, 4, 3)]
#  b <- M3.inla.bym$mchoice[1:4]
#  a <- rbind(a, b)
#  rownames(a) <- c("Independent", "Leroux",  "INLA-BYM")
#  colnames(a) <- c("pDIC", "DIC", "pWAIC",  "WAIC")
#  table4.3 <- a
#  dput(table4.3, file=paste0(tablepath, "/table4.3.txt"))

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
# table4.3 <- structure(c(5.01545172916434, 141.392412442474, 119.355981097098, 
# 473.514497590557, 325.070205308015, 343.268611084862, 6.05910211982487, 
# 106.79520878445, 94.4196627626064, 474.726483321535, 320.089832117357, 
# 341.890578348917), .Dim = 3:4, .Dimnames = list(c("Independent", 
# "Leroux", "INLA-BYM"), c("pDIC", "DIC", "pWAIC", "WAIC")))
   table4.3 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen")) %>%
   add_colnames("Model") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>% 
   set_caption('(#tab:staticnormal) Comparison of Gaussian models for NO2 data')

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  nweek <- rep(1, nrow(engdeaths))

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  f1 <- highdeathsmr ~  jsa + log10(houseprice) + log(popdensity)

## ---- echo=T, eval=FALSE------------------------------------------------------
#  M1st <- Bcartime(formula=f1, data=engdeaths, scol=scol, tcol=tcol, trials=nweek,
#  W=Weng, model="linear", family="binomial", package="CARBayesST",  N=Ncar,
#  burn.in=burn.in.car, thin=thin)

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
  table8 %>%
  as_hux(add_colnames = FALSE) %>%
  set_number_format(2)     %>%
  map_text_color(by_cols("darkred", "blue", "darkgreen")) %>%
  add_colnames("Model") %>%
  set_header_rows(1, TRUE) %>% 
  add_rownames() %>%
  set_all_borders(1)  %>%
  set_bold(1, everywhere) %>% 
  style_headers(bold = TRUE, text_color = "red") %>% 
  set_caption('(#tab:m1st) Model choice criteria values for various spatio-temporal binomial model.')

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  f2 <-  covid ~ offset(logEdeaths) + jsa + log10(houseprice) + log(popdensity) + n0 + n1 + n2 + n3

## ---- echo=FALSE, eval=FALSE--------------------------------------------------
#  M2st <- Bcartime(formula=f2, data=engdeaths, scol=scol, tcol=tcol,  W=Weng,
#  model="ar", family="poisson", package="CARBayesST", N=Ncar, burn.in=burn.in.car, thin=thin)

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
   table9 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen")) %>%
   add_colnames("Model") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red")%>% 
   set_caption('(#tab:m2st) Model choice criteria values for the spatio-temporal Poisson model.')

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  vs <- sample(nrow(engdeaths), 0.1*nrow(engdeaths))

## ----m2st-valid, echo=FALSE, eval=TRUE----------------------------------------
   table10 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen")) %>%
   add_colnames("Model") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red")%>% 
   set_caption('(#tab:m2st-valid) Model validation statistics for the Anova and AR model.')

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
#  ggsave(filename = paste0(figpath, "/inlavAR2.png"))

## ----obsvpredplot, echo=FALSE, eval=TRUE, fig.cap="Predictions with 95% limits against observations for two models: AR (2) on the left panel and INLA on the right panel", out.width = '90%'----
knitr::include_graphics(paste0(figpath, "/inlavAR2.png"))

## ---- echo=T, eval=FALSE------------------------------------------------------
#  M3st <- Bcartime(formula=f3, data=engdeaths, scol=scol, tcol=tcol,
#  W=Weng, model="ar", family="gaussian", package="CARBayesST",
#  N=Ncar, burn.in=burn.in.car, thin=thin)

## ----m3st-no2, echo=FALSE, eval=TRUE, caption="Model choice criteria values for the spatio-temporal Gaussian model."----
   table11 %>%
   as_hux(add_colnames = FALSE) %>%
   set_number_format(2)     %>%
   map_text_color(by_cols("darkred", "blue", "darkgreen")) %>%
   add_colnames("Model") %>%
   set_header_rows(1, TRUE) %>% 
   add_rownames() %>%
   set_all_borders(1)  %>%
   set_bold(1, everywhere) %>% 
   style_headers(bold = TRUE, text_color = "red") %>% 
   set_caption('(#tab:m3st-no2) Model choice criteria values for the spatio-temporal Gaussian model.')

