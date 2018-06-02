##R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"
##Copyright (C) 2016 The R Foundation for Statistical Computing
##Platform: x86_64-pc-linux-gnu (64-bit)



##  Oualid Bada, Dominik Liebl (2014). The R Package phtt: Panel Data
##  Analysis with Heterogeneous Time Trends. Journal of Statistical
##  Software, 59(6), 1-34. URL http://www.jstatsoft.org/v59/i06/.


##  H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
##  Springer-Verlag New York, 2009.


##  Simon Urbanek and Jeffrey Horner (2015). Cairo: R graphics device
##  using cairo graphics library for creating high-quality bitmap (PNG,
##  JPEG, TIFF), vector (PDF, SVG, PostScript) and display (X11 and
##  Win32) output. R package version 1.5-9.
##  https://CRAN.R-project.org/package=Cairo




## Load package
library("phtt")

## Load Data ##The paths below depend on your particular file directories.
GDP.REG <- read.csv(file = "/home/kautu/Documents/GDP_REG/10r_2gdp_2013.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.REG) <- GDP.REG$TIME
g.regio <- as.matrix(GDP.REG[2:204])

GDP.REG.PPS <- read.csv(file = "/home/kautu/Documents/GDP_REG/10r_2gdp_2013_pps.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.REG.PPS) <- GDP.REG.PPS$TIME
g.pps <- as.matrix(GDP.REG.PPS[2:204])

GDP.GFCF <- read.csv(file = "/home/kautu/Documents/GDP_REG/gfcf.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.GFCF) <- GDP.GFCF$TIME
g.gfcf <- as.matrix(GDP.GFCF[2:204])


GDP.EMHR <- read.csv(file = "/home/kautu/Documents/GDP_REG/emhrw.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.EMHR) <- GDP.EMHR$TIME
emhrw <- as.matrix(GDP.EMHR[2:204])


GDP.TOUR <- read.csv(file = "/home/kautu/Documents/GDP_REG/tour_2013.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.TOUR) <- GDP.TOUR$TIME
tour.nin <- as.matrix(GDP.TOUR[2:204])

GDP.TOUR.2 <- read.csv(file = "/home/kautu/Documents/GDP_REG/tour_arn2.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.TOUR.2) <- GDP.TOUR.2$TIME
tour.arr <- as.matrix(GDP.TOUR.2[2:204])

GDP.TOUR.CAP <- read.csv(file = "/home/kautu/Documents/GDP_REG/tour_cap.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.TOUR.CAP) <- GDP.TOUR.CAP$TIME
tour.cap <- as.matrix(GDP.TOUR.CAP[2:204])



## Panel I: Ye,p,f = Tour.nin,arr,cap
## The Communication Factors of European Regional GDP Determinants(1).

#Euro measurement, Yeur = Tour.nin + e
Ge.n <- KSS(formula = log(g.regio) ~ log(tour.nin))
(Ge.n.summary <- summary(Ge.n))

Ge.i <- KSS(formula = log(g.regio) ~ log(tour.nin), additive.effects = "individual")
(Ge.i.summary <- summary(Ge.i))

Ge.t <- KSS(formula = log(g.regio) ~ log(tour.nin), additive.effects = "time")
(Ge.t.summary <- summary(Ge.t))

Ge.w <- KSS(formula = log(g.regio) ~ log(tour.nin), additive.effects = "twoways")
(Ge.w.summary <- summary(Ge.w))

#PPS measurement, Ypps
Gp.n <- KSS(formula = log(g.pps) ~ log(tour.nin))
(Gp.n.summary <- summary(Gp.n))

Gp.i <- KSS(formula = log(g.pps) ~ log(tour.nin), additive.effects = "individual")
(Gp.i.summary <- summary(Gp.i))

Gp.t <- KSS(formula = log(g.pps) ~ log(tour.nin), additive.effects = "time")
(Gp.t.summary <- summary(Gp.t))

Gp.w <- KSS(formula = log(g.pps) ~ log(tour.nin), additive.effects = "twoways")
(Gp.w.summary <- summary(Gp.w))

#GDP compoment, Ygfcf
Gf.n <- KSS(formula = log(g.gfcf) ~ log(tour.nin))
(Gf.n.summary <- summary(Gf.n))

Gf.i <- KSS(formula = log(g.gfcf) ~ log(tour.nin), additive.effects = "individual")
(Gf.i.summary <- summary(Gf.i))

Gf.t <- KSS(formula = log(g.gfcf) ~ log(tour.nin), additive.effects = "time")
(Gf.t.summary <- summary(Gf.t))

Gf.w <- KSS(formula = log(g.gfcf) ~ log(tour.nin), additive.effects = "twoways")
(Gf.w.summary <- summary(Gf.w))



## Euro measurement, Yeur = Tour.arr + e
## The Communication Factors of European Regional GDP Determinants(2).

Ge.na <- KSS(formula = log(g.regio) ~ log(tour.arr))
(Ge.na.summary <- summary(Ge.na))

Ge.ia <- KSS(formula = log(g.regio) ~ log(tour.arr), additive.effects = "individual")
(Ge.ia.summary <- summary(Ge.ia))

Ge.ta <- KSS(formula = log(g.regio) ~ log(tour.arr), additive.effects = "time")
(Ge.ta.summary <- summary(Ge.ta))

Ge.wa <- KSS(formula = log(g.regio) ~ log(tour.arr), additive.effects = "twoways")
(Ge.wa.summary <- summary(Ge.wa))

#PPS measurement, Ypps
Gp.na <- KSS(formula = log(g.pps) ~ log(tour.arr))
(Gp.na.summary <- summary(Gp.na))

Gp.ia <- KSS(formula = log(g.pps) ~ log(tour.arr), additive.effects = "individual")
(Gp.ia.summary <- summary(Gp.ia))

Gp.ta <- KSS(formula = log(g.pps) ~ log(tour.arr), additive.effects = "time")
(Gp.ta.summary <- summary(Gp.ta))

Gp.wa <- KSS(formula = log(g.pps) ~ log(tour.arr), additive.effects = "twoways")
(Gp.wa.summary <- summary(Gp.wa))

#GDP compoment, Ygfcf
Gf.na <- KSS(formula = log(g.gfcf) ~ log(tour.arr))
(Gf.na.summary <- summary(Gf.na))

Gf.ia <- KSS(formula = log(g.gfcf) ~ log(tour.arr), additive.effects = "individual")
(Gf.ia.summary <- summary(Gf.ia))

Gf.ta <- KSS(formula = log(g.gfcf) ~ log(tour.arr), additive.effects = "time")
(Gf.ta.summary <- summary(Gf.ta))

Gf.wa <- KSS(formula = log(g.gfcf) ~ log(tour.arr), additive.effects = "twoways")
(Gf.wa.summary <- summary(Gf.wa))



## Euro measurement, Yeur = Tour.cap + e
## The Communication Factors of European Regional GDP Determinants(3).

Ge.nc <- KSS(formula = log(g.regio) ~ log(tour.cap))
(Ge.nc.summary <- summary(Ge.nc))

Ge.ic <- KSS(formula = log(g.regio) ~ log(tour.cap), additive.effects = "individual")
(Ge.ic.summary <- summary(Ge.ic))

Ge.tc <- KSS(formula = log(g.regio) ~ log(tour.cap), additive.effects = "time")
(Ge.tc.summary <- summary(Ge.tc))

Ge.wc <- KSS(formula = log(g.regio) ~ log(tour.cap), additive.effects = "twoways")
(Ge.wc.summary <- summary(Ge.wc))

#PPS measurement, Ypps
Gp.nc <- KSS(formula = log(g.pps) ~ log(tour.cap))
(Gp.nc.summary <- summary(Gp.nc))

Gp.ic <- KSS(formula = log(g.pps) ~ log(tour.cap), additive.effects = "individual")
(Gp.ic.summary <- summary(Gp.ic))

Gp.tc <- KSS(formula = log(g.pps) ~ log(tour.cap), additive.effects = "time")
(Gp.tc.summary <- summary(Gp.tc))

Gp.wc <- KSS(formula = log(g.pps) ~ log(tour.cap), additive.effects = "twoways")
(Gp.wc.summary <- summary(Gp.wc))

#GDP compoment, Ygfcf
Gf.nc <- KSS(formula = log(g.gfcf) ~ log(tour.cap))
(Gf.nc.summary <- summary(Gf.nc))

Gf.ic <- KSS(formula = log(g.gfcf) ~ log(tour.cap), additive.effects = "individual")
(Gf.ic.summary <- summary(Gf.ic))

Gf.tc <- KSS(formula = log(g.gfcf) ~ log(tour.cap), additive.effects = "time")
(Gf.tc.summary <- summary(Gf.tc))

Gf.wc <- KSS(formula = log(g.gfcf) ~ log(tour.cap), additive.effects = "twoways")
(Gf.wc.summary <- summary(Gf.wc))



## Panel II: Tour.nin,arr,cap = Ye,p,f
## European Regional GDP and the Communication Factors (1).

#Tour.nin = Ye
Tn.n <- KSS(formula = log(tour.nin) ~ log(g.regio) + log(emhrw))
(Tn.n.summary <- summary(Tn.n))

Tn.i <- KSS(formula = log(tour.nin) ~ log(g.regio) + log(emhrw), additive.effects = "individual")
(Tn.i.summary <- summary(Tn.i))

Tn.t <- KSS(formula = log(tour.nin) ~ log(g.regio) + log(emhrw), additive.effects = "time")
(Tn.t.summary <- summary(Tn.t))

Tn.w <- KSS(formula = log(tour.nin) ~ log(g.regio) + log(emhrw), additive.effects = "twoways")
(Tn.w.summary <- summary(Tn.w))

#Tour.arr
Ta.n <- KSS(formula = log(tour.arr) ~ log(g.regio) + log(emhrw))
(Ta.n.summary <- summary(Ta.n))

Ta.i <- KSS(formula = log(tour.arr) ~ log(g.regio) + log(emhrw), additive.effects = "individual")
(Ta.i.summary <- summary(Ta.i))

Ta.t <- KSS(formula = log(tour.arr) ~ log(g.regio) + log(emhrw), additive.effects = "time")
(Ta.t.summary <- summary(Ta.t))

Ta.w <- KSS(formula = log(tour.arr) ~ log(g.regio) + log(emhrw), additive.effects = "twoways")
(Ta.w.summary <- summary(Ta.w))

#Tour.cap
Tc.n <- KSS(formula = log(tour.cap) ~ log(g.regio) + log(emhrw))
(Tc.n.summary <- summary(Tc.n))

Tc.i <- KSS(formula = log(tour.cap) ~ log(g.regio) + log(emhrw), additive.effects = "individual")
(Tc.i.summary <- summary(Tc.i))

Tc.t <- KSS(formula = log(tour.cap) ~ log(g.regio) + log(emhrw), additive.effects = "time")
(Tc.t.summary <- summary(Tc.t))

Tc.w <- KSS(formula = log(tour.cap) ~ log(g.regio) + log(emhrw), additive.effects = "twoways")
(Tc.w.summary <- summary(Tc.w))



## Tour.nin = Yp
## European Regional GDP and the Communication Factors (2).

Tn.np <- KSS(formula = log(tour.nin) ~ log(g.pps) + log(emhrw))
(Tn.np.summary <- summary(Tn.np))

Tn.ip <- KSS(formula = log(tour.nin) ~ log(g.pps) + log(emhrw), additive.effects = "individual")
(Tn.ip.summary <- summary(Tn.ip))

Tn.tp <- KSS(formula = log(tour.nin) ~ log(g.pps) + log(emhrw), additive.effects = "time")
(Tn.t.summary <- summary(Tn.tp))

Tn.wp <- KSS(formula = log(tour.nin) ~ log(g.pps) + log(emhrw), additive.effects = "twoways")
(Tn.wp.summary <- summary(Tn.wp))

#Tour.arr
Ta.np <- KSS(formula = log(tour.arr) ~ log(g.pps) + log(emhrw))
(Ta.np.summary <- summary(Ta.np))

Ta.ip <- KSS(formula = log(tour.arr) ~ log(g.pps) + log(emhrw), additive.effects = "individual")
(Ta.ip.summary <- summary(Ta.ip))

Ta.tp <- KSS(formula = log(tour.arr) ~ log(g.pps) + log(emhrw), additive.effects = "time")
(Ta.tp.summary <- summary(Ta.tp))

Ta.wp <- KSS(formula = log(tour.arr) ~ log(g.pps) + log(emhrw), additive.effects = "twoways")
(Ta.wp.summary <- summary(Ta.wp))

#Tour.cap
Tc.np <- KSS(formula = log(tour.cap) ~ log(g.pps) + log(emhrw))
(Tc.np.summary <- summary(Tc.np))

Tc.ip <- KSS(formula = log(tour.cap) ~ log(g.pps) + log(emhrw), additive.effects = "individual")
(Tc.ip.summary <- summary(Tc.ip))

Tc.tp <- KSS(formula = log(tour.cap) ~ log(g.pps) + log(emhrw), additive.effects = "time")
(Tc.tp.summary <- summary(Tc.tp))

Tc.wp <- KSS(formula = log(tour.cap) ~ log(g.pps) + log(emhrw), additive.effects = "twoways")
(Tc.wp.summary <- summary(Tc.wp))


## Tour.nin = Yf
## European Regional GDP and the Communication Factors (3).

Tn.ng <- KSS(formula = log(tour.nin) ~ log(g.gfcf) + log(emhrw))
(Tn.ng.summary <- summary(Tn.ng))

Tn.ig <- KSS(formula = log(tour.nin) ~ log(g.gfcf) + log(emhrw), additive.effects = "individual")
(Tn.ig.summary <- summary(Tn.ig))

Tn.tg <- KSS(formula = log(tour.nin) ~ log(g.gfcf) + log(emhrw), additive.effects = "time")
(Tn.tg.summary <- summary(Tn.tg))

Tn.wg <- KSS(formula = log(tour.nin) ~ log(g.gfcf) + log(emhrw), additive.effects = "twoways")
(Tn.wg.summary <- summary(Tn.wg))

#Tour.arr
Ta.ng <- KSS(formula = log(tour.arr) ~ log(g.gfcf) + log(emhrw))
(Ta.ng.summary <- summary(Ta.ng))

Ta.ig <- KSS(formula = log(tour.arr) ~ log(g.gfcf) + log(emhrw), additive.effects = "individual")
(Ta.ig.summary <- summary(Ta.ig))

Ta.tg <- KSS(formula = log(tour.arr) ~ log(g.gfcf) + log(emhrw), additive.effects = "time")
(Ta.tg.summary <- summary(Ta.tg))

Ta.wg <- KSS(formula = log(tour.arr) ~ log(g.gfcf) + log(emhrw), additive.effects = "twoways")
(Ta.wg.summary <- summary(Ta.wg))

#Tour.cap
Tc.ng <- KSS(formula = log(tour.cap) ~ log(g.gfcf) + log(emhrw))
(Tc.ng.summary <- summary(Tc.ng))

Tc.ig <- KSS(formula = log(tour.cap) ~ log(g.gfcf) + log(emhrw), additive.effects = "individual")
(Tc.ig.summary <- summary(Tc.ig))

Tc.tg <- KSS(formula = log(tour.cap) ~ log(g.gfcf) + log(emhrw), additive.effects = "time")
(Tc.tg.summary <- summary(Tc.tg))

Tc.wg <- KSS(formula = log(tour.cap) ~ log(g.gfcf) + log(emhrw), additive.effects = "twoways")
(Tc.wg.summary <- summary(Tc.wg))


## NOT INCLUDING IN THE PAPER. 

## Loading GFCF Compoments
GDP.GFCF.A <- read.csv(file = "/home/kautu/Documents/GDP_REG/gfcf_a.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.GFCF.A) <- GDP.GFCF.A$TIME
gfcf.a <- as.matrix(GDP.GFCF.A[2:204])

GDP.GFCF.B <- read.csv(file = "/home/kautu/Documents/GDP_REG/gfcf_b.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.GFCF.B) <- GDP.GFCF.B$TIME
gfcf.b <- as.matrix(GDP.GFCF.B[2:204])

GDP.GFCF.C <- read.csv(file = "/home/kautu/Documents/GDP_REG/gfcf_c.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.GFCF.C) <- GDP.GFCF.C$TIME
gfcf.c <- as.matrix(GDP.GFCF.C[2:204])

GDP.GFCF.f <- read.csv(file = "/home/kautu/Documents/GDP_REG/gfcf_f.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.GFCF.f) <- GDP.GFCF.f$TIME
gfcf.f <- as.matrix(GDP.GFCF.f[2:204])

GDP.GFCF.G <- read.csv(file = "/home/kautu/Documents/GDP_REG/gfcf_g.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.GFCF.G) <- GDP.GFCF.G$TIME
gfcf.g <- as.matrix(GDP.GFCF.G[2:204])

GDP.GFCF.K <- read.csv(file = "/home/kautu/Documents/GDP_REG/gfcf_k.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.GFCF.K) <- GDP.GFCF.K$TIME
gfcf.k <- as.matrix(GDP.GFCF.K[2:204])

GDP.GFCF.O <- read.csv(file = "/home/kautu/Documents/GDP_REG/gfcf_o.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.GFCF.O) <- GDP.GFCF.O$TIME
gfcf.o <- as.matrix(GDP.GFCF.O[2:204])



## NOT INCLUDING IN THE PAPER. 
## PANEL

# Estimation of the KSS model
GFCF.n <- KSS(formula = log(tour.nin) ~ log(gfcf.a) + log(gfcf.b) + log(gfcf.c) + log(gfcf.g) + log(gfcf.k) + log(gfcf.o))
(GFCF.n.summary <- summary(GFCF.n))
 
# Estimation of the KSS model with additive individual effects
GFCF.i <- KSS(formula = log(tour.nin) ~ log(gfcf.a) + log(gfcf.b) + log(gfcf.c) + log(gfcf.g) + log(gfcf.k) + log(gfcf.o), additive.effects = "individual")
(GFCF.i.summary <- summary(GFCF.i))

# Estimation of the KSS model with additive time effects
GFCF.t <- KSS(formula = log(tour.nin) ~ log(gfcf.a) + log(gfcf.b) + log(gfcf.c) + log(gfcf.g) + log(gfcf.k) + log(gfcf.o),additive.effects = "time")
(GFCF.t.summary <- summary(GFCF.t))

## Estimation of the KSS model with additive twoways effects
GFCF.w <- KSS(formula = log(tour.nin) ~ log(gfcf.a) + log(gfcf.b) + log(gfcf.c) + log(gfcf.g) + log(gfcf.k) + log(gfcf.o),additive.effects = "twoways")
(GFCF.w.summary <- summary(GFCF.w))


# 
GFCF.bgko.n <- KSS(formula = log(tour.nin) ~ log(gfcf.b) + log(gfcf.g) + log(gfcf.k) + log(gfcf.o))
(GFCF.bgko.n.summary <- summary(GFCF.bgko.n))

# 
GFCF.bgko.i <- KSS(formula = log(tour.nin) ~ log(gfcf.b) + log(gfcf.g) + log(gfcf.k) + log(gfcf.o), additive.effects = "individual")
(GFCF.bgko.i.summary <- summary(GFCF.bgko.i))

# 
GFCF.bgko.t <- KSS(formula = log(tour.nin) ~ log(gfcf.b) + log(gfcf.g) + log(gfcf.k) + log(gfcf.o), additive.effects = "time")
(GFCF.bgko.t.summary <- summary(GFCF.bgko.t))

# 
GFCF.bgko.w <- KSS(formula = log(tour.nin) ~ log(gfcf.b) + log(gfcf.g) + log(gfcf.k) + log(gfcf.o), additive.effects = "twoways")
(GFCF.bgko.w.summary <- summary(GFCF.bgko.w))


# 
GFCF.acf.n <- KSS(formula = log(tour.nin) ~ log(gfcf.a) + log(gfcf.c) + log(gfcf.f))
(GFCF.acf.n.summary <- summary(GFCF.acf.n))

# 
GFCF.acf.i <- KSS(formula = log(tour.nin) ~ log(gfcf.a) + log(gfcf.c) + log(gfcf.f), additive.effects = "individual")
(GFCF.acf.i.summary <- summary(GFCF.acf.i))

# 
GFCF.acf.t <- KSS(formula = log(tour.nin) ~ log(gfcf.a) + log(gfcf.c) + log(gfcf.f), additive.effects = "time")
(GFCF.acf.t.summary <- summary(GFCF.acf.t))

# 
GFCF.acf.w <- KSS(formula = log(tour.nin) ~ log(gfcf.a) + log(gfcf.c) + log(gfcf.f), additive.effects = "twoways")
(GFCF.acf.w.summary <- summary(GFCF.acf.w))






##Alternative Explanation Variables Loading

##Dependent variable: GDP at current market prices by NUTS 2 regions
GDPRE <- read.csv(file = "/home/kautu/Documents/GDP_REG/10r_2gdp.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDPRE) <- GDPRE$TIME
Gregio <- as.matrix(GDPRE[2:269])

GDPRE.PPS <- read.csv(file = "/home/kautu/Documents/GDP_REG/10r_2gdp_pps.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDPRE.PPS) <- GDPRE.PPS$TIME
Gregio.pps <- as.matrix(GDPRE.PPS[2:269])

POP <- read.csv(file = "/home/kautu/Documents/GDP_REG/population.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(POP) <- POP$TIME
popu <- as.matrix(POP[2:269])

## Independent variables: HRST by category and NUTS 2 regions
GDP.HRST <- read.csv(file = "/home/kautu/Documents/GDP_REG/hrst.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.HRST) <- GDP.HRST$TIME
hrst <- as.matrix(GDP.HRST[2:269])

## Independent variables: Population aged 25-64 by educational attainment level
GDP.EDAT <- read.csv(file = "/home/kautu/Documents/GDP_REG/edat.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDP.EDAT) <- GDP.EDAT$TIME
edat <- as.matrix(GDP.EDAT[2:269])

## Independent variables: Nights spent at tourist accommodation establishments
GDPRE.TOUR <- read.csv(file = "/home/kautu/Documents/GDP_REG/tour.csv", 
		    sep = ",", 
		    header = TRUE)
row.names(GDPRE.TOUR) <- GDPRE.TOUR$TIME
Grtour <- as.matrix(GDPRE.TOUR[2:269])


##The Alternative Factors of European Regional GDP Determinants.

## Estimation of the KSS model
Gregio.n <- KSS(formula = log(Gregio) ~ log(popu) + log(hrst) + log(edat) + log(Grtour))
(Gregio.n.summary <- summary(Gregio.n))

## 
Gregio.i <- KSS(formula = log(Gregio) ~ log(popu) + log(hrst) + log(edat) + log(Grtour), additive.effects = "individual")
(Gregio.i.summary <- summary(Gregio.i))

##
Gregio.t <- KSS(formula = log(Gregio) ~ log(popu) + log(hrst) + log(edat) + log(Grtour), additive.effects = "time")
(Gregio.t.summary <- summary(Gregio.t))

## 
Gregio.w <- KSS(formula = log(Gregio) ~ log(popu) + log(hrst) + log(edat) + log(Grtour), additive.effects = "twoways")
(Gregio.w.summary <- summary(Gregio.w))


## Estimation of the KSS model
Gregio.np <- KSS(formula = log(Gregio.pps) ~ log(popu) + log(hrst) + log(edat) + log(Grtour))
(Gregio.np.summary <- summary(Gregio.np))

## 
Gregio.ip <- KSS(formula = log(Gregio.pps) ~ log(popu) + log(hrst) + log(edat) + log(Grtour), additive.effects = "individual")
(Gregio.ip.summary <- summary(Gregio.ip))

##
Gregio.tp <- KSS(formula = log(Gregio.pps) ~ log(popu) + log(hrst) + log(edat) + log(Grtour), additive.effects = "time")
(Gregio.tp.summary <- summary(Gregio.tp))

## 
Gregio.wp <- KSS(formula = log(Gregio.pps) ~ log(popu) + log(hrst) + log(edat) + log(Grtour), additive.effects = "twoways")
(Gregio.wp.summary <- summary(Gregio.wp))






##FIGURE 1 - Communication and European Regional GDP

library("ggplot2")

# Loading the geometric mean data
er <- read.csv(file = "/home/kautu/Documents/GDP_REG/plot.csv", 
		    sep = ",", 
		    header = TRUE)

library("Cairo")

CairoPDF("FIGUREI.pdf", 12, 10)
ggplot(er, aes(x = log(PPS), y = log(TOUR))) +
geom_point() +
xlab("The Logarithm of the European Regional GDP(PPS)") +
ylab("The Logarithm of the Tourist Accommodation Occupancy") +
labs(title = "Five Years Geometric Mean of Communication Factors and European Regional GDP from 2010 to 2014") +
theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank()) +
geom_smooth(method = lm) +
geom_text(aes(y = log(TOUR) + .1, label = NAME))
dev.off()



##FIGURE 2 - European Regional GDP Determinants (2010-2013)

library("ggplot2", "Cairo")

er <- read.csv(file = "/home/kautu/Documents/GDP_REG/plot.csv", 
		    sep = ",", 
		    header = TRUE)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




f1 <- ggplot(er, aes(x = log(PPS), y = log(HRPO))) +
      geom_point() +
      xlab("ln GDP(PPS)") +
      ylab("ln (HRST/POPULATION)") +
      labs(title = "(B) HRST/POPULATION") +
      theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank()) +
      geom_smooth(method = lm) #+
#          geom_text(aes(y = log(HRPO) + .05, label = NAME))


f2 <- ggplot(er, aes(x = log(PPS), y = log(TOPO))) +
      geom_point() +
      xlab("ln GDP(PPS)") +
      ylab("ln (TOUR/POPULATION)") +
      labs(title = "(A) TOUR/POPULATION") +
      theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank()) +
      geom_smooth(method = lm) #+
#          geom_text(aes(y = log(TOPO) + .1, label = NAME))


p1 <- ggplot(er, aes(x = log(HRST), y = log(PPS))) +
      geom_point() +
      xlab("ln HRST") +
      ylab("ln GDP(PPS)") +
      labs(title = "(E) HRST") +
      theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank()) +
      geom_smooth(method = lm) 

e <- ggplot(er, aes(x = log(PPS), y = log(EDAT))) +
     geom_point() +
     xlab("ln GDP(PPS)") +
     ylab("ln EDAT") +
     labs(title = "(C) EDAT") +
     theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank()) +
     geom_smooth(method = lm) 

ep <- ggplot(er, aes(x = log(EDAT*POPU), y = log(PPS))) +
      geom_point() +
      xlab("ln(EDAT*POPULATION)") +
      ylab("ln GDP(PPS)") +
      labs(title ="(F) EDUCATION") +
      theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank()) +
      geom_smooth(method = lm) 


p0 <- ggplot(er, aes(x = log(POPU), y = log(PPS))) +
      geom_point() +
      xlab("ln Population") +
      ylab("ln GDP(PPS)") +
      labs(title = "(D) POPULATION") +
      theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank()) +
      geom_smooth(method = lm) 


CairoPDF("FIGUREII.pdf", 12, 18)
FIGUREII <- multiplot(f2, f1, e, p0, p1, ep, cols = 2)
dev.off()


