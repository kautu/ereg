GDPRE <- read.csv(file = "10r_2gdp_pps.csv")
row.names(GDPRE) <- GDPRE$TIME
Gregio <- as.matrix(GDPRE[2:269])

group <- kmeans(t(Gregio), 7, nstart = 1)
plot(t(Gregio)[,1]/1000, group$cluster)

library('dplyr')
ppsgr <- cbind.data.frame(t(Gregio), data.frame(group$cluster))
r <- ppsgr %>% filter(group.cluster == 6)
rr <- ppsgr %>% filter(group.cluster == 4)
rrr <- ppsgr %>% filter(group.cluster == 3)
r4 <- ppsgr %>% filter(group.cluster == 2)
r5 <- ppsgr %>% filter(group.cluster == 1)
r6 <- ppsgr %>% filter(group.cluster == 7)
#write.csv(ppsgr, 'ppsgr_kmeans.csv')

TOUR <- read.csv(file = "tour.csv")
row.names(TOUR) <- TOUR$TIME
Gtour <- as.matrix(TOUR[2:269])
gtour <- cbind.data.frame(t(Gtour), data.frame(group$cluster)) 
o <- gtour %>% filter(group.cluster == 6)
oo <- gtour %>% filter(group.cluster == 4)
ooo <- gtour %>% filter(group.cluster == 3)
o4 <- gtour %>% filter(group.cluster == 2)
o5 <- gtour %>% filter(group.cluster == 1)
o6 <- gtour %>% filter(group.cluster == 7)

POP <- read.csv(file = "population.csv")
row.names(POP) <- POP$TIME
Gpop <- as.matrix(POP[2:269])
gpop <- cbind.data.frame(t(Gpop), data.frame(group$cluster)) 
p <- gpop %>% filter(group.cluster == 6)
pp <- gpop %>% filter(group.cluster == 4)
ppp <- gpop %>% filter(group.cluster == 3)
p4 <- gpop %>% filter(group.cluster == 2)
p5 <- gpop %>% filter(group.cluster == 1)
p6 <- gpop %>% filter(group.cluster == 7)


HR <- read.csv(file = "hrst.csv")
row.names(HR) <- HR$TIME
Ghr <- as.matrix(HR[2:269])
ghrst <- cbind.data.frame(t(Ghr), data.frame(group$cluster)) 
t <- ghrst %>% filter(group.cluster == 6)
tt <- ghrst %>% filter(group.cluster == 4)
ttt <- ghrst %>% filter(group.cluster == 3)
t4 <- ghrst %>% filter(group.cluster == 2)
t5 <- ghrst %>% filter(group.cluster == 1)
t6 <- ghrst %>% filter(group.cluster == 7)

ED <- read.csv(file = "edat.csv")
row.names(ED) <- ED$TIME
Ged <- as.matrix(ED[2:269])
gedat <- cbind.data.frame(t(Ged), data.frame(group$cluster)) 
e <- gedat %>% filter(group.cluster == 6)
ee <- gedat %>% filter(group.cluster == 4)
eee <- gedat %>% filter(group.cluster == 3)
e4 <- gedat %>% filter(group.cluster == 2)
e5 <- gedat %>% filter(group.cluster == 1)
e6 <- gedat %>% filter(group.cluster == 7)


#group$size
#e1 <- gedat[1:19,]         #2
#e3 <- gedat[23:66,]        #3
#e4 <- gedat[67:(66+65),]   #6
#e5 <- gedat[132:(131+66),] #5
#e6 <- gedat[198:(197+10),] #1
#e7 <- gedat[208:(207+61),] #4

library('phtt')
rpteo <- KSS(formula = t(log(r[,-16])) ~ + t(log(p[,-16])) + t(log(t[,-16])) + t(log(e[,-16]))+ t(log(o[,-16])), 
             additive.effects = "none")
rpteo <- Eup(formula = t(log(r[,-16])) ~ t(log(p[,-16])) + t(log(t[,-16])) + t(log(e[,-16]))+ t(log(o[,-16])), 
             additive.effects = "none")
(rpteo.summary <- summary(rpteo))

rpteo2 <- KSS(formula = t(log(rr[,-16])) ~ t(log(pp[,-16])) + t(log(tt[,-16])) + t(log(ee[,-16]))+ t(log(oo[,-16])), 
              additive.effects = "none")
(rpteo2.summary <- summary(rpteo2))

rpteo3 <- KSS(formula = t(log(rrr[,-16])) ~ t(log(ppp[,-16])) + t(log(ttt[,-16])) + t(log(eee[,-16]))+ t(log(ooo[,-16])), 
              additive.effects = "none")
(rpteo3.summary <- summary(rpteo3))

rpteo4 <- KSS(formula = t(log(r4[,-16])) ~ t(log(p4[,-16])) + t(log(t4[,-16])) + t(log(e4[,-16]))+ t(log(o4[,-16])), 
             additive.effects = "none")
(rpteo4.summary <- summary(rpteo4))

rpteo5 <- KSS(formula = t(log(r5[,-16])) ~ t(log(p5[,-16])) + t(log(t5[,-16])) + t(log(e5[,-16]))+ t(log(o5[,-16])), 
              additive.effects = "none")
(rpteo5.summary <- summary(rpteo5))

rpteo6 <- KSS(formula = t(log(r6[,-16])) ~ t(log(p6[,-16])) + t(log(t6[,-16])) + t(log(e6[,-16]))+ t(log(o6[,-16])), 
              additive.effects = "none")
(rpteo6.summary <- summary(rpteo6))

##

ro <- KSS(formula = t(log(r[,-16])) ~ t(log(o[,-16])), 
              additive.effects = "individual")
(ro.summary <- summary(ro))

ro2 <- KSS(formula = t(log(rr[,-16])) ~ t(log(oo[,-16])), 
              additive.effects = "individual")
(ro2.summary <- summary(ro2))

ro3 <- KSS(formula = t(log(rrr[,-16])) ~ t(log(ooo[,-16])), 
           additive.effects = "individual")
(ro3.summary <- summary(ro3))

ro4 <- KSS(formula = t(log(r4[,-16])) ~  t(log(o4[,-16])), 
           additive.effects = "individual")
(ro4.summary <- summary(ro4))

ro5 <- KSS(formula = t(log(r5[,-16])) ~  t(log(o5[,-16])), 
           additive.effects = "individual")
(ro5.summary <- summary(ro5))

ro6 <- KSS(formula = t(log(r6[,-16])) ~  t(log(o6[,-16])), 
           additive.effects = "individual")
(ro6.summary <- summary(ro6))

## 
rpo3 <- KSS(formula = t(log(rrr[,-16])) ~ t(log(ppp[,-16])) + t(log(ooo[,-16])), 
          additive.effects = "individual")
(rpo3.summary <- summary(rpo3))

rpo4 <- KSS(formula = t(log(r4[,-16])) ~ t(log(p4[,-16])) + t(log(o4[,-16])), 
           additive.effects = "individual")
(rpo4.summary <- summary(rpo4))

rpo5 <- KSS(formula = t(log(r5[,-16])) ~ t(log(p5[,-16])) + t(log(o5[,-16])), 
            additive.effects = "individual")
(rpo5.summary <- summary(rpo5))
##
rpto5 <- KSS(formula = t(log(r5[,-16])) ~ t(log(p5[,-16])) + t(log(t5[,-16]))+ t(log(o5[,-16])), 
            additive.effects = "individual")
(rpto5.summary <- summary(rpto5))

re6 <- KSS(formula = t(log(r6[,-16])) ~  t(log(p6[,-16])), 
           additive.effects = "individual")
(re6.summary <- summary(re6))


##Appendix
rpteo <- Eup(formula = t(log(r[,-16])) ~ t(log(p[,-16]))  + t(log(e[,-16]))+ t(log(o[,-16])), 
             additive.effects = "twoways")
(rpteo.summary <- summary(rpteo))

rpteo2 <- Eup(formula = t(log(rr[,-16])) ~ t(log(pp[,-16])) + t(log(tt[,-16])) + t(log(oo[,-16])), 
              additive.effects = "twoways")
(rpteo2.summary <- summary(rpteo2))

rpteo3 <- Eup(formula = t(log(rrr[,-16])) ~ t(log(ppp[,-16])) + t(log(ttt[,-16])) + t(log(ooo[,-16])), 
              additive.effects = "time")
(rpteo3.summary <- summary(rpteo3))

rpteo4 <- Eup(formula = t(log(r4[,-16])) ~ t(log(p4[,-16])) + t(log(t4[,-16]))+ t(log(o4[,-16])), 
              additive.effects = "time")
(rpteo4.summary <- summary(rpteo4))

rpteo5 <- Eup(formula = t(log(r5[,-16])) ~ t(log(p5[,-16])) + t(log(t5[,-16])) + t(log(o5[,-16])), 
              additive.effects = "time")
(rpteo5.summary <- summary(rpteo5))

rpteo6 <- Eup(formula = t(log(r6[,-16])) ~ t(log(p6[,-16])) + t(log(e6[,-16]))+ t(log(o6[,-16])), 
              additive.effects = "time")
(rpteo6.summary <- summary(rpteo6))


##

he <- KSS(formula = t(log(t[,-16])) ~  t(log(e[,-16])), 
           additive.effects = "twoways")
(he.summary <- summary(he))

he2 <- KSS(formula = t(log(tt[,-16])) ~  t(log(ee[,-16])), 
          additive.effects = "twoways")
(he2.summary <- summary(he2))

he3 <- KSS(formula = t(log(ttt[,-16])) ~  t(log(eee[,-16])), 
          additive.effects = "twoways")
(he3.summary <- summary(he3))

he4 <- KSS(formula = t(log(t4[,-16])) ~  t(log(e4[,-16])), 
           additive.effects = "twoways")
(he4.summary <- summary(he4))

he5 <- KSS(formula = t(log(t5[,-16])) ~  t(log(e5[,-16])), 
           additive.effects = "twoways")
(he5.summary <- summary(he5))

he6 <- KSS(formula = t(log(t6[,-16])) ~  t(log(e6[,-16])), 
           additive.effects = "twoways")
(he6.summary <- summary(he6))
