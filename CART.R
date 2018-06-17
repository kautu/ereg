GDPRE <- read.csv("10r_2gdp_pps.csv")
row.names(GDPRE) <- GDPRE$TIME
Gregio <- as.matrix(GDPRE[2:269])

TOUR <- read.csv(file = "tour.csv")
row.names(TOUR) <- TOUR$TIME
Gtour <- as.matrix(TOUR[2:269])

POP <- read.csv(file = "population.csv")
row.names(POP) <- POP$TIME
Gpop <- as.matrix(POP[2:269])

HR <- read.csv(file = "hrst.csv")
row.names(HR) <- HR$TIME
Ghr <- as.matrix(HR[2:269])

ED <- read.csv(file = "edat.csv")
row.names(ED) <- ED$TIME
Ged <- as.matrix(ED[2:269])

library('dplyr')
tree <- cbind.data.frame(t(Gregio)[,1], 
                         t(Gregio)[,15], t(Gregio)[,5], 
                         t(Gpop)[,15], t(Gpop)[,5], 
                         t(Ghr)[,15], t(Ghr)[,5], 
                         t(Ged)[,15], t(Ged)[,5], 
                         t(Gtour)[,15], t(Gtour)[,5])
colnames(tree) <- c('GDP', 'GDP2000', 'GDP2010',  
                    'POP2000', 'POP2010', 'HRST2000', 'HRST2010', 
                    'EDAT2000', 'EDAT2010', 'Occupancy2000', 'Occupancy2010')
library('tree')
region.tree <- tree(GDP ~ GDP2000 + HRST2010, tree)
summary(region.tree)
plot(region.tree)
text(region.tree, pretty = 0)

library('ggplot2')
tree.cart <- cbind.data.frame(tree, tree = region.tree$where) %>%
  mutate(tree = as.factor(tree))
gd <- ggplot(tree.cart, 
       aes(log(HRST2010), log(GDP2000), color = tree, size = GDP/1000)) +
  geom_point(alpha = 2/5) +
  theme(legend.position = 'none') +
  labs(title = 'Panel A. Initial GDP and Post-Crisis HRST') +
  geom_vline(xintercept = log(149.8), linetype="dashed") +
  geom_vline(xintercept = log(914.15), linetype="dashed") 


##
region.pe <- tree(GDP ~ POP2010 + EDAT2010 , tree)
summary(region.pe)
plot(region.pe)
text(region.pe, pretty = 0)

pe.cart <- cbind.data.frame(tree, tree = region.pe$where) %>%
  mutate(tree = as.factor(tree))
pe <- ggplot(pe.cart, 
       aes(EDAT2010, POP2010, color = tree, size = GDP/1000)) +
  geom_point(alpha = 2/5) +
  theme(legend.position = 'none') +
  labs(title = 'Panel B. Post-Crisis Population and EDAT') +
  geom_hline(yintercept = 5.99453e6, linetype="dashed") +
  #geom_hline(yintercept = 3.3013e6, linetype="dashed") +
  geom_hline(yintercept = 1.51429e6, linetype="dashed") 
  #geom_hline(yintercept = 1.0161e6, linetype="dashed") 
  #geom_vline(xintercept = 13.5, linetype="dashed") +
  #geom_vline(xintercept = 22.75, linetype="dashed") 


##
tree <- tree %>%
  mutate(delta.edat = EDAT2010-EDAT2000)
region.ed <- tree(GDP ~ delta.edat , tree)
summary(region.ed)
plot(region.ed)
text(region.ed, pretty = 0)
ed.cart <- cbind.data.frame(tree, tree = region.ed$where) %>%
  mutate(tree = as.factor(tree))
ed <- ggplot(ed.cart, 
       aes(delta.edat, HRST2010, color = tree, size = GDP/1000)) +
  geom_point(alpha = 2/5) +
  theme(legend.position = 'none') +
  labs(title = 'Panel C. EDAT Difference') +
  xlab("EDAT2010 - EDAT2000") 


##
region.oo <- tree(GDP ~ Occupancy2010 , tree)
summary(region.oo)
plot(region.oo)
text(region.oo, pretty = 0)
oo.cart <- cbind.data.frame(tree, tree = region.oo$where) %>%
  mutate(tree = as.factor(tree))
oo <- ggplot(oo.cart, 
       aes(log(Occupancy2010), HRST2010, color = tree, size = GDP/1000)) +
  geom_point(alpha = 2/5) +
  theme(legend.position = 'none') +
  labs(title = 'Panel D. Post-Crisis Occupancy') +
  geom_vline(xintercept = log(2.0038e7), linetype="dashed") +
  geom_vline(xintercept = log(4.5688e6), linetype="dashed") 
  #geom_vline(xintercept = log(5.15769e7), linetype="dashed") 

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

library('Cairo')
CairoPDF("CART.pdf", 15, 10)
multiplot(gd, ed, pe, oo, cols = 2)
dev.off()

