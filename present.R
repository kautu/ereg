library('dplyr')
library('tidyr')
library('ggplot2')

euro <- read.csv('10r_2gdp_2013.csv') 
row.names(euro) <- euro$TIME
gdp.euro <- euro %>%
  gather(colnames(euro)[-1], key = "Regions", value = "GDP") %>%
  #filter(!Regions %in% c('FR10', 'UKI', 'ITC4')) %>%
  arrange(TIME)
g1 <- ggplot(gdp.euro, aes(TIME, log(GDP), color = Regions)) +
        geom_line(alpha = 2/5) +
#  facet_wrap(~Group, nrow = 3, labeller = "label_both", scales = "free") +
#  geom_text(aes(label = Regions), size = 2.5, vjust = 1) +
        labs(title = 'A. Regional GDP in Euro Measurement') +
        ylab('Ln') +
        theme(legend.position = 'none')                   

pps <- read.csv('10r_2gdp_2013_pps.csv') 
row.names(pps) <- pps$TIME
gdp.pps <- pps %>%
  gather(colnames(pps)[-1], key = "Regions", value = "GDP") %>%
  #filter(!Regions %in% c('FR10', 'UKI', 'ITC4')) %>%
  arrange(TIME)
g2 <- ggplot(gdp.pps, aes(TIME, log(GDP), color = Regions)) +
  geom_line(alpha = 2/5) +
  #  facet_wrap(~Group, nrow = 3, labeller = "label_both", scales = "free") +
  #  geom_text(aes(label = Regions), size = 2.5, vjust = 1) +
  labs(title = 'B. Regional GDP in PPS Conversion') +
  ylab('Ln') +
  theme(legend.position = 'none')  

tour <- read.csv('tour_arn2.csv') 
row.names(tour) <- tour$TIME
occupancy <- tour %>%
  gather(colnames(tour)[-1], key = "Regions", value = "Occupancy") %>%
  #filter(!Regions %in% c('FR10')) %>%
  arrange(TIME)
oc <- ggplot(occupancy, aes(TIME, log(Occupancy), color = Regions)) +
  geom_line(alpha = 2/5) +
  #geom_text(aes(label = Regions), size = 2.5, vjust = 1) +
  labs(title = 'C. Occupancy') +
  ylab('Ln') + 
  theme(legend.position = 'none')  

population <- read.csv('population.csv')
row.names(population) <- population$TIME
popu <- population %>%
  gather(colnames(population)[-1], key = "Regions", value = "POPULATION") %>%
  #filter(!Regions %in% c('FR10', 'ITC4')) %>%
  arrange(TIME)
po <- ggplot(popu, aes(TIME, log(POPULATION), color = Regions)) +
  geom_line(alpha = 2/5) +
  #geom_text(aes(label = Regions), size = 2.5, vjust = 1) +
  labs(title = 'D. POPULATION') +
  ylab('Ln ') +
  theme(legend.position = 'none') 

technology <- read.csv('hrst.csv') 
row.names(technology) <- technology$TIME
knowledge <- technology %>%
  gather(colnames(technology)[-1], key = "Regions", value = "HRST") %>%
  #filter(!Regions %in% c('FR10', 'UKI', 'ITC4')) %>%
  arrange(TIME)
hr <- ggplot(knowledge, aes(TIME, log(HRST), color = Regions)) +
  geom_line(alpha = 2/5) +
  #geom_text(aes(label = Regions), size = 2.5, vjust = 1) +
  labs(title = 'E. Human Resources in Science and Technology') +
  ylab('Ln ') +
  theme(legend.position = 'none')  

education <- read.csv('edat.csv') 
row.names(education) <- education$TIME
edat <- education %>%
  gather(colnames(education)[-1], key = "Regions", value = "EDAT") %>%
  # filter(!Regions %in% c('FR10', 'UKI', 'ITC4')) %>%
  arrange(TIME)
ed <- ggplot(edat, aes(TIME, log(EDAT), color = Regions)) +
  geom_line(alpha = 2/5) +
  #geom_text(aes(label = Regions), size = 2.5, vjust = 1) +
  labs(title = 'F. Education Attainment') +
  ylab('Ln ') +
  theme(legend.position = 'none')  

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
CairoPDF("PRESENT.pdf", 15, 10)
PRESENT <- multiplot(g1, po, g2, hr, oc, ed, cols = 3)
dev.off()

