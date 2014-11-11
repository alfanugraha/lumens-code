## Multiple plot function
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
  require(grid)
  
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

################################################################################
#DIFA multitemporal comparison
################################################################################
n.chart<-10;#slider id

#DIFA initial
difa.init.dyn<-ggplot(sumtab1.init, aes(x =sumtab1.init$teci, y =sumtab1.init$Cum.Sum, xend=100, yend=100)) + 
  geom_area(position='') + ggtitle(year1) +
  labs(x = "Sorted TECI value (%)", y='Focal area proportion (%)'); #main chart
difa.init.dyn<-difa.init.dyn+ geom_hline(aes(yintercept=sumtab1.init$Cum.Sum[n.chart]))+ geom_vline(aes(xintercept=sumtab1.init$teci[n.chart])); #add vline and hline interception
difa.init.dyn<-difa.init.dyn+geom_point(aes(x =sumtab1.init$teci[n.chart], y =sumtab1.init$Cum.Sum[n.chart]), colour="green"); #add specific point on the graph
sumtab1.init.trunc<-sumtab1.init[1:n.chart,]; #create new table
AUC.init.dyn<-round((trapz(na.omit(sumtab1.init.trunc$teci),sumtab1.init.trunc$Cum.Sum))/100,digits=2)
difa.init.dyn<-difa.init.dyn + 
  geom_area(data=sumtab1.init.trunc, aes(x =sumtab1.init.trunc$teci, y =sumtab1.init.trunc$Cum.Sum,xend=100, yend=100),position='', fill='blue')+#add blue area
  annotate("text", label = paste('AUC=', AUC.init.dyn, sep=' '), x = nrow(sumtab1.init)*0.5, y = nrow(sumtab1.init)*0.1, size = 6, colour = "white")



#DIFA final
difa.final.dyn<-ggplot(sumtab1.final, aes(x =sumtab1.final$teci, y =sumtab1.final$Cum.Sum, xend=100, yend=100)) + 
  geom_area(position='') + ggtitle(year2) +
  labs(x = "Sorted TECI value (%)", y='Focal area proportion (%)'); #main chart
difa.final.dyn<-difa.final.dyn+ geom_hline(aes(yintercept=sumtab1.final$Cum.Sum[n.chart]))+ geom_vline(aes(xintercept=sumtab1.final$teci[n.chart])); #add vline and hline interception
difa.final.dyn<-difa.final.dyn+geom_point(aes(x =sumtab1.final$teci[n.chart], y =sumtab1.final$Cum.Sum[n.chart]), colour="green"); #add specific point on the graph
sumtab1.final.trunc<-sumtab1.final[1:n.chart,]; #create new table
difa.final.dyn<-difa.final.dyn + 
  geom_area(data=sumtab1.final.trunc, aes(x =sumtab1.final.trunc$teci, y =sumtab1.final.trunc$Cum.Sum,xend=100, yend=100),position='', fill='blue')+#add blue area
  annotate("text", label = paste('AUC=', AUC.final.dyn, sep=' '), x = nrow(sumtab1.final)*0.5, y = nrow(sumtab1.final)*0.1, size = 6, colour = "white")
AUC.final.dyn<-round((trapz(na.omit(sumtab1.final.trunc$teci),sumtab1.final.trunc$Cum.Sum))/100,digits=2)

multiplot(difa.init.dyn, difa.final.dyn, cols=2)

#Question:
#difa.final/difa.init teci and Cum.Sum data values should be interpolated to synchronize the values between those two chart
#Question:
##############################################################################
##############################################################################



################################################################################
#Spatially explicit DIFA interactive chart
################################################################################
n.chart<-10;#slider id

#DIFA initial
difa.init.dyn<-ggplot(sumtab1.init, aes(x =sumtab1.init$teci, y =sumtab1.init$Cum.Sum, xend=100, yend=100)) + 
  geom_area(position='') + ggtitle(year1) +
  labs(x = "Sorted TECI value (%)", y='Focal area proportion (%)'); #main chart
difa.init.dyn<-difa.init.dyn+ geom_hline(aes(yintercept=sumtab1.init$Cum.Sum[n.chart]))+ geom_vline(aes(xintercept=sumtab1.init$teci[n.chart])); #add vline and hline interception
difa.init.dyn<-difa.init.dyn+geom_point(aes(x =sumtab1.init$teci[n.chart], y =sumtab1.init$Cum.Sum[n.chart]), colour="green"); #add specific point on the graph
sumtab1.init.trunc<-sumtab1.init[1:n.chart,]; #create new table
difa.init.dyn<-difa.init.dyn + 
  geom_area(data=sumtab1.init.trunc, aes(x =sumtab1.init.trunc$teci, y =sumtab1.init.trunc$Cum.Sum,xend=100, yend=100),position='', fill='blue')#add blue area

AUC.init.dyn<-round((trapz(na.omit(sumtab1.init.trunc$teci),sumtab1.init.trunc$Cum.Sum))/100,digits=2)
difa.init.dyn

centro.id.final<-sumtab1.init[n.chart,1]
difa.coord.final<-as.data.frame(as.character(centro[centro.id]@coords))
rownames(difa.coord.final)<-cbind('X','Y')
colnames(difa.coord.final)<-cbind('coordinates')

plot(mwfile.init)
points(centro[centro.id], add=T)


#DIFA final
difa.final.dyn<-ggplot(sumtab1.final, aes(x =sumtab1.final$teci, y =sumtab1.final$Cum.Sum, xend=100, yend=100)) + 
  geom_area(position='') + ggtitle(year1) +
  labs(x = "Sorted TECI value (%)", y='Focal area proportion (%)'); #main chart
difa.final.dyn<-difa.final.dyn+ geom_hline(aes(yintercept=sumtab1.final$Cum.Sum[n.chart]))+ geom_vline(aes(xintercept=sumtab1.final$teci[n.chart])); #add vline and hline interception
difa.final.dyn<-difa.final.dyn+geom_point(aes(x =sumtab1.final$teci[n.chart], y =sumtab1.final$Cum.Sum[n.chart]), colour="green"); #add specific point on the graph
sumtab1.final.trunc<-sumtab1.final[1:n.chart,]; #create new table
difa.final.dyn<-difa.final.dyn + 
  geom_area(data=sumtab1.final.trunc, aes(x =sumtab1.final.trunc$teci, y =sumtab1.final.trunc$Cum.Sum,xend=100, yend=100),position='', fill='blue');#add blue area

AUC.final.dyn<-round((trapz(na.omit(sumtab1.final.trunc$teci),sumtab1.final.trunc$Cum.Sum))/100,digits=2)
difa.final.dyn

centro.id.final<-sumtab1.final[n.chart,1]
difa.coord.final<-as.data.frame(as.character(centro[centro.id.final]@coords))
rownames(difa.coord.final)<-cbind('X','Y')
colnames(difa.coord.final)<-cbind('coordinates')

plot(mwfile.final)
points(centro[centro.id], add=T)
