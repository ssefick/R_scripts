#working can be better

gg.pls <- function(pls.object, group, comp.1="Comp 1", comp.2="Comp 2", hjust=0, vjust=0, vip_cut=0, equal_contribution=0){
  
  require(BioMark)
  
##test comment out when finished testing##

  test.data=0
  
  if(test.data==1){
  pls.object <-  plsr.bug.red
  
  group <- Y
  
  comp.1 <- "Comp 1"

  comp.2 <- "Comp 2"
  
  hjust <- 1 
    
  vjust <- 1  
  
  vip_cut=0
  
  equal_contribution==0
  
}
##########################################
  
  c.1 <- as.numeric(strsplit(comp.1, split=" ")[[1]][2])
  
  c.2 <- as.numeric(strsplit(comp.2, split=" ")[[1]][2])
  
#vip <- get.biom(pls.object$model$x, group, fmethod="vip", ncomp=1:pls.object$ncomp, type="coef")$vip
 
  vip <- get.biom(pls.object$model$x, group, fmethod="vip", ncomp=pls.object$ncomp, type="coef")$vip
  
  levels(group) <- c(19, 17)

  shape <- as.numeric(as.character(group))
  
  levels(group) <- c(1, 2)
  
  color <- as.numeric(as.character(group))
  
if(length(levels(group))!=2){stop("Only able to work with 2 and only 2 components")}

x.s <- scores(pls.object)[,comp.1]

y.s <- scores(pls.object)[,comp.2]

#P
x.l <- loadings(pls.object)[,comp.1]

y.l <- loadings(pls.object)[,comp.2]

  #W
#x.l <- pls.object$projection[,comp.1]

#y.l <- pls.object$projection[,comp.2]
  
  #make loadings NA if vip < 1 for model
  x.l[grep("FALSE", vip>=vip_cut)] <- NA
  y.l[grep("FALSE", vip>=vip_cut)] <- NA
  
  
  if(equal_contribution==1){
    
    
 equal_load <- sum(abs(loadings(pls.object)[,1:pls.object$ncomp]))/length(abs(loadings(pls.object)[,1:pls.object$ncomp]))
    
  match2 <- c(names(loadings(pls.object)[,comp.1])[abs(loadings(pls.object)[,comp.1])>=equal_load], names(loadings(pls.object)[,comp.2])[abs(loadings(pls.object)[,comp.2])>=equal_load])
    
    x.l <- x.l[names(x.l)%in%match2]
    y.l <- y.l[names(y.l)%in%match2]
    
  
  }
  
  
#limits
lim <- max(abs(c(x.s, y.s, x.l, y.l)), na.rm=T)


  #labels
  
  xlab <- comp.1

  ylab <- comp.2
  

  #circle of equal 
  
#, col=I(color)

bug.pls.plot <- qplot(x.s, y.s, xlab=xlab, ylab=ylab, geom="blank", xlim=c(-lim, lim), ylim=c(-lim, lim))+geom_hline(yintercept=0, linetype="longdash", col="#999999")+geom_vline(xintercept=0, linetype="longdash", col="#999999")+geom_point(pch=I(shape), size=I(5))+geom_text(aes(x=x.l, y=y.l, label=names(y.l), hjust=hjust, vjust=vjust), size=I(7))+geom_segment(aes(x = 0, y = 0, xend = x.l, yend = y.l), arrow = arrow(length = unit(1/2, 'picas')), color = "#666666")+presentation()+coord_fixed(ratio=0.5)

bug.pls.plot

}
