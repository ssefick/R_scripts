# This program is released under the GNU GPL >=2 license.  For Details, see http://librestats.com/licenses
 
library(ggplot2)
 
autoplot.lm <- function(x, ..., which=c(1:3, 5), mfrow=c(1,1)){
 
  require(grid)
  df <- fortify(model)
  df <- cbind(df, rows=1:nrow(df))
 
  # residuals vs fitted
  g1 <- ggplot(df, aes(.fitted, .resid)) +
    geom_point()  +
    geom_smooth(se=FALSE) +
    geom_hline(linetype=2, size=.2) +
      scale_x_continuous("Fitted Values") +
      scale_y_continuous("Residual") +
      opts(title="Residuals vs Fitted")
 
  # normal qq
  a <- quantile(df$.stdresid, c(0.25, 0.75))
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  g2 <- ggplot(df, aes(sample=.resid)) +
    stat_qq() +
    geom_abline(slope=slope, intercept=int) +
      scale_x_continuous("Theoretical Quantiles") +
      scale_y_continuous("Standardized Residuals") +
      opts(title="Normal Q-Q")
 
  # scale-location
  g3 <- ggplot(df, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point() +
    geom_smooth(se=FALSE) +
      scale_x_continuous("Fitted Values") +
      scale_y_continuous("Root of Standardized Residuals") +
      opts(title="Scale-Location")
 
  # cook's distance
  g4 <-  ggplot(df, aes(rows, .cooksd, ymin=0, ymax=.cooksd)) +
    geom_point() + geom_linerange() +
      scale_x_continuous("Observation Number") +
      scale_y_continuous("Cook's distance") +
      opts(title="Cook's Distance")
 
  # residuals vs leverage
  g5 <- ggplot(df, aes(.hat, .stdresid)) +
    geom_point() +
    geom_smooth(se=FALSE) +
    geom_hline(linetype=2, size=.2) +
      scale_x_continuous("Leverage") +
      scale_y_continuous("Standardized Residuals") +
      opts(title="Residuals vs Leverage")
 
  # cooksd vs leverage
  g6 <- ggplot(df, aes(.hat, .cooksd)) +
    geom_point() +
    geom_smooth(se=FALSE) +
      scale_x_continuous("Leverage") +
      scale_y_continuous("Cook's distance") +
      opts(title="Cook's dist vs Leverage")
 
  plots <- list(g1, g2, g3, g4, g5, g6)
 
  # making the plots
  grid.newpage()
 
  if (prod(mfrow)>1) {
    mypos <- expand.grid(1:mfrow[1], 1:mfrow[2])
    mypos <- mypos[with(mypos, order(Var1)), ]
    pushViewport(viewport(layout = grid.layout(mfrow[1], mfrow[2])))
    formatter <- function(.){}
  } else {
    mypos <- data.frame(matrix(1, length(which), 2))
    pushViewport(viewport(layout = grid.layout(1, 1)))
    formatter <- function(.) {
      .dontcare <- readline("Hit <Return> to see next plot: ")
      grid.newpage()
    }
  }
 
  j <- 1
  for (i in which){
    formatter()
    print(plots[[i]], vp=viewport(layout.pos.row=mypos[j,][1], layout.pos.col=mypos[j,][2]))
    j <- j+1
  }
}
