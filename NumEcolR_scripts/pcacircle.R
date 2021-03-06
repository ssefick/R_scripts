pcacircle = function (pca, ordiplot, ...) 
#
# Draws a circle of equilibrium contribution on a PCA plot
# License: GPL-2 
# Author: Francois Gillet, February 2009
#
{
    drawcircle <- function(x0 = 0, y0 = 0, radius = 1, npoints = 100, 
        ...) {
        a <- seq(0, 2 * pi, len = npoints)
        c <- array(dim = c(2, npoints))
        c[1, ] <- x0 + cos(a) * radius
        c[2, ] <- y0 + sin(a) * radius
        for (i in 1:(npoints - 1)) {
            segments(c[1, i], c[2, i], c[1, 1 + i], c[2, 1 + 
                i], ...)
        }
        segments(c[1, i], c[2, i], c[1, npoints], c[2, npoints], 
            ...)
    }
    eigenv <- pca$CA$eig
    p <- length(eigenv)
    n <- nrow(pca$CA$u)
    tot <- sum(eigenv)
    const <- ((n - 1) * tot)^0.25
    radius <- (2/p)^0.5
    radius <- radius * const
    result <- list(radius = radius, constant = const)
    drawcircle(radius = radius, ...)
    speciescoord <- scores(ordiplot, display = "species")
    for (i in 1:nrow(speciescoord)) {
        lengthr <- (speciescoord[i, 1]^2 + speciescoord[i, 2]^2)^0.5
        if (lengthr >= radius) {
            arrows(0, 0, speciescoord[i, 1], speciescoord[i, 2], ...)
        }
    }
    return(result)
}
