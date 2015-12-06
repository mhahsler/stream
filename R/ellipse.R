### this is taken form: sfsmisc - Utilities from Seminar 
###					    fuer Statistik ETH Zurich
### by Martin Maechler et al.
### Version: 1.0-23
### License: GPL (>= 2)

ellipsePoints <- function(a,b, alpha = 0, loc = c(0,0), n = 201,
                          keep.ab.order = FALSE)
{
    ## Purpose: ellipse points,radially equispaced, given geometric par.s
    ## -------------------------------------------------------------------------
    ## Arguments: a, b : length of half axes in (x,y) direction
    ##            alpha: angle (in degrees) for rotation
    ##            loc  : center of ellipse
    ##            n    : number of points
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 19 Mar 2002

    stopifnot(is.numeric(a), is.numeric(b))
    reorder <- a < b && keep.ab.order
    B <- min(a,b)
    A <- max(a,b)
    ## B <= A
    d2 <- (A-B)*(A+B) ## = A^2 - B^2
    phi <- 2*pi*seq(0,1, len = n)
    sp <- sin(phi)
    cp <- cos(phi)
    r <- a*b / sqrt(B^2 + d2 * sp^2)
    xy <- r * if(reorder) cbind(sp, cp) else cbind(cp, sp)
    ## xy are the ellipse points for alpha = 0 and loc = (0,0)
    al <- alpha * pi/180
    ca <- cos(al)
    sa <- sin(al)
    xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(loc[1],n),
                                                rep(loc[2],n))
}
