#############################################################################################
##  Funcion de clasificacion alrededor de centros moviles k-means                          ##
##  Tiene en cuenta los pesos de los elementos a clasificar                                ##
##                                                                                         ##
##                                                                                         ##
## Elaborado por: Pedro Cesar del Campo Neira                                              ##
##                Campo Elias Pardo                                                        ##
## Modificado por: Mauricio SadinleCamilo Jose Torres. Nov. 25 2009.                       ##
## Universidad Nacional de Colombia                                                        ##
##                                                                                         ##
##                                                                                         ##
## kmeansW  ( x       = un vector o matriz numerica                                        ##
##            centers = centros de las clasificaciones iniciales. si es un numero,         ##
##                      un conjunto aleatorio de centros es seleccionado para iniciar.     ##
##            weight  = peso de los elementos a clasificar, por defecto pesos iguales      ##
##            iter.max= numero maximo de iteracciones a desarrollar en el proceso.         ##
##            nstart  = si centers es un numero, como se seleccionan aleatoriamente los    ##
##                      centros iniciales?                                                 ##
##           )                                                                             ##
##                                                                                         ##
##                                                                                         ##
#############################################################################################

kmeansW <- function(x, centers, weight = rep(1,nrow(x)),
                  iter.max = 10, nstart = 1){
    x <- as.matrix(x)
    m <- nrow(x)
    if (missing(centers))
        stop("'centers' must be a number or a matrix")

    if (length(centers) == 1) {
        k <- centers
        if (nstart == 1)
            centers <- x[sample(1:m, k), , drop = FALSE]
        if (nstart >= 2 || any(duplicated(centers))) {
            cn <- unique(x)
            mm <- nrow(cn)
            if (mm < k)
                stop("more cluster centers than distinct data points.")
            centers <- cn[sample(1:mm, k), , drop = FALSE]
        }
    }
    else {
        centers <- as.matrix(centers)
        if (any(duplicated(centers)))
            stop("initial centers are not distinct")
        cn <- NULL
        k <- nrow(centers)
        if (m < k)
            stop("more cluster centers than data points")
    }
    if (iter.max < 1)
        stop("'iter.max' must be positive")
    if (ncol(x) != ncol(centers))
        stop("must have same number of columns in 'x' and 'centers'")
    Z <- .C("kmnsw", as.double(x), as.integer(m),
            as.integer(ncol(x)),
            centers = as.double(centers), as.double(weight),
            as.integer(k), c1 = integer(m), nc = integer(k), 
            as.integer(iter.max), wss = double(k),
            ifault = as.integer(0), PACKAGE="stream")
    if (nstart >= 2 && !is.null(cn)) {
        best <- sum(Z$wss)
        for (i in 2:nstart) {
            centers <- cn[sample(1:mm, k), , drop = FALSE]
            ZZ <-  .C("kmnsw", as.double(x), as.integer(m),
                      as.integer(ncol(x)),
                      centers = as.double(centers), as.double(weight),
                      as.integer(k), c1 = integer(m), nc = integer(k), 
                      as.integer(iter.max), wss = double(k),
                      ifault = as.integer(0L), PACKAGE="stream")
            if ((z <- sum(ZZ$wss)) < best) {
                Z <- ZZ
                best <- z
            }
        }
    }
    centers <- matrix(Z$centers, k)
    dimnames(centers) <- list(1:k, dimnames(x)[[2]])
    cluster <- Z$c1
    if (!is.null(rn <- rownames(x)))
        names(cluster) <- rn
    out <- list(cluster = cluster, centers = centers, withinss = Z$wss,
        size = Z$nc)
    class(out) <- "kmeans"
    out}
