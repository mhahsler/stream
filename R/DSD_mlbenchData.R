#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest 
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


DSD_mlbenchData <- function(data=NULL, loop=FALSE, random=FALSE, scale = FALSE) {
  
  datasets <- c("BostonHousing", "BostonHousing2", "BreastCancer", 
    "DNA", "Glass", "Ionosphere", "LetterRecognition", 
    "Ozone", "PimaIndiansDiabetes", "Satellite", "Servo", 
    "Shuttle", "Sonar", "Soybean", "Vehicle", "Vowel", 
    "Zoo", "HouseVotes84")
  
  if(is.null(data)) {
    cat("Available data sets:\n")
    print(datasets)
    return(invisible(datasets))
  }
  
  #finds index of partial match in array of datasets
  m <- pmatch(tolower(data), tolower(datasets))
  if(is.na(m)) stop("Invalid data name: ", data)
  
  data(list=datasets[m], package="mlbench", envir=environment())
  x <- get(datasets[m], envir=environment())
  
  if(m == 1) {
    d <- x
    a <- NULL
  }
  else if(m == 2) {
    d <- x
    a <- NULL
  }
  else if(m == 3) {
    d <- x[,2:10]
    a <- as.numeric(x[,11])
  }
  else if(m == 4) {
    d <- x[,1:180]
    a <- x[,181]
    levels(a)<-1:3
    a <- as.numeric(a)
  }
  else if(m == 5) {
    d <- x[,1:9]
    a <- x[,10]
  }
  else if(m == 6) {
    d <- x[,1:34]
    a <- as.numeric(x[,35])
  }
  else if(m == 7) {
    d <- x[,2:17]
    a <- as.numeric(x[,1])
  }
  else if(m == 8) {
    d <- x
    a <- NULL
  }
  else if(m == 9) {
    d <- x[,1:8]
    a <- as.numeric(x[,9])
  }
  else if(m == 10) {
    d <- x[,1:36]
    a <- as.numeric(x[,37])
  }
  else if(m == 11) {
    d <- x[,1:4]
    d[,1] <- as.numeric(d[,1])
    d[,2] <- as.numeric(d[,2])
    a <- x[,5]
  }
  else if(m == 12) {
    d <- x[,1:9]
    a <- as.numeric(x[,10])
  }
  else if(m == 13) {
    d <- x[,1:60]
    a <- as.numeric(x[,61])
  }
  else if(m == 14) {
    d <- x[,2:36]
    a <- as.numeric(x[,1])
  }
  else if(m == 15) {
    d <- x[,1:18]
    a <- as.numeric(x[,19])
  }
  else if(m == 16) {
    d <- x[,1:10]
    a <- as.numeric(x[,11])
  }
  else if(m == 17) {
    d <- x[,1:16]
    a <- as.numeric(x[,17])
  }
  else if(m == 18) {
    d <- matrix(0,nrow(x),ncol(x))
    d[which(is.na(x[,2:17]))]<--1
    d[which(x[,2:17]=='n')]<-0
    d[which(x[,2:17]=='y')]<-1
    a <- rep(0,nrow(x))
    a[which(x[,1]=='democrat')] <- 1
  }
  
  complete <- complete.cases(d)
  a <- a[complete]
  d <- d[complete,]
  
  if(random) {
    rand <- sample(1:length(a),length(a),replace=F)
    a <- a[rand]
    d <- d[rand,]
  }
  
  d <- apply(d, 2L, as.numeric)
  if(scale) d <- scale(d)

  a <- as.integer(a)
  
  k <- length(unique(a))
  
  l <- DSD_Memory(d, k=k, class=a, description=paste("mlbench:", data))
  class(l) <- c("DSD_mlbenchData", class(l))
  l
}
