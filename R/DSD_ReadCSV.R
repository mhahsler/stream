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


### FIXME: test class and take

# accepts an open connection
# ... goes to read.table
DSD_ReadCSV <- function(file, k=NA,
  take=NULL, class=NULL, loop=FALSE, 
  sep=",", header=FALSE, skip=0, colClasses = NA, ...) {
  
  # if the user passes a string, create a new connection and open it
  if (is(file, "character")) file <- file(file)
  
  # error out if no string or connection is passed
  if (!is(file, "connection")) stop("Please pass a valid connection!")
  
  # open the connection if its closed
  if (!isOpen(file)) open(file)
  
  # filename
  filename <- basename(summary(file)$description)
  
  # seekable?
  if(loop && !isSeekable(file)) stop("Loop only allowed for seekable connections!")
  
  # read first point to figure out structure!
  if(skip>0) readLines(file, n=skip)
  point <- read.table(text=readLines(con=file, n=1+header), 
    sep=sep, header=header, colClasses = colClasses, ...)
 
  # reset stream if possible (otherwise first point is lost)
  if(isSeekable(file)) {
    seek(file, where=0)
    if(skip>0) readLines(file, n=skip)
    if(header) readLines(file, n=1)
  }
  
  # select columns take
  if(!is.null(take)) {
    if(is.character(take)) take <- pmatch(take, colnames(point))
    if(any(is.na(take))) stop("Invalid column name specified in take!")
    point <- point[,take]
  }
  
  # dimensions
  d <- ncol(point) - !is.null(class)
    
  # header?
  if(header) header <- colnames(point)
  else header <- NULL
  
  # data types?
  colClasses <- sapply(point[1,], class)
  ### integer -> numeric, factor -> character
  colClasses[colClasses == "integer"] <- "numeric"
  colClasses[colClasses == "factor"] <- "character"
  
  
 
  # class?
  if(is.character(class)) {
    if(is.null(header)) stop("Only numeric column index allowed if no headers are available!")
    class <- pmatch(class, header)
    if(is.na(class)) stop("No matching column name for class!")
  } else if(!is.null(class)) {
    if(!is.null(take)) class <- match(class, take)
    if(is.na(class)) stop("Invalid class column index!")
  }
  
  # creating the DSD object
  l <- list(
    description = paste('File Data Stream (', filename, ')', sep=''),
    d = d,
    k = k,
    file = file,
    sep = sep,
    take = take,
    header = header,
    colClasses = colClasses,
    read.table.args = list(...),
    class = class,
    loop = loop,
    skip = skip)
  class(l) <- c("DSD_ReadCSV", "DSD_R", "DSD_data.frame", "DSD")
  
  l
}

## it is important that the connection is OPEN
get_points.DSD_ReadCSV <- function(x, n=1, 
  outofpoints=c("stop", "warn", "ignore"), 
  cluster = FALSE, class = FALSE,  ...) {
  .nodots(...)
  
  #.DEBUG <- TRUE
  .DEBUG <- FALSE
  
  if((class || cluster) && is.null(x$class)) 
    stop("Stream does not support class/cluster labels.")
  
  outofpoints <- match.arg(outofpoints)
  noop <- function(...) {}
  msg <- switch(outofpoints,
    "stop" = stop,
    "warn" = warning,
    "ignor" = noop
  )
  
  n <- as.integer(n)  
  
  ## remember position
  if(!isSeekable(x$file)) pos <- NA else pos <- seek(x$file)
  
  d <- NULL
  eof <- FALSE
  
  ## only text connections can do read.table without readLine (would be faster)
  #if(summary(x$file)$text == "text"){
  #  suppressWarnings(
  #    try(d <- do.call(read.table, c(list(file=x$file, sep=x$sep, nrows=n, 
  #      colClasses=x$colClasses), x$read.table.args)), 
  #      silent = TRUE))
  #}    
  
  try(lines <- readLines(con=x$file, n=n), silent = !.DEBUG)
  
  ## EOF?
  if(length(lines) < 1) eof <- TRUE
  else {
    suppressWarnings(
      try(d <- do.call(read.table, 
        c(list(text=lines, sep=x$sep, nrows=n, 
          colClasses=x$colClasses), x$read.table.args)), 
        silent = !.DEBUG))
  }
  
  if(eof) msg("The stream is at its end (EOF)!")
  ## loop?
  if(is.null(d) || nrow(d) < n || eof) {
    if(!x$loop) {
      ## try to undo read in case of stop
      if(outofpoints == "stop" && !is.na(pos)) seek(x$file, pos) 
      if(!eof) msg("Not enough points in the stream!")
    } else { ## looping
      while(nrow(d) < n) {
        reset_stream(x)
        try(lines <- readLines(con=x$file, n=n-nrow(d)), silent = !.DEBUG)
        
        ## EOF?
        if(length(lines) == 0) eof <- TRUE
        else {
          d2 <- NULL
          suppressWarnings(
            try(d2 <- do.call(read.table, 
              c(list(text=lines, sep=x$sep, nrows=n, 
                colClasses=x$colClasses), x$read.table.args)), 
              silent = !.DEBUG))
          if(!is.null(d2) && nrow(d2 > 0)) d <- rbind(d, d2)
          else msg("Read failed (use smaller n for unreliable sources)!")
        }
        
      }
    }      
  }
  
  ## no data!
  if(is.null(d)) {
    if(!eof) msg("Read failed (use smaller n for unreliable sources)!")
   
    ## create conforming data.frame with 0 rows
    d <- data.frame()
    for(i in 1:length(x$colClasses)) 
      d[[i]] <- do.call(x$colClasses[i], list(0))
  } else {
    ## take columns
    if(!is.null(x$take)) d <- d[,x$take, drop=FALSE]
  }
  
  ## remove additional columns from a bad line
  if(ncol(d) > x$d+!is.null(class)) d <- d[, 1:(x$d+!is.null(class)), drop=FALSE]
   
  if(nrow(d)>0) {
    if(!is.null(x$header)) colnames(d) <- x$header
    
    if(!is.null(x$class)) {
      cl <- d[,x$class]
      d <- d[,-x$class, drop=FALSE]
      if(class) d <- cbind(d, class = cl)
      if(cluster) attr(d, "cluster") <- cl
    }
  }  
  
  d
}

reset_stream.DSD_ReadCSV <- function(dsd, pos=1) {
  pos <- as.integer(pos)
  
  if(!isSeekable(dsd$file)) stop("Underlying conneciton does not support seek!")
  seek(dsd$file, where=0)
  
  if(dsd$skip>0) readLines(dsd$file, n=dsd$skip)
  if(!is.null(dsd$header)) readLines(dsd$file, n=1)
  if(pos>1) readLines(dsd$file, n=pos-1L) 
  invisible(NULL)
}

close_stream <- function(dsd) {
  if(!is(dsd, "DSD_ReadCSV")) 
    stop("'dsd' is not of class 'DSD_ReadCSV'")
  close(dsd$file)
}
