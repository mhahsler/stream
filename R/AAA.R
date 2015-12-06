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



### noise symbol and color
.noise_pch <- 20L 
.noise_col <- gray(.5, alpha = .3)
.points_col <- gray(.5, alpha = .5)

### helper for doing things in blocks
.make_block <- function(n, block) {
    if(n<block) return(n)
    
    b <- rep(block, times=as.integer(n/block))
    if(n%%block) b<- c(b, n%%block)
    b
}

### line break helper
.line_break <- function(x, width=options("width")) {
  form <- paste('(.{1,', width,'})(\\s|$)', sep='')
  gsub(form, '\\1\n', x)
}

### nodots
.nodots <- function(...) {
  l <- list(...)
  if(length(l) > 0L) warning("Unknown arguments: ", 
    paste(names(l), "=",l, collapse=", "))
}

