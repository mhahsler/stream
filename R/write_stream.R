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

### write data from a stream to a file

write_stream <- function(dsd, file, n=100, block=100000L, 
  class=FALSE, append = FALSE, sep=",", 
  header=FALSE, row.names=FALSE, ...) UseMethod("write_stream")

write_stream.default <- function(dsd, file, n=100, block=100000L, 
  class=FALSE, append = FALSE, sep=",", header=FALSE, row.names=FALSE, ...) {
  stop(gettextf("write_stream not implemented for class '%s'.", class(dsd)))
}

write_stream.DSD <- function(dsd, file, n=100, block=100000L, 
  class=FALSE, append = FALSE, sep=",", header=FALSE, row.names=FALSE, ...) {	

  # make sure files are not overwritten
  if(is(file, "character") && file.exists(file) && !append) 
    stop("file exists already. Please remove the file first.")
  
  # string w/ file name (clears the file)
  if (is(file, "character")) {
    if(append) file <- file(file, open="a")
    else file <- file(file, open="w")
  }
  # error	
  else if (!is(file, "connection")) stop("Please pass a valid connection!")
  
  # needs opening
  else if (!isOpen(file)) open(file)
  
  # all following calls have to have col.names=FALSE regardless
  for (bl in .make_block(n, block)) {
    p <- get_points(dsd, bl, class=class)
    
    ## suppress warning for append and col.names
    suppressWarnings(write.table(p, 
      file, sep=sep, append=TRUE, col.names=header,
      row.names=row.names, ...))
  }
  close(file)
}

