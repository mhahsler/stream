\name{DSD_ReadDB}
\alias{DSD_ReadDB}
\title{Read a Data Stream from an open DB Query}

\description{
A DSD class that reads a data stream from an open DB result set from a relational database with using R's data base interface (DBI).
}

\usage{
DSD_ReadDB(result, k=NA, class=NULL, description=NULL) 
}

\arguments{
	\item{result}{An open DBI result set.}
	\item{k}{Number of true clusters, if known.}
	\item{class}{column index for the class/cluster assignment.}
	\item{description}{a character string describing the data.}
}

\details{
This class provides a streaming interface for result sets from a data base 
with via \pkg{DBI}. You need to connect to the data base and submit a SQL query 
using \code{dbGetQuery()} to 
obtain a result set. Make sure that your query only includes the columns
that should be included in the stream. Do not forget to close the result set and 
the data base connection.
}

\value{
    An object of class \code{DSD_ReadDB} (subclass of \code{DSD_R},
    \code{DSD}).
}

\seealso{
	\code{\link{DSD}},
  \code{\link[DBI]{dbGetQuery}}
}

\author{
    Michael Hahsler
}

\examples{
### create a data base with a table with 3 Gaussians
library("RSQLite")
con <- dbConnect(RSQLite::SQLite(), ":memory:")

points <- get_points(DSD_Gaussians(k=3, d=2), 600, class = TRUE)
head(points)

dbWriteTable(con, "gaussians", points)
  
### prepare a query result set  
res <- dbSendQuery(con, "SELECT X1, X2, class FROM gaussians")
res
  
### create a stream interface to the result set  
stream <- DSD_ReadDB(res, k=3, class = 3)

### get points
get_points(stream, 5, class = TRUE)  
plot(stream)
  
### clean up  
dbClearResult(res)
dbDisconnect(con)
}

