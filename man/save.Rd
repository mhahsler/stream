\name{save}
\alias{save}
\alias{saveDSC}
\alias{readDSC}
\title{Save and Read DSC Objects}

\description{
Save and Read DSC objects savely (serializes the underlying data structure).
This also works for \pkg{streamMOA} DSC objects.
}

\usage{
saveDSC(object, file, ...)
readDSC(file)
}

\arguments{
  \item{object}{a DSC object.}
  \item{file}{filename.}
  \item{...}{further arguments.}
}


\seealso{
  \code{\link{saveRDS}} and \code{\link{readRDS}}. 
}

\author{
    Michael Hahsler
}


\examples{
stream <- DSD_Gaussians(k = 3, noise = 0.05)

# create clusterer with r = 0.05
dbstream1 <- DSC_DBSTREAM(r = .05)
update(dbstream1, stream, 1000)
dbstream1

saveDSC(dbstream1, file="dbstream.Rds")

dbstream2 <- readDSC("dbstream.Rds")
dbstream2

## cleanup
unlink("dbstream.Rds")
}
