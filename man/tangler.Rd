\name{tangler}
\docType{methods}
\alias{tangler}
\alias{tangler}
\title{Spatial point patterns and raster objects deidentified with a given \code{detangler} object.}
\description{For a given \code{detangler} object, spatial coordinates are deidentified accordingly.}

\usage{tangler(data=NULL, tanglerInfo = NULL, raster_object = FALSE, stub = NULL)}

\arguments{
  \item{data}{Can be either a 2 column \code{matrix} of spatial coordinates or a \code{raster} object.} 
  \item{tanglerInfo}{output from \code{tangles} which encdoes the steps and values of the deidentification process.}
  \item{raster_object}{logical; If \code{TRUE} the data to be deidentified is a raster object. This means the data associated with the raster are carried through to the outputs.}
  \item{stub}{Character; A string of user-defined setting for establishing naming conventions.}
  
}

\value{The transformed coordinates or raster object (dependent on input data).}


\note{Outputs are also saved to the working directory with the hash key associated with the deidentification and user-defined stub in the file name. This will be saved as an \code{.rds} file that will also have \code{tanglerXY} in the file name.

\code{tangles} is coordinate reference system agnostic. Deidentification will proceed regardless of whether data is in geographic or projected space. Deidentified coordinates will not exist in reality, but the spatial properties of the original data will be maintained.}

\author{Brendan Malone}

\references{
\itemize{
\item CM O`Keefe, S Otorepec, M Elliot, E Mackey, and K O`Hara (2017) \href{https://www.data61.csiro.au/en/Our-Work/Safety-and-Security/Privacy-Preservation/De-identification-Decision-Making-Framework}{The De-Identification Decision Making Framework}. CSIRO Reports EP173122 and EP175702.}}

\examples{
## POINT DATA tangles
# library(digest)
# data("HV_subsoilpH")
# str(HV_subsoilpH)
# dat.xy<- HV_subsoilpH[,1:2]
# xyData<- as.matrix(dat.xy)
# deidentify with 5 levels of abstraction
# tangles.out<- tangles(data = xyData, depth = 5, rasterdata = TRUE, raster_object = FALSE)


# tangle associated RASTER DATA
# library(raster)
# data("hunterCovariates_sub")
# tangled.origi<- tangler(data = hunterCovariates_sub, tanglerInfo = tangles.out[[2]], 
#                          raster_object = TRUE, stub = "hv1")

# PLOT real
# par(mfrow=c(1,2))
# plot(hunterCovariates_sub[[1]], main = "original")
# coordinates(dat.xy)<- ~ X + Y
# plot(dat.xy, add=T)

# PLOT deidentified
# plot(tangled.origi[[1]], main = "deidentified")
# ndat.xy<- as.data.frame(tangles.out[[1]])
# coordinates(ndat.xy)<- ~ X + Y
# plot(ndat.xy, add = T)

}

\keyword{methods}
