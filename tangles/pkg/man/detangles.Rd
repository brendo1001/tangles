\name{detangles}
\docType{methods}
\alias{detangles}
\title{Revert deidentified point pattern or raster object}
\description{Deidentified spatial data can be reverted back to their original coordinates with a given detangler object. For correct re-identification, there is an assumed match in the hash key between deidentified data and detangler object.}

\usage{detangles(data=NULL, tanglerInfo=NULL, raster_object = FALSE, stub = NULL, hash_key=NULL)}

\arguments{
\item{data}{Can be either a 2 column \code{matrix} of spatial coordinates or a \code{raster} object.} 
\item{tanglerInfo}{output from \code{tangles} which encodes the steps and values of the re-identification process.}
\item{raster_object}{logical; If \code{TRUE} the data to be re-identified is a raster object. This means the data associated with the raster are carried through to the outputs.}
\item{stub}{Character; A string of user-defined setting for establishing naming conventions.}
\item{hash_key}{Character; A string of the hash key that was used for the original deidentification.}
}

\value{The transformed coordinates or raster object (dependent on input data).}


\note{Outputs are also saved to the working directory with the hash key associated with the \code{detangler} object and user-defined stub in the file name. This will be saved as an \code{.rds} file.}

\author{Brendan Malone}

\references{
\itemize{
\item CM O`Keefe, S Otorepec, M Elliot, E Mackey, and K O`Hara (2017) \href{https://www.data61.csiro.au/en/Our-Work/Safety-and-Security/Privacy-Preservation/De-identification-Decision-Making-Framework}{The De-Identification Decision Making Framework}. CSIRO Reports EP173122 and EP175702.}
}

\examples{

## POINT DATA tangles
# library(digest)
# data("HV_subsoilpH")
# str(HV_subsoilpH)
# dat.xy<- HV_subsoilpH[,1:2]
# xyData<- as.matrix(dat.xy)
# deidentify with 5 levels of abstraction
# tangles.out<- tangles(data = xyData, depth = 5, rasterdata = TRUE, raster_object = FALSE)

# detangle
# xyData<- as.matrix(tangles.out[[1]])
# point_detang<- detangles(data=xyData, tanglerInfo=tangles.out[[2]], raster_object = FALSE, stub = "hv_fix", hash_key = "Enter In Hash Key Here")
}

\keyword{methods}
