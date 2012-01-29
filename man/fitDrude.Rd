\name{fitDrude}
\alias{fitDrude}
\title{fitFDrude}
\usage{
  fitDrude(p, material, ...)
}
\arguments{
  \item{p}{parameters vector (3)}

  \item{material}{data.frame with wavelength in microns and
  complex epsilon}

  \item{...}{passed to Drude}
}
\value{
  sum of squares
}
\description{
  Objective function for the Drude model
}
\details{
  Used to fit a Drude model to a material
}
\author{
  Baptiste Auguie
}

