\name{fit_drude}
\alias{fit_drude}
\title{fit_drude}
\usage{
  fit_drude(p, material, ...)
}
\arguments{
  \item{p}{parameters vector (3)}

  \item{material}{data.frame with wavelength in nm and
  complex epsilon}

  \item{...}{passed to drude}
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

