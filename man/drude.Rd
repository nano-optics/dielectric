\name{drude}
\alias{drude}
\title{drude}
\usage{
  drude(wavelength, omega = NULL, p = c(1e+16, 1e+14, 1),
    omega_p = p[1], gamma_p = p[2], epsilon_inf = p[3],
    ...)
}
\arguments{
  \item{wavelength}{wavelength in m}

  \item{omega}{angular frequency in rad/s}

  \item{p}{vector of 3 parameters}

  \item{omega_p}{plasma frequency in rad/s}

  \item{gamma_p}{damping constant, in rad/s}

  \item{epsilon_inf}{background dielectric function}

  \item{...}{not used}
}
\value{
  a data.frame with wavelength in microns and complex
  dielectric function
}
\description{
  Drude model for the dielectric function of good (governed
  by free electrons) metals
}
\details{
  a bacground contribution eps_inf is assumed for the core
  electrons
}
\author{
  Baptiste Auguie
}

