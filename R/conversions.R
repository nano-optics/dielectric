
# constants <- list(c = structure(299792458, comment = "speed of light in vacuum [m.s^-1]"), 
#                h = structure(6.62606896e-34, comment = "Planck constant [J.s]"), 
#                hbar = structure(1.054571628e-34, comment = "reduced Planck constant [J.s]"), 
#                ee = structure(1.602176487e-19, comment = "electron charge [C]"), 
#                eps0 = structure(8.854187817e-12, comment = "permittivity of vacuum [F.m^-1]"), 
#                Z0 = structure(376.730313461, comment = "vacuum impedance [ohm]"), 
#                Nav = structure(6.0221415e+23, comment = "Avogadro number [mol^-1]"), 
#                mu0 = structure(1.25663706143592e-06, comment = "permeability of vacuum [N.A^-2]"))                                                                                                            

##' Unit conversions
##'
##' Unit conversions
##' @title L2eV
##' @param wavelength wavelength in m
##' @return converted unit
##' @family conversion 
`L2eV` <- 
function(wavelength)
{
  h <- 6.62606896e-34
  c <- 299792458
  e <- 1.602176487e-19
  h * c / e / wavelength
}

##' Unit conversions
##'
##' Unit conversions
##' @title eV2L
##' @param energy energy in eV
##' @family conversion 
`eV2L` <- function(energy)
{
  h <- 6.62606896e-34
  c <- 299792458
  e <- 1.602176487e-19
  h * c / e / energy
}

##' Unit conversions
##'
##' Unit conversions
##' @title L2w
##' @param wavelength wavelength in m
##' @family conversion 
`L2w` <- 
function(wavelength)
{
  c <- 299792458
  2*pi * c / wavelength
}
##' Unit conversions
##'
##' Unit conversions
##' @title t2eV
##' @param time time in s
##' @family conversion 
t2eV <- function(time){
  h <- 6.62606896e-34
  e <- 1.602176487e-19
  h / (pi*time*e)
}

