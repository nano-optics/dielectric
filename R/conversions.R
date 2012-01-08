##' Unit conversions
##'
##' Unit conversions
##' @title L2eV
##' @aliases eV2L L2w w2L t2eV
##' @param wavelength
##' @param energy 
##' @param time 
##' @return converted unit
##' @author baptiste Auguié
`L2eV` <- 
function(wavelength)
{
  with(constants, h * cel / ee / wavelength)
}

`eV2L` <- function(energy)
{
  with(constants, h * cel / ee / energy)
}

`L2w` <- 
function(wavelength)
{
  with(constants, 2*pi * cel / wavelength)
}


t2eV <- function(time){
  with(constants, h / (pi*time*ee))
}

