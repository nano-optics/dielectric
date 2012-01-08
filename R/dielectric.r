
dielectric <- setRefClass("dielectric",
                          fields = list( wavelength = "vector",
                            epsilon = "vector",
                            comment = "list"),
                          methods = list(
                            ##' spline interpolation of permittivity
                            ##'
                            ##' spline interpolation of permittivity
                            ##' @title spline
                            ##' @param ... 
                            ##' @return list
                            ##' @author baptiste Auguié
                            spline = function(...) {
                              'returns a list of splinefun for the real and imaginary parts'
                              sp <- 
                                list(real=smooth.spline(wavelength, Re(epsilon), ...),
                                     imag=smooth.spline(wavelength, Im(epsilon), ...))
                              invisible(sp)
                            },
                            raw = function(){
                              'return the raw data as real numbers'
                            data.frame(wavelength=wavelength,
                                       epsilon=epsilon, real = Re(epsilon), imag=Im(epsilon))
                          },
                            permittivity = function(new.wavelength, ...){
                              'predict a single value'
                              predict(range=rep(new.wavelength, 2), n=1, ...)
                              },
                            predict = function(sp=NULL, range=c(0.4, 0.8),
                              n=length(epsilon), new.wavelength=NULL,...)
                            {
                              'interpolation with splines'
                              if(is.null(range))
                                 range <- range(wavelength)
                              
                              if(is.null(new.wavelength)){ 
                                new.wavelength <- if(is.null(n)) { # use the original points
                                  wavelength[findInterval(wavelength, range)] } else
                                {
                                  seq(range[1], range[2], length=n)
                                }
                              }
                              
                              sp <- if(is.null(sp)) spline(...)
                              
                              smooth <-
                                data.frame(wavelength=new.wavelength,
                                           epsilon = complex(real=stats::predict(sp$real,
                                                               new.wavelength)$y,
                                             imag=stats::predict(sp$imag, new.wavelength)$y))
                              invisible(smooth)
                            }
                            ))

