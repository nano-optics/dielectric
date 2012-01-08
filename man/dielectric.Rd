\name{dielectric-class}
\Rdversion{1.1}
\docType{class}
\alias{dielectric-class}
\alias{dielectric}

\title{Class "dielectric"}
\description{
Set of R methods to transform dielectric functions
} 
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{wavelength}:}{numeric vector}
    \item{\code{epsilon}:}{complex vector}
  }
}
\section{Methods}{
  \describe{
    \item{\code{predict(sp, range, n, new.wavelength, ...)}:}{ new values from spline interpolation of the data }
    \item{\code{spline(...)}:}{ spline interpolation of the data }
    \item{\code{raw()}:}{ raw data as a data.frame with real and imaginary parts }
  }
}

