library(dielectric)
library(ggplot2)

data(AgPalik)

silver <- AgPalik$predict(range=c(0.2, 0.8), n=300, all.knots=TRUE)

d <- dielectric2plot(silver)
raw <- dielectric2plot(AgPalik$raw())


ggplot(d, aes(wavelength, value)) + geom_path() +
  facet_grid(variable~., scales="free") +
  geom_point(data=subset(raw, wavelength > 0.2 & wavelength < 0.8))


dwide <- with(silver, data.frame(wavelength,
                                 real=Re(epsilon),
                                 imag=Im(epsilon)))


