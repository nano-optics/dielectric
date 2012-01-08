library(dielectric)
library(ggplot2)

data(AuJC)

gold <- AuJC$predict(n=300)

d <- dielectric2plot(gold)
raw <- dielectric2plot(AuJC$raw())

ggplot(d, aes(wavelength, value)) + geom_path() +
  facet_grid(variable~., scales="free") +
  geom_point(data=subset(raw, wavelength > 0.4 & wavelength < 0.8))

dwide <- with(gold, data.frame(wavelength,
                                 real=Re(epsilon),
                                 imag=Im(epsilon)))

