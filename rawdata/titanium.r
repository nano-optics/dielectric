library(ggplot2)
library(dielectric)

## http://refractiveindex.info/legacy/?group=METALS&material=Chromium
raw <- read.table("Ti.nk",head=F, skip=8)
names(raw) <- c("lambda","n","k")

comment(raw) <- c(source = "Ti_llnl_cxro + Ti_windt, luxpop", 
                          units = "nanometres")

raw <- subset(raw, lambda > 28)
Ti <- with(raw, new("dielectric", wavelength=lambda*10, 
                                 epsilon=(n+1i*k)^2, 
                                 span=range(lambda*10), 
                                 comment=as.list(comment(raw))))

save(Ti, file="Ti.rda")

Ti$set_span(200, 800)
raw <- dielectric2plot(Ti$raw())
pred <- Ti$predict()

d <- dielectric2plot(pred)

ggplot(d, aes(wavelength, value)) + geom_path() +
  facet_grid(variable~., scales="free") +
  geom_point(data=raw)

