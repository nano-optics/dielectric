library(ggplot2)
library(dielectric)

## http://refractiveindex.info/legacy/?group=METALS&material=Chromium
Chromium_raw <- read.table("METALS_Chromium_sopra.txt",head=TRUE)

comment(Chromium_raw) <- c(source = "Chromium, SOPRA [http://refractiveindex.info/legacy/?group=METALS&material=Chromium]", 
                          units = "nanometres")

Chromium <- with(Chromium_raw, new("dielectric", wavelength=lambda*1e3, 
                                 epsilon=(n+1i*k)^2, 
                                 span=range(lambda*1e3), 
                                 comment=as.list(comment(Chromium_raw))))

save(Chromium, file="Chromium.rda")

Chromium$set_span(200, 800)
raw <- dielectric2plot(Chromium$raw())
cr <- Chromium$predict()

d <- dielectric2plot(cr)

ggplot(d, aes(wavelength, value)) + geom_path() +
  facet_grid(variable~., scales="free") +
  geom_point(data=raw)

