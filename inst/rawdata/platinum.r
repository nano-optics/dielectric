library(ggplot2)
library(dielectric)

## http://refractiveindex.info/?shelf=main&book=Al&page=Rakic
PtPalik_raw <- read.table("METALS_Platinum_Palik.txt",head=TRUE)

comment(AlRakic_raw) <- c(source = "Platinum, Palik [http://refractiveindex.info/legacy/?group=METALS&material=Platinum]", 
                          units = "nanometres")

PtPalik <- with(PtPalik_raw, new("dielectric", wavelength=lambda*1e3, 
                                 epsilon=(n+1i*k)^2, 
                                 span=range(lambda*1e3), 
                                 comment=as.list(comment(PtPalik_raw))))

save(PtPalik, file="PtPalik.rda")

PtPalik$set_span(200, 800)
raw <- dielectric2plot(PtPalik$raw())
pt <- PtPalik$predict()

d <- dielectric2plot(pt)

ggplot(d, aes(wavelength, value)) + geom_path() +
  facet_grid(variable~., scales="free") +
  geom_point(data=raw)

