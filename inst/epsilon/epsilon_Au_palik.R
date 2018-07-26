n <- read.csv("Au_n_Palik.dat")
k <- read.csv("Au_k_Palik.dat")

all(n[,1]==k[,1])
gold <- data.frame(wavelength = k[,1], n = n[,2]+1i*k[,2])
gold$epsilon = gold$n^2

library(dielectric)

d <- subset(gold, wavelength > 300 & wavelength<1000)
m <- dielectric2plot(d)

ggplot(m, aes(wavelength, value))+
  geom_line() + facet_wrap(~variable)

head(d)
write.table(d, file='AuPalik.txt', row.names = FALSE)


