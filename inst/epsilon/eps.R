# d <- read.table('ph5004237_si_002.txt')
d2 <-  read.table('Olmon_PRB2012_SC.dat', skip=3)
wat <- read.table('water.txt')
gold <- data.frame(wavelength = d$V1, imag = d$V5, real = d$V4)
m <- subset(melt(gold, id="wavelength"), wavelength < 1000)
gold2 <- data.frame(wavelength = d2$V2*1e9, imag = d2$V4, real = d2$V3)
m2 <- subset(melt(gold2, id="wavelength"), wavelength > min(m$wavelength) &
               wavelength < max(m$wavelength))
library(ggplot2)
library(reshape2)

ggplot(m, aes(wavelength, value, colour=variable)) +
  geom_line() +
  geom_line(data=m2, linetype=2)

epsWater <- function(wavelength){
  raw <- structure(list(wavelength = 300:800, V1 = c(1.8474, 1.8468, 1.8461, 
                                                     1.8454, 1.8447, 1.8441, 1.8434, 1.8428, 1.8422, 1.8415, 1.8409, 
                                                     1.8403, 1.8397, 1.8391, 1.8385, 1.8379, 1.8373, 1.8368, 1.8362, 
                                                     1.8357, 1.8351, 1.8346, 1.834, 1.8335, 1.833, 1.8324, 1.8319, 
                                                     1.8314, 1.8309, 1.8304, 1.8299, 1.8294, 1.829, 1.8285, 1.828, 
                                                     1.8275, 1.8271, 1.8266, 1.8262, 1.8257, 1.8253, 1.8248, 1.8244, 
                                                     1.824, 1.8236, 1.8231, 1.8227, 1.8223, 1.8219, 1.8215, 1.8211, 
                                                     1.8207, 1.8203, 1.8199, 1.8195, 1.8192, 1.8188, 1.8184, 1.818, 
                                                     1.8177, 1.8173, 1.8169, 1.8166, 1.8162, 1.8159, 1.8155, 1.8152, 
                                                     1.8148, 1.8145, 1.8142, 1.8138, 1.8135, 1.8132, 1.8129, 1.8125, 
                                                     1.8122, 1.8119, 1.8116, 1.8113, 1.811, 1.8107, 1.8104, 1.8101, 
                                                     1.8098, 1.8095, 1.8092, 1.8089, 1.8086, 1.8084, 1.8081, 1.8078, 
                                                     1.8075, 1.8073, 1.807, 1.8067, 1.8064, 1.8062, 1.8059, 1.8057, 
                                                     1.8054, 1.8051, 1.8049, 1.8046, 1.8044, 1.8041, 1.8039, 1.8037, 
                                                     1.8034, 1.8032, 1.8029, 1.8027, 1.8025, 1.8022, 1.802, 1.8018, 
                                                     1.8015, 1.8013, 1.8011, 1.8009, 1.8006, 1.8004, 1.8002, 1.8, 
                                                     1.7998, 1.7996, 1.7994, 1.7991, 1.7989, 1.7987, 1.7985, 1.7983, 
                                                     1.7981, 1.7979, 1.7977, 1.7975, 1.7973, 1.7971, 1.7969, 1.7968, 
                                                     1.7966, 1.7964, 1.7962, 1.796, 1.7958, 1.7956, 1.7954, 1.7953, 
                                                     1.7951, 1.7949, 1.7947, 1.7946, 1.7944, 1.7942, 1.794, 1.7939, 
                                                     1.7937, 1.7935, 1.7933, 1.7932, 1.793, 1.7928, 1.7927, 1.7925, 
                                                     1.7924, 1.7922, 1.792, 1.7919, 1.7917, 1.7916, 1.7914, 1.7913, 
                                                     1.7911, 1.7909, 1.7908, 1.7906, 1.7905, 1.7903, 1.7902, 1.79, 
                                                     1.7899, 1.7898, 1.7896, 1.7895, 1.7893, 1.7892, 1.789, 1.7889, 
                                                     1.7888, 1.7886, 1.7885, 1.7884, 1.7882, 1.7881, 1.7879, 1.7878, 
                                                     1.7877, 1.7875, 1.7874, 1.7873, 1.7872, 1.787, 1.7869, 1.7868, 
                                                     1.7866, 1.7865, 1.7864, 1.7863, 1.7861, 1.786, 1.7859, 1.7858, 
                                                     1.7856, 1.7855, 1.7854, 1.7853, 1.7852, 1.7851, 1.7849, 1.7848, 
                                                     1.7847, 1.7846, 1.7845, 1.7844, 1.7842, 1.7841, 1.784, 1.7839, 
                                                     1.7838, 1.7837, 1.7836, 1.7835, 1.7834, 1.7832, 1.7831, 1.783, 
                                                     1.7829, 1.7828, 1.7827, 1.7826, 1.7825, 1.7824, 1.7823, 1.7822, 
                                                     1.7821, 1.782, 1.7819, 1.7818, 1.7817, 1.7816, 1.7815, 1.7814, 
                                                     1.7813, 1.7812, 1.7811, 1.781, 1.7809, 1.7808, 1.7807, 1.7806, 
                                                     1.7805, 1.7804, 1.7803, 1.7802, 1.7801, 1.78, 1.7799, 1.7799, 
                                                     1.7798, 1.7797, 1.7796, 1.7795, 1.7794, 1.7793, 1.7792, 1.7791, 
                                                     1.779, 1.779, 1.7789, 1.7788, 1.7787, 1.7786, 1.7785, 1.7784, 
                                                     1.7783, 1.7783, 1.7782, 1.7781, 1.778, 1.7779, 1.7778, 1.7778, 
                                                     1.7777, 1.7776, 1.7775, 1.7774, 1.7774, 1.7773, 1.7772, 1.7771, 
                                                     1.777, 1.777, 1.7769, 1.7768, 1.7767, 1.7766, 1.7766, 1.7765, 
                                                     1.7764, 1.7763, 1.7762, 1.7762, 1.7761, 1.776, 1.7759, 1.7759, 
                                                     1.7758, 1.7757, 1.7756, 1.7756, 1.7755, 1.7754, 1.7754, 1.7753, 
                                                     1.7752, 1.7751, 1.7751, 1.775, 1.7749, 1.7748, 1.7748, 1.7747, 
                                                     1.7746, 1.7746, 1.7745, 1.7744, 1.7744, 1.7743, 1.7742, 1.7741, 
                                                     1.7741, 1.774, 1.7739, 1.7739, 1.7738, 1.7737, 1.7737, 1.7736, 
                                                     1.7735, 1.7735, 1.7734, 1.7733, 1.7733, 1.7732, 1.7731, 1.7731, 
                                                     1.773, 1.7729, 1.7729, 1.7728, 1.7728, 1.7727, 1.7726, 1.7726, 
                                                     1.7725, 1.7724, 1.7724, 1.7723, 1.7722, 1.7722, 1.7721, 1.7721, 
                                                     1.772, 1.7719, 1.7719, 1.7718, 1.7717, 1.7717, 1.7716, 1.7716, 
                                                     1.7715, 1.7714, 1.7714, 1.7713, 1.7713, 1.7712, 1.7711, 1.7711, 
                                                     1.771, 1.771, 1.7709, 1.7709, 1.7708, 1.7707, 1.7707, 1.7706, 
                                                     1.7706, 1.7705, 1.7704, 1.7704, 1.7703, 1.7703, 1.7702, 1.7702, 
                                                     1.7701, 1.7701, 1.77, 1.7699, 1.7699, 1.7698, 1.7698, 1.7697, 
                                                     1.7697, 1.7696, 1.7696, 1.7695, 1.7694, 1.7694, 1.7693, 1.7693, 
                                                     1.7692, 1.7692, 1.7691, 1.7691, 1.769, 1.769, 1.7689, 1.7688, 
                                                     1.7688, 1.7687, 1.7687, 1.7686, 1.7686, 1.7685, 1.7685, 1.7684, 
                                                     1.7684, 1.7683, 1.7683, 1.7682, 1.7682, 1.7681, 1.7681, 1.768, 
                                                     1.768, 1.7679, 1.7679, 1.7678, 1.7678, 1.7677, 1.7677, 1.7676, 
                                                     1.7676, 1.7675, 1.7675, 1.7674, 1.7674, 1.7673, 1.7673, 1.7672, 
                                                     1.7672, 1.7671, 1.7671, 1.767, 1.767, 1.7669, 1.7669, 1.7668, 
                                                     1.7668, 1.7667, 1.7667, 1.7666, 1.7666, 1.7665, 1.7665, 1.7664, 
                                                     1.7664, 1.7663, 1.7663, 1.7662, 1.7662, 1.7661, 1.7661, 1.766, 
                                                     1.766, 1.7659, 1.7659, 1.7659, 1.7658, 1.7658, 1.7657, 1.7657, 
                                                     1.7656, 1.7656, 1.7655, 1.7655, 1.7654, 1.7654, 1.7653, 1.7653, 
                                                     1.7652, 1.7652)), .Names = c("wavelength", "V1"), row.names = c(NA, 
                                                                                                                     -501L), class = "data.frame")
  data.frame(wavelength=wavelength, epsilon = approx(raw[,1],raw[,2],wavelength)$y)
}

epsGold <- function(wavelength){
  
  epsr <- approx(gold$wavelength,gold$real,wavelength)$y
  epsi <- approx(gold$wavelength,gold$imag,wavelength)$y
  data.frame(wavelength=wavelength, 
             epsilon = epsr + 1i*epsi)
  
}

epsGold2 <- function(wavelength){
  
  epsr <- approx(gold2$wavelength,gold2$real,wavelength)$y
  epsi <- approx(gold2$wavelength,gold2$imag,wavelength)$y
  data.frame(wavelength=wavelength, 
             epsilon = epsr + 1i*epsi)
  
}

library(mie)
library(dielectric)

goldJC <- epsAu(seq(400, 800))
goldACS <- epsGold(seq(400, 800))
goldPRB <- epsGold2(seq(400, 800))

water <- sqrt(epsWater(seq(400, 800)))[["epsilon"]]

xsec1 <- with(goldJC, mie(wavelength, epsilon, radius=29, medium=water, efficiency=FALSE))
xsec2 <- with(goldACS, mie(wavelength, epsilon, radius=29, medium=water, efficiency=FALSE))
xsec3 <- with(goldPRB, mie(wavelength, epsilon, radius=29, medium=water, efficiency=FALSE))

# matplot(cross_sections$wavelength, cross_sections[, -1], type="l", lty=1,
#         xlab=expression(lambda/mu*m), ylab=expression(sigma/mu*m^2))
# matlines(cross_sections2$wavelength, cross_sections2[, -1], lty=2)
# matlines(cross_sections3$wavelength, cross_sections3[, -1], lty=3)
# 
# legend("topright", names(cross_sections)[-1], col=1:3, lty=1)

# xsec1$name <- "JC"
xsec2$name <- "ACS"
xsec3$name <- "PRB"

all <- rbind(xsec2, xsec3)
mm <- melt(all, id=c("wavelength","name"))
ggplot(mm, aes(wavelength, value, group=interaction(variable,name))) +
  geom_line(aes(colour=name)) +
  facet_wrap(~variable, scales="free") +
  theme_minimal() +
  labs(y="",colour="")

ggsave("epsAu.pdf",width=12,height=3)


library(cda)
library(cubature)
library(egg)
library(tidyr)
library(purrr)
library(dplyr)
library(tibble)
library(dielectric)

weight <- function(a, sd=10){
  dnorm(a, mean = 28, sd = sd)
}

tibble(a = seq(1,100,length=100)) %>% 
  mutate(p = weight(a)) %>%
  ggplot(aes(a, p)) + geom_line()

## check that the area is 1
integrate(weight, -Inf, Inf)
# 
# mie(goldACS$wavelength, goldACS$epsilon, radius=30, 
#     medium=water, efficiency=FALSE)

wrap_model <- function(p, material=goldACS, sd=5,...){
  xsec1 <- mie(material$wavelength, material$epsilon, radius=p, 
                              medium=water, efficiency=FALSE)
  w <- weight(p,sd=sd)
  c(xsec1$extinction, xsec1$absorption, xsec1$scattering)* w
}

length(wrap_model(30))


wrap_me <- function(sd){
tmp <- pcubature(wrap_model, sd=sd, material=goldPRB, lowerLimit = 1, upperLimit = 100,  
                 tol=1e-3, maxEval = 300, norm = "PAIRED",
                 fDim = 3*nrow(goldACS) )
 tibble(wavelength = rep(goldACS$wavelength, 3),
                     value = tmp$integral, 
                     variable = rep(c("extinction", "absorption", "scattering"),
                                    each=nrow(goldACS)),
                     name='distribution'
                     )
}

all <- plyr::mdply(data.frame(sd=seq(2,8,by=2)), wrap_me)
# quadrature <- wrap_me(5)

raw <- setNames(read.table("raw.txt"), 
                c("wavelength", "scattering", "absorption", "extinction"))
md <- melt(raw, id=1)
md$name <- "data"

ggplot(subset(mm, name=="PRB"), aes(wavelength, value, group=interaction(variable,name))) +
  geom_line(data=md,aes(y=value*4e4),lwd=1.2,col="grey70") +
  # geom_line(col='black') +
  geom_line(data=all, aes(colour=factor(sd),group=sd),lty=2) +
  facet_wrap(~variable, scales="free") +
  theme_minimal() +
  labs(y="",colour="sd")

# ggsave("epsAu-disp2.pdf",width=12,height=3)
