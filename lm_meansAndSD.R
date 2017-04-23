library(plyr)

nsoil <- 6
nrep <- 8

ba <- factor(rep(c('before', 'after'), each = nsoil))
x <- rep(seq(0, 1, length.out = nsoil), 2)

baEffect <- c(3, 0)
xSlope <- 3
xbaInter <- c(5, 0)

dat <- data.frame(ba = rep(ba, each = nrep), x = rep(x, each = nrep))
dat$mpn <- rnorm(nrow(dat), mean = baEffect[as.numeric(dat$ba)] + 
                     (xSlope + xbaInter[as.numeric(dat$ba)])*dat$x, 
                 sd = 0.5)

plot(dat$x, dat$mpn, col = dat$ba)

mod <- lm(mpn ~ ba * x, data = dat)
anova(mod)

datDegrade <- ddply(dat, c('ba', 'x'), function(x) c(mpn = mean(x$mpn), mpnSD = sd(x$mpn)))

fun <- function(par) {
    dnorm(datDegrade$mpn, mean = par[1] + ()*datDegrade$x, sd = datDegrade$mpnSD)
}
