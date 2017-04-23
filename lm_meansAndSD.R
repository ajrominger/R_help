library(plyr)
library(socorro)

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

datDegrade <- ddply(dat, c('ba', 'x'), function(x) c(mpn = mean(x$mpn), mpnSD = sd(x$mpn)))

anova(mod)

modDeg <- lm(mpn ~ ba * x, data = datDegrade)
anova(modDeg)



## SS_ba = SS for ANOVA of just mpn ~ ba
## SS_x = SS for ANOVA of just mpn ~ x
## SS_ba:x = SS_mod - (SS_ba + SS_x)

mean((predict(modDeg) - mean(datDegrade$mpn))^2)
mean((predict(mod) - mean(dat$mpn))^2)

## SS_mod for undegraded = MS_mod(degraded) * N_true
ssMod <- nrow(dat) * mean((predict(lm(mpn ~ ba * x, data = datDegrade)) - 
                               mean(datDegrade$mpn))^2)
ssBA <- nrow(dat) * mean((predict(lm(mpn ~ ba, data = datDegrade)) - 
                              mean(datDegrade$mpn))^2)
ssX <- nrow(dat) * mean((predict(lm(mpn ~ x, data = datDegrade)) - 
                             mean(datDegrade$mpn))^2)
ssBAX <- ssMod - ssBA - ssX

ssResid <- sum(((datDegrade$mpn - yhat)^2 + datDegrade$mpnSD^2)*8)
msResid <- ssResid / (nrow(dat) - 4)

ssBAX / msResid


lmMeanCI <- function(mpn, mpnLo, mpnHi, beforeAfter, x, nrep) {
    ci <- cbind(mpn - mpnLo, mpnHi - mpn)
    sd <- sapply(1:nrow(ci), function(i) mean(ci[i, ], na.rm = TRUE) / 1.96)
    
    trueN <- nrep * length(mpn)
    
    mod <- lm(mpn ~ beforeAfter * x)
    yhat <- predict(mod)
    
    ssMod <- trueN * mean((yhat - mean(mpn))^2)
    ssBA <- trueN * mean((predict(lm(mpn ~ beforeAfter)) - mean(mpn))^2)
    ssX <- trueN * mean((predict(lm(mpn ~ x)) - mean(mpn))^2)
    ssBAX <- ssMod - ssBA - ssX
    
    ssResid <- sum(((mpn - yhat)^2 + sd^2)*nrep)
    msResid <- ssResid / (trueN - 4)
    
    fstat <- c(ssBA, ssX, ssBAX, NA) / msResid
    aovTab <- matrix(c(c(1, 1, 1, trueN - 4),
                       c(ssBA, ssX, ssBAX, ssResid), 
                       fstat, 
                       pf(fstat, 1, trueN - 4, lower.tail = FALSE)), 
                     ncol = 4)
    
    colnames(aovTab) <- c('df', 'Sum Sq', 'F value', 'Pr(>F)')
    rownames(aovTab) <- c('before_after', 'x', 'before_after:x', 'residuals')
    
    sumTab <- summary(mod)$coefficients[, 1, drop = FALSE]
    
    return(list(aovTab, sumTab))
}

with(datDegrade, lmMeanCI(mpn = mpn, mpnLo = mpn - 1.96*mpnSD, mpnHi = mpn + 1.96*mpnSD, 
                          beforeAfter = ba, x = x, nrep = 8))
