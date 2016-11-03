## ================================================
## functions to fit and plot Michaelis–Menten curve
## ================================================


## function to calculate the curve as a function of 
## substrate concentration, Vmax, and Km

mmcurve <- function(S, Vmax, Km) {
    Vmax * S / (Km + S)
}


## function to find best values for Vmax and Km given
## data on v and S

mmsol <- function(v, S) {
    ## sum of squares function to be minimized
    ss <- function(par) {
        sum((mmcurve(S, par[1], par[2]) - v)^2)
    }
    
    ## use Hanes–Woolf linearization to get initial guess
    SoverV <- S/v
    init <- lm(SoverV ~ S)
    initVmax <- 1/init$coefficients[2]
    initKm <- init$coefficients[1] * initVmax
    
    ## combine initial estimates for use in optimization
    initPar <- c(initVmax, initKm)
    names(initPar) <- c('Vmax', 'Km')
    
    ## minimize the sum of squares funciton
    sol <- optim(initPar, ss, method = 'L-BFGS-B', lower = 0)
    
    ## check optimization convereged
    if(sol$convergence > 0) {
        ## if it didn't, return Hanes–Woolf estimate
        warning('cannot fit Michaelis–Menten curve')
        return(c(NA, NA))
    } else {
        ## if it did, return the optimized estimates
        return(sol$par)
    }
}

## =========================================================
## bootstrap method to get confidence intervals on estimates
## =========================================================

## helper function to do bootstrap sampling
mmsamp <- function(v, S, n) {
    boot <- replicate(n, {
        i <- sample(length(S), replace = TRUE)
        newS <- S[i]
        newv <- v[i]
        
        return(mmsol(newv, newS))
    })
    
    ## estimate from non-bootstrapped data
    est <- mmsol(v, S)
    
    ## remove spuriously large outliers
    boot <- boot[, boot[1, ] <= est + est - min(boot[1, ], na.rm = TRUE)]
    
    return(boot)
}

mmboot <- function(v, S, n = 1000) {
    boot <- mmsamp(v, S, n)
    
    out <- t(apply(boot, 1, function(x) c(median(x, na.rm = TRUE), 
                                          quantile(x, prob = c(0.025, 0.975), na.rm = TRUE))))
    colnames(out) <- c('est', 'ci_2.5', 'ci_97.5')
    
    ## make est the real estimate
    out[, 1] <- mmsol(v, S)
    
    return(out)
}


mmttest <- function(v1, S1, v2, S2, n = 1000) {
    boot1 <- mmboot(v1, S1, n)
    boot2 <- mmboot(v2, S2, n)
    
    n1 <- length(S1)
    n2 <- length(S2)
    
    tt <- function(b1, b2, n1, n2) {
        s1 <- n1 * ((b1[1] - b1[2]) / -qnorm(0.025))^2
        s2 <- n2 * ((b2[1] - boot2[2]) / -qnorm(0.025))^2
        
        tval <- (b1[1] - b2[2]) / sqrt(s1/n1 + s2/n2)
        df <- (s1/n1 + s2/n2)^2 / (s1^2/(n1*(n1-1)) + s2^2/(n1*(n2-1)))
        
        pval <- pt(tval, df, lower.tail = ifelse(tval < 0, TRUE, FALSE))*2
        
        return(pval)
    }
    
    pVmax <- tt(boot1[1, ], boot2[1, ], n1, n2)
    pKm <- tt(boot1[2, ], boot2[2, ], n1, n2)
    
    return(c(pVal_Vmax = pVmax, pVal_Km = pKm))
}


mmbootP <- function(v1, S1, v2, S2, n = 1000) {
    boot1 <- mmsamp(v1, S1, n)
    boot2 <- mmsamp(v2, S2, n)
    
    obs1 <- mmsol(v1, S1)
    obs2 <- mmsol(v2, S2)
    
    pval <- function(b1, b2, o1, o2) {
        r11 <- sample(b1[!is.na(b1)], size = sum(!is.na(b1)), replace = TRUE)
        r12 <- sample(b1[!is.na(b1)], size = sum(!is.na(b1)), replace = TRUE)
        r21 <- sample(b2[!is.na(b2)], size = sum(!is.na(b2)), replace = TRUE)
        r22 <- sample(b2[!is.na(b2)], size = sum(!is.na(b2)), replace = TRUE)
        
        null <- sample(c(r11 - r12, r21 - r22), size = n, 
                       replace = ifelse(length(r11) + length(r22) < n, TRUE, FALSE))
        stat <- o1 - o2
        
        if(stat < mean(null)) {
            return(sum(null < stat) / n)
        } else {
            return(sum(null > stat) / n)
        }
    }
    
    pVmax <- pval(boot1[1, ], boot2[1, ], obs1[1], obs2[1])
    pKm <- pval(boot1[2, ], boot2[2, ], obs1[2], obs2[2])
    
    return(c(pVal_Vmax = pVmax, pVal_Km = pKm))
}

## ====================================
## examples of how to use the functions
## ====================================

# ## first simulate some data
# data <- data.frame(S = seq(0.1, 0.9, length = 10),
#                    v = mmcurve(seq(0.1, 0.9, length = 10), 10, 0.2))
# data$v <- data$v + rnorm(nrow(data), 0, 0.25)
# 
# ## plot it
# plot(data)
# 
# ## find Vmax and Km
# pars <- mmsol(data$v, data$S)
# 
# ## find confidence intervals
# mmboot(data$v, data$S)
# 
# ## add them to plot
# curve(mmcurve(x, Vmax = pars['Vmax'], Km = pars['Km']), add = TRUE)
# 
# ## return the predicted mm values and add to data
# data$pred <- mmcurve(data$S, Vmax = pars['Vmax'], Km = pars['Km'])
