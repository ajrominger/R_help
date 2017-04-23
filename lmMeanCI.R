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
