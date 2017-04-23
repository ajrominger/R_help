library(multcompView)

lmMeanCI <- function(mpn, mpnLo, mpnHi, beforeAfter, x, nrep) {
    beforeAfter <- factor(as.character(beforeAfter), 
                          levels = rev(sort(unique(as.character(beforeAfter)))))
    
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


lmMeanCIFull <- function(mpn, mpnLo, mpnHi, beforeAfter, site, depth, nrep) {
    beforeAfter <- factor(as.character(beforeAfter), 
                          levels = rev(sort(unique(as.character(beforeAfter)))))
    
    
    ci <- cbind(mpn - mpnLo, mpnHi - mpn)
    sd <- sapply(1:nrow(ci), function(i) mean(ci[i, ], na.rm = TRUE) / 1.96)
    
    trueN <- nrep * length(mpn)
    
    mod <- lm(mpn ~ beforeAfter * site * depth)
    modAOV <- anova(mod)
    yhat <- predict(mod)
    
    
    ssq <- trueN * (modAOV[['Sum Sq']] / length(mpn))
    
    ssResid <- sum(((mpn - yhat)^2 + sd^2)*nrep)
    msResid <- ssResid / (trueN - 5)
    ssq[length(ssq)] <- ssResid
    
    fstat <- ssq / modAOV$Df / msResid
    fstat[length(fstat)] <- NA
    
    df <- modAOV$Df
    
    aovTab <- matrix(c(df,
                       ssq, 
                       fstat, 
                       pf(fstat, 1, trueN - sum(df[-length(df)]), lower.tail = FALSE)), 
                     ncol = 4)
    
    colnames(aovTab) <- c('df', 'Sum Sq', 'F value', 'Pr(>F)')
    rownames(aovTab) <- rownames(modAOV)
    
    sumTab <- summary(mod)$coefficients[, 1, drop = FALSE]
    
    
    ## tukey
    newBA <- rep(beforeAfter, nrep)
    newSite <- rep(site, nrep)
    newDepth <- rep(depth, nrep)
    thsdOut <- replicate(100, {
        y <- rnorm(trueN, rep(mpn, nrep), rep(sd, nrep))
        thsd <- TukeyHSD(aov(lm(y ~ newBA * newSite * newDepth)))
        thsd[['newBA:newSite:newDepth']][, 4]
    })
    
    tukeyLetters <- multcompLetters(rowMeans(thsdOut))$Letters
    
    return(list(aovTab, sumTab, tukeyLetters))
}
