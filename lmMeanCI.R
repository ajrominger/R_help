## funciton to calculate ANOVA stats for case where summary of data are given
## in the form of a mean and CI of MPN (= most probable number) for each treatment

lmMeanCI <- function(formula, data, mpnLo, mpnHi, nrep, runTukey = FALSE) {
    ## make model
    mod <- lm(formula, data = data, qr = FALSE)
    
    ## extract mpn
    mpn <- mod$model[, 1]
    
    ## calculate sd from CI
    ci <- cbind(mpn - mpnLo, mpnHi - mpn)
    sd <- sapply(1:nrow(ci), function(i) mean(ci[i, ], na.rm = TRUE) / 1.96)
    
    ## true number of data poins
    trueN <- nrep * length(mpn)
    
    ## anova and prediction of model
    modAOV <- anova(mod)
    yhat <- predict(mod)
    
    ## adjusted model sum of squares
    ssq <- trueN * (modAOV[['Sum Sq']] / length(mpn))
    
    ## adjusted residual sum of squares
    ssResid <- sum(((mpn - yhat)^2 + sd^2)*nrep)
    msResid <- ssResid / (trueN - 5)
    ssq[length(ssq)] <- ssResid
    
    ## calculate f-statistic
    fstat <- ssq / modAOV$Df / msResid
    fstat[length(fstat)] <- NA
    
    ## calculate degrees of freedom
    df <- modAOV$Df
    df[length(df)] <- trueN - (sum(df[-length(df)]) + 1)
    
    aovTab <- matrix(c(df,
                       ssq, 
                       fstat, 
                       pf(fstat, 1, trueN - sum(df[-length(df)]), lower.tail = FALSE)), 
                     ncol = 4)
    
    colnames(aovTab) <- c('df', 'Sum Sq', 'F value', 'Pr(>F)')
    rownames(aovTab) <- rownames(modAOV)
    
    sumTab <- summary(mod)$coefficients[, 1, drop = FALSE]
    
    out <- list(aovTab, sumTab)
    
    ## tukey
    if(runTukey) {
        newBA <- rep(beforeAfter, nrep)
        newSite <- rep(site, nrep)
        newDepth <- rep(depth, nrep)
        mod$model <- mod$model[rep(1:length(mpn), nrep), ]
        thsdOut <- replicate(100, {
            mod$model[, 1] <- rnorm(trueN, rep(mpn, nrep), rep(sd, nrep))
            thsd <- TukeyHSD(aov(mod))
            thsd[['newBA:newSite:newDepth']][, 4]
        })
        
        tukeyLetters <- multcompView::multcompLetters(rowMeans(thsdOut))$Letters
        out <- c(out, tukeyLetters)
    }
    
    return(out)
}
