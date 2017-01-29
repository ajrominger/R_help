## `f' should be a vector of all the file names you want to process
formatSLAC <- function(f) {
    ## function to pad a string with empty spaces
    padd <- function(s, N) {
        addSpace <- sapply(N - nchar(s), function(n) {
            paste(rep(' ', n), collapse = '')
        })
        paste(addSpace, s)
    }
    
    for(i in 1:length(f)) {
        x <- readLines(f[i])
        x <- x[(grep('Data:', x) + 1):length(x)]
        
        xnames <- x[1:(which(nchar(x) == 0) - 1)]
        xdata <- x[(which(nchar(x) == 0) + 1):length(x)]
        
        xdata <- sapply(strsplit(xdata, '  '), function(s) {
            s <- gsub(' ', '', s)
            paste(padd(s, nchar(xnames)[1]), collapse = '')
        })
        
        out <- c(paste(padd(gsub(' ', '', xnames), nchar(xnames)[1]), collapse = ''), xdata)
        out <- c(paste(rep('-', nchar(out[1])), collapse = ''), out)
        
        writeLines(out, paste(f[i], 'formatted.txt', sep = '_'))
    }
}


## example usage
# toFormat <- c('~/Dropbox/Research/R_help/xas/1BE1_1_011_A.001', 
#               '~/Dropbox/Research/R_help/xas/ferrStd_trial_062_A.001',
#               '~/Dropbox/Research/R_help/xas/tub_Fe_066_A.002')
# formatSLAC(toFormat)
# 
## taking advantage of `setwd' can make the code look more tidy, but does the same thing:
# setwd('~/Dropbox/Research/R_help/xas')
# toFormat <- c('1BE1_1_011_A.001', 'ferrStd_trial_062_A.001','tub_Fe_066_A.002')
# formatSLAC(toFormat)
