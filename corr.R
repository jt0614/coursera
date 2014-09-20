corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
 
        cv <- vector(length = 0)
        
        for (i in 1:332) {
                
                index <- as.character(i)                
                if (i < 10) index <- paste("0", index, sep = "")
                if (i < 100) index <- paste("0", index, sep = "")
                
                filepath <- paste(directory, "/", index, ".csv", sep = "")
                table <- read.csv(filepath)
                
                good <- complete.cases(table) ## generate boolean vector of "good" rows
                goodtable <- table[good,]     ## get subset of "table" with only "good" rows
                goodrows <- nrow(goodtable)   ## get number of rows of this table subset

                if (goodrows > threshold) {

                        buf <- cor(goodtable$nitrate, goodtable$sulfate)
                        cv <- c(cv, buf)
                }
                
        }
        
        round(cv, digits = 6) # Return value
}        