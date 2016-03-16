corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    # utlize complete to build a data frame to evaluate against threshold
    compDF <- complete(directory,1:332)
    # evaluate data frame against threshold and save in var
    corDF <- compDF[compDF$nobs >= threshold,]
    # create vector of station ids that match the threshold, 
    # this will be used to open files into a data frame to calculate correlation
    ids <- corDF$id
    r <- 1
    results <- numeric()
    # work through files individual based on station id
    for (i in ids) {
        workingDF <- read.csv(list.files(directory, full.names = TRUE)[i])
        # populate vector with results one position at a time
        results[r] <- cor(workingDF$sulfate, workingDF$nitrate, use = "complete")
        r <- r + 1
    }
    results
}