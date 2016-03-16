complete <- function(directory,id = 1:332){
    library(dplyr)
    library(readr)
    ## get list of files corresponding to the selected id
    files_list <- list.files(directory, full.names = TRUE)
    ## combine list of files into data frame (uses package dplyr - defines %>% - found on StackOverflow)
    ## when ID is a range: always throws a warning message In rbind_all(x, .id) : Unequal factor levels: coercing to character but works
    compDataSet <- files_list %>% lapply(read.csv) %>% bind_rows
    X <- nobs <- numeric()
    v1 <- id
    ID <- Ct <- numeric()
    idx <- 1
    for(i in v1){
        X[idx] <- i
        df1 <- compDataSet[compDataSet$ID == i,]
        df2 <- df1[!is.na(df1$sulfate) & !is.na(df1$nitrate),]
        nobs[idx] <- sum(!is.na(df2$sulfate))
        idx <- idx+1
    }
    id <- X
    data.frame(id,nobs)
}