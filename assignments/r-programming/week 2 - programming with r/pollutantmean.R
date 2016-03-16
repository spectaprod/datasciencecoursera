pollutantmean <- function(directory, pollutant, id = 1:332){
    library(dplyr)
    library(readr)
    # get list of files corresponding to the selected id
    files_list <- list.files(directory, full.names = TRUE)
    # combine list of files into data frame (uses package dplyr - defines %>% - found on StackOverflow)
    # when ID is a range: always throws a warning message In rbind_all(x, .id) : Unequal factor levels: coercing to character but works
    pDataSet <- files_list %>% lapply(read.csv) %>% bind_rows
    # select column to take mean of
    if (pollutant == "sulfate") {
        x <- pDataSet$sulfate
    } else if (pollutant == "nitrate"){
        x <- pDataSet$nitrate
    }
    # remove na values and take mean based on the ID - forget where I found this but I like how it flows
    y <- !is.na(x)
    z <- pDataSet$ID
    mean(x[y & z %in% id])
}