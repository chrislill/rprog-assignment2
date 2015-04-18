best <- function(state, outcome) {
    ## Read outcome data
    outcome.data <- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")
    states <- unique(outcome.data$State)
    
    ## Use two similar vectors to find the right column in the outcome data
    diseases <- c("heart attack", "heart failure", "pneumonia")
    columns <- c(11, 17, 23)
    data.column <- columns[match(outcome, diseases)]
    
    ## Check that state and outcome are valid
    if (is.na(data.column)) {
        stop("invalid outcome")
    } 
    if (!state %in% states) {
        stop("invalid state")
    }
    
    ## Cast the mortality rates to a numeric value
    outcome.data[, data.column] <- 
        suppressWarnings(as.numeric(outcome.data[, data.column]))

    ## Find the rows that match the minimum value
    is.in.state <- outcome.data$State == state
    min.mortality <- min(outcome.data[is.in.state, data.column], na.rm = TRUE)
    is.min.value <- outcome.data[, data.column] == min.mortality
    
    ## Remove NAs inserted by casting values to a numberic
    is.min.value <- is.min.value %in% TRUE
    is.min.in.state <- is.in.state & is.min.value

    ## TODO(chris): Handle ties
    outcome.data[is.min.in.state, "Hospital.Name"]
}