rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Find the hospitals with data in the State
    is.state <- outcome.data$State == state
    is.value <- outcome.data[, data.column] != "Not Available"
    is.state.value <- is.state & is.value
    
    ## Set the rank value from num
    if (num == "best") {
        rank = 1
    } else if (num == "worst") {
        rank = sum(is.state.value)
    } else if (num <= sum(is.state.value)) {
        rank = as.numeric(num)
    } else {
        return(NA)
    }
    
    ## Create a subset of the data
    state.data <- outcome.data[is.state.value, c(2, data.column)]
    colnames(state.data)[2] <- "Mortality.Rate"
    
    ## Cast the mortality rates to a numeric value
    state.data$Mortality.Rate <- as.numeric(state.data$Mortality.Rate)
    
    ## Create an index giving the rank of holidays
    index <- with(state.data, order(Mortality.Rate, Hospital.Name))
    
    ## Return hospital name in that state with the given rank 30-day death rate
    state.data[index[rank], "Hospital.Name"]
    
    }