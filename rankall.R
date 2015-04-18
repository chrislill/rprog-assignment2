rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome.data <- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")
    state <- sort(unique(outcome.data$State))
    
    ## Use two similar vectors to find the right column in the outcome data
    diseases <- c("heart attack", "heart failure", "pneumonia")
    columns <- c(11, 17, 23)
    data.column <- columns[match(outcome, diseases)]
    
    ## Check that outcome is valid
    if (is.na(data.column)) {
        stop("invalid outcome")
    }     
    
    ## Create a data frame to collect and output the data
    hospital <- vector("character", length(state))
    ranking.by.state <- data.frame(hospital, state, stringsAsFactors = FALSE)
    
    ## Some operations only need to be done once
    is.value <- outcome.data[, data.column] != "Not Available"   
    
    ## Loop through each state
    for (s in state) {
        
        ## Find the hospitals with data in the State
        is.state <- outcome.data$State == s        
        is.state.value <- is.state & is.value
 
        ## Set the rank value from num
        if (num == "best") {
            rank = 1
        } else if (num == "worst") {
            rank = sum(is.state.value)
        } else if (num <= sum(is.state.value)) {
            rank = as.numeric(num)
        } else {
            ranking.by.state[match(s, state),"hospital"] <- NA 
            next
        }
        
        ## Create a subset of the data
        state.data <- outcome.data[is.state.value, c(2, data.column)]
        colnames(state.data)[2] <- "Mortality.Rate"        
 
        ## Cast the mortality rates to a numeric value
        state.data$Mortality.Rate <- as.numeric(state.data$Mortality.Rate)
        
        ## Create an index giving the rank of holidays
        index <- with(state.data, order(Mortality.Rate, Hospital.Name))        
        
        ## Set the hospital for this state
        ranking.by.state[match(s, state),"hospital"] <- state.data[index[rank], "Hospital.Name"]            
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name   
    ranking.by.state
}