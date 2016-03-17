rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        
        ## Check that state and outcome are valid
        valid_outcome = c("heart attack","heart failure","pneumonia")
        if (!outcome %in% valid_outcome) stop("invalid outcome")
        
        valid_state = unique(data[ , 7])
        if (!state %in% valid_state) stop("invalid state")
        
        ## convert outcome name into column name
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome, valid_outcome)]
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        data_state <- data[data$State==state, c(2, outcomes[outcome])]
        order_data_state <- data_state[order(data_state[ , 2], data_state[ , 1]), ]
        cc_order_data_state <- order_data_state[complete.cases(order_data_state), ]
        
        if (num == "best") {
                num <- 1
        } else if (num == "worst") {
                num <- nrow(cc_order_data_state)
        }

        find_rank <- cc_order_data_state[num, 1]
        
        return(find_rank)
        
}