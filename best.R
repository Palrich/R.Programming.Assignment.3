best <- function(state, outcome) {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        ## Check that state and outcome are valid
        check_outcome <- names(outcomes)
        check_state <- unique(df[ , 7])
        
        if (!outcome %in% check_outcome) stop("invalid outcome")
        if (!state %in% check_state) stop("invalid state")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        ## Make a new data frame consisting of only the columns of interest; hospital name(2),
        ## state(7), and one of the outcome columns: 11, 17, or 23, depending on what the user
        ## has specified as outcome --> see variable 'outcomes' at top
        df_interested_columns <- df[ , c(2, 7, outcomes[outcome])]
        ## Split our newly created data frame by state name
        df_by_state <- split(df_interested_columns, df$State)
        ## The above 'dif_by_state' is a variable holding a list of data frames each separated
        ## by state. dfs will subset out the data frame with the state name provided in the
        ## function argument.
        dfs <- df_by_state[[state]]
        ## Order our subsetted state data frame. First by the outcome column, then within those
        ## outcome columns, order by hospital name (so that in the event of a tie for lowest
        ## outcome value, the function will return the hospital name which takes precedent by
        ## alphabetical order)
        ordered_dfs <- dfs[order(dfs[ , 3], dfs[ , 1]), ]
        ## Take a complete case of our ordered data frame
        cc_ordered_dfs <- ordered_dfs[complete.cases(ordered_dfs), ]
        ## Vector to store the column of ordered hospital names
        vector_interest <- cc_ordered_dfs[ , 1]
        ## Take the first element of our vector so as to retrieve the hospital name which had the 
        ## best (least mortality) value in the outcome column
        best_hospital <- vector_interest[1]
        return(best_hospital)
}