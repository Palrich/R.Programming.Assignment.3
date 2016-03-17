rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
        
        ## Check that state and outcome are valid
        checkOutcome = c("heart attack","heart failure","pneumonia")
        if (!outcome %in% checkOutcome) { stop("invalid outcome")}
        
        checkState = sort(unique(data[,7]))
        
        ## convert outcome name into column name
        file.column.name <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        column.name <- file.column.name[match(outcome,checkOutcome)]
        
        ## For each state, find the hospital of the given rank
        hospital<-character(0)
        
        for (i in seq_along(checkState)) {
                outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
                ## Return hospital name in that state with the given rank 30-day death rate
                data.state <- data[data$State == checkState[i], c(2, outcomes[outcome])]
                
                # order data by outcome
                sorted.data.state <- data.state[order(as.numeric(data.state[[column.name]]), data.state[["Hospital.Name"]], decreasing=FALSE, na.last=NA), ]
                cc.sorted.data.state <- sorted.data.state[complete.cases(sorted.data.state), ]
                
                #handle num input
                this.num = num
                if (this.num =="best") this.num = 1
                if (this.num =='worst') this.num = nrow(cc.sorted.data.state)
                
                hospital[i] <- cc.sorted.data.state[this.num, "Hospital.Name"]
        }
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        data.frame(hospital = hospital, state = checkState, row.names = checkState)
}