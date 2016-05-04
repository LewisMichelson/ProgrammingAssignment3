best <- function(state, outcome) {
        
        ## Read outcome data
        
        df <- read.csv("outcome-of-care-measures.csv", na.strings ="Not Available", stringsAsFactors = FALSE)
        ## Check that state and outcome are valid

        ## valid outcomes
        ## valid outcomes
        ha <- "heart attack"
        hf <- "heart failure"
        pn <- "pneumonia"
        
        ##Fix case to match outcome-of-care-measures.csv
        test_outcome <- tolower(outcome)
        test_state <- toupper(state)
        
        ##Test for valid outcome. Stop if invalid
        if(test_outcome == ha){
                best_outcome <- ha
                col_index <- 11
        } else if( test_outcome == hf){
                best_outcome <- hf
                col_index <- 17
        } else if(test_outcome == pn){
                best_outcome <- pn
                col_index <- 23
        } else {
                stop("invalid outcome")
        }
        
        df_subset <- cbind(df[,c(2,7,col_index)])
        
        names(df_subset) <- c("hospital", "state", "outcome")

        ##Test for valid state. Stop if invalid
        if(!(test_state %in% df_subset[,2])) {
                stop("invalid state")
        }
        df_subset <- na.omit(df_subset)
        df_subset
}
