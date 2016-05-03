best <- function(state, outcome) {
        
        ## Read outcome data
        
        df_outcome_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        #df_outcome_measures_states <-  df_outcome_measures[,7]
        
        ## Check that state and outcome are valid
        
        ## valid outcomes
        ha <- "heart attack"
        hf <- "heart failure"
        pn <- "pneumonia"
        
        ##Fix case to lower
        test_outcome <- tolower(outcome)
        test_state <- toupper(state)

        ##Test for valid out come. Stop if invalid
        if(test_outcome == ha){
                best_outcome <- ha
                col_index <- 13
        } else if( test_outcome == hf){
                best_outcome <- hf
                col_index <- 19
        } else if(test_outcome == pn){
                best_outcome <- pn
                col_index <- 25
        } else {
                stop("invalid outcome")
        }
        
        ##Test for valid state
        if(!(test_state %in% df_outcome_measures[,7])) {
                stop("invalid state")
        }
        
        
        
        # for(i in length(df_outcome_measures_states)){
        #         
        # }
        df_state <- subset(df_outcome_measures, select = test_state)
        df_state
        #Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from
        # if( identical(df_outcome_measures_states, state)){
        #         print(state)        
        # }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}