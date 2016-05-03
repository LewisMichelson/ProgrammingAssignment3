best <- function(state, outcome) {
        
        ## Read outcome data
        
        df_outcome_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        ## Check that state and outcome are valid
        
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
        
        ##Test for valid state. Stop if invalid
        if(!(test_state %in% df_outcome_measures[,7])) {
                stop("invalid state")
        }
        

        df_best_hospital_names <- vector(mode = "character", length = 0)
        df_best_hospital_outcome <- vector(mode = "numeric", length = 0)

        ##Extract hospital name and outcome column
        for(i in 1:nrow(df_outcome_measures)){
                if(test_state == df_outcome_measures[i,7]){
                        df_best_hospital_names <- rbind(df_best_hospital_names,df_outcome_measures[i,2])
                        df_best_hospital_outcome <- rbind(df_best_hospital_outcome,df_outcome_measures[i,col_index])
                }
        }

        df_final <- cbind(df_best_hospital_names,df_best_hospital_outcome)
        df_lower_outcomes <- min(as.numeric(df_best_hospital_outcome), na.rm = TRUE)
        df_final_hospital <- vector(mode = "character", length = 0)
        
        for(i in 1:nrow(df_final)){
                if(df_lower_outcomes == df_final[i,2]){
                        df_final_hospital <- rbind(df_final_hospital, df_final[i,1])
                }
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        final <- sort(df_final_hospital)
        final
}
