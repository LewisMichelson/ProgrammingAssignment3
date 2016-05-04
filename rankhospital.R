rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", na.strings ="Not Available", stringsAsFactors = FALSE)
        ## Check that state and outcome are valid
        
        ##Fix case to match outcome-of-care-measures.csv
        test_outcome <- tolower(outcome)
        test_state <- toupper(state)
        
        ##Test for valid outcome. Stop if invalid
        if(test_outcome == "heart attack"){
                col_index <- 11
        } else if( test_outcome == "heart failure"){
                col_index <- 17
        } else if(test_outcome == "pneumonia"){
                col_index <- 23
        } else {
                stop("invalid outcome")
        }

        df_subset <- cbind(df[,c(2,7,col_index)])
        
        ##Test for valid state. Stop if invalid
        if(!(test_state %in% df_subset[,2])) {
                stop("invalid state")
        }
        
        ##Get Hospital.Name, State and lowest outcome columns
        df_subset <- cbind(df[,c(2,7,col_index)])
        
        names(df_subset) <- c("Hospital.Name", "state", "Rate")

        ##Omit NA
        df_subset <- na.omit(df_subset)

        ##Get the state we are interested in        
        df_subset_state <- subset(df_subset, state == test_state)

        ##Sort by Rate and then by Hospital.Name
        df_subset_sort <- df_subset_state[order(df_subset_state$Rate,df_subset_state$Hospital.Name),]
        df_length <- length(df_subset_sort$Hospital.Name)
        
        df_rank <- c(1:df_length)
        
        df_sort_and_rank <- cbind(df_subset_sort,df_rank)
        names(df_sort_and_rank) <- c("Hospital.Name", "State", "Rate","Rank")

        if(num == "worst"){
                num <- df_length
        }
        if(num == "best"){
                num <- 1
        }
        
        df_final <- subset(df_sort_and_rank$Hospital.Name, df_sort_and_rank$Rank == num)
        df_final

        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}
