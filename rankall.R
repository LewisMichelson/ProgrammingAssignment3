rankall <- function(outcome, num = "best") {
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
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name

        names(df_subset) <- c("Hospital.Name", "state", "Rate")

        df_sub_sort <- df_subset[order(df_subset$state,df_subset$Rate,df_subset$Hospital.Name),]
        df_by_state <- split(df_sub_sort, df_sub_sort$state)
        df_unique <- lapply(df_sub_sort, function(x) unique(x))
        df_unique_state <-c(df_unique$state)

        for(i in df_unique$state){     
                print(i)
                # df_sub_length <- length(df_by_state$i)
                # df_rank_col <- c(1:df_sub_length)
                # df_by_state$i <- cbind(df_by_state$i,df_rank_col)
        }
        df_factor <- factor(df_subset, levels = df_unique$state)
        
}
