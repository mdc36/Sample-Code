rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!any(outcomes == outcome)){
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    names_states <- unique(data$State)
    vect_states <- as.vector(names_states[order(names_states)])
    vect_hospitals <- vector(mode = "character", length = 0)
    x <- 0
    y <- 0
    new_hospital <- "" 
    for(i in vect_states){
        state_data = subset(data, data$State == i)
        if(outcome == "heart attack"){
            state_data[, 11] <- as.numeric(state_data[, 11])
            no.na = subset(state_data, is.na(state_data[, 11]) == FALSE)
            ordered <- no.na[order(no.na[, 11], no.na$Hospital.Name), ]
            x <- nrow(state_data)
            y <- nrow(ordered)
        } else if(outcome == "heart failure"){
            state_data[, 17] <- as.numeric(state_data[, 17])
            no.na = subset(state_data, is.na(state_data[, 17]) == FALSE)
            ordered <- no.na[order(no.na[, 17], no.na$Hospital.Name), ]
            x <- nrow(state_data)
            y <- nrow(ordered)
        } else if(outcome == "pneumonia"){
            state_data[, 23] <- as.numeric(state_data[, 23])
            no.na = subset(state_data, is.na(state_data[, 23]) == FALSE)
            ordered <- no.na[order(no.na[, 23], no.na$Hospital.Name), ]
            x <- nrow(state_data)
            y <- nrow(ordered)
        }
        if(class(num) == "numeric"){
            if(num <= x){
                new_hospital <- as.character(ordered[num, 2])
            } else if (num > x){
                new_hospital <- NA
            }
        } else if(num == "best"){
            new_hospital <- as.character(ordered[1, 2])
        } else if(num == "worst"){
            new_hospital <- as.character(ordered[y, 2])
        }
        vect_hospitals <- c(vect_hospitals, new_hospital)
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    result <- data.frame(hospital = vect_hospitals, state = vect_states)
    result   
}
