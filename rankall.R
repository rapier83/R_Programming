best <- function(state,outcome) {
    
    x <- setData(state,outcome)
    if(outcome == "heart attack") { oc = 3 }
    if(outcome == "heart failure") { oc = 4 }
    if(outcome == "pneumonia") { oc = 5 }
    x_sort <- subset(x, x[,oc] == min(x[,oc]))
    x_name <- apply(x_sort,2,sort)
    
    x_name[1]
}

outcomeNum <- function(outcome) {
    
    if(outcome == "heart attack") { oc = 3 }
    if(outcome == "heart failure") { oc = 4 }
    if(outcome == "pneumonia") { oc = 5 }
    
    return(oc)
}

setData <- function(state="All",outcome="All") {
    #set data
    open <- read.csv("./hospital/outcome-of-care-measures.csv")
    x <- open[c(2,7,11,17,23)]
    x[,3] <- suppressWarnings(as.numeric(as.character(x[,3])))
    x[,4] <- suppressWarnings(as.numeric(as.character(x[,4])))
    x[,5] <- suppressWarnings(as.numeric(as.character(x[,5])))
    # delete NA is chagned by using !is.na instead of complete.cases
    # x <- x[complete.cases(x[,1:5]),]
    
    #Check state, outcome
    if(state == "All" & outcome == "All") {
        return(x)
    }    
    
    if(state == "All" & outcome != "All") {
        if((outcome %in% c("heart attack","heart failure","pneumonia")) == FALSE) {
            stop("invalid outcome")
        }
        return_col <- outcomeNum(outcome)
        return(x[c(1,2,return_col)])
    }
    
    if(state != "All" & outcome == "All") {
        if((state %in% x$State) == FALSE) {
            stop("invalid state")
        }
        return(subset(x,x[,2] == state))
    }
    
    if(state != "All" & outcome != "All"){
        if((state %in% x$State) == FALSE) {
            stop("invalid state")
        }
        if((outcome %in% c("heart attack","heart failure","pneumonia")) == FALSE) {
            stop("invalid outcome")
        }
        return_col <- outcomeNum(outcome)
        x <- x[c(1,2,return_col)]        
        return(subset(x,x[,2] == state))
    }

}

returnNum <- function(num="best") {
    if (is.numeric(num) == FALSE) {
        if (num == "best") {
            return_n <- 1
        }
        
        if (num == "worst") {
            return_n <- nrow(x)
        }
    }
    else {
        if (num > nrow(x)) {
            return(NA)
            
        }
        else {
            return_n <- num
        }
    }
    
}

rankhospital <- function(state, outcome, num="best") {
    x <- setData(state, outcome)
    oc <- outcomeNum(outcome)
    x <- x[order(x[,oc],x[,1]),]
    x$Rank <- 1:nrow(x)
    return_n <- returnNum(num)

    return(as.character(x[x$Rank == return_n,1]))
}

rankall <- function(outcome, num="best") {
    x <- setData("All",outcome)
    x <- x[!is.na(x[,3]),]
    colnames(x) <- c("hospital","state","outcome")
    
    stateList <- sort(unique(x$state))
    
    # ranking outcome group by state with split
    tmp.x <- split(x,x$state)
    tmp.x <- lapply(tmp.x, 
                    function(x) 
                        x[order(x[,3],x[,1]),]
                    )
                    
    #return values by head, tail or approaching number
    if (is.numeric(num) == TRUE) {
        tmp.rank <- lapply(tmp.x, function(x) x[num, ])
    }
    else {
        if (num == "best") {
            tmp.rank <- lapply(tmp.x, function(x) head(x,1))
        }
        if (num == "worst"){
            tmp.rank <- lapply(tmp.x, function(x) tail(x,1))
        }
    }
        
    x_fin <- do.call(rbind.data.frame,tmp.rank)
    
    # set temporary dataframe for return
    tmp.df <- data.frame(
        hospital = c(rep("<NA>",length(stateList))),
        state = as.character(stateList)
    )
    
    x_df <- merge(x=tmp.df,y=x_fin,by="state",all.x=TRUE)
    
    x_fin <- data.frame(
        hospital = x_df$hospital.y,
        state = x_df$state,
        row.names = stateList)
    return(x_fin)
}


