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
    x <- x[complete.cases(x[,1:5]),]
    
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
        x <- x[1,2,return_col]        
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
    x_return <- x[order(x[,oc],x[,1]),]
    x_return$Rank <- 1:nrow(x)
    return_n <- returnNum(num)

    return(as.character(x_return[x_return$Rank == return_n,1]))
}

rankall <- function(outcome, num="best") {
    x <- setData("All",outcome)
    
    stateList <- sort(unique(x$State))
    
    x <- x[order(x$State),]
    x$state.rank <- unlist(with(x, tapply(x[,3],x[,2], function(x) rank(x,ties.method = "first"))))
    
    
    
    
    # set temporary dataframe for return
    tmp.df <- matrix(NA,length(stateList),2)
    rownames(tmp.df) <- stateList
    colnames(tmp.df) <- c("hospital","state")
    
    # set rank number
    rankNum <- num
    if (num == "best") {
        rankNum <- 1
    }
    if(num=="worst") {
        
    }
    
    x_rank <- x[x$state.rank == rankNum,]
    
   # put value to temporary dataframe
    for (s in stateList) {
        tmp.df[s] <- x_rank[x_rank$state == s,][1:2]
    }
    tmp.df
}


