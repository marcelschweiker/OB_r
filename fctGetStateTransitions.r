
## function that returns lines of a given dataframe before and after a detected action
fctGetStateTransitions <- function(dfData, action, threshold = 5, linesBefore=15, linesAfter = 5, filterCloseActions = 1){
actionShifted <- action
actionShifted <- append(actionShifted, NA, after=0)
actionShifted <- actionShifted[-length(actionShifted)]

transAction <- round(action - actionShifted,2)

#get all lines above threshold
actionLines <- which(transAction >= threshold)

# delete actions happening in consecutive timesteps
if(filterCloseActions == 1){

diffLines <- 1

while(diffLines > 0){
	lenLinesPre <- length(actionLines)
	k <- 1
	listClose <- NA
	for(i in 1:(length(actionLines)-1)){
		if(actionLines[i+1]-actionLines[i]==1){
			listClose[k] <- i+1
			k <- k+1
		}
	}
	if(!is.na(listClose[1])){
		actionLines <- actionLines[-listClose]
	}
	lenLinesPost <- length(actionLines)
	diffLines <- lenLinesPre-lenLinesPost
}
} # end if filterCloseActions

# calculate line number before and after action to be considered
# do not use lines below first line of data.frame or beyond line number
startLines <- ifelse(actionLines - linesBefore <0, 0, actionLines - linesBefore)
endLines <- ifelse(actionLines + linesAfter > length(action), length(action), actionLines + linesAfter)

for(i in 1:length(actionLines)){
	dfSub <- dfData[startLines[i]:endLines[i],]
	dfSub$ActionNr <- i
if(i == 1){
	dfX <- dfSub
} else {
	dfX <- rbind(dfX, dfSub)
}
}

# returning dfX
dfX

} # end of function


### usage (not running!)
##dfData: a data frame with at least one column containing the state of a device (e.g. blinds)

#action <- dfData$Action # the column with the state of the device to be analysed (e.g. blinds)
#threshold <- 5 # threshold of change in state variable to be considered as action (e.g. .5 for binary variables)
#linesBefore <- 5 # number of lines to be returned before the detected action
#linesAfter <- 5 # number of lines to be returned after the detected action
#filterCloseActions <- 1 ### 1 = actions happening in consecutive timesteps will be erased (e.g. one blind action could appear in two to three timesteps in data due to length of blind movement being longer than 1 minute)

### call function
# dfActionsBefAft <- fctGetStateTransitions(dfData, action, .5, 15, 5, 0)
