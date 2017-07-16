#This contains all of the functions used in the model

CorpsReduce <- function(CorpsName, GEFrame, VisFrame, MusFrame) {
	#First, find the row for the corps in each frame
	CRow = which(GEFrame$Corps == CorpsName)
	#Now pull out that row for each data frame
	GERow = GEFrame[CRow,]
	MusRow = MusFrame[CRow,]
	VisRow = VisFrame[CRow,]
	
	#Now we need to get rid of the NAs and the corps name
	goodRowsGE = !is.na( GERow )
	goodRowsVis = !is.na( VisRow )
	goodRowsMus = !is.na( MusRow )

	#Now we need to make sure all the column names are the same
	if ( !identical(goodRowsGE, goodRowsVis) ) { 
		stop( paste('Score formatting issue with',CorpsName,': Date misplacement') )
	} else if ( !identical(goodRowsVis, goodRowsMus) ) {
		stop( paste('Score formatting issue with',CorpsName,': Date misplacement') )
	}
	
	#Now we can make the data frame
	#Using as.numeric trips an NA warning for the corps name, so we want to suprress that worning
	GEVec = suppressWarnings( as.numeric(GERow[goodRowsGE]) )
	VisVec = suppressWarnings( as.numeric(VisRow[goodRowsVis]) )
	MusVec = suppressWarnings( as.numeric(MusRow[goodRowsMus]) )
	#Make the day vector
	DayVec = as.character(names(GERow)[goodRowsGE])
	#Make a list of vectors, X and day
	DaySplit = strsplit(DayVec, 'X')
	DayNum = as.numeric( lapply(DaySplit, '[', 2) ) #pull out the number, coerce it
	DayNum = DayNum - 42907 #Sets 0 to June 21, 2017
	
	#Now make the data frame
	OutFrame = data.frame(Day = DayNum, GE=GEVec, Vis=VisVec, Mus=MusVec)
	#The first row is NAs from the corps name, so we want to get rid of that
	OutFrame = OutFrame[2:nrow(OutFrame) ,]
	return(OutFrame)
}

#Create a function that fits the exponential curve to a data frame
ExpFitter <- function(CorpsFrame) {
	#Start by checking the number of shows
	suppressWarnings( library(robustbase) )
	if (nrow(CorpsFrame) < 6) { return(NULL) }
	
	#Pull out the individual vectors to match nls input better
	GE = CorpsFrame$GE
	Vis = CorpsFrame$Vis
	Mus = CorpsFrame$Mus
	Day = CorpsFrame$Day
	#Now fit the curve for each caption, wrapped in a try to avoid catastrophic errors
	tryCatch({ 
		GEModel = nlrob(GE ~ a + Day^b, data=data.frame(GE, Day), start=list(a=GE[1], b=0.5), 
			maxit=100, tol=1e-5)
		VisModel = nlrob(Vis ~ a + Day^b, data=data.frame(Vis, Day), start=list(a=Vis[1], b=0.5), 
			maxit=100, tol=1e-5)
		MusModel = nlrob(Mus ~ a + Day^b, data=data.frame(Mus, Day), start=list(a=Mus[1], b=0.5), 
			maxit=100, tol=1e-5)
	}, warning = function(w) {}) #This makes it so scenarios of non-convergence don't return bad coefficients
	
	#Check to see if all of the models exist. if not, return NA
	ExistVec = c(exists('GEModel'), exists('VisModel'), exists('MusModel'))
	if (sum(ExistVec) < 3) { return(NA) }
	
	#Pull out the summary of the models
	GESum = summary(GEModel)
	VisSum = summary(VisModel)
	MusSum = summary(MusModel)
	
	#Now pull out the coefficients
	GECoef = GESum$coefficients[,1]
	VisCoef = VisSum$coefficients[,1]
	MusCoef = MusSum$coefficients[,1]
	#And now pull out the standard errors
	GESE = GESum$coefficients[,2]
	VisSE = VisSum$coefficients[,2]
	MusSE = MusSum$coefficients[,2]
	
	#Impose a limit on b based on caption score limits
	#The maximum b can be is set by the equation which solves for the highest possible b
	#That equation is 53^b = MaxScore-a, where MaxScore is 40 for GE and 30 for music and visual
	if (GECoef[2] > (log10(40-GECoef[1])/log10(53)) ) { GECoef[2] = log10(40-GECoef[1])/log10(53) }
	if (VisCoef[2] > (log10(30-VisCoef[1])/log10(53)) ) { VisCoef[2] = log10(30-VisCoef[1])/log10(53) }
	if (MusCoef[2] > (log10(30-MusCoef[1])/log10(53)) ) { MusCoef[2] = log10(30-MusCoef[1])/log10(53) }
	
	#Put the coefficients and standard errors into a data frame
	OutFrame = data.frame(GECoef=GECoef, VisCoef=VisCoef, MusCoef=MusCoef,
		GESE=GESE, VisSE=VisSE, MusSE=MusSE)
	row.names(OutFrame) = c('a','b')
	#return the data frame
	return(OutFrame)
}

#Now we can make a series of forecasting functions
Predictor <- function(CoefsList, PredictDays) {
	#Make a function that produces the a and b for each corps and caption
	RandCoef <- function(CoefsList, Caption) {
		#Start by making the columname strings
		ColCoef = paste(Caption, 'Coef', sep='')
		ColSE = paste(Caption, 'SE', sep='')
		CorpsNames = names(CoefsList)
		#Make a vector of a noise and b noise
		NCorps = length(CoefsList)
		Random_a = rnorm(n=NCorps, mean=0, sd=1)
		Random_b = rnorm(n=NCorps, mean=0, sd=1)
		
		#make a vector of coefficients in a for loop
		avec = vector(mode='double', length=NCorps)
		bvec = vector(mode='double', length=NCorps)
		for (n in 1:NCorps) {
			#Current scores tell us more about future scores than the model thinks because of "slotting", so we cut the noise by (2/3)
			avec[n] = CoefsList[[n]]['a',ColCoef] + Random_a[n] * CoefsList[[n]]['b',ColSE] * (2/3)
			bvec[n] = CoefsList[[n]]['b',ColCoef] + Random_b[n] * CoefsList[[n]]['b',ColSE] * (2/3)
		}
		
		#return the coefficients in a data frame
		OutFrame = data.frame(a=avec, b=bvec)
		row.names(OutFrame) = CorpsNames
		return(OutFrame)
	}
	
	#Make a propagator function to the list of competition days
	CaptionScore <- function(CaptionFrame, CompDays, CaptionName) {
		#First, load the MASS package
		suppressWarnings( library(MASS) )
		#Find the number of corps
		NCorps = nrow(CaptionFrame)
		
		#Now make a correlation matrix for the number of corps. Scores are correlated with r=0.3
		CorrMatrix = matrix(nrow=NCorps, ncol=NCorps, data=0.3)
		diag(CorrMatrix) = 1
		
		#make the big ole matrix of random numbers
		if (CaptionName == 'GE') {
			RandMat = mvrnorm(30, mu=rep(0,NCorps), Sigma=CorrMatrix, empirical=FALSE) * 0.09
		} else { 
			RandMat = mvrnorm(30, mu=rep(0,NCorps), Sigma=CorrMatrix, empirical=FALSE) * 0.05 
		}
		
		#Make a function that predicts the scores
		ScorePredict <- function(CoefVec, NoiseVec, CompDays) {
			a = as.numeric(CoefVec[1])
			b = as.numeric(CoefVec[2])
			#find the appropriate noise indices
			NoiseInds = CompDays - 22
			#find the score for each CompDay
			FinalScores = a + CompDays^b + NoiseVec[NoiseInds]
			return(FinalScores)
		}
		
		#Create a vector of resulting scores in a for loop
		Scores = vector(mode='list', length=NCorps)
		for (n in 1:NCorps) {
			Scores[[n]] = ScorePredict(CaptionFrame[n,], RandMat[,n], CompDays)
		}
		#Return scores as a data frame
		ScoresFrame = suppressWarnings( data.frame(Reduce(rbind, Scores), stringsAsFactors=F) )
		names(ScoresFrame) = as.character(CompDays)
		row.names(ScoresFrame) = row.names(CaptionFrame)
		return(ScoresFrame)
	}
	
	#Get scores for all three captions
	RandGE = RandCoef(CoefsList, 'GE'); 	GEScores = CaptionScore(RandGE, PredictDays, 'GE')
	RandVis = RandCoef(CoefsList, 'Vis');	VisScores = CaptionScore(RandVis, PredictDays, 'Vis')
	RandMus = RandCoef(CoefsList, 'Mus'); MusScores = CaptionScore(RandMus, PredictDays, 'Mus')
	FinalScores = GEScores + VisScores + MusScores
	return(FinalScores)
}

#Now make a function that turns a run into rank
Scores2Ranks <- function(ScoresFrame) {
	for (C in 1:ncol(ScoresFrame)) {
		ScoresFrame[,C] = rank(0-ScoresFrame[,C], ties.method='random')
	}
	return(ScoresFrame)
}

#Make a function for processing the 3 final shows
TFS_CorpsSummary <- function(CorpsName, ScoresList, RanksList) {
	#create a sub-function that returns the row of each frame for the corps
	CorpsRowExtract <- function(Frame, CorpsName, ColNum) {
		return(Frame[CorpsName,ColNum])
	}
	
	#Get vectors of scores
	ThursdayScores = sapply(ScoresList, CorpsRowExtract, CorpsName=CorpsName, ColNum=1)
	FridayScores = sapply(ScoresList, CorpsRowExtract, CorpsName=CorpsName, ColNum=2)
	SaturdayScores = sapply(ScoresList, CorpsRowExtract, CorpsName=CorpsName, ColNum=3)
	#Get vectors of ranks
	ThursdayRanks = sapply(RanksList, CorpsRowExtract, CorpsName=CorpsName, ColNum=1)
	FridayRanks = sapply(RanksList, CorpsRowExtract, CorpsName=CorpsName, ColNum=2)
	SaturdayRanks = sapply(RanksList, CorpsRowExtract, CorpsName=CorpsName, ColNum=3)

	#Now find the mean score for each day
	MeanThursday = mean(ThursdayScores)
	MeanFriday = mean(FridayScores)
	MeanSaturday = mean(SaturdayScores)
	
	#Now find the odds of making Friday and Saturday
	NRuns = length(ThursdayRanks)
	PercSemis = sum(ThursdayRanks <= 25) / NRuns
	PercFinals = sum(FridayRanks <= 12) / NRuns
	#Find the odds of getting bronze, silver, and gold
	PercBronze = sum(SaturdayRanks == 3) / NRuns
	PercSilver = sum(SaturdayRanks == 2) / NRuns
	PercGold = sum(SaturdayRanks == 1) / NRuns
	
	#Return a vector with all the information
	OutVec = c(MeanThursday, MeanFriday, MeanSaturday, PercSemis*100, PercFinals*100, 
		PercBronze*100, PercSilver*100, PercGold*100)
	return(OutVec)
}

#Create a function for procesing Open Class Finals
OCF_CorpsSummary <- function(CorpsName, ScoresList, RanksList) {
	#create a sub-function that returns the row of each frame for the corps
	CorpsRowExtract <- function(Frame, CorpsName, ColNum) {
		return(Frame[CorpsName,ColNum])
	}
	
	#get a vector of the scores
	TuesdayScores = sapply(ScoresList, CorpsRowExtract, CorpsName=CorpsName, ColNum=1)
	#get a vector of the ranks
	TuesdayRanks = sapply(RanksList, CorpsRowExtract, CorpsName=CorpsName, ColNum=1)
	#Find the mean score for each day
	MeanTuesday = mean(TuesdayScores)
	
	#Find the odds of getting bronze, silver, gold
	NRuns = length(TuesdayRanks)
	PercBronze = sum(TuesdayRanks == 3) / NRuns
	PercSilver = sum(TuesdayRanks == 2) / NRuns
	PercGold = sum(TuesdayRanks == 1) / NRuns
	
	#return a vector with all the information
	OutVec = c(MeanTuesday, PercBronze*100, PercSilver*100, PercGold*100)
	return(OutVec)
}

#Make a function that turns the summary list into a summary frame
SummaryList_2_Frame <- function(SummaryList, Type=c('OCF','TFS')) {
	if (Type == 'TFS') {
		SummaryFrame = suppressWarnings( data.frame(Reduce(rbind, SummaryList), stringsAsFactors=F) )
		names(SummaryFrame) = c('MeanPrelims','MeanSemis','MeanFinals','PercSemis','PercFinals',
			'PercBronze','PercSilver','PercGold')
		row.names(SummaryFrame) = names(SummaryList)
		#order the data frame by mean finals score
		SummaryFrame = SummaryFrame[order(SummaryFrame$MeanFinals, decreasing=T) ,]
		#add a ranking for easy viewing
		SummaryFrame$Rank = 1:nrow(SummaryFrame)
		#Set the order of the columns to most logical order
		SummaryFrame = SummaryFrame[,c(9,1,4,2,5,3,6:8)]
		
		#Turn all irrelevant scores etc to 0
		SummaryFrame[SummaryFrame$MakeSemis == 0, 4:9] = 0
		SummaryFrame[SummaryFrame$MakeFinals == 0, 6:9] = 0
	} else if (Type == 'OCF') {
		SummaryFrame = suppressWarnings( data.frame(Reduce(rbind, SummaryList), stringsAsFactors=F) )
		names(SummaryFrame) = c('MeanTuesday','PercBronze','PercSilver','PercGold')
		row.names(SummaryFrame) = names(SummaryList)
		#order the data frame by mean finals score
		SummaryFrame = SummaryFrame[order(SummaryFrame$MeanTuesday, decreasing=T) ,]
		#add a ranking for easy viewing
		SummaryFrame$Rank = 1:nrow(SummaryFrame)
		SummaryFrame = SummaryFrame[,c(5,1:4)]
	} else { stop('Invalid Type argument: must be either "TFS" or "OCF"') }
	return(SummaryFrame)
}
