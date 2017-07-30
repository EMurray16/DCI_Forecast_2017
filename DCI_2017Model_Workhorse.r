#This is the workhorse script that actually does the prediction

#import libraries and functions
library(xlsx); library(tictoc)
source('DCIModelFunctions.r')

#First, make a list of all the sheet names in the Excel sheet
SheetNames = c('WC_GE','WC_Vis','WC_Mus','OC_GE','OC_Vis','OC_Mus')
#Now read the sheets into their own data frame
ScoreFrameList = lapply(SheetNames, read.xlsx, file='DCI_Scores_2017.xls', stringsAsFactors=F)
#Pull out the individual frames and name them accordingly
for (i in 1:length(ScoreFrameList)) {
	FrameName = SheetNames[i]
	assign(FrameName, ScoreFrameList[[i]])
}

#Make a list of the World Class corps and open class corps
WorldCorps = as.list(WC_GE$Corps)
OpenCorps = as.list(OC_GE$Corps)

#Now run the reducer function on each corps
WorldReduce = lapply(WorldCorps, CorpsReduce, GEFrame=WC_GE, VisFrame=WC_Vis, MusFrame=WC_Mus)
names(WorldReduce) = WorldCorps
OpenReduce = lapply(OpenCorps, CorpsReduce, GEFrame=OC_GE, VisFrame=OC_Vis, MusFrame=OC_Mus)
names(OpenReduce) = OpenCorps

#Make the model data frames for all corps
WorldCoefs = lapply(WorldReduce, ExpFitter)
WorldCoefsClean = WorldCoefs[ WorldCoefs != 'NULL' & WorldCoefs != 'NA']
OpenCoefs = lapply(OpenReduce, ExpFitter)
OpenCoefsClean = OpenCoefs[ OpenCoefs != 'NULL' & OpenCoefs != 'NA']
#Combine the lists into an all corps list
AllCoefsClean = c(OpenCoefsClean,WorldCoefsClean)

#Find the corps names for each list
OpenNames = names(OpenCoefsClean)
WorldNames = names(WorldCoefsClean)
AllNames = names(AllCoefsClean)

# NOTE: The quickest way to do the simulation is to do both Monte Carlos back-to-back in parallel.
#		I didn't do this here because of uncertainty as to whether parallel computing is feasible 
#		on all machines this code will run on. 

#Now predict the WC finals week shows
tic(); AllRuns = replicate(10000, Predictor(AllCoefsClean, 50:52), simplify=F); toc()
AllRanks = lapply(AllRuns, Scores2Ranks)
tic(); AllSumList = lapply(AllNames, TFS_CorpsSummary, ScoresList=AllRuns, RanksList=AllRanks); toc()
names(AllSumList) = AllNames
TFS_Frame = SummaryList_2_Frame(AllSumList, Type='TFS')

#Predict Open Class finals
tic(); OCFRuns = replicate(1000, Predictor(OpenCoefsClean, 48), simplify=F); toc()
OCFRanks = lapply(OCFRuns, Scores2Ranks)
tic(); OCFSumList = lapply(OpenNames, OCF_CorpsSummary, ScoresList=OCFRuns, RanksList=OCFRanks); toc()
names(OCFSumList) = OpenNames
OCF_Frame = SummaryList_2_Frame(OCFSumList, Type='OCF')
