
path1 <- "C:\\Users\\Mind Is Moving\\Dropbox\\Completed and Published Projects\\1-papers\\Bateman Pimbwe\\RSOS Submission - Revision\\PublicWorkflow"
setwd(path1)

############################################################################################## Main Model
load('PimbweBase.RData') # Builds a database from several data files, exports and R session

source('./Code/RunModels.R') # Runs all Stan models on a server

source('./Code/Process-SpouseNumber.R')
source('./Code/Process-SpouseYears.R') 
source('./Code/Process-TimingWeights.R') 
source('./Code/Process-QualityWeights.R')   
source('./Code/Process-BothWeights.R') 
source('./Code/Process-Full.R')  

source('./Code/ResultsMain.R') 
source('./Code/ResultsDiffs.R')

source('./Code/ResultsRS.R')

source('./Code/SuppPlots.R')

############################################################################################## Robustness Model
rm(list=ls(all=TRUE))
path1 <- "C:\\Users\\Mind Is Moving\\Dropbox\\Completed and Published Projects\\1-papers\\Bateman Pimbwe\\RSOS Submission - Revision\\PublicWorkflow"
setwd(path1)

load('PimbweBase.RData') # Builds a database from several data files, exports and R session

source('./CodeImpute/TrimData.R') # 

source('./CodeImpute/RunModels.R') # Runs all Stan models on a server

source('./CodeImpute/Process-SpouseNumber.R')
source('./CodeImpute/Process-SpouseYears.R') 
source('./CodeImpute/Process-TimingWeights.R') 
source('./CodeImpute/Process-QualityWeights.R')   
source('./CodeImpute/Process-BothWeights.R') 
source('./CodeImpute/Process-Full.R') 

source('./CodeImpute/ResultsMain.R') 
source('./CodeImpute/ResultsDiffs.R')



rm(list=ls(all=TRUE))
path1 <- "C:\\Users\\Mind Is Moving\\Dropbox\\Completed and Published Projects\\1-papers\\Bateman Pimbwe\\RSOS Submission - Revision\\PublicWorkflow"
setwd(path1)

load('PimbweBase.RData') 
source('./Code/SimpleModels.R')













                            
                         

 

 

