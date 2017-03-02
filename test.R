#! /usr/bin/env Rscript

# File needs to be in Other Study Data

## Include Libraries
# File I/O Library
library('xlsx')

# Plot Libraries
# library(ggplot2)
# library(gridExtra)
# library(grid)
# library("reshape2")


# FUNCTIONS
#
# Returns if the BR, HR, peda, pp signal is valid(+1) or not(-1)
#
SigAnalyze = function(fPath,sigName){
	isValid = 1
	if(sigName=="pp"){
		# pp data is already cleaned
		return(isValid)
	}

	# Acceptable ranges of each the signals
	sigRange = list(BR=c(4,70), HR=c(40,140), peda=c(10,4700))
	# Extract the lower and upper ranges based on the signal
	LowerB = sigRange[[sigName]][1]
	UpperB = sigRange[[sigName]][2]
	# Read the data file, data headers start from row 9, and signal data is in col 3
	# for all these four signal types
	df=read.xlsx2(fPath,1, startRow=9, colIndex=3)
	# We loop through all the entries in the data, and return 1 : valid and -1 : invalid
	nRowsDf = nrow(df)
	for(i in 1:nRowsDf){
		# xlsx2 is faster but reads entries as factors so we convert it to a number
		testVal = as.numeric(as.character(df[i,1]))
		# If any entry is above or below the lower bound, the signal is faulty,
		# set to -1 and break from the loop
		if(testVal>UpperB){
			isValid = -1
			print(fPath)
			break
		}
		if(testVal<LowerB){
			isValid = -1
			print(fPath)
			break
		}
	}
	# Removing the data frame from memory
	rm(df)
	# Returning the signal validity
	isValid
}

#
# Cleans the performance signals namely acceleration, speed and breaking
# Returns (+1) if the signal is already clean, (-1) if the cleaning was performed
#
SigClean = function(fPath, foldPath){
	# Do stuff
	isValid = list(res=1,Speed=1,Accel=1,Brake=1)
	
	# Read Time Values, as they are common across all the signals that are being analysed
	dfT=read.xlsx2(fPath,1, startRow=9, colIndex=2, colClasses="numeric")
	
	##############
	# Do Speed Cleaning
	###############
	# -0.1 - 0.1 = 0
	# <-0.1 = NA
	# Col No = 3
	# Read the data file, data headers start from row 9, and signal data is in col 3
	df=read.xlsx2(fPath,1, startRow=9, colIndex=3, colClasses="numeric")
	# We loop through all the entries in the data, and return 1 : valid and -1 : invalid
	nRowsDf = nrow(df)
	for(i in 1:nRowsDf){
		# xlsx2 is faster but reads entries as factors so we convert it to a number
		testVal = df[i,1]
		# If any entry is b/w -0.1 and 0.1 substitute 0, if < -0.1 substitute NA
		if(testVal>=-0.1 && testVal<=0.1){
			isValid['Speed'] = -1
			df[i,1] = 0
		}
		else if(testVal< -0.1){
			isValid['Speed'] = -1
			df[i,1] = as.numeric(NA)
		}
	}

	if(isValid['Speed']==-1){
		# Write the file to disk
		savePath = paste(foldPath,'pedaSpeed.xlsx',sep='/')
		# Combine the time axis and the value axis
		dfC <- cbind(dfT, df)
		write.xlsx2(dfC, savePath, sheetName='Sheet1', col.names=TRUE, row.names=TRUE, append=FALSE, showNA = TRUE)
	}
	# Removing the data frame from memory
	rm(df)
	# rm(dfC)

	###############
	# Do Acceleration Cleaning
	###############
	# <0 = NA
	# Col No = 4

	# Read the data file, data headers start from row 9, and signal data is in col 4
	df=read.xlsx2(fPath,1, startRow=9, colIndex=4, colClasses="numeric")
	# We loop through all the entries in the data, and return 1 : valid and -1 : invalid
	nRowsDf = nrow(df)
	for(i in 1:nRowsDf){
		# xlsx2 is faster but reads entries as factors so we convert it to a number
		testVal = df[i,1]
		# If any entry is <0 plug in NA
		if(testVal< 0){
			isValid['Accel'] = -1
			df[i,1] = as.numeric(NA)
		}
	}

	if(isValid['Accel']==-1){
		# Write the file to disk
		savePath = paste(foldPath,'pedaAccel.xlsx',sep='/')
		# Combine the time axis and the value axis
		dfC <- cbind(dfT, df)
		write.xlsx2(dfC, savePath, sheetName='Sheet1', col.names=TRUE, row.names=TRUE, append=FALSE, showNA = TRUE)
	}
	# Removing the data frame from memory
	rm(df)
	# rm(dfC)

	###############
	# Do Brake Cleaning
	###############
	# >300 = 300
	# Col No = 5

	# Read the data file, data headers start from row 9, and signal data is in col 5
	df=read.xlsx2(fPath,1, startRow=9, colIndex=5, colClasses="numeric")
	# We loop through all the entries in the data, and return 1 : valid and -1 : invalid
	nRowsDf = nrow(df)
	for(i in 1:nRowsDf){
		# xlsx2 is faster but reads entries as factors so we convert it to a number
		testVal = df[i,1]
		# If any entry is <0 plug in NA
		if(testVal< 300){
			isValid['Brake'] = -1
			df[i,1] = 300
		}
	}

	if(isValid['Brake']==-1){
		# Write the file to disk
		savePath = paste(foldPath,'pedaBrake.xlsx',sep='/')
		# Combine the time axis and the value axis
		dfC <- cbind(dfT, df)
		write.xlsx2(dfC, savePath, sheetName='Sheet1', col.names=TRUE, row.names=TRUE, append=FALSE, showNA = TRUE)
	}
	# Removing the data frame from memory
	rm(df)
	# rm(dfC)

	# print(isValid)

	# Returning the signal validity
	isValid
}

#################################
######## Begin R Script
#################################

# Extract data paths
path = getwd()
data_path = "Other Study Data"
path = paste(path,data_path,sep='/')
dir_items = dir(path)

# List of Signals
Signals = c('pp', 'HR', 'BR', 'peda', 'res')
# Signals = c('pp', 'HR', 'BR','peda')
# Signals = c('res')
subSignal = c('Speed', 'Accel', 'Brake')

# List of Sessions
Sessions = c('BL', 'CD', 'ED', 'FDN', 'FDL', 'MD', 'ND', 'PD', 'RD')

# Create Initial Data Frame Coloumns,
# Create Subject Column, Session Column, and give the first entry a dummy value 
# which we will del at the end
df_List = list()
df_List$Subject[1] = 'Init'
df_List$Session[1] = 'Init'

for(signal in Signals){
	# Iteration No : If first iteration for signal we must add a col to the Idx Table DF.
	iter = 1
	# Adding a column for the new signal and initializing it's first entry to 0
	# We do this to not create an empty column in the data frame
	ctr = 1
	df_List[signal][ctr] = 0
	if(signal=="res"){
		# Add an entry for all the subsignals that we are cleaning
		# Namely : Speed Acceleration Braking/Breaking
		for(subSig in subSignal){
			df_List[subSig][ctr] = 0
		}
	}
	ctr = ctr + 1

	# For a given signal, iterate through all the subjects
	for(subject in dir_items){
		# Computing data path of subject
		sub_path = paste(path,subject,sep="/")

		# For a given signal, for a given subject iterate through all possible treatments
		for(session in Sessions){
			# Search patter for session folder
			sessPat = paste('.',session,sep="*")
			# Finding folder for the desired session, in subject folder
			sess_folder = dir(sub_path, pattern=sessPat)
		
			if(length(sess_folder)==0){
				# Session Does Not Exist
				if(session=='FDL' || session == 'FDN'){
					# Subjects mutually exclusively have FDL or FDN
					# If it's the other folder go the next iter without updating
					# any counters
					next
				}
				# Add entry for this subject and session if this is the first signal
				if(iter==1){
					df_List$Subject[ctr] = subject
					df_List$Session[ctr] = session
				}
				# if session folder did not exist, set signal entry to NA
				df_List[[signal]][ctr] = NA
			}
			
			# Add Table Entry for subjects and sessions if this is the first signal
			if(iter == 1){
				df_List$Subject[ctr] = subject
				df_List$Session[ctr] = session
			}
			# Computing the full path of the session folder
			sess_path = paste(sub_path,sess_folder,sep="/")
			# Search patter for the desired signal
			sigPat = paste('.',signal,sep='*')
			# Finding file for the desired signal
			sigFile = dir(sess_path,pattern=sigPat)
			if(length(sigFile)==0){
				# File does not exist
				# Set it's table entry to 0
				df_List[[signal]][ctr] = 0
				ctr = ctr + 1
				next
			}
			# Computing the full path of the signal file
			sigFilePath = paste(sess_path, sigFile, sep="/")
			print(sigFilePath, length(sigFile))
			# Perform Signal Quality Analysis, or Cleaning For (RES)
			if(signal=="res"){
				# Do cleaning
				isValid = SigClean(sigFilePath, sess_path)
				# Update Entry in IdxTable
				# 1 - No cleaning done, -1 cleaning done on one of the signals
				df_List[[signal]][ctr] = isValid[[signal]]
				# Update Entries for the subsignals that we clean
				for(subSig in subSignal){
					# Update Whether Signal is Cleaned Or Not
					df_List[[subSig]][ctr] = isValid[[subSig]]
				}
			}
			else{
				isValid = SigAnalyze(sigFilePath, signal)
				# Update Entry in IdxTable 1 - Valid, -1 - Invalid
				df_List[[signal]][ctr] = isValid
			}
			ctr = ctr + 1
		
		}
	
	}

	# Print confirmation of completion
	print(paste(signal,"Done",sep=" "))
	iter = iter + 1
}

# Convert the list containing the IdxTable to a data frame
df_Table = data.frame(df_List)
# Delete the first entry in the table, as it was a dummy initialization
df_Table = df_Table[-1,]

# Write the Idx Table to the excel file
write.xlsx(df_Table, 'IdxTable.xlsx', sheetName='Sheet1', col.names=TRUE, row.names=TRUE, append=FALSE, showNA = TRUE)

