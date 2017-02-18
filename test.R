#! /usr/bin/env Rscript

# File needs to be in Other Study Data

#TODO: Differentiate between missing(0) and Not Applicable(NA) right now both as NA.
#	Makes no difference in plotting
#TODO: Perform Signal Analysis
#TODO: Plot Signals
#TODO: Perinasal Analysis : They are stored in zip files


#Include Libraries
library('xlsx')


# FUNCTIONS
SigAnalyze = function(fPath,sigName){
	sigRange = list(BR=c(4,70))
	df=read.xlsx(fPath,1)
}

# Begin R Commands
path = getwd()
data_path = "Other Study Data"
path = paste(path,data_path,sep='/')
dir_items = dir(path)
# Add pp, res
Signals = c('BR','HR','peda','res')
# Signals = c('BR')

Sessions = c('BL', 'CD', 'ED', 'FDN', 'FDL', 'MD', 'ND', 'PD', 'RD')

#######

# print(path)
# print(dir_items)

#######

# Create Initial Data Frame Coloumns
df_List = list()
df_List$Subject[1] = 'Init'
df_List$Session[1] = 'Init'

for(signal in Signals){
	# Iter No
	iter = 1
	# Add The Signal Coloumn
	ctr = 1
	df_List[signal][ctr] = 0
#	print(df_List)
#	break
	ctr = ctr + 1

	for(subject in dir_items){
		sub_path = paste(path,subject,sep="/")
		for(session in Sessions){
			sessPat = paste('.',session,sep="*")
			# print(sigPat)
			sess_folder = dir(sub_path, pattern=sessPat)
			# print(length(sess_folder))
			if(length(sess_folder)==0){
				# Session Does Not Exist
				if(session=='FDL' || session == 'FDN'){
					next
				}
				df_List$Subject[ctr] = subject
				df_List$Session[ctr] = session
				df_List[[signal]][ctr] = NA
			}
			# Add session to data frame coloumn
			# print(sess_folder)

			# Add Table Entry if First iteration
			if(iter == 1){
				df_List$Subject[ctr] = subject
				df_List$Session[ctr] = session
			}
			sess_path = paste(sub_path,sess_folder,sep="/")
			# print(sess_path)
			sigPat = paste('.',signal,sep='*')
			sigFile = dir(sess_path,pattern=sigPat)
			if(length(sigFile)==0){
				# No File For Given Signal
				# Update Entry Appropriately
				df_List[[signal]][ctr] = NA
				ctr = ctr + 1
				next
			}
			# Perform Signal Quality Analysis
			SigAnalysis()
			# And Update Entry 1 - Valid, -1 - Invalid
			df_List[[signal]][ctr] = 1
			ctr = ctr + 1
			# print(sigFile)
		}
		# break
	}
	# break
	# Draw Signal Plot
	iter = iter + 1
}

df_Table = data.frame(df_List)
df_Table = df_Table[-1,]
# print(df_Table)

## Write to the excel file
write.xlsx(df_Table, 'IdxTable.xlsx', sheetName='Sheet1', col.names=TRUE, row.names=TRUE, append=FALSE, showNA = TRUE)

