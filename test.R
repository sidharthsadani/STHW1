#! /usr/bin/env Rscript

# File needs to be in Other Study Data

#Include Libraries
library('xlsx')


# Begin R Commands
path = getwd()
data_path = "Other Study Data"
path = paste(path,data_path,sep='/')
dir_items = dir(path)
# Add pp, res
# Signals = c('.BR','.HR','.FACS','.peda')
Signals = c('.BR')

Sessions = c('BL', 'CD', 'ED', 'FDN', 'FDL', 'MD', 'ND', 'PD', 'RD')

#######

# print(path)
# print(dir_items)

#######

for(signal in Signals){
	for(item in dir_items){
		sub_path = paste(path,item,sep="/")
		sub_items = dir(sub_path)
		print(sub_items)
		break
	}
}


