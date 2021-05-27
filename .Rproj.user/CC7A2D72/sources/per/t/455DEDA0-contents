library(tidyverse)
library(plyr)

dir <- setwd("D:/PhD/Experiments/Exp7/rawdata/prolific_3321")

#saving all the csv files in that directory into one variable
allOldata <- list.files(path = dir, pattern = "*.csv")

#making the file names into one list
Exp7data <- lapply(allOldata, read_csv)

#using rbind to save it al in on df
#using rbind.fill because some columns do not exist in some datafiles, namely the "preResp.key" 
Exp7 <- do.call(rbind.fill,Exp7data)

write.csv(Exp7, file = "Exp7_fulldataset.csv")
