library(tidyverse)
library(plyr)

dir <- setwd("D:/PhD/Experiments/Exp8/rawdata/Final batch")

#saving all the csv files in that directory into one variable
allOldata <- list.files(path = dir, pattern = "*.csv")

#making the file names into one list
Exp8data <- lapply(allOldata, read_csv)

#using rbind to save it al in on df
#using rbind.fill because some columns do not exist in some datafiles, namely the "preResp.key" 
Exp8 <- do.call(rbind.fill,Exp8data)

write.csv(Exp8, file = "Exp8_fulldataset.csv")
rm(Exp8)
Exp8Bonus <- aggregate(data = Exp8, Bonus~PROLIFIC_ID, sum)
Exp8Bonus$Perf <- Exp8$Performance
Exp8Prolific <- as.data.frame(Exp8Prolific)
