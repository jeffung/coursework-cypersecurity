library(RCurl)

#Get URL's from GitHub for train and test data
trainURL <- getURL("https://raw.githubusercontent.com/CMPT-318/Cybersecurity-Project/master/Data/Train/train.txt")
test1URL <- getURL("https://raw.githubusercontent.com/CMPT-318/Cybersecurity-Project/master/Data/Test/test1.txt")
test2URL <- getURL("https://raw.githubusercontent.com/CMPT-318/Cybersecurity-Project/master/Data/Test/test2.txt")

#Load csv data into tables
trainFull <- read.table(text = trainURL, sep = ",", header = TRUE)
test1Full <- read.table(text = test1URL, sep = ",", header = TRUE)
test2Full <- read.table(text = test2URL, sep = ",", header = TRUE)

rm(trainURL)
rm(test1URL)
rm(test2URL)