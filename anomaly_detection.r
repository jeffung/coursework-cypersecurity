#Install the RCurl package to use the RCurl library
library(RCurl)

#Get URL's from GitHub for train and test data
trainURL <- getURL("https://raw.githubusercontent.com/CMPT-318/Cybersecurity-Project/master/Data/Train/train.csv")
test1URL <- getURL("https://raw.githubusercontent.com/CMPT-318/Cybersecurity-Project/master/Data/Test/test_v1.csv")
test2URL <- getURL("https://raw.githubusercontent.com/CMPT-318/Cybersecurity-Project/master/Data/Test/test_v2.csv")

#Load csv data into tables
trainTable <- read.csv(text = trainURL)
test1Table <- read.csv(text = test1URL)
test2Table <- read.csv(text = test2URL)
