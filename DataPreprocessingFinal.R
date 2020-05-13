#modules
install.packages("readr") # read file as string
library(readr)
install.packages("tidyverse")
library(tidyverse)
library( broom )
install.packages("reshape")
library(reshape)
install.packages("mice")
library(mice)
install.packages("VIM")
install.packages("SparseM")
library(VIM)
library(SparseM)


#get data
entryData <- read.delim("https://archive.ics.uci.edu/ml/machine-learning-databases/00211/CommViolPredUnnormalizedData.txt",header = TRUE, sep = ",",dec = ".")

#format as DF
entryData <- as.data.frame(entryData)

#get headers
headers <- read_file("headers.txt")
headers <- strsplit(headers, "-- ")
headersClean <- c()
for(i in 2:148){
  el <- unlist(strsplit(headers[[1]][i], split=':', fixed=TRUE))[1]
  headersClean <- c(headersClean, el)
}

#set headers in entryData
colnames(entryData)<-c(headersClean)

#mark missing as NA
entryData[entryData == "?"] <- NA
countNA <- as.data.frame(sapply(entryData, function(x) sum(is.na(x))))

#transform counting of NA to DF
resultsVal <- countNA[,1]
results <- data.frame(resultsVal)
results <- as.data.frame(t(results[,1]))
names(results) <- colnames(entryData)

#save entry dataset to file entryDataSet.csv
write.csv(entryData, "./PROJECT/entryData.csv", )

#save missing data summary to file entryDatasetMissingInfo.csv
write.csv(results, "./PROJECT/entryDatasetMissingInfo.csv", )

#select 6 columns for further analysis: "rapes","rapesPerPop","arsons","arsonsPerPop","ViolentCrimesPerPop","nonViolPerPop" 
selectedDataset<- entryData %>% select(132,133,144,145,146,147)

#save selected dataset to file selectedDataset.csv
write.csv(selectedDataset, "./PROJECT/selectedDataset.csv", )

#prepare dataset for analysis
#1:"rapes"
#2:"rapesPerPop"
#3: arsons"
#4: arsonsPerPop"
#5: "ViolentCrimesPerPop"
#6: "nonViolPerPop"

columnLabels <- colnames(selectedDataset)
colnames(selectedDataset) <- c(1:length((columnLabels)))

#basic info about missing data for selected variables
md.pattern(selectedDataset)
#-> file Pos 1

#more precise info about misisng data for selected variables
mice_plot <- aggr(selectedDataset, col=c('green','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(selectedDataset), cex.axis=.7,
                  gap=1, ylab=c("Missing data","Pattern"))

#-> file Pos 2

#basic statistics for columns before data modifications
selectedStats <- summary(selectedDataset)
print(selectedStats)

#-> file Pos 3
fields <- columnLabels
fieldsNumeric <- c(1:length((columnLabels)))
  

filename<- paste("./PROJECT/IMAGES/boxPlotRapes.png")
png(file=filename,
    width=1280, height=720)
boxplot(c(selectedDataset$`1`), horizontal=TRUE, main="rapes")
dev.off()

filename<- paste("./PROJECT/IMAGES/boxPlotRapesPerPop.png")
png(file=filename,
    width=1280, height=720)
boxplot(c(selectedDataset$`1`), horizontal=TRUE, main="rapesPerPop")
dev.off()

filename<- paste("./PROJECT/IMAGES/boxPlotArsons.png")
png(file=filename,
    width=1280, height=720)
boxplot(c(selectedDataset$`1`), horizontal=TRUE, main="arsons")
dev.off()

filename<- paste("./PROJECT/IMAGES/boxPlotArsonsPerPop.png")
png(file=filename,
    width=1280, height=720)
boxplot(c(selectedDataset$`1`), horizontal=TRUE, main="arsonsPerPop")
dev.off()

filename<- paste("./PROJECT/IMAGES/boxPlotViolentCrimesPerPop.png")
png(file=filename,
    width=1280, height=720)
boxplot(c(selectedDataset$`1`), horizontal=TRUE, main="ViolentCrimesPerPop")
dev.off()

filename<- paste("./PROJECT/IMAGES/boxPlotNonViolPerPop.png")
png(file=filename,
    width=1280, height=720)
boxplot(c(selectedDataset$`1`), horizontal=TRUE, main="nonViolPerPop")
dev.off()

fields <- columnLabels
fieldsNumeric <- c(1:length((columnLabels)))
colnames <- fields
#histograms for variables 
filename<- paste("./PROJECT/IMAGES/histogramCombined.png")
png(file=filename,width=1200, height=1200)
par(mfrow=c(3,2))
for (i in 1:length(fieldsNumeric)) {
  hist(c(selectedDataset[,i]), main=i, probability=TRUE, col="gray", border="white")
}
dev.off()

#denstity and curver plots => check later why not working
filename<- paste("./PROJECT/IMAGES/normalCurveCombined.png")
png(file=filename,width=1200, height=1200)
par(mfrow=c(3, 2))
colnames <- fields
for (i in 1:length(fieldsNumeric)) {
  myhist <- hist(c(selectedDataset[,i]))
  multiplier <- myhist$counts / myhist$density
  mydensity <- density(selectedDataset[,i])
  mydensity$y <- mydensity$y * multiplier[1]
  
  plot(myhist)
  lines(mydensity)
}


#imputations

#MICE imputation
#m - number of features
#maxit - number of iterations
#method - pmm - related to continuous data

datasetToImpute <- selectedDatasetToImpute

imputedDataMICE <- mice(datasetToImpute, m=6, maxit = 5, method = 'pmm', seed = 500) 
summary(imputedDataMICE)


imputedDataMICE$imp$rapes
#row numbers on the left which were imputed and on the top iteration number

#=> we have to choose iteration which we think is best
completedData <- complete(imputedDataMICE,4)
head(completedData,n=15)

#imputed dataset with MICE
write.csv(x=completedData,file="./PROJECT/selectedDataImputedMICE.csv", append=FALSE)



datasetToImputeHMISC <- selectedDatasetToImpute

#SECOND -> HMISC
install.packages("Hmisc")
install.packages("survival")
library(Hmisc)
library(survival)

#impute with mean value
head(datasetToImputeHMISC, n=15)
#adds new columns with imputed values using HMISC
datasetToImputeHMISC$imputed_rapes <- with(datasetToImputeHMISC, impute(rapes,mean))
#check where it was imputed compare original with imputed in dataset
head(datasetToImputeHMISC, n=15)

#impute with random
datasetToImputeHMISC$imputed_rapesR <- with(datasetToImputeHMISC, impute(rapes,'random'))
head(datasetToImputeHMISC, n=15)


#impute with mean value
head(datasetToImputeHMISC, n=15)
#adds new columns with imputed values using HMISC
datasetToImputeHMISC$imputed_ViolentCrimesPerPop <- with(datasetToImputeHMISC, impute(ViolentCrimesPerPop,mean))
#check where it was imputed compare original with imputed in dataset
head(datasetToImputeHMISC, n=15)

#impute with random
datasetToImputeHMISC$ViolentCrimesPerPopR <- with(datasetToImputeHMISC, impute(ViolentCrimesPerPop,'random'))
head(datasetToImputeHMISC, n=15)


#impute with mean value
head(datasetToImputeHMISC, n=15)
#adds new columns with imputed values using HMISC
datasetToImputeHMISC$imputed_arsons <- with(datasetToImputeHMISC, impute(arsons,mean))
#check where it was imputed compare original with imputed in dataset
head(datasetToImputeHMISC, n=15)

#impute with random
datasetToImputeHMISC$arsonsR <- with(datasetToImputeHMISC, impute(arsons,'random'))
head(datasetToImputeHMISC, n=15)

#impute with mean value
head(datasetToImputeHMISC, n=15)
#adds new columns with imputed values using HMISC
datasetToImputeHMISC$imputed_arsonsPerPop <- with(datasetToImputeHMISC, impute(arsonsPerPop,mean))
#check where it was imputed compare original with imputed in dataset
head(datasetToImputeHMISC, n=15)

#impute with random
datasetToImputeHMISC$arsonsPerPopR <- with(datasetToImputeHMISC, impute(arsonsPerPop,'random'))
head(datasetToImputeHMISC, n=15)


#impute with mean value
head(datasetToImputeHMISC, n=15)
#adds new columns with imputed values using HMISC
datasetToImputeHMISC$imputed_ViolentCrimesPerPop <- with(datasetToImputeHMISC, impute(ViolentCrimesPerPop,mean))
#check where it was imputed compare original with imputed in dataset
head(datasetToImputeHMISC, n=15)

#impute with random
datasetToImputeHMISC$ViolentCrimesPerPopR <- with(datasetToImputeHMISC, impute(ViolentCrimesPerPop,'random'))
head(datasetToImputeHMISC, n=15)



#impute with mean value
head(datasetToImputeHMISC, n=15)
#adds new columns with imputed values using HMISC
datasetToImputeHMISC$imputed_nonViolPerPop <- with(datasetToImputeHMISC, impute(nonViolPerPop,mean))
#check where it was imputed compare original with imputed in dataset
head(datasetToImputeHMISC, n=15)

#impute with random
datasetToImputeHMISC$nonViolPerPopR <- with(datasetToImputeHMISC, impute(nonViolPerPop,'random'))
head(datasetToImputeHMISC, n=15)


#impute with mean value
head(datasetToImputeHMISC, n=15)
#adds new columns with imputed values using HMISC
datasetToImputeHMISC$imputed_rapesPerPop <- with(datasetToImputeHMISC, impute(rapesPerPop,mean))
#check where it was imputed compare original with imputed in dataset
head(datasetToImputeHMISC, n=15)

#impute with random
datasetToImputeHMISC$rapesPerPopR <- with(datasetToImputeHMISC, impute(rapesPerPop,'random'))
head(datasetToImputeHMISC, n=15)

#save file with only imputations for some values
write.csv(x=datasetToImputeHMISC,file="./PROJECT/dataSelectedImputedHMISC.csv")

head(datasetToImputeHMISC,15)





#THIRD => MISS FOREST
datasetToImputeMISSFOREST <- selectedDatasetToImpute


install.packages("missForest")
library(missForest)

#impute missing values using all parameters as default values -. uses random
datasetToImputeMISSFOREST.imp <- missForest(datasetToImputeMISSFOREST)

#check values imputed
datasetToImputeMISSFOREST.imp$ximp

#check accuracy of imputation
head(datasetToImputeMISSFOREST.imp)
#NRMSE - Normalalized Mean Squared Error. Represent errors derived from imputing continuous values - 12% error 
#PFC - Proportion of Falsely Classified - Represents error derived from imputing categorical values - 46 % -> to high, caused becase of lots of missign data

#comparing actual data accuracy
#mixError(ximp,xmis,xtrue)
#ximp - imputed data
#xmis - missing data
#xtrue - original data without any missing values

View(datasetToImputeMISSFOREST.imp$ximp)

#save
write.csv(datasetToImputeMISSFOREST.imp$ximp, './PROJECT/selectedDatasetImputedMISSFOREST.csv')





