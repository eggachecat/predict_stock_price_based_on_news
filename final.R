# please instal jiebaR before demo
# > install.packages("jiebaR")
library("jiebaR")
library("quantmod")
library("xts")


G.INITIAL.FREQUENCY <- 2   # define min initial frequency => in case prob = 0
G.RETURN.THERSHOLD <- 0.5   # if > thershold -> up; if < thershold -> down
G.TrainRtio <- 0.95
G.STOCK.ID <- "2330.TW"   # MS ttr code
G.PERIOD <- "2015/2016"
G.NEWS.SOURCE.DIR <- "UTF8NEWS/"
G.NEWS.HEADER.SEP <- "[\uFF0E]" # the unicode for seperator in news
# Settings for dirs
G.ROOT.DIR <- "output/"

PathJoin <- function(p1, p2){
  return(paste(p1, p2, sep = ""))
}

G.KEYWORDS.DIR <- PathJoin(G.ROOT.DIR, "keywords.txt")
G.LABEL.DIR <- PathJoin(G.ROOT.DIR, "labels.txt")

G.COMBINE.DIR <- PathJoin(G.ROOT.DIR, "coreData.txt")
G.TEST.DIR <- PathJoin(G.ROOT.DIR, "testData.txt")

G.FREQUENCY.DIR <- PathJoin(G.ROOT.DIR, "frequency.txt")
G.PROBABILITY.DIR <- PathJoin(G.ROOT.DIR, "probability.txt")
G.ABUSIVE.DIR <- PathJoin(G.ROOT.DIR, "abusive.txt")

LabelReturn <- function(x){
  label <- ""
  if(x > G.RETURN.THERSHOLD){
    label <- "up"
  }else if(x < -1 * G.RETURN.THERSHOLD){
    label <- "down"
  }else{
    label <- "flat"
  }
  return (label)
}

LoadIndexTable <- function(fileName){
  
  dataToReturn <-  read.table(file = fileName, 
                              row.names = 1,
                              fileEncoding = "utf-8",   # source file encoding in utf-8
                              header = TRUE, sep = ";")
  return (dataToReturn)
}

LoadTable <- function(fileName){
  
  dataToReturn <-  read.table(file = fileName, 
                              fileEncoding = "utf-8",   # source file encoding in utf-8
                              header = TRUE, sep = ";")
  return (dataToReturn)
}

Classifier <- function(abuTable, probTable, keyWords){
  
  prob <- vector(mode = "numeric", length = length(abuTable)) + 1
  
  for (word in keyWords) {

    if(!any(is.na(probTable[word,]))){
      prob <-  prob * probTable[word,]
    }

  }
 
  prob <- prob * abuTable

  return (as.character(names(which.max(prob))))
}

TrainAbusive <- function(cats, freqMatrix){
  
  totalFreq <- sum(freqMatrix)
  
  catsAbusive <- c()
  for (cat in cats) {
    catProb <- c(sum(freqMatrix[cat]) / totalFreq)
    names(catProb) <- cat
    catsAbusive <- cbind(catsAbusive, t(catProb))
  }
  
  return (catsAbusive)
} 

TrainProbability <- function(freqMatrix, Categories){
  
  calProb <- function(x) {
    rowFreq <- sum(x)
    return (sapply(x, function(x) x / rowFreq))
    
  }
  return (t(apply(freqMatrix, 1, calProb)))
  
}
CalFreq <- function(catName, catMatrix){
  
  countVector <- as.vector(unlist(strsplit( as.character(catMatrix$keyWords), "[,]")))
  
  countResult <- as.data.frame(table(countVector, dnn = c("words")))
  colnames(countResult) <- c("words", catName)
  
  return (countResult)
}  

ModifyFrequencyMatrix <- function(freqMatrix){
  
  rownames(freqMatrix) <- freqMatrix[,"words"]
  freqMatrix[,"words"] <- NULL
  freqMatrix[is.na(freqMatrix)] <- 0
  
  return (freqMatrix + G.INITIAL.FREQUENCY)
}

TrainNB <- function() {
  stat.matrix <-  read.table(file = G.COMBINE.DIR, 
                             fileEncoding = "utf-8",   # source file encoding in utf-8
                             header = TRUE, sep = ";")
  
  cats <- unique(stat.matrix$label)
  freqMatrix <- data.frame()
  
  
  for (cat in cats) {
    catMatrix <- as.data.frame(stat.matrix[stat.matrix$label == cat, ])   # get all data labeled as cat
    catFreqMatrix <- CalFreq(cat, catMatrix)
    
    if(nrow(freqMatrix) == 0){
      freqMatrix <- catFreqMatrix;
    }else{
      freqMatrix <- merge(x = freqMatrix, y = catFreqMatrix, all = TRUE, by = "words")
    }
  }
  
  # do some modification
  freqMatrix <- ModifyFrequencyMatrix(freqMatrix)
  
  probTable <- TrainProbability(freqMatrix)
  abusTable <- TrainAbusive(cats, freqMatrix)
  
  
  write.table(freqMatrix, file = G.FREQUENCY.DIR, fileEncoding = "utf-8", sep = ";",row.names = TRUE)
  write.table(probTable, file = G.PROBABILITY.DIR, fileEncoding = "utf-8", sep = ";",row.names = TRUE)
  write.table(abusTable, file = G.ABUSIVE.DIR, fileEncoding = "utf-8", sep = ";",row.names = FALSE)
}

SaveStockData <- function(stockID, timeInterval){
  getSymbols(stockID, src = "yahoo")
  stockData <- get(stockID)[timeInterval]
  stockReturn <- 100 * dailyReturn(stockData)
  labelStock <- apply.daily(stockReturn, LabelReturn)
  colnames(labelStock) <- "label"
  labelStock <- data.frame(time = index(labelStock), label = coredata(labelStock))
  
  write.table(labelStock, file = G.LABEL.DIR, fileEncoding = "utf-8", sep = ";", row.names = FALSE, col.names = TRUE)
}

SaveNewsData <- function(){
  
  newsFileNames <- list.files(path = G.NEWS.SOURCE.DIR, pattern = "*.txt", full.names = T)
  keyworker = worker("keywords", topn = 10)
  cutter = worker()
  
  splitNews <- sapply(newsFileNames, function(newsFileName){
    tryCatch({
      newsText <- read.table(newsFileName, fileEncoding = "utf-8", sep = "\t")
      newsHeader <- as.character(newsText[1,]) 
      newsTime <- unlist(strsplit(newsHeader, G.NEWS.HEADER.SEP))[1]
      
      newsContent <- toString (newsText[-1,])
      keyWords <- vector_keywords(cutter[newsContent],keyworker)
      
      returnVecotr <- c(time = newsTime, keyWords = paste(keyWords, collapse = ','))
      if(!any(is.na(returnVecotr))){
        return (c(time = newsTime, keyWords = paste(keyWords, collapse = ',')))
      }
      return (c(time = "0000-00-00", keyWords = ""))
    }, error = function(e) {
      return (c(time = "0000-00-00", keyWords = ""))
    })
  })
  splitNews <- t(splitNews)

  
  splitNews <- as.data.frame(splitNews)
  splitNews <- subset(splitNews, time != "0000-00-00")    # remove error items
  
  splitNews$time <- as.Date(splitNews$time,  format ="%Y-%m-%d")

  Dates <- unique(splitNews$time)
  

  xtsSplitNews <- xts(splitNews$keyWords, splitNews$time)

  newsData <- sapply(Dates, function(date, xtsData){
    cKeyWords <- paste(as.character(xtsData[date][,1]), collapse = ',')
    newsVector <- c(time = as.character(date), keyWords = cKeyWords)   # vector can only store one type object!!!
    return (newsVector)
  }, xtsData = xtsSplitNews)
  
  write.table(t(newsData), file = G.KEYWORDS.DIR, fileEncoding = "utf-8", sep = ";", row.names = FALSE, col.names = TRUE)
}
CombineData <- function(){
  wordsTable <- LoadTable(G.KEYWORDS.DIR)
  labelTable <- LoadTable(G.LABEL.DIR)
  mergeTable <- merge(labelTable, wordsTable, by = "time", all = FALSE)
  
  #Sample Indexes
  indexes = sample(1:nrow(mergeTable), size=G.TrainRtio * nrow(mergeTable))
  
  # Split data
  trainNewsData = mergeTable[indexes,]
  testNewsData = mergeTable[-indexes,]

  write.table(mergeTable[-1], file = G.COMBINE.DIR, fileEncoding = "utf-8", sep = ";", row.names = FALSE, col.names = TRUE)
  write.table(testNewsData[-1], file = G.TEST.DIR, fileEncoding = "utf-8", sep = ";", row.names = FALSE, col.names = TRUE)
  
    
  #write.table(mergeTable[-1], file = G.COMBINE.DIR, fileEncoding = "utf-8", sep = ";", row.names = FALSE, col.names = TRUE)

}
GetSourceData <- function(){
  if(!dir.exists(G.ROOT.DIR)){
    dir.create(G.ROOT.DIR)
  }
  SaveStockData(G.STOCK.ID, G.PERIOD)#############################################
  SaveNewsData()
}

Main <- function(){
  GetSourceData()
  CombineData()
  TrainNB()
  abusiveProb <-LoadTable(G.ABUSIVE.DIR)  
  probTable <-LoadIndexTable(G.PROBABILITY.DIR) 
  
  testDataSet <- read.table(G.TEST.DIR, fileEncoding = "utf-8",header = TRUE, sep = ";")


  ScoreVector <- apply(testDataSet, 1, function(row){
 
    tobePredicted <- as.character(unlist(strsplit(row["keyWords"], "[,]"))) 
    predictLabel <- Classifier(abusiveProb, probTable, tobePredicted)
    cat("System Predict it predicts in: ",predictLabel,"and Real: ", row["label"], "\n")
    
    if(predictLabel == row["label"]){
      return (TRUE)
    }
    return (FALSE)
  })
  print(table(ScoreVector))
}
Main()

