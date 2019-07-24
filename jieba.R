library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(dplyr)

setwd("D:/GitHub Files/Extradition-Bill/PttData")

days.processed <- 5
data.processing <- NULL
date.processing <- NULL
pttData <- NULL

for (days.processed in 5 : 51) {
  print(days.processed)
  if((days.processed / 30) < 1) {
    month.d <- 'Jun'
  }else{
    month.d <- 'Jul'
  }
  date.d <- days.processed %% 30
  if(month.d == 'Jul') date.d = date.d + 1
  date.processing <- paste(month.d, date.d, sep = '_')
  data.processing <- paste0('D:/GitHub Files/Extradition-Bill/PttData/data_', month.d, '_', date.d, '.csv') 
  data.processing
  pttData <- read.csv(data.processing, header=T, sep=",")
  if(!file.exists(paste0('./titles/', date.processing,' title.txt'))){file.create(paste0('./titles/', date.processing,' title.txt'))}
  if(!file.exists(paste0('./texts/', date.processing,' texts.txt'))){file.create(paste0('./texts/', date.processing,' texts.txt'))}
  write(character(0), paste0('./titles/', date.processing,' title.txt'), append = FALSE)
  write(character(0), paste0('./texts/', date.processing,' texts.txt'), append = FALSE)
  doc <- function(d){
    write(d['titles'], paste0('./titles/', date.processing,' title.txt'), append = TRUE)
    write(d['texts'], paste0('./texts/', date.processing,' texts.txt'), append = TRUE)    
  }
  apply(pttData, 1, doc)
}

folder <- './titles/'
d.corpus <- Corpus(DirSource(folder))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
  })
mixseg = worker()
tokenizer = function(d)
{
  unlist(segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, tokenizer)
  
count_token = function(d)
{
   as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM.titles = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)

for(id in 2 : 47)
{
  print(id)
  TDM.titles = merge(TDM.titles, tokens[[id]], by="d", all = TRUE)
  names(TDM.titles) = c('d', colNames[1 : id])
}
TDM.titles[is.na(TDM.titles)] <- 0


folder <- 'texts/'
d.corpus <- Corpus(DirSource(folder))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})
mixseg = worker()
tokenizer = function(d)
{
  unlist(segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)
n = length(seg)
TDM.texts = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)

for(id in 2 : 47)
{
  print(id)
  TDM.texts = merge(TDM.texts, tokens[[id]], by="d", all = TRUE)
  names(TDM.texts) = c('d', colNames[1 : id])
}
TDM.texts[is.na(TDM.texts)] <- 0

setwd("D:/GitHub Files/Extradition-Bill/PttData/output")
write.csv(TDM.titles, 'TDM_titles', row.names = TRUE)
write.csv(TDM.titles, 'TDM_texts', row.names = TRUE)
