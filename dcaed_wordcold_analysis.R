library(httr)
library(jsonlite)
library(tm)
library(jiebaRD)
library(jiebaR)
library(dplyr)
library(ggplot2)
library(wordcloud)
options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")
dcontent<-function(dcardurl,id){
  myurl<- paste0(dcardurl,id,collapse="")
  resdata<- jsonlite::fromJSON(httr::content(GET(myurl),"text",encoding = "UTF-8"))
  toString(resdata$content)
}
dcomment<-function(dcardurl,id){
  myurl<- paste0(dcardurl,id,'/comments?limit=50',collapse="")
  json<-httr::content(GET(myurl), "text",encoding = "UTF-8")
  resdata<- jsonlite::fromJSON(json)
  toString(resdata$content)
}
dseg<- function(str,term) {
  str = gsub("[A-Za-z0-9]", "", str)
  seg = cutter[str]
  seg = seg[names(seg)==term]
  id = which(nchar(seg) > 1)
  result = seg[id]
}
dcardurl <- 'https://www.dcard.tw/_api/forums/trending/'
myurl <- paste0(dcardurl,'posts?popular=false&limit=500&before=23170601')
resdata<- fromJSON(httr::content(GET(myurl), 'text'))

content<-unlist(lapply(resdata$id,dcontent,dcardurl='https://www.dcard.tw/_api/posts/'))
comment<-unlist(lapply(resdata$id,dcomment,dcardurl='https://www.dcard.tw/_api/posts/'))

Dcard<-data.frame(resdata$id,resdata$title,resdata$createdAt,
                  resdata$updatedAt,resdata$department,
                  resdata$commentCount,resdata$likeCount,
                  resdata$gender,content,comment)
head(Dcard$content)
cutter <- worker('tag',write = "NOFILE")
new_user_word(cutter,'中天','n')
new_user_word(cutter,'總統','n')
new_user_word(cutter,'韓粉','n')
new_user_word(cutter,'民主','n')
new_user_word(cutter,'自由','n')
new_user_word(cutter,'選舉','n')
new_user_word(cutter,'大選','n')
new_user_word(cutter,'香港','n')
new_user_word(cutter,'台灣','n')
new_user_word(cutter,'中國','n')
new_user_word(cutter,'韓導','n')
new_user_word(cutter,'蔡英文','n')
new_user_word(cutter,'韓國瑜','n')
new_user_word(cutter,'柯文哲','n')
new_user_word(cutter,'民進黨','n')
new_user_word(cutter,'國民黨','n')
new_user_word(cutter,'反送中','n')
new_user_word(cutter,'一國兩制','n')
txt1<-unlist(lapply(Dcard$content[1:100],dseg,'n'))
st1<-freq(txt1)
st1<-st1[order(st1$freq,decreasing = TRUE),]
wordcloud(words=st1$char,freq=st1$freq,scale = c(3,0.1), 
          random.order = F,
          ordered.colors = F,
          rot.per = F,
          min.freq = 5,
          colors = brewer.pal(8,"Dark2")
)
