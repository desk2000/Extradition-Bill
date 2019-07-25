#讀檔
library(readxl)

titleData<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//TDM_titles.csv")
txtData<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//TDM_texts.csv")

library(data.table)
titleData1<-setDT(titleData, key='d')[.("香港")]
titleData2<-setDT(titleData, key='d')[.("反送中")]
titleData3<-setDT(titleData, key='d')[.("逃犯條例")]
titleData4<-setDT(titleData, key='d')[.("共產黨")]
titleData5<-setDT(titleData, key='d')[.("遊行")]
titleData6<-setDT(titleData, key='d')[.("中共")]
titleData7<-setDT(titleData, key='d')[.("元朗")]
titleData8<-setDT(titleData, key='d')[.("立法會")]
titleData9<-setDT(txtData, key='d')[.("阿翔")]
titleData10<-setDT(txtData, key='d')[.("韓國瑜")]
titleData11<-setDT(txtData, key='d')[.("發大財")]
#titleDataFinal<-rbind(titleData1,titleData2,titleData3,titleData4,titleData5,titleData6,titleData7,titleData8)
#write.csv(titleDataFinal, file = "titleDataFinal.csv")
titleDataFinal2<-rbind(titleData1,titleData9,titleData10,titleData11)
write.csv(titleDataFinal2,file="titleDataFinalT.csv")

txtData1<-setDT(txtData, key='d')[.("香港")]
txtData2<-setDT(txtData, key='d')[.("反送中")]
txtData3<-setDT(txtData, key='d')[.("逃犯條例")]
txtData4<-setDT(txtData, key='d')[.("共產黨")]
txtData5<-setDT(txtData, key='d')[.("遊行")]
txtData6<-setDT(txtData, key='d')[.("中共")]
txtData7<-setDT(txtData, key='d')[.("元朗")]
txtData8<-setDT(txtData, key='d')[.("立法會")]
txtData9<-setDT(txtData, key='d')[.("阿翔")]
txtData10<-setDT(txtData, key='d')[.("韓國瑜")]
txtData11<-setDT(txtData, key='d')[.("發大財")]


#txtDataFinal<-rbind(txtData1,txtData2,txtData3,txtData4,txtData5,txtData6,txtData7,txtData8)
#write.csv(txtDataFinal, file = "txtDataFinal.csv")
txtDataFinal2<-rbind(txtData1,txtData9,txtData10,txtData11)
write.csv(txtDataFinal2,file="txtDataFinalT.csv")

#讀檔
titleDataT<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//txtDataFinalT.csv",sep = ",", as.is = TRUE)

titleDataT$t <- factor(titleDataT$t, levels = c("香港","阿翔","韓國瑜","發大財"), ordered = TRUE )
titleDataT$date <- as.Date(titleDataT$date,format="%Y-%m-%d")

library(ggplot2)

Graph_txt_T<-ggplot(data=titleDataT,aes(x=date,y=qua,group=t))+
  geom_line(aes(color=t))+
  geom_point(aes(color=t))

Graph_plus_txt_T<-Graph_txt_T+scale_color_manual(values=c("#28004d","#ea0000", "#00db00","#ff8000"))+ scale_x_date(date_minor_breaks="3 day ")+scale_y_continuous(trans = 'sqrt') 
print(Graph_plus_txt_T)