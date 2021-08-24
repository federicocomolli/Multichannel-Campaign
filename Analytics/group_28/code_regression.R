#REGRESSION PART 
library(data.table)
library(readxl)
library(stats)
library(corrgram)
library(dummies)
library(data.table)
library(psych)
library(GPArotation)
library(MLmetrics)
library(openxlsx)
Dataset <- read_excel("data.xlsx")
Data <-Dataset[c(2, 16, 17, 18, 26, 37, 38, 39, 45, 82, 83, 84, 85, 81)]
Data_likert <- Dataset[c(72:79, 19:25, 46:48, 81)]
names(Data)[1]="Q2"
names(Data)[2]="Q4"
names(Data)[3]="Q5"
names(Data)[4]="Q6"
names(Data)[5]="Q8"
names(Data)[6]="Q10"
names(Data)[7]="Q11"
names(Data)[8]="Q12"
names(Data)[9]="Q14"
names(Data)[10]="Q21"
names(Data)[11]="Q22"
names(Data)[12]="Q23"
names(Data)[13]="Q24"
names(Data)[14]="success"
names(Data_likert)[1]="Q19_1"
names(Data_likert)[2]="Q19_2"
names(Data_likert)[3]="Q19_3"
names(Data_likert)[4]="Q19_4"
names(Data_likert)[5]="Q19_5"
names(Data_likert)[6]="Q19_6"
names(Data_likert)[7]="Q19_7"
names(Data_likert)[8]="Q19_8"
names(Data_likert)[9]="Q7_1"
names(Data_likert)[10]="Q7_2"
names(Data_likert)[11]="Q7_3"
names(Data_likert)[12]="Q7_4"
names(Data_likert)[13]="Q7_5"
names(Data_likert)[14]="Q7_6"
names(Data_likert)[15]="Q7_7"
names(Data_likert)[16]="Q15_1"
names(Data_likert)[17]="Q15_2"
names(Data_likert)[18]="Q15_3"
names(Data_likert)[19]="success"
Data <- as.data.frame(Data)
Data_likert <- as.data.frame(Data_likert)
Data$success[Data$success<=2]  <- 0
Data <- Data[Data$success!=3,]
Data$success[Data$success>=4]  <- 1
Data_likert$success[Data_likert$success<=2]  <- 0
Data_likert <- Data_likert[Data_likert$success!=3,]
Data_likert$success[Data_likert$success>=4]  <- 1
Data <- dummy.data.frame(Data, names = c("Q2"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q4"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q5"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q6"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q8"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q10"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q11"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q12"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q14"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q21"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q22"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q23"), sep="_")
Data <- dummy.data.frame(Data, names = c("Q24"), sep="_")
Data <- dummy.data.frame(Data, names = c("success"), sep="_")
Data_likert <- dummy.data.frame(Data_likert, names = c("success"), sep="_")
Data_likert <- cbind(scale(Data_likert[c(1:18)]), Data_likert[c(20)])
#corrgram(Data,lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)
#corrgram(Data_likert,lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

Dataset <-as.data.table(Dataset)
temp <- na.omit(Dataset, cols=c(72:79))
temp <- as.data.frame(temp)
data <-temp[c(73, 74, 77, 78, 23, 16, 17, 18, 26, 82, 83, 84, 85, 80, 81)]

names(data)[1]="Q19_2"
names(data)[2]="Q19_3"
names(data)[3]="Q19_6"
names(data)[4]="Q19_7"
names(data)[5]="Q7_5"
names(data)[6]="Q4"
names(data)[7]="Q5"
names(data)[8]="Q6"
names(data)[9]="Q8"
names(data)[10]="Q21"
names(data)[11]="Q22"
names(data)[12]="Q23"
names(data)[13]="Q24"
names(data)[14]="AB"
names(data)[15]="success"
data <- as.data.frame(data)
data$success[data$success==1]  <- 0
data$success[data$success==2]  <- 0
data$success[data$success==3]  <- 1
#data <- data[data$success!=3,]
data$success[data$success==4]  <- 1
data$success[data$success==5]  <- 1
#newdataA <- subset(newdata, newdata$AB=='A')
#newdataB <- subset(newdata, newdata$AB=='B')


data <- dummy.data.frame(data, names = c("Q4"), sep="_")
data <- dummy.data.frame(data, names = c("Q5"), sep="_")
data <- dummy.data.frame(data, names = c("Q6"), sep="_")
data <- dummy.data.frame(data, names = c("Q8"), sep="_")
data <- dummy.data.frame(data, names = c("Q21"), sep="_")
data <- dummy.data.frame(data, names = c("Q22"), sep="_")
data <- dummy.data.frame(data, names = c("Q23"), sep="_")
data <- dummy.data.frame(data, names = c("Q24"), sep="_")
data <- dummy.data.frame(data, names = c("success"), sep="_")
data <- data[-c(11)]
data <- data[c(1, 2, 3, 4, 5, 8, 12, 13, 14, 17, 22, 26, 32, 41, 48, 50, 52)]

dataA <- subset(data, data$AB=="Consider that the service costs &euro;0,15 per minute of use. How likely are you going to try the service?")
dataA <- dataA[c(1:15, 17)]
dataB <- subset(data, data$AB=="Consider that the service requires a monthly subscription of &euro;8, and costs&euro;0,05 per minute of use. How likely are you going to try the service?")
dataB <- dataB[c(1:15, 17)]

logisticA <- glm(success_1 ~ ., data = dataA)
summary(logisticA)
logisticB <- glm(success_1 ~ ., data = dataB)
summary(logisticB)

predBonA <- cbind(dataA[1:15],predict(logisticB, dataA[1:15], type="response"))
predAonB <- cbind(dataB[1:15],predict(logisticA, dataB[1:15], type="response"))
colnames(predBonA)[colnames(predBonA)=="predict(logisticB, dataA[1:15], type = \"response\")"] <- "target"
colnames(predAonB)[colnames(predAonB)=="predict(logisticA, dataB[1:15], type = \"response\")"] <- "target"

predBonA[16][predBonA[16] >=	0.46006997]  <- 1
predBonA[16][predBonA[16] <	0.46006997]  <- 0	
predAonB[16][predAonB[16] >=	0.43164078]  <- 1  	
predAonB[16][predAonB[16] <	0.43164078]  <- 0 
F1_Score(y_pred = predBonA$target, y_true = dataA$success_1, positive = "1")
F1_Score(y_pred = predAonB$target, y_true = dataB$success_1, positive = "1")

colnames(dataA)[colnames(dataA)=="success_1"] <- "target"
colnames(dataB)[colnames(dataB)=="success_1"] <- "target"

totA<-rbind(dataA,predAonB)
totB<-rbind(dataB,predBonA)
totA<-na.omit(totA,cols="target")
totB<-na.omit(totB,cols="target")
table(totA$target)
table(totB$target)