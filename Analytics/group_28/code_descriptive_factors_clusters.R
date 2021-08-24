# We need this libraries to perforn the analysis
library(data.table)
library(cluster)
library(psych)
library(GPArotation)
library(psych)
library(readxl)
library(rgl)
library(openxlsx)
library(dummies)
library(MLmetrics)
library(stats)
library(corrgram)

#DESCRIPTIVE ANALYSIS
data <- read_excel("data.xlsx")
table(data[82]) #male vs famale
table(data[2])#kind of trasportation
mean(data$Response...83,na.rm=TRUE)+15 #avarage age
mean(data$Facebook,na.rm=TRUE)
table(data[83])#who live in Milan and who are commuters
#avarage time in public trasportation
time<-c(10,20,45,60)
time_p<-table(data[26])
sum(time*time_p)/sum(time_p) #result
#avarage time in own car
time<-c(20,45,60)
time_c<-table(data[39])
sum(time*time_c)/sum(time_c) #result
table(data[17]) #frequency of travel with public trasportation
table(data[37])#frequency of travel with own car
table(data[45])#percentage of use of sharing mobility
#satisfaction about the 3 different kind of trasportation
mean(data[19],na.rm=TRUE)
mean(data$Availability...19,na.rm=TRUE)
mean(data$Punctuality,na.rm=TRUE)
mean(data$Speed,na.rm=TRUE)
mean(data$Comfort...22,na.rm=TRUE)
mean(data$Safety...23,na.rm=TRUE)
mean(data$Cost...24,na.rm=TRUE)
mean(data$Reliability...25,na.rm=TRUE)
# factor on Q25 to group the media channels
dataQ25 <- na.omit(data,cols=c(86:96))
data_factorQ25=dataQ25[c(86:96)]
fourfactorQ25<- fa(data_factorQ25, nfactors = 4, rotate = "varimax", max.iter = 500, fm = "minres")
print(fourfactorQ25, sort=TRUE)
factorQ25 <- factor.scores(x=data_factorQ25, f=fourfactorQ25)
dataQ25 <- cbind(dataQ25, factorQ25$scores)
# The other information that are in the ppt are been extracted with excel


#SEGMENTATION AND PROFILING
data <- as.data.table(data)
# FACTOR ANALYSIS FOR QUESTION 19
# in order to not be repetitive we have write here only the code to obtain the number of factors choosen
newdata<- na.omit(data,cols=c(72:79))
Q19_data<- newdata[,72:79]
ThreeFactor <- fa(Q19_data, nfactors = 3, rotate = "varimax")
print(ThreeFactor, sort=TRUE)
Q19_factors <- factor.scores(x=Q19_data, f=ThreeFactor)
Q19_data <- cbind(Q19_data, Q19_factors$scores)
colnames(Q19_data)[colnames(Q19_data)=="MR1"] <- "Funny"
colnames(Q19_data)[colnames(Q19_data)=="MR2"] <- "Safety"
colnames(Q19_data)[colnames(Q19_data)=="MR3"] <- "Envy"

# CLUSTER  ANALYSIS FOR QUESTION 19
# we have write only the code for the number of clusters choosen
cluster4_Q19 <- kmeans(Q19_data[,9:11], centers = 4, nstart = 100)
#3D plot of the clusters
plot3d(Q19_data[,9:11], col=cluster4_Q19$cluster, size=10)
Q19_data <- cbind(Q19_data, cluster4_Q19$cluster)
colnames(Q19_data)[colnames(Q19_data)=="V2"] <- "Cluster"
# ANOVA on the Cluster
Q19_data$Cluster<-factor(Q19_data$Cluster)
anova1 <- aov(Q19_data$Funny ~ Q19_data$Cluster)#per tutti
summary(anova1)
TukeyHSD(anova1)
anova2 <- aov(Q19_data$Safety ~ Q19_data$Cluster)#per tutti
summary(anova2)
TukeyHSD(anova2)
anova3 <- aov(Q19_data$Envy ~ Q19_data$Cluster)#per tutti
summary(anova3)
TukeyHSD(anova3)
#Export xlsx
data_cluster=cbind(newdata,Q19_data$Funny,Q19_data$Safety,Q19_data$Envy,Q19_data[,12:12])
colnames(data_cluster)[colnames(data_cluster)=="V2"] <- "Funny"
colnames(data_cluster)[colnames(data_cluster)=="V3"] <- "Safety"
colnames(data_cluster)[colnames(data_cluster)=="V4"] <- "Envy"
cluster1<-subset(data_cluster,data_cluster$Cluster==1,drop=FALSE)
cluster2<-subset(data_cluster,data_cluster$Cluster==2,drop=FALSE)
cluster3<-subset(data_cluster,data_cluster$Cluster==3,drop=FALSE)
cluster4<-subset(data_cluster,data_cluster$Cluster==4,drop=FALSE)
#write.xlsx(data_cluster, "C:/Users/posta/Desktop/datacluster.xlsx")

# PROFILATION:
# per every cluster
mean(cluster1$Response...83,na.rm=TRUE)+15
mean(cluster1$'Bike sharing (BikeMi, Mobile, etc.)',na.rm=TRUE)
mean(cluster1$'Car sharing (Enjoy, Car2Go, DriveNow, etc.)',na.rm=TRUE)
mean(cluster1$'Motorbike sharing (MiMoto, Cityscoot, etc.)',na.rm=TRUE)
mean(cluster1$Response...81,na.rm=TRUE)
mean(cluster1$Availability...19,na.rm=TRUE)
mean(cluster1$Punctuality,na.rm=TRUE)
mean(cluster1$Speed,na.rm=TRUE)
mean(cluster1$Comfort...22,na.rm=TRUE)
mean(cluster1$Safety...23,na.rm=TRUE)
mean(cluster1$Cost...24,na.rm=TRUE)
mean(cluster1$Reliability...25,na.rm=TRUE)


#AB test Q20
ABtest1<-subset(data,data$`Question Viewed`=="Consider that the service costs &euro;0,15 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABtest1<-ABtest1$Response...81
mean(ABtest1, na.rm=TRUE)
ABtest2<-subset(data,data$`Question Viewed`=="Consider that the service requires a monthly subscription of &euro;8, and costs&euro;0,05 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABtest2<-ABtest2$Response...81
mean(ABtest2, na.rm=TRUE)
#First we have split the data set to make AB per every clusters
ABQ1<-subset(cluster1,cluster1$`Question Viewed`=="Consider that the service costs &euro;0,15 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABQ2<-subset(cluster2,cluster2$`Question Viewed`=="Consider that the service costs &euro;0,15 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABQ3<-subset(cluster3,cluster3$`Question Viewed`=="Consider that the service costs &euro;0,15 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABQ4<-subset(cluster4,cluster4$`Question Viewed`=="Consider that the service costs &euro;0,15 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABQ1<-ABQ1$Response...81
ABQ2<-ABQ2$Response...81
ABQ3<-ABQ3$Response...81
ABQ4<-ABQ4$Response...81
ABQ1_1<-subset(cluster1,cluster1$`Question Viewed`=="Consider that the service requires a monthly subscription of &euro;8, and costs&euro;0,05 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABQ2_1<-subset(cluster2,cluster2$`Question Viewed`=="Consider that the service requires a monthly subscription of &euro;8, and costs&euro;0,05 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABQ3_1<-subset(cluster3,cluster3$`Question Viewed`=="Consider that the service requires a monthly subscription of &euro;8, and costs&euro;0,05 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABQ4_1<-subset(cluster4,cluster4$`Question Viewed`=="Consider that the service requires a monthly subscription of &euro;8, and costs&euro;0,05 per minute of use. How likely are you going to try the service?",drop=FALSE)
ABQ1_1<-ABQ1_1$Response...81
ABQ2_1<-ABQ2_1$Response...81
ABQ3_1<-ABQ3_1$Response...81
ABQ4_1<-ABQ4_1$Response...81
# Then we have compute the results
mean(ABQ1, na.rm=TRUE)
mean(ABQ2, na.rm=TRUE)
mean(ABQ3, na.rm=TRUE)
mean(ABQ4, na.rm=TRUE)
mean(ABQ1_1, na.rm=TRUE)
mean(ABQ2_1, na.rm=TRUE)
mean(ABQ3_1, na.rm=TRUE)
mean(ABQ4_1, na.rm=TRUE)




