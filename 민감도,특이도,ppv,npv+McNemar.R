#Read the data
library(geepack)
library(doBy)
library(readxl)
data <- read_excel("C:/Users/ysbios/Desktop/radiology/GI/data.xlsx",sheet = "intergrated",
                   col_types = c("text","text", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric"))

#Rename the columns
name<-c("gold_standard","file_num","patient_num","size","ml_c","ml_h","ml_m","lim_c","lim_h","lim_m","lee_c","lee_h","lee_m")
colnames(data)<-name

View(data)
data <- data[-1,]
data
#in order to compare two groups (benign vs metastasis)
#merge the score into one column
lim_ch<-data$lim_c+data$lim_h
lee_ch<-data$lee_c+data$lee_h
ml_ch<-data$ml_c+data$ml_h
data<-cbind(data,lim_ch,lee_ch,ml_ch)
View(data)

data<-data[,-c(5,6,8,9,11,12)]

#convert the diagnostic result as a one - hot encoding
#if it is over the threshold value(0.5 or 5), consider it as 1.
ml_dig_b<-ifelse(data$ml_ch>=0.5,1,0)
lim_dig_b<-ifelse(data$lim_ch>=5,1,0)
lee_dig_b<-ifelse(data$lee_ch>=5,1,0)
data$gold_standard<-ifelse(data$gold_standard=="c"|data$gold_standard=="h","b","m")
ref<-ifelse(data$gold_standard=="b",1,0)
data<-cbind(data,ml_dig_b,lim_dig_b,lee_dig_b,ref)
matched_ml<-ifelse(data$ml_dig_b==data$ref,1,0)
matched_lim<-ifelse(data$lim_dig_b==data$ref,1,0)
matched_lee<-ifelse(data$lee_dig_b==data$ref,1,0)
data<-cbind(data,matched_ml,matched_lim,matched_lee)


#reconstruct the data from column to row.

data_ml<-data[,c(1,2,3,4,11,14,15)]
col<-c("gold_standard","filenum","ID","size","result","ref","matched")
colnames(data_ml)<-col

data_lim<-data[,c(1,2,3,4,12,14,16)]
colnames(data_lim)<-col

data_lee<-data[,c(1,2,3,4,13,14,17)]
colnames(data_lee)<-col

data_reconstruct<-rbind(data_ml,data_lim,data_lee)
View(data_reconstruct)

test<-c(rep("machine-learning",nrow(data)),rep("prof.lim",nrow(data)),rep("prof.lee",nrow(data)))
data_reconstruct$test<-test

setwd("c:/Users/ysbios/Desktop/GI")
write.csv(data_reconstruct,"data_reconstructed.csv")


#fitting logistic regression for sensitivity and specificity

sen<-geeglm(result~factor(test),data=data_reconstruct[data_reconstruct$ref==1,],family="binomial",id=ID,corstr = "independence")
summary(sen)

sen
spc<-geeglm(result==0~factor(test),data=data_reconstruct[data_reconstruct$ref==0,],family = "binomial",id=ID,corstr="independence")
summary(spc)

#liear function for beta
cal<-rbind(c(1,0,0),c(1,1,0),c(1,0,1))

#making result table
table<-matrix(NA,nrow=4 ,ncol=5) 
rownames(table)<-c("machine_learning","prof.lim","prof.lee","p-value")
colnames(table)<-c("Sensitivity","Specificity","PPV","NPV","Accuracy")

#calculating sensitivity and specificity with the coefficient from above.
es<-esticon(sen,cal)
sen_est<-(exp(es$Estimate)/(1+exp(es$Estimate)))
sen_low<-(exp(es$Lower)/(1+exp(es$Lower)))
sen_upp<-(exp(es$Upper)/(1+exp(es$Upper)))

formula=paste(round(sen_est*100,2)," (",round(sen_low*100,2),", ",round(sen_upp*100,2),")",sep = "")
table[1:3,1]<-formula 
table[4,1]<-round(anova(sen)[[3]],4) 
table

es<-esticon(spc,cal)
spc_est<-(exp(es$Estimate)/(1+exp(es$Estimate)))
spc_low<-(exp(es$Lower)/(1+exp(es$Lower)))
spc_upp<-(exp(es$Upper)/(1+exp(es$Upper)))

formula=paste(round(spc_est*100,2)," (",round(spc_low*100,2),", ",round(spc_upp*100,2),")",sep = "")
table[1:3,2]<-formula 
table[4,2]<-round(anova(spc)[[3]],3) 
table


#calculating ppv,npv 

ppv<-geeglm(ref~factor(test),data=data_reconstruct[data_reconstruct$result==1,],family = "binomial",id=ID,corstr="independence")

es<-esticon(ppv,cal)
ppv_est<-(exp(es$Estimate)/(1+exp(es$Estimate)))
ppv_low<-(exp(es$Lower)/(1+exp(es$Lower)))
ppv_upp<-(exp(es$Upper)/(1+exp(es$Upper)))
formula<-paste(round(ppv_est*100,2)," (",round(ppv_low*100,2),", ",round(ppv_upp*100,2),")",sep="")
table[1:3,3]<-formula
table[4,3]<-round(anova(ppv)[[3]],3)


npv<-geeglm(ref==0~factor(test),data=data_reconstruct[data_reconstruct$result==0,],family = "binomial",id=ID,corstr="independence")
es<-esticon(npv,cal)
npv_est<-(exp(es$Estimate)/(1+exp(es$Estimate)))
npv_low<-(exp(es$Lower)/(1+exp(es$Lower)))
npv_upp<-(exp(es$Upper)/(1+exp(es$Upper)))
formula<-paste(round(npv_est*100,2)," (",round(npv_low*100,2),", ",round(npv_upp*100,2),")",sep="")
table[1:3,4]<-formula
table[4,4]<-round(anova(npv)[[3]],3)

table

#calculating accuracy
acc<-geeglm(matched~factor(test),data=data_reconstruct,family = "binomial",id=ID,corstr = "independence")

es<-esticon(acc,cal)
acc_est<-(exp(es$Estimate)/(1+exp(es$Estimate)))
acc_low<-(exp(es$Lower)/(1+exp(es$Lower)))
acc_upp<-(exp(es$Upper)/(1+exp(es$Upper)))

formula<-paste(round(acc_est*100,2)," (",round(acc_low*100,2),", ",round(acc_upp*100,2),")",sep="")
table[1:3,5]<-formula
table[4,5]<-round(anova(acc)[[3]],4)

table

#pair-wise comparison
#sensitivity comparison ml-lim , ml-lee, lim-lee
Lmat<-rbind(c(0,1,0),c(0,0,1),c(0,1,-1))
sen_comp<-esticon(sen,Lmat)
spc_comp<-esticon(spc,Lmat)
ppv_comp<-esticon(ppv,Lmat)
npv_comp<-esticon(npv,Lmat)
acc_comp<-esticon(acc,Lmat)
sen_comp
spc_comp
ppv_comp
npv_comp
acc_comp
#McNemar's test
#mcnemar.test(x,y) x = either matrix or factor object to compare , y = factor object. if x is a matrix, it will be ignored.

datab<-data[data$gold_standard=="b",]
data
ml_lim<-mcnemar.test(as.factor(datab$ml_dig_b),as.factor(datab$lim_dig_b))
ml_lee<-mcnemar.test(as.factor(datab$ml_dig_b),as.factor(datab$lee_dig_b))
lim_lee<-mcnemar.test(as.factor(datab$lim_dig_b),as.factor(datab$lee_dig_b))

table_mcnemar_sen <- matrix(NA,ncol=3,nrow=2)
colnames(table_mcnemar_sen)<-c("ml vs prof.lim","ml vs prof.lee","prof.lee vs prof.lim")
rownames(table_mcnemar_sen)<-c("chi-squared statistic","P-value")

table_mcnemar_sen[1,1]<-ml_lim$statistic
table_mcnemar_sen[2,1]<-ml_lim$p.value

table_mcnemar_sen[1,2]<-ml_lee$statistic
table_mcnemar_sen[2,2]<-ml_lee$p.value

table_mcnemar_sen[1,3]<-lim_lee$statistic
table_mcnemar_sen[2,3]<-lim_lee$p.value
table_mcnemar


datam<-data[data$gold_standard=="m",]
ml_lim<-mcnemar.test(as.factor(datam$ml_dig_b==0),as.factor(datam$lim_dig_b==0))
ml_lee<-mcnemar.test(as.factor(datam$ml_dig_b==0),as.factor(datam$lee_dig_b==0))
lim_lee<-mcnemar.test(as.factor(datam$lim_dig_b==0),as.factor(datam$lee_dig_b==0))

table_mcnemar_spc <- matrix(NA,ncol=3,nrow=2)
colnames(table_mcnemar_spc)<-c("ml vs prof.lim","ml vs prof.lee","prof.lee vs prof.lim")
rownames(table_mcnemar_spc)<-c("chi-squared statistic","P-value")

table_mcnemar_spc[1,1]<-ml_lim$statistic
table_mcnemar_spc[2,1]<-ml_lim$p.value

table_mcnemar_spc[1,2]<-ml_lee$statistic
table_mcnemar_spc[2,2]<-ml_lee$p.value

table_mcnemar_spc[1,3]<-lim_lee$statistic
table_mcnemar_spc[2,3]<-lim_lee$p.value
table_mcnemar_spc
??esticon
mc
