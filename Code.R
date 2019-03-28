data=read.csv("ckddata.csv")
data=data[,-1]  ## remove ID
out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status
data_in=na.omit(data_in)

# Switch the continuous numeric variables into categorical variables
for (n in 1:nrow(data_in)) {
  if (data_in[n, 'Age']<=10) data_in[n, 'Age_Level']<-"One"
  else {if (data_in[n, 'Age']<=20) data_in[n, 'Age_Level']<-"Two"
  else {if (data_in[n, 'Age']<=30) data_in[n, 'Age_Level']<-"Three"
  else {if (data_in[n, 'Age']<=40) data_in[n, 'Age_Level']<-"Four"
  else {if (data_in[n, 'Age']<=50) data_in[n, 'Age_Level']<-"Five"
  else {if (data_in[n, 'Age']<=60) data_in[n, 'Age_Level']<-"Six"
  else {if (data_in[n, 'Age']<=70) data_in[n, 'Age_Level']<-"Seven"
  else {if (data_in[n, 'Age']<=80) data_in[n, 'Age_Level']<-"Eight"
  else {if (data_in[n, 'Age']<=90) data_in[n, 'Age_Level']<-"Nine"
  else {data_in[n, 'Age_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_in)) {
  if (data_in[n, 'Weight']<=20) data_in[n, 'Weight_Level']<-"One"
  else {if (data_in[n, 'Weight']<=40) data_in[n, 'Weight_Level']<-"Two"
  else {if (data_in[n, 'Weight']<=60) data_in[n, 'Weight_Level']<-"Three"
  else {if (data_in[n, 'Weight']<=80) data_in[n, 'Weight_Level']<-"Four"
  else {if (data_in[n, 'Weight']<=100) data_in[n, 'Weight_Level']<-"Five"
  else {if (data_in[n, 'Weight']<=120) data_in[n, 'Weight_Level']<-"Six"
  else {if (data_in[n, 'Weight']<=140) data_in[n, 'Weight_Level']<-"Seven"
  else {if (data_in[n, 'Weight']<=160) data_in[n, 'Weight_Level']<-"Eight"
  else {if (data_in[n, 'Weight']<=180) data_in[n, 'Weight_Level']<-"Nine"
  else {data_in[n, 'Weight_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_in)) {
  if (data_in[n, 'Height']<=110) data_in[n, 'Height_Level']<-"One"
  else {if (data_in[n, 'Height']<=120) data_in[n, 'Height_Level']<-"Two"
  else {if (data_in[n, 'Height']<=130) data_in[n, 'Height_Level']<-"Three"
  else {if (data_in[n, 'Height']<=140) data_in[n, 'Height_Level']<-"Four"
  else {if (data_in[n, 'Height']<=150) data_in[n, 'Height_Level']<-"Five"
  else {if (data_in[n, 'Height']<=160) data_in[n, 'Height_Level']<-"Six"
  else {if (data_in[n, 'Height']<=170) data_in[n, 'Height_Level']<-"Seven"
  else {if (data_in[n, 'Height']<=180) data_in[n, 'Height_Level']<-"Eight"
  else {if (data_in[n, 'Height']<=190) data_in[n, 'Height_Level']<-"Nine"
  else {data_in[n, 'Height_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_in)) {
  if (data_in[n, 'BMI']<18.5) data_in[n, 'BMI_Level']<-"Underweight"
  else {if (data_in[n, 'BMI']<=24.9) data_in[n, 'BMI_Level']<-"Healthy"
  else {if (data_in[n, 'BMI']<=29.9) data_in[n, 'BMI_Level']<-"Overweight"
  else {data_in[n, 'BMI_Level']<-"Obese"
  }}}}

for (n in 1:nrow(data_in)) {
  if (data_in[n, 'Waist']<=60) data_in[n, 'Waist_Level']<-"One"
  else {if (data_in[n, 'Waist']<=70) data_in[n, 'Waist_Level']<-"Two"
  else {if (data_in[n, 'Waist']<=80) data_in[n, 'Waist_Level']<-"Three"
  else {if (data_in[n, 'Waist']<=90) data_in[n, 'Waist_Level']<-"Four"
  else {if (data_in[n, 'Waist']<=100) data_in[n, 'Waist_Level']<-"Five"
  else {if (data_in[n, 'Waist']<=110) data_in[n, 'Waist_Level']<-"Six"
  else {if (data_in[n, 'Waist']<=120) data_in[n, 'Waist_Level']<-"Seven"
  else {if (data_in[n, 'Waist']<=130) data_in[n, 'Waist_Level']<-"Eight"
  else {if (data_in[n, 'Waist']<=140) data_in[n, 'Waist_Level']<-"Nine"
  else {data_in[n, 'Waist_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_in)) {
  if (data_in[n, 'SBP']<=50) data_in[n, 'SBP_Level']<-"One"
  else {if (data_in[n, 'SBP']<=75) data_in[n, 'SBP_Level']<-"Two"
  else {if (data_in[n, 'SBP']<=100) data_in[n, 'SBP_Level']<-"Three"
  else {if (data_in[n, 'SBP']<=125) data_in[n, 'SBP_Level']<-"Four"
  else {if (data_in[n, 'SBP']<=150) data_in[n, 'SBP_Level']<-"Five"
  else {if (data_in[n, 'SBP']<=175) data_in[n, 'SBP_Level']<-"Six"
  else {if (data_in[n, 'SBP']<=200) data_in[n, 'SBP_Level']<-"Seven"
  else {if (data_in[n, 'SBP']<=225) data_in[n, 'SBP_Level']<-"Eight"
  else {data_in[n, 'SBP_Level']<-"Nine"
  }}}}}}}}}

for (n in 1:nrow(data_in)) {
  if (data_in[n, 'DBP']<=20) data_in[n, 'DBP_Level']<-"One"
  else {if (data_in[n, 'DBP']<=30) data_in[n, 'DBP_Level']<-"Two"
  else {if (data_in[n, 'DBP']<=40) data_in[n, 'DBP_Level']<-"Three"
  else {if (data_in[n, 'DBP']<=50) data_in[n, 'DBP_Level']<-"Four"
  else {if (data_in[n, 'DBP']<=60) data_in[n, 'DBP_Level']<-"Five"
  else {if (data_in[n, 'DBP']<=70) data_in[n, 'DBP_Level']<-"Six"
  else {if (data_in[n, 'DBP']<=80) data_in[n, 'DBP_Level']<-"Seven"
  else {if (data_in[n, 'DBP']<=90) data_in[n, 'DBP_Level']<-"Eight"
  else {if (data_in[n, 'DBP']<=100) data_in[n, 'DBP_Level']<-"Nine"
  else {data_in[n, 'DBP_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_in)) {
  if (data_in[n, 'HDL']<=20) data_in[n, 'HDL_Level']<-"One"
  else {if (data_in[n, 'HDL']<=30) data_in[n, 'HDL_Level']<-"Two"
  else {if (data_in[n, 'HDL']<=40) data_in[n, 'HDL_Level']<-"Three"
  else {if (data_in[n, 'HDL']<=50) data_in[n, 'HDL_Level']<-"Four"
  else {if (data_in[n, 'HDL']<=60) data_in[n, 'HDL_Level']<-"Five"
  else {if (data_in[n, 'HDL']<=70) data_in[n, 'HDL_Level']<-"Six"
  else {if (data_in[n, 'HDL']<=80) data_in[n, 'HDL_Level']<-"Seven"
  else {if (data_in[n, 'HDL']<=90) data_in[n, 'HDL_Level']<-"Eight"
  else {if (data_in[n, 'HDL']<=100) data_in[n, 'HDL_Level']<-"Nine"
  else {data_in[n, 'HDL_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_in)) {
  if (data_in[n, 'LDL']<=50) data_in[n, 'LDL_Level']<-"One"
  else {if (data_in[n, 'LDL']<=100) data_in[n, 'LDL_Level']<-"Two"
  else {if (data_in[n, 'LDL']<=125) data_in[n, 'LDL_Level']<-"Three"
  else {if (data_in[n, 'LDL']<=150) data_in[n, 'LDL_Level']<-"Four"
  else {if (data_in[n, 'LDL']<=175) data_in[n, 'LDL_Level']<-"Five"
  else {if (data_in[n, 'LDL']<=200) data_in[n, 'LDL_Level']<-"Six"
  else {if (data_in[n, 'LDL']<=300) data_in[n, 'LDL_Level']<-"Seven"
  else {if (data_in[n, 'LDL']<=500) data_in[n, 'LDL_Level']<-"Eight"
  else {data_in[n, 'LDL_Level']<-"Nine"
  }}}}}}}}}

for (n in 1:nrow(data_in)) {
  if (data_in[n, 'Activity']==1) data_in[n, 'Activity_Level']<-"One"
  else {if (data_in[n, 'Activity']==2) data_in[n, 'Activity_Level']<-"Two"
  else {if (data_in[n, 'Activity']<=3) data_in[n, 'Activity_Level']<-"Three"
  else {if (data_in[n, 'Activity']<=4) data_in[n, 'Activity_Level']<-"Four"
  }}}}

# Remove the orginal continuous numeric variables
data_in <- data_in[, -c(1, 9, 10, 11, 13, 14, 15, 16, 17, 18, 21)]

# Build a model based on 2/3 of the "data_in" data, and test the model based in the rest 1/3 of data
s <- sample(4136, 2757)
data_in_train <- data_in[s,]
data_in_test <- data_in[-s,]

model_partofdata <- glm(CKD~.,family="binomial",data=data_in_train)
summary(model)

model3_partofdata <- step(model_partofdata,direction="backward")
formula(model3_partofdata)
summary(model3_partofdata)

phat_test <- predict(model3_partofdata, newdata = data_in_test, type = "response")
classify_test <- ifelse(phat_test>.5,1,0)
table(classify_test, data_in_test[, 'CKD'])
mean(classify_test==data_in_test[, 'CKD'])

# Build a model based on all the data from "data_in"
model_allofdata <- glm(CKD~.,family="binomial",data=data_in)
summary(model_allofdata)

model3_allofdata <- step(model_allofdata,direction="backward")
formula(model3_allofdata)
summary(model3_allofdata)

# Process the "data_out" dataset
out_sample2 <- which(is.na(data_out$Age)==1)
data_out <- data_out[-out_sample2,]
CKD <- data_out$CKD
data_out_withoutCKD <- data_out[, -33]

# Replace the missing date in the "data_out" dataset based on KNN methodology
install.packages("DMwR")
library(DMwR)
require(imput)
data_out_withoutCKD <- data_out_withoutCKD[order(data_out_withoutCKD$Age),] 
head(data_out_withoutCKD)
knnOutput <- knnImputation(data_out_withoutCKD, k = 7, meth = "median")
#knnOutput <- knnImputation(data_in[,names(data_in)]) 
anyNA(knnOutput)

#round(knnOutput$Educ)
#round(knnOutput$Unmarried)
#round(knnOutput$Income)
#round(knnOutput$Insured)
#round(knnOutput$Obese)
#round(knnOutput$Activity)
#round(knnOutput$PoorVision)
#round(knnOutput$Hypertension)
#round(knnOutput$Diabetes)
#round(knnOutput$Stroke)
#round(knnOutput$CVD)
#round(knnOutput$Fam.CVD)
#round(knnOutput$CHF)	
#round(knnOutput$Anemia)

data_out_withoutCKD <- knnOutput
data_out_withoutCKD <- data_out_withoutCKD[order(row.names(data_out_withoutCKD)), ]
data_out <- data.frame(data_out_withoutCKD, CKD)

# Switch the continuous numeric variables in the "data_out" dataset into categorical variables
for (n in 1:nrow(data_out)) {
  if (data_out[n, 'Age']<=10) data_out[n, 'Age_Level']<-"One"
  else {if (data_out[n, 'Age']<=20) data_out[n, 'Age_Level']<-"Two"
  else {if (data_out[n, 'Age']<=30) data_out[n, 'Age_Level']<-"Three"
  else {if (data_out[n, 'Age']<=40) data_out[n, 'Age_Level']<-"Four"
  else {if (data_out[n, 'Age']<=50) data_out[n, 'Age_Level']<-"Five"
  else {if (data_out[n, 'Age']<=60) data_out[n, 'Age_Level']<-"Six"
  else {if (data_out[n, 'Age']<=70) data_out[n, 'Age_Level']<-"Seven"
  else {if (data_out[n, 'Age']<=80) data_out[n, 'Age_Level']<-"Eight"
  else {if (data_out[n, 'Age']<=90) data_out[n, 'Age_Level']<-"Nine"
  else {data_out[n, 'Age_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_out)) {
  if (data_out[n, 'Weight']<=20) data_out[n, 'Weight_Level']<-"One"
  else {if (data_out[n, 'Weight']<=40) data_out[n, 'Weight_Level']<-"Two"
  else {if (data_out[n, 'Weight']<=60) data_out[n, 'Weight_Level']<-"Three"
  else {if (data_out[n, 'Weight']<=80) data_out[n, 'Weight_Level']<-"Four"
  else {if (data_out[n, 'Weight']<=100) data_out[n, 'Weight_Level']<-"Five"
  else {if (data_out[n, 'Weight']<=120) data_out[n, 'Weight_Level']<-"Six"
  else {if (data_out[n, 'Weight']<=140) data_out[n, 'Weight_Level']<-"Seven"
  else {if (data_out[n, 'Weight']<=160) data_out[n, 'Weight_Level']<-"Eight"
  else {if (data_out[n, 'Weight']<=180) data_out[n, 'Weight_Level']<-"Nine"
  else {data_out[n, 'Weight_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_out)) {
  if (data_out[n, 'Height']<=110) data_out[n, 'Height_Level']<-"One"
  else {if (data_out[n, 'Height']<=120) data_out[n, 'Height_Level']<-"Two"
  else {if (data_out[n, 'Height']<=130) data_out[n, 'Height_Level']<-"Three"
  else {if (data_out[n, 'Height']<=140) data_out[n, 'Height_Level']<-"Four"
  else {if (data_out[n, 'Height']<=150) data_out[n, 'Height_Level']<-"Five"
  else {if (data_out[n, 'Height']<=160) data_out[n, 'Height_Level']<-"Six"
  else {if (data_out[n, 'Height']<=170) data_out[n, 'Height_Level']<-"Seven"
  else {if (data_out[n, 'Height']<=180) data_out[n, 'Height_Level']<-"Eight"
  else {if (data_out[n, 'Height']<=190) data_out[n, 'Height_Level']<-"Nine"
  else {data_out[n, 'Height_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_out)) {
  if (data_out[n, 'BMI']<18.5) data_out[n, 'BMI_Level']<-"Underweight"
  else {if (data_out[n, 'BMI']<=24.9) data_out[n, 'BMI_Level']<-"Healthy"
  else {if (data_out[n, 'BMI']<=29.9) data_out[n, 'BMI_Level']<-"Overweight"
  else {data_out[n, 'BMI_Level']<-"Obese"
  }}}}

for (n in 1:nrow(data_out)) {
  if (data_out[n, 'Waist']<=60) data_out[n, 'Waist_Level']<-"One"
  else {if (data_out[n, 'Waist']<=70) data_out[n, 'Waist_Level']<-"Two"
  else {if (data_out[n, 'Waist']<=80) data_out[n, 'Waist_Level']<-"Three"
  else {if (data_out[n, 'Waist']<=90) data_out[n, 'Waist_Level']<-"Four"
  else {if (data_out[n, 'Waist']<=100) data_out[n, 'Waist_Level']<-"Five"
  else {if (data_out[n, 'Waist']<=110) data_out[n, 'Waist_Level']<-"Six"
  else {if (data_out[n, 'Waist']<=120) data_out[n, 'Waist_Level']<-"Seven"
  else {if (data_out[n, 'Waist']<=130) data_out[n, 'Waist_Level']<-"Eight"
  else {if (data_out[n, 'Waist']<=140) data_out[n, 'Waist_Level']<-"Nine"
  else {data_out[n, 'Waist_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_out)) {
  if (data_out[n, 'SBP']<=50) data_out[n, 'SBP_Level']<-"One"
  else {if (data_out[n, 'SBP']<=75) data_out[n, 'SBP_Level']<-"Two"
  else {if (data_out[n, 'SBP']<=100) data_out[n, 'SBP_Level']<-"Three"
  else {if (data_out[n, 'SBP']<=125) data_out[n, 'SBP_Level']<-"Four"
  else {if (data_out[n, 'SBP']<=150) data_out[n, 'SBP_Level']<-"Five"
  else {if (data_out[n, 'SBP']<=175) data_out[n, 'SBP_Level']<-"Six"
  else {if (data_out[n, 'SBP']<=200) data_out[n, 'SBP_Level']<-"Seven"
  else {if (data_out[n, 'SBP']<=225) data_out[n, 'SBP_Level']<-"Eight"
  else {data_out[n, 'SBP_Level']<-"Nine"
  }}}}}}}}}

for (n in 1:nrow(data_out)) {
  if (data_out[n, 'DBP']<=20) data_out[n, 'DBP_Level']<-"One"
  else {if (data_out[n, 'DBP']<=30) data_out[n, 'DBP_Level']<-"Two"
  else {if (data_out[n, 'DBP']<=40) data_out[n, 'DBP_Level']<-"Three"
  else {if (data_out[n, 'DBP']<=50) data_out[n, 'DBP_Level']<-"Four"
  else {if (data_out[n, 'DBP']<=60) data_out[n, 'DBP_Level']<-"Five"
  else {if (data_out[n, 'DBP']<=70) data_out[n, 'DBP_Level']<-"Six"
  else {if (data_out[n, 'DBP']<=80) data_out[n, 'DBP_Level']<-"Seven"
  else {if (data_out[n, 'DBP']<=90) data_out[n, 'DBP_Level']<-"Eight"
  else {if (data_out[n, 'DBP']<=100) data_out[n, 'DBP_Level']<-"Nine"
  else {data_out[n, 'DBP_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_out)) {
  if (data_out[n, 'HDL']<=20) data_out[n, 'HDL_Level']<-"One"
  else {if (data_out[n, 'HDL']<=30) data_out[n, 'HDL_Level']<-"Two"
  else {if (data_out[n, 'HDL']<=40) data_out[n, 'HDL_Level']<-"Three"
  else {if (data_out[n, 'HDL']<=50) data_out[n, 'HDL_Level']<-"Four"
  else {if (data_out[n, 'HDL']<=60) data_out[n, 'HDL_Level']<-"Five"
  else {if (data_out[n, 'HDL']<=70) data_out[n, 'HDL_Level']<-"Six"
  else {if (data_out[n, 'HDL']<=80) data_out[n, 'HDL_Level']<-"Seven"
  else {if (data_out[n, 'HDL']<=90) data_out[n, 'HDL_Level']<-"Eight"
  else {if (data_out[n, 'HDL']<=100) data_out[n, 'HDL_Level']<-"Nine"
  else {data_out[n, 'HDL_Level']<-"Ten"
  }}}}}}}}}}

for (n in 1:nrow(data_out)) {
  if (data_out[n, 'LDL']<=50) data_out[n, 'LDL_Level']<-"One"
  else {if (data_out[n, 'LDL']<=100) data_out[n, 'LDL_Level']<-"Two"
  else {if (data_out[n, 'LDL']<=125) data_out[n, 'LDL_Level']<-"Three"
  else {if (data_out[n, 'LDL']<=150) data_out[n, 'LDL_Level']<-"Four"
  else {if (data_out[n, 'LDL']<=175) data_out[n, 'LDL_Level']<-"Five"
  else {if (data_out[n, 'LDL']<=200) data_out[n, 'LDL_Level']<-"Six"
  else {if (data_out[n, 'LDL']<=300) data_out[n, 'LDL_Level']<-"Seven"
  else {if (data_out[n, 'LDL']<=500) data_out[n, 'LDL_Level']<-"Eight"
  else {data_out[n, 'LDL_Level']<-"Nine"
  }}}}}}}}}

for (n in 1:nrow(data_out)) {
  if (data_out[n, 'Activity']==1) data_out[n, 'Activity_Level']<-"One"
  else {if (data_out[n, 'Activity']==2) data_out[n, 'Activity_Level']<-"Two"
  else {if (data_out[n, 'Activity']<=3) data_out[n, 'Activity_Level']<-"Three"
  else {if (data_out[n, 'Activity']<=4) data_out[n, 'Activity_Level']<-"Four"
  }}}}

data_out <- data_out[, -c(1, 9, 10, 11, 13, 14, 15, 16, 17, 18, 21)]

# Predict probability for each patients in "data_out" dataset
Probability <- predict(model3_allofdata, newdata = data_out, type = "response")

Classification <- ifelse(Probability>.5,1,0)

# Calculate the total points based on the screening tool for each patients in the "data_out" dataset
i <- nrow(data_out)
Q1 <- vector(length=i)
Q2 <- vector(length=i)
Q3 <- vector(length=i)
Q4 <- vector(length=i)
Q5 <- vector(length=i)
Q6 <- vector(length=i)
Q7 <- vector(length=i)
Q8 <- vector(length=i)
Q9 <- vector(length=i)
Q10 <- vector(length=i)
Score <- vector(length=i)

for (n in 1:i) {
  if (data_out[n, 'Female']==1) Q1[n]<-3.08 else Q1[n]<-0}

for (n in 1:i) {
  if (data_out[n, 'Age_Level']=="One") Q2[n]<--157.29 
  else {if (data_out[n, 'Age_Level']=="Two") Q2[n]<--157.29
  else {if (data_out[n, 'Age_Level']=="Three") Q2[n]<--47.71
  else {if (data_out[n, 'Age_Level']=="Four") Q2[n]<--29.93
  else {if (data_out[n, 'Age_Level']=="Five") Q2[n]<--22.93
  else {if (data_out[n, 'Age_Level']=="Six") Q2[n]<--18.22
  else {if (data_out[n, 'Age_Level']=="Seven") Q2[n]<--7.69
  else {if (data_out[n, 'Age_Level']=="Eight") Q2[n]<-0
  else {Q2[n]<-4.76
  }}}}}}}}}

for (n in 1:i) {
  if (data_out[n, 'Racegrp']=="white") Q3[n]<-1.26 
  else {if (data_out[n, 'Racegrp']=="black") Q3[n]<-0
  else {if (data_out[n, 'Racegrp']=="hispa") Q3[n]<--9.48
  else {Q3[n]<--2.09
  }}}}

for (n in 1:i) {
  if (data_out[n, 'Unmarried']==1) Q4[n]<-2.80 else Q4[n]<-0}

for (n in 1:i) {
  if (data_out[n, 'PVD']==1) Q5[n]<-3.81 else Q5[n]<-0}

for (n in 1:i) {
  if (data_out[n, 'Hypertension']==1) Q6[n]<-7.20 else Q6[n]<-0}

for (n in 1:i) {
  if (data_out[n, 'Diabetes']==1) Q7[n]<-6.09 else Q7[n]<-0}

for (n in 1:i) {
  if (data_out[n, 'CVD']==1) Q8[n]<-8.24 else Q8[n]<-0}

for (n in 1:i) {
  if (data_out[n, 'Anemia']==1) Q9[n]<-10.74 else Q9[n]<-0}

for (n in 1:i) {
  if (data_out[n, 'HDL_Level']=="One") Q10[n]<--132.31 
  else {if (data_out[n, 'HDL_Level']=="Two") Q10[n]<-12.55
  else {if (data_out[n, 'HDL_Level']=="Three") Q10[n]<-11.63
  else {if (data_out[n, 'HDL_Level']=="Four") Q10[n]<-11.65
  else {if (data_out[n, 'HDL_Level']=="Five") Q10[n]<-6.72
  else {if (data_out[n, 'HDL_Level']=="Six") Q10[n]<-4.92
  else {if (data_out[n, 'HDL_Level']=="Seven") Q10[n]<-5.14
  else {if (data_out[n, 'HDL_Level']=="Eight") Q10[n]<-5.64
  else {if (data_out[n, 'HDL_Level']=="Nine") Q10[n]<-6.14
  else {Q10[n]<--0.68
  }}}}}}}}}}

for (n in 1:i) {
  Score[n] <- Q1[n]+Q2[n]+Q3[n]+Q4[n]+Q5[n]+Q6[n]+Q7[n]+Q8[n]+Q9[n]+Q10[n]}

# Integrate the final table
Final_Table <- data.frame(Probability, Classification, Score)