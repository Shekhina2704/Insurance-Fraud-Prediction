"""
	@script-author: Jyothi Tom, Shekhina Neha, Gokul S, Aishwarya Lakshmi, Srija S, Robin Wilson
	@script-description: R Code to clean the data and train the Logistic Regression Model  
  @script-details: Written in RStudio
"""

library(DescTools)    #For Cramer's V
insurance <- read.csv("C:\\Users\\admin\\Desktop\\SJC\\Projects\\R Proj\\Insurance\\insurance_claims.csv")

#Get column names having '?' values
null_cols = unique(names(insurance)[which(insurance == '?', arr.ind=T)[, "col"]])

#Check the relevance of the columns with '?' values using:
#1. Proportion of '?'
sapply(insurance, function(x) length(x[x=="?"])/nrow(insurance))
#2. Chi-sq Test
for (i in null_cols){
  # print(i)
  coll = insurance[,i][which(insurance[,i] !='?', arr.ind=T)];coll
  fraud = insurance$fraud_reported[which(insurance[,i]!='?', arr.ind=T)]
  #print("Chi-sq test results between Fraud cases reported and %s",%(i))
  print(chisq.test(coll,fraud))}

#Since it's seen that these columns have no relevant effect on 'fraud_reported', drop them
insurance[,c(null_cols,"policy_number")]<-NULL

#PCA
corr_nums<- c("months_as_customer","age","Avg_capital_loss","capital_gains","Avg_capital_gains","total_claim_amount","injury_claim","property_claim","vehicle_claim" )
insurance.pca<-prcomp(insurance[corr_nums],center = TRUE, scale. = TRUE)
summary(insurance.pca)
insurance.pca
var = insurance.pca$sdev^2
prop_var <- var/sum(var); 
plot(cumsum(prop_var))  
screeplot(insurance.pca, type="lines")
biplot(insurance.pca,scale=0)
insurance[, corr_nums] <- NULL
insurance <- cbind(insurance, insurance.pca$x[,1:4])


num<-NULL
num <- c(split(names(insurance),sapply(insurance, function(x) paste(class(x), collapse=" ")))$integer, split(names(insurance),sapply(insurance, function(x) paste(class(x), collapse=" ")))$numeric)
num<-num[!(num %in% c("insured_zip", "auto_year", "policy_number", "incident_hour_of_the_day",corr_nums))]
num
corr <- cor(insurance[, num])     #Computing the correlation between the numerical variables
write.csv(corr, "corr.csv")

#ANOVA to check the effect of numeric features on the target variable
for (i in num){
  blah <- aov(insurance[,i]~insurance$fraud_reported, data = insurance)
  cat("--------------------------------------------------------")
  cat(sprintf('\nAnova Table: fraud_reported and %s\n',i))
  print(summary(blah))
}
nums_final<-c("umbrella_limit","PC1")
# names(insurance)

rem<-c("policy_bind_date","insured_zip","incident_date","incident_location" )   #Other columns to be removed
insurance[,rem]<-NULL

others = names(insurance)[!(names(insurance) %in% c(num,rem,"fraud_reported"))]

categ<-dummy.data.frame(insurance,names=others)   #Creating dummy variables from the categorical variables 
names(categ)      
vars<-c()
#Chi-Sq Test of Independence and Cramer's V computed for all the dummy vairables and the target variable
for (i in names(categ)){
  cat("\n------------------------------------------------------------------------------")
  cat(sprintf("\nChi-sq test results between \'fraud_reported\' and \"%s\" \n",i))
  p <- chisq.test(categ[,i],insurance$fraud_reported)
  print(p)
  if(p$p.value<0.07){
  vars<-c(vars,i)}
  cat(sprintf("Cramer's V: %s\n", CramerV(insurance[,i],insurance$fraud_reported)))
  }
vars
final<-c(vars,nums_final);final

insurance[,!(names(insurance) %in% nums_final)]<-NULL
insurance<- cbind(categ[,vars], insurance)     #Final cleaned dataset with the required features and the target variable
names(insurance)

train<- insurance[1:(0.75*nrow(insurance)),]                  #Training data containing 75% of the entire dataset
test<- insurance[(0.75*nrow(insurance)):nrow(insurance),]     #Training data containing 25% of the entire dataset


names(train)
final<-c("insured_occupationexec-managerial","insured_hobbiescamping","insured_hobbieschess","insured_hobbiescross-fit","incident_severityMajor Damage","auto_year2004","umbrella_limit","fraud_reported")
train<-train[,final]
bm2<-glm(fraud_reported~., data = train, family=binomial(link="logit"))   #Logistic Model
#bm1<-glm(fraud_reported~incident_severity_minor_damage+incident_severity_total_loss+incident_severity_trivial_damage+insured_hobbies_bungie.jumping+insured_hobbies_camping+insured_hobbies_chess+insured_hobbies_crossfit, data = train, family=binomial(link="logit"))
summary(bm2)

probabilities <- predict(bm2,test, type = "response")        
predicted.classes <- ifelse(probabilities > 0.5, "YES", "NO")
predicted.classes

