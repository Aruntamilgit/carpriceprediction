#Load libraries that are required
#install.packages("tidyr")
library(tidyr)
#install.packages("MASS")
library(MASS)
#install.packages("car")
library(car)
#Read CSV file to dataframe
carprice<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
#Check for NA
sum(is.na(carprice))
#Count of NA is zero so no action required

#Verify for duplicate rows
nrow(unique(carprice[,-1]))
#No duplicates

#Separate Car Name field into Company name and model name
carprice<-separate(carprice,CarName,c("carcompanyname","carmodelname"),sep=" ",extra="merge")
#Remove Car_id and Car model name as it is not required for final modelling
carprice<-carprice[,!names(carprice) %in% c("car_ID","carmodelname")]

#Check unique car company names and fix any errors
unique(carprice$carcompanyname)
carprice$carcompanyname[which(carprice$carcompanyname=="maxda")]<-"mazda"
carprice$carcompanyname[which(carprice$carcompanyname=="Nissan")]<-"nissan"
carprice$carcompanyname[which(carprice$carcompanyname=="porcshce")]<-"porsche"
carprice$carcompanyname[which(carprice$carcompanyname=="toyouta")]<-"toyota"
carprice$carcompanyname[which(carprice$carcompanyname=="vokswagen")]<-"volkswagen"
carprice$carcompanyname[which(carprice$carcompanyname=="vw")]<-"volkswagen"
unique(carprice$carcompanyname)

#Convert categorical fields to factor
str(carprice)
carprice_categorical_colnames<-c("symboling","carcompanyname","fueltype","aspiration","doornumber","carbody","drivewheel","enginelocation","enginetype","cylindernumber","fuelsystem")
carprice_categorical_columns<-carprice[,carprice_categorical_colnames]
carprice[,carprice_categorical_colnames]<-lapply(carprice_categorical_columns,factor)
str(carprice)

#Outlier Check for numerical fields
quantile(carprice$wheelbase,seq(0,1,0.01))
quantile(carprice$carlength,seq(0,1,0.01))
quantile(carprice$carwidth,seq(0,1,0.01))
quantile(carprice$carheight,seq(0,1,0.01))
quantile(carprice$curbweight,seq(0,1,0.01))#check for 0%
carprice$curbweight[which(carprice$curbweight<1819.72)]<-1819.72
quantile(carprice$enginesize,seq(0,1,0.01))#check for 2% and 96%
carprice$enginesize[which(carprice$enginesize<90)]<-90
carprice$enginesize[which(carprice$enginesize>209)]<-209
quantile(carprice$boreratio,seq(0,1,0.01))#check for 0%
carprice$boreratio[which(carprice$boreratio<2.9100)]<-2.9100
quantile(carprice$stroke,seq(0,1,0.01))#check for 1%
carprice$stroke[which(carprice$stroke<2.6400)]<-2.6400
quantile(carprice$compressionratio,seq(0,1,0.01))#check for 90%
carprice$compressionratio[which(carprice$compressionratio>10.9400)]<-10.9400
quantile(carprice$horsepower,seq(0,1,0.01))#check for 99%
carprice$horsepower[which(carprice$horsepower>207)]<-207
quantile(carprice$peakrpm,seq(0,1,0.01))
quantile(carprice$citympg,seq(0,1,0.01))#check for 98%
carprice$citympg[which(carprice$citympg>38.00)]<-38.00
quantile(carprice$highwaympg,seq(0,1,0.01))#check for 98%

#Derived variables

#Convert Symboling into levels
levels(carprice$symboling)[1:2]<-"safe"
levels(carprice$symboling)[2:3]<-"neutral"
levels(carprice$symboling)[3:4]<-"risky"
str(carprice)

#Convert factors with 2 levels into numerical values
levels(carprice$fueltype)<-c(1,0)
carprice$fueltype<-as.numeric(levels(carprice$fueltype))[carprice$fueltype]
levels(carprice$aspiration)<-c(1,0)
carprice$aspiration<-as.numeric(levels(carprice$aspiration))[carprice$aspiration]
levels(carprice$doornumber)<-c(1,0)
carprice$doornumber<-as.numeric(levels(carprice$doornumber))[carprice$doornumber]
levels(carprice$enginelocation)<-c(1,0)
carprice$enginelocation<-as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

#Function to convert factors with more than 2 levels into dummy variables
create_dummy_values<-function(dummy,df_name,column_name) {
  dummy<-dummy[,-1]
  df_name<-cbind(df_name[,!names(df_name)==column_name],dummy)
  return (df_name)
}

carprice<-create_dummy_values(data.frame(model.matrix(~symboling ,data=carprice)),carprice,"symboling")
carprice<-create_dummy_values(data.frame(model.matrix(~carcompanyname -1,data=carprice)),carprice,"carcompanyname")
carprice<-create_dummy_values(data.frame(model.matrix(~carbody -1,data=carprice)),carprice,"carbody")
carprice<-create_dummy_values(data.frame(model.matrix(~drivewheel -1,data=carprice)),carprice,"drivewheel")
carprice<-create_dummy_values(data.frame(model.matrix(~enginetype -1,data=carprice)),carprice,"enginetype")
carprice<-create_dummy_values(data.frame(model.matrix(~cylindernumber -1,data=carprice)),carprice,"cylindernumber")
carprice<-create_dummy_values(data.frame(model.matrix(~fuelsystem -1,data=carprice)),carprice,"fuelsystem")

#Set seed
set.seed(100)
#create train and test data set
carprice_training_indices<-sample(1:nrow(carprice),0.7*nrow(carprice))
carprice_train<-carprice[carprice_training_indices,]
carprice_test<-carprice[-carprice_training_indices,]

#Build first iteration of the model
carprice_model_iteration1<-lm(price~.,data=carprice)
summary(carprice_model_iteration1)

#Execute stepAIC
step <- stepAIC(carprice_model_iteration1, direction="both")
step

carprice_model_iteration2<-lm(formula = price ~ fueltype + enginelocation + wheelbase + 
     carlength + carwidth + carheight + curbweight + compressionratio + 
     horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
     carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
     carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
     carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
     carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
     carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
     enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
     enginetyperotor + cylindernumberfive + cylindernumberfour + 
     cylindernumbersix + fuelsystem2bbl, data = carprice)
summary(carprice_model_iteration2)
vif(carprice_model_iteration2)
#Adjust R Square=0.9536

#Create collinearity matrix and check for high collinearity
carprice_cor_matrix<-cor(carprice)
write.csv(carprice_cor_matrix,"carprice_cor_matrix.csv")

#Car length and curb weight has a high VIF. CHeck for collinearity
cor(carprice$carlength,carprice$curbweight)
#It has high collinearity of 0.87 so removing carlength 
carprice_model_iteration3<-lm(formula = price ~ fueltype + enginelocation + wheelbase + 
                                carwidth + carheight + curbweight + compressionratio + 
                                horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
                                carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                cylindernumbersix + fuelsystem2bbl, data = carprice)
summary(carprice_model_iteration3)
vif(carprice_model_iteration3)
#Adjust R Square=0.9492

#Car width and curb weight has a high VIF. CHeck for collinearity
cor(carprice$carwidth,carprice$curbweight)
#It has high collinearity of 0.86 so removing carwidth 
carprice_model_iteration4<-lm(formula = price ~ fueltype + enginelocation + wheelbase + 
                                carheight + curbweight + compressionratio + 
                                horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
                                carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                cylindernumbersix + fuelsystem2bbl, data = carprice)
summary(carprice_model_iteration4)
vif(carprice_model_iteration4)
#Adjust R Square=0.9478

#Wheelbase and curb weight has a high VIF. CHeck for collinearity
cor(carprice$wheelbase,carprice$curbweight)
#It has high collinearity of 0.77 so removing wheelbase 
carprice_model_iteration5<-lm(formula = price ~ fueltype + enginelocation +  
                                carheight + curbweight + compressionratio + 
                                horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
                                carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                cylindernumbersix + fuelsystem2bbl, data = carprice)
summary(carprice_model_iteration5)
vif(carprice_model_iteration5)
#Adjust R Square=0.9404

#carbodyhardtop has a high p value of 0.30 so removing it
carprice_model_iteration6<-lm(formula = price ~ fueltype + enginelocation +  
                                carheight + curbweight + compressionratio + 
                                horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
                                carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                carbodyhatchback + carbodysedan + carbodywagon + 
                                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                cylindernumbersix + fuelsystem2bbl, data = carprice)
summary(carprice_model_iteration6)
vif(carprice_model_iteration6)
#Adjust R Square=0.9404

#carbodysedan has a high p value of 0.26 so removing it
carprice_model_iteration7<-lm(formula = price ~ fueltype + enginelocation +  
                                carheight + curbweight + compressionratio + 
                                horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
                                carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                carbodyhatchback + carbodywagon + 
                                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                cylindernumbersix + fuelsystem2bbl, data = carprice)
summary(carprice_model_iteration7)
vif(carprice_model_iteration7)
#Adjust R Square=0.9403

#carbodyhatchback has a high p value of 0.15 so removing it
carprice_model_iteration8<-lm(formula = price ~ fueltype + enginelocation +  
                                carheight + curbweight + compressionratio + 
                                horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
                                carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                carbodywagon + 
                                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                cylindernumbersix + fuelsystem2bbl, data = carprice)
summary(carprice_model_iteration8)
vif(carprice_model_iteration8)
#Adjust R Square=0.94

#compressionratio has a high p value of 0.12 so removing it
carprice_model_iteration9<-lm(formula = price ~ fueltype + enginelocation +  
                                carheight + curbweight +  
                                horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
                                carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                carbodywagon + 
                                enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                cylindernumbersix + fuelsystem2bbl, data = carprice)
summary(carprice_model_iteration9)
vif(carprice_model_iteration9)
#Adjust R Square=0.9395

#fueltype has a high p value of 0.24 so removing it
carprice_model_iteration10<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
                                carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                carbodywagon + enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                cylindernumbersix + fuelsystem2bbl, data = carprice)
summary(carprice_model_iteration10)
vif(carprice_model_iteration10)
#Adjust R Square=0.9393

#fuelsystem2bbl has a high p value of 0.37 so removing it
carprice_model_iteration11<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + peakrpm + highwaympg + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                 carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                 carbodywagon + enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration11)
vif(carprice_model_iteration11)
#Adjust R Square=0.9394

#peakrpm has a high p value of 0.18 so removing it
carprice_model_iteration12<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + highwaympg + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                 carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + carcompanynamevolkswagen + carcompanynamevolvo + 
                                 carbodywagon + enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration12)
vif(carprice_model_iteration12)
#Adjust R Square=0.9391

#carcompanynamevolvo has a high p value of 0.22 so removing it
carprice_model_iteration13<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + highwaympg + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                 carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + carcompanynamevolkswagen +  
                                 carbodywagon + enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration13)
vif(carprice_model_iteration13)
#Adjust R Square=0.939

#enginetypeohc has a high p value of 0.23 so removing it
carprice_model_iteration14<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + highwaympg + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                 carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + carcompanynamevolkswagen +  
                                 carbodywagon + enginetypedohcv + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration14)
vif(carprice_model_iteration14)
#Adjust R Square=0.9388

#highwaympg has a high p value of 0.08 so removing it
carprice_model_iteration15<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                 carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + carcompanynamevolkswagen +  
                                 carbodywagon + enginetypedohcv + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration15)
vif(carprice_model_iteration15)
#Adjust R Square=0.9381

#enginetypedohcv has a high p value of 0.12 so removing it
carprice_model_iteration16<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                 carcompanynamemercury + carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + carcompanynamevolkswagen +  
                                 carbodywagon + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration16)
vif(carprice_model_iteration16)
#Adjust R Square=0.9376

#carcompanynamemercury has a high p value of 0.5 so removing it
carprice_model_iteration17<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                 carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + carcompanynamevolkswagen +  
                                 carbodywagon + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration17)
vif(carprice_model_iteration17)
#Adjust R Square=0.9366

#carbodywagon has one star for p value of 0.01 so removing it
carprice_model_iteration18<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                 carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + carcompanynamevolkswagen +  
                                 enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration18)
vif(carprice_model_iteration18)
#Adjust R Square=0.9353

#carcompanynamevolkswagen has one star for p value of 0.01 so removing it
carprice_model_iteration19<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu + carcompanynamemazda + 
                                 carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration19)
vif(carprice_model_iteration19)
#Adjust R Square=0.9334

#carcompanynamemazda has one star for p value of 0.029 so removing it
carprice_model_iteration20<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynameisuzu +  
                                 carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration20)
vif(carprice_model_iteration20)
#Adjust R Square=0.9319

#carcompanynameisuzu has high p value of 0.08 so removing it
carprice_model_iteration21<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamechevrolet + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration21)
vif(carprice_model_iteration21)
#Adjust R Square=0.9312

#carcompanynamechevrolet has high p value of 0.058 so removing it
carprice_model_iteration22<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamedodge + 
                                 carcompanynamehonda + carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration22)
vif(carprice_model_iteration22)
#Adjust R Square=0.9302

#carcompanynamehonda has high p value of 0.075 so removing it
carprice_model_iteration23<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamedodge + 
                                 carcompanynamemitsubishi + carcompanynamenissan + 
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration23)
vif(carprice_model_iteration23)
#Adjust R Square=0.9293

#carcompanynamenissan has one star for p value of 0.04 so removing it
carprice_model_iteration24<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamedodge + carcompanynamemitsubishi +  
                                 carcompanynameplymouth + carcompanynamerenault + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration24)
vif(carprice_model_iteration24)
#Adjust R Square=0.9281

#carcompanynamerenault has one star for p value of 0.039 so removing it
carprice_model_iteration25<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + symbolingneutral + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamedodge + carcompanynamemitsubishi +  
                                 carcompanynameplymouth + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration25)
vif(carprice_model_iteration25)
#Adjust R Square=0.9268

#symbolingneutral has one star for p value of 0.032 so removing it
carprice_model_iteration26<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamedodge + carcompanynamemitsubishi +  
                                 carcompanynameplymouth + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration26)
vif(carprice_model_iteration26)
#Adjust R Square=0.9254

#carcompanynamedodge has one star for p value of 0.036 so removing it
carprice_model_iteration27<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamemitsubishi +  
                                 carcompanynameplymouth + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration27)
vif(carprice_model_iteration27)
#Adjust R Square=0.924

#carcompanynameplymouth has one star for p value of 0.032 so removing it
carprice_model_iteration28<-lm(formula = price ~  enginelocation +  carheight + curbweight +  
                                 horsepower + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamemitsubishi + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration28)
vif(carprice_model_iteration28)
#Adjust R Square=0.9226

#carheight has high value for p value of 0.05 so removing it
carprice_model_iteration29<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamemitsubishi + carcompanynamesubaru + 
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration29)
vif(carprice_model_iteration29)
#Adjust R Square=0.9214

#carcompanynamesubaru has one star for p value of 0.016 so removing it
carprice_model_iteration30<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamemitsubishi +  
                                 carcompanynametoyota + enginetypel + enginetypeohcv + 
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration30)
vif(carprice_model_iteration30)
#Adjust R Square=0.9194

#enginetypeohcv has two star for p value of 0.007 so removing it
carprice_model_iteration31<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + 
                                 carcompanynamebuick + carcompanynamemitsubishi +  
                                 carcompanynametoyota + enginetypel +  
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration31)
vif(carprice_model_iteration31)
#Adjust R Square=0.9168

#carcompanynamemitsubishi has two star for p value of 0.0059 so removing it
carprice_model_iteration32<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + carcompanynamebuick +   
                                 carcompanynametoyota + enginetypel +  
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration32)
vif(carprice_model_iteration32)
#Adjust R Square=0.9139

#carcompanynametoyota has two star for p value of 0.011 so removing it
carprice_model_iteration33<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + carcompanynamebuick +   
                                 enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration33)
vif(carprice_model_iteration33)
#Adjust R Square=0.9114

#enginetypel has two star for p value of 0.002 so removing it
carprice_model_iteration34<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + carcompanynamebuick +   
                                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration34)
vif(carprice_model_iteration34)
#Adjust R Square=0.9077

#cylindernumberfour has two star for p value of 0.002 so removing it
carprice_model_iteration35<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + carcompanynamebuick +   
                                 enginetyperotor + cylindernumberfive +  
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration35)
vif(carprice_model_iteration35)
#Adjust R Square=0.891

#cylindernumberfive has high p value of 0.83 so removing it
carprice_model_iteration36<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + carcompanynamebuick +   
                                 enginetyperotor +   
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration36)
vif(carprice_model_iteration36)
#Adjust R Square=0.8915

#enginetyperotor has high p value of 0.32 so removing it
carprice_model_iteration37<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + carcompanynamebuick +   
                                 cylindernumbersix , data = carprice)
summary(carprice_model_iteration37)
vif(carprice_model_iteration37)
#Adjust R Square=0.8915

#cylindernumbersix has high p value of 0.28 so removing it
carprice_model_iteration38<-lm(formula = price ~  enginelocation +  curbweight +  
                                 horsepower + carcompanynamebmw + carcompanynamebuick , data = carprice)
summary(carprice_model_iteration38)
vif(carprice_model_iteration38)
#Adjust R Square=0.8914

#cylindernumbersix has high p value of 0.28 so removing it
carprice_model_iteration39<-lm(formula = price ~  enginelocation +  horsepower + carcompanynamebmw + carcompanynamebuick , data = carprice)
summary(carprice_model_iteration39)
vif(carprice_model_iteration39)
#Adjust R Square=0.8914

#Pvalue of intercept remains high for all models after model34 so choosing model34 as final one for prediction
#Adjusted R square of model34 is 0.9077
carprice_model_predict_value<-predict(carprice_model_iteration34,carprice_test[,-62])
carprice_test$price_predicted<-carprice_model_predict_value
carprice_r_square<-cor(carprice_test$price,carprice_test$price_predicted)^2
carprice_r_square
#carprice_r_square=0.87

#Predicted R Square=0.87 and Adjusted R square of the model34 - 0.9077. THis is in the acceptable range.
#Below are the variables that are part of the final model
#1.enginelocation
#2.curbweight
#3.horsepower
#4.carcompanynamebmw
#5.carcompanynamebuick  
#6.enginetyperotor
#7.cylindernumberfive
#8.cylindernumberfour
#9.cylindernumbersix 