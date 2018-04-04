#import datasets
dataset=read.csv('../input/train.csv')
head(dataset,n=5)
#show columns with respective number of missing values
colSums(is.na(dataset))
colnames(dataset)[colSums(is.na(dataset)) > 0]

#Add average of values in this column in place of NAs
summary(dataset$LotFrontage)
head(dataset$LotFrontage)

dataset$LotFrontage=ifelse(is.na(dataset$LotFrontage),
                           ave(dataset$LotFrontage, FUN = function(x) mean(x, na.rm = TRUE)),
                           dataset$LotFrontage)

#contains lot of missing values so drop column
summary(dataset$Alley)

dataset[,'Alley']<-list(NULL)

#correct-----s

summary(dataset$MasVnrType)
head(dataset$MasVnrType,n=5)
str(head(dataset$MasVnrType))


dataset$MasVnrType[is.na(dataset$MasVnrType)]<-"None"

summary(dataset$MasVnrArea)
head(dataset$MasVnrArea,n=5)
dataset$MasVnrArea=ifelse(is.na(dataset$MasVnrArea),0,dataset$MasVnrArea)
#correct-----x

#drop rows with NA BSMT HEight as NA
summary(dataset$BsmtQual)
head(dataset$BsmtQual,n=5)
str(head(dataset$BsmtQual))

dataset=dataset[!(is.na(dataset$BsmtQual)),]

#remove one row of BSmtexe
summary(dataset$BsmtExposure)
head(dataset$BsmtExposure,n=5)
str(head(dataset$BsmtExposure))

dataset=dataset[!(is.na(dataset$BsmtExposure)),]

#remove one row of BSmtexe
summary(dataset$BsmtFinType2)

dataset=dataset[!(is.na(dataset$BsmtFinType2)),]

#remove one row of ELEctrical
summary(dataset$Electrical)

dataset=dataset[!(is.na(dataset$Electrical)),]

#refactor FireplaceQu with respect to NA values
dat1=dataset[is.na(dataset$FireplaceQu),"Fireplaces"]

level=levels(dataset$FireplaceQu)
level[length(level)+1]='NA'
dataset$FireplaceQu=factor(dataset$FireplaceQu, levels = level)
dataset$FireplaceQu[is.na(dataset$FireplaceQu)]<-"NA"

#refactor garage columns
dataset[is.na(dataset$GarageType),c("GarageYrBlt","GarageFinish","GarageQual","GarageCond")]

level=levels(dataset$GarageType)
level[length(level)+1]='NA'
dataset$GarageType=factor(dataset$GarageType, levels = level)
dataset$GarageType[is.na(dataset$GarageType)]<-"NA"


#bin garage year built
min(dataset$GarageYrBlt,na.rm=TRUE)
max(dataset$GarageYrBlt,na.rm=TRUE)
library('binst')
dataset$GarageYrBlt=create_bins(dataset$GarageYrBlt,
                                c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010))
dataset$GarageYrBlt=ifelse(is.na(dataset$GarageYrBlt),0,dataset$GarageYrBlt)


#refactor garage finish
level=levels(dataset$GarageFinish)
level[length(level)+1]='NA'
dataset$GarageFinish=factor(dataset$GarageFinish, levels = level)
dataset$GarageFinish[is.na(dataset$GarageFinish)]<-"NA"

#refactor garage quality
level=levels(dataset$GarageQual)
level[length(level)+1]='NA'
dataset$GarageQual=factor(dataset$GarageQual, levels = level)
dataset$GarageQual[is.na(dataset$GarageQual)]<-"NA"


#refactor garage cond
level=levels(dataset$GarageCond)
level[length(level)+1]='NA'
dataset$GarageCond=factor(dataset$GarageCond, levels = level)
dataset$GarageCond[is.na(dataset$GarageCond)]<-"NA"

summary(dataset$MiscFeature)
#refactor garage cond
level=levels(dataset$PoolQC)
level[length(level)+1]='NA'
dataset$PoolQC=factor(dataset$PoolQC, levels = level)
dataset$PoolQC[is.na(dataset$PoolQC)]<-"NA"

#refactor fence
level=levels(dataset$Fence)
level[length(level)+1]='NA'
dataset$Fence=factor(dataset$Fence, levels = level)
dataset$Fence[is.na(dataset$Fence)]<-"NA"

#refactor miscfeature
level=levels(dataset$MiscFeature)
level[length(level)+1]='NA'
dataset$MiscFeature=factor(dataset$MiscFeature, levels = level)
dataset$MiscFeature[is.na(dataset$MiscFeature)]<-"NA"

summary(dataset$SaleCondition)

##data preprocessed

regressor=lm(formula = SalePrice~.,data = dataset)

summary(dataset$Neighborhood)
head(dataset$MSZoning,n=10)
str(dataset$MSZoning)

#import dataset_tests
dataset_test=read.csv('../input/test.csv')
str(dataset_test$MSZoning)
#show columns with respective number of missing values
colSums(is.na(dataset_test))
colnames(dataset_test)[colSums(is.na(dataset_test)) > 0]

#Add average of values in this column in place of NAs
summary(dataset_test$LotFrontage)
head(dataset_test$LotFrontage)

dataset_test$LotFrontage=ifelse(is.na(dataset_test$LotFrontage),
                                ave(dataset_test$LotFrontage, FUN = function(x) mean(x, na.rm = TRUE)),
                                dataset_test$LotFrontage)

#contains lot of missing values so drop column
summary(dataset_test$Alley)

dataset_test[,'Alley']<-list(NULL)

#correct-----s

summary(dataset_test$MasVnrType)
head(dataset_test$MasVnrType,n=5)
str(head(dataset_test$MasVnrType))


dataset_test$MasVnrType[is.na(dataset_test$MasVnrType)]<-"None"

summary(dataset_test$MasVnrArea)
head(dataset_test$MasVnrArea,n=5)
dataset_test$MasVnrArea=ifelse(is.na(dataset_test$MasVnrArea),0,dataset_test$MasVnrArea)
#correct-----x

#drop rows with NA BSMT HEight as NA
summary(dataset_test$BsmtQual)
head(dataset_test$BsmtQual,n=5)
str(head(dataset_test$BsmtQual))

dataset_test=dataset_test[!(is.na(dataset_test$BsmtQual)),]

#remove one row of BSmtexe
summary(dataset_test$BsmtExposure)
head(dataset_test$BsmtExposure,n=5)
str(head(dataset_test$BsmtExposure))

dataset_test=dataset_test[!(is.na(dataset_test$BsmtExposure)),]

#remove one row of BSmtexe
summary(dataset_test$BsmtFinType2)

dataset_test=dataset_test[!(is.na(dataset_test$BsmtFinType2)),]

#remove one row of ELEctrical
summary(dataset_test$Electrical)

dataset_test=dataset_test[!(is.na(dataset_test$Electrical)),]

#refactor FireplaceQu with respect to NA values
dat1=dataset_test[is.na(dataset_test$FireplaceQu),"Fireplaces"]

level=levels(dataset_test$FireplaceQu)
level[length(level)+1]='NA'
dataset_test$FireplaceQu=factor(dataset_test$FireplaceQu, levels = level)
dataset_test$FireplaceQu[is.na(dataset_test$FireplaceQu)]<-"NA"

#refactor garage columns
dataset_test[is.na(dataset_test$GarageType),c("GarageYrBlt","GarageFinish","GarageQual","GarageCond")]

level=levels(dataset_test$GarageType)
level[length(level)+1]='NA'
dataset_test$GarageType=factor(dataset_test$GarageType, levels = level)
dataset_test$GarageType[is.na(dataset_test$GarageType)]<-"NA"


#bin garage year built
min(dataset_test$GarageYrBlt,na.rm=TRUE)
max(dataset_test$GarageYrBlt,na.rm=TRUE)
library('binst')
dataset_test$GarageYrBlt=create_bins(dataset_test$GarageYrBlt,
                                     c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010))
dataset_test$GarageYrBlt=ifelse(is.na(dataset_test$GarageYrBlt),0,dataset_test$GarageYrBlt)


#refactor garage finish
level=levels(dataset_test$GarageFinish)
level[length(level)+1]='NA'
dataset_test$GarageFinish=factor(dataset_test$GarageFinish, levels = level)
dataset_test$GarageFinish[is.na(dataset_test$GarageFinish)]<-"NA"

#refactor garage quality
level=levels(dataset_test$GarageQual)
level[length(level)+1]='NA'
dataset_test$GarageQual=factor(dataset_test$GarageQual, levels = level)
dataset_test$GarageQual[is.na(dataset_test$GarageQual)]<-"NA"


#refactor garage cond
level=levels(dataset_test$GarageCond)
level[length(level)+1]='NA'
dataset_test$GarageCond=factor(dataset_test$GarageCond, levels = level)
dataset_test$GarageCond[is.na(dataset_test$GarageCond)]<-"NA"

summary(dataset_test$MiscFeature)
#refactor garage cond
level=levels(dataset_test$PoolQC)
level[length(level)+1]='NA'
dataset_test$PoolQC=factor(dataset_test$PoolQC, levels = level)
dataset_test$PoolQC[is.na(dataset_test$PoolQC)]<-"NA"

#refactor fence
level=levels(dataset_test$Fence)
level[length(level)+1]='NA'
dataset_test$Fence=factor(dataset_test$Fence, levels = level)
dataset_test$Fence[is.na(dataset_test$Fence)]<-"NA"

#refactor miscfeature
level=levels(dataset_test$MiscFeature)
level[length(level)+1]='NA'
dataset_test$MiscFeature=factor(dataset_test$MiscFeature, levels = level)
dataset_test$MiscFeature[is.na(dataset_test$MiscFeature)]<-"NA"

summary(dataset_test$SaleCondition)

##data preprocessed

summary(dataset_test$MSZoning)
head(dataset_test$Neighborhood,n=10)

colnames(dataset)[colSums(is.na(dataset)) > 0]
colSums(is.na(dataset_test))

#drop columns with NA's
dataset_test=dataset[!(is.na(dataset_test$MSZoning)),]
dataset_test=dataset[!(is.na(dataset_test$Utilities)),]
dataset_test=dataset[!(is.na(dataset_test$Exterior1st)),]
dataset_test=dataset[!(is.na(dataset_test$Exterior2nd)),]
dataset_test=dataset[!(is.na(dataset_test$BsmtCond)),]
dataset_test=dataset[!(is.na(dataset_test$KitchenQual)),]
dataset_test=dataset[!(is.na(dataset_test$Functional)),]
dataset_test=dataset[!(is.na(dataset_test$GarageCars)),]
dataset_test=dataset[!(is.na(dataset_test$GarageArea)),]
dataset_test=dataset[!(is.na(dataset_test$SaleType)),]

regressor=lm(formula = SalePrice~.,data = dataset)

summary(regressor)

dataset_test1=dataset_test[, names(dataset_test) != "SalePrice"]
res=predict(regressor,dataset=dataset_test1)
#difference=dataset_test$SalePrice-res
#diff=abs(difference)
#avd_lin=sum(diff)/1420
#---------------Decision Tree and Random Forest regressors.
#library(rpart)
#library(randomForest)

#regressor_dt=rpart(formula = SalePrice~.,data = dataset, method = "anova")
#summary(regressor_dt)
#res1=predict(regressor_dt,dataset=dataset_test1)
#difference1=dataset_test$SalePrice-res1
#diff1=abs(difference1)
#avd_dt=sum(diff1)/1420


#regressor_rf=randomForest(x=dataset[, names(dataset_test) != "SalePrice"]
#,y=dataset$SalePrice,ntree = 500)
#res2=predict(regressor_rf,dataset=dataset_test1)
#difference2=dataset_test$SalePrice-res2
#diff2=abs(difference2)
#avd_rf=sum(diff2)/1420

r_sq=function(calc, avg){
  m=mean(avg)
  numrow=NROW(avg)
  m[1:numrow]=m
  m=avg-m
  m=m*m
  m=sum(m)
  
  s=calc-avg
  s=s*s
  s=sum(s)
  
  d=s/m
  return(1-d)
}
#r_sq(res1,dataset_test$SalePrice)

datframe=data.frame(Id=dataset_test$Id,SalePrice=res)
print(datframe)
write.csv(datframe, file = "SAlePrice.csv",row.names = FALSE)
#ALL THE RESULTS ARE IN 'datframe' variable
