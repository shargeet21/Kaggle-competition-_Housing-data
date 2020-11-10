library('ggplot2')
library('scales')
library('dplyr') 
library('mice')
library('data.table')

house_train <- read.csv("C:\\Users\\sharg\\Desktop\\Geeta College Assignments\\867- Predictive Modelling\\Assignment 1\\train.csv")
house_test <- read.csv("C:\\Users\\sharg\\Desktop\\Geeta College Assignments\\867- Predictive Modelling\\Assignment 1\\test.csv")
str(house_train)
str(house_test)

house_test$SalePrice <- rep(NA,1459)
house<-rbind(house_train,house_test)
str(house)

dim(house_train)
dim(house_test)

sum(sapply(house_train[,1:81], typeof) == "Factor" )
sum(sapply(house_train[,1:81], typeof) == "integer")

summary(house_train[,sapply(house_train[,1:81], typeof) == "integer"])


cat('house_train has', dim(house_train)[1], 'rows and', dim(house_train)[2], 'columns.')
cat('house_Test has', dim(house_test)[1], 'rows and', dim(house_test)[2], ' columns.')

sum(is.na(house_train))/(nrow(house_train)* ncol(house_train))
sum(is.na(house_test))/(nrow(house_test)* ncol(house_test))

#combine dataset

house_test$SalePrice <- rep(NA,1459)
house_data <- bind_rows(house_train, house_test)
warnings()


str(house_data)

#missing values
summary(house_train$SalePrice)
quantile(house_train$SalePrice)

hist(house_data$SalePrice)
ggplot(house_train,aes(y=SalePrice,x=GrLivArea))+geom_point(colour = "Royalblue")

summary(house_train$GrLivArea)

house_train <- house_train[house_train$GrLivArea<=4000,]

colSums(sapply(house_train,is.na))
sapply(house_data[,1:80], function(x) sum(is.na(x)))

Miss_data <- sapply(house_train, function(x) sum(is.na(x)))
Miss_summary <- data.frame(Index = names(house_train), Missing_values = Miss_data )
Miss_summary[Miss_summary$Missing_values > 0,]

#MsZoning 
house_data$zone[house_data$MSZoning %in% c("FV")] <- 4
house_data$zone[house_data$MSZoning %in% c("RL")] <- 3
house_data$zone[house_data$MSZoning %in% c("RH","RM")] <- 2
house_data$zone[house_data$MSZoning %in% c("C (all)")] <- 1
str(house_data$MSZoning)

#Street
house_data$Street_dum[house_data$Street %in% c("Grvl")] <- 4
house_data$Street_dum[house_data$Street %in% c("Pave")] <- 3
summary(house_data$Street_dum)

house_data$Alley<-as.character(house_data$Alley) 
house_data$Alley[which(is.na(house_data$Alley))] <- "Mode"
# Alley  
house_data$Alley_dum[house_data$Alley %in% c("Grvl")] <- 2
house_data$Alley_dum[house_data$Alley %in% c("Pave")] <- 3
house_data$Alley_dum[house_data$Alley %in% c("Mode")] <- 1
summary(house_data$Alley_dum)


#LotShape
house_data$LotShape_dum[house_data$LotShape %in% c("IR1")] <- 4
house_data$LotShape_dum[house_data$LotShape %in% c("IR2")] <- 3
house_data$LotShape_dum[house_data$LotShape %in% c("IR3")] <- 2
house_data$LotShape_dum[house_data$LotShape %in% c("Reg")] <- 1
summary(house_data$LotShape_dum)


#LandContour
house_data$LandContour_dum[house_data$LandContour %in% c("Bnk")] <-5
house_data$LandContour_dum[house_data$LandContour %in% c("HLS")] <-7
house_data$LandContour_dum[house_data$LandContour %in% c("Low")] <-9
house_data$LandContour_dum[house_data$LandContour %in% c("LVl")] <-8
str(house_data$LandContour_dum)

#Utilities
house_data$Utilities_dum[house_data$Utilities %in% c("AllPub")] <- 4
house_data$Utilities_dum[house_data$Utilities %in% c("NA")] <- 1
house_data$Utilities_dum[house_data$Utilities %in% c("NoSeWa")] <- 2
str(house_data$Utilities_dum)

#LotConfig
house_data$LotConfig_dum[house_data$LotConfig %in% c("Corner")] <- 4
house_data$LotConfig_dum[house_data$LotConfig %in% c("CulDSac")] <- 5
house_data$LotConfig_dum[house_data$LotConfig %in% c("FR2")] <- 3
house_data$LotConfig_dum[house_data$LotConfig %in% c("FR3")] <- 1
house_data$LotConfig_dum[house_data$LotConfig %in% c("Inside")] <- 1

#LandSlope
house_data$LandSlope_dum[house_data$LandSlope %in% c("Gtl")] <- 4
house_data$LandSlope_dum[house_data$LandSlope %in% c("Mod")] <- 5
house_data$LandSlope_dum[house_data$LandSlope %in% c("Sev")] <- 3

#Neighborhood
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Blmngtn")] <- 25
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Blueste")] <- 24
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("BrDale")] <- 23
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("BrkSide")] <- 22
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("ClearCr")] <- 21
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("CollgCr")] <- 20
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("NAmes")] <- 19
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Crawfor")] <- 18
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("NPkVill")] <- 17
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("NridgHt")] <- 16
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("StoneBr")] <- 15
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Gilbert")] <- 14
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("NoRidge")] <- 13
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Somerst")] <- 12
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("SawyerW")] <- 11
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Sawyer")] <- 10
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("NWAmes")] <- 9
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("OldTown")] <- 8
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("SWISU")] <- 7
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Edwards")] <- 6
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("IDOTRR")] <- 5
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Mitchel")] <- 4
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Timber")] <- 3
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("MeadowV")] <- 2
house_data$Neighborhood_dum[house_data$Neighborhood %in% c("Veenker")] <- 1

#Condition1
house_data$Condition1_dum[house_data$Condition1 %in% c("Artery")] <- 7
house_data$Condition1_dum[house_data$Condition1 %in% c("Feedr")] <- 9
house_data$Condition1_dum[house_data$Condition1 %in% c("Norm")] <- 8
house_data$Condition1_dum[house_data$Condition1 %in% c("PosA")] <- 1
house_data$Condition1_dum[house_data$Condition1 %in% c("PosN")] <- 2
house_data$Condition1_dum[house_data$Condition1 %in% c("RRAe")] <- 4
house_data$Condition1_dum[house_data$Condition1 %in% c("RRAn","RRNe")] <- 6
house_data$Condition1_dum[house_data$Condition1 %in% c("RRNn")] <- 5

#Condition2
house_data$Condition2_dum[house_data$Condition2 %in% c("Artery")] <- 7
house_data$Condition2_dum[house_data$Condition2 %in% c("Feedr")] <- 9
house_data$Condition2_dum[house_data$Condition2 %in% c("Norm")] <- 8
house_data$Condition2_dum[house_data$Condition2 %in% c("PosA")] <- 1
house_data$Condition2_dum[house_data$Condition2 %in% c("PosN")] <- 2
house_data$Condition2_dum[house_data$Condition2 %in% c("RRAe")] <- 4
house_data$Condition2_dum[house_data$Condition2 %in% c("RRAn","RRNe")] <- 6
house_data$Condition2_dum[house_data$Condition2 %in% c("RRNn")] <- 5


# BldgType
house_data$BldgType_dum[house_data$BldgType %in% c("1Fam")] <- 1
house_data$BldgType_dum[house_data$BldgType %in% c("2fmCon")] <-2
house_data$BldgType_dum[house_data$BldgType %in% c("Duplex")] <- 3
house_data$BldgType_dum[house_data$BldgType %in% c("Twnhs")] <- 4
house_data$BldgType_dum[house_data$BldgType %in% c("TwnhsE")] <- 5

#HouseStyle
house_data$HouseStyle_dum[house_data$HouseStyle %in% c("1Story")] <- 7
house_data$HouseStyle_dum[house_data$HouseStyle %in% c("2Story")] <- 6
house_data$HouseStyle_dum[house_data$HouseStyle %in% c("1.5Fin")] <- 5
house_data$HouseStyle_dum[house_data$HouseStyle %in% c("1.5Unf")] <- 4
house_data$HouseStyle_dum[house_data$HouseStyle %in% c("2.5Fin")] <- 3
house_data$HouseStyle_dum[house_data$HouseStyle %in% c("2.5Unf")] <- 2
house_data$HouseStyle_dum[house_data$HouseStyle %in% c("SFoyer")] <- 1
house_data$HouseStyle_dum[house_data$HouseStyle %in% c("SLvl")] <- 8


#RoofStyle
house_data$RoofStyle_dum[house_data$RoofStyle %in% c("Flat")] <- 1
house_data$RoofStyle_dum[house_data$RoofStyle %in% c("Gable")] <- 2
house_data$RoofStyle_dum[house_data$RoofStyle %in% c("Hip")] <- 3
house_data$RoofStyle_dum[house_data$RoofStyle %in% c("Gambrel")] <- 4
house_data$RoofStyle_dum[house_data$RoofStyle %in% c("Mansard")] <- 5
house_data$RoofStyle_dum[house_data$RoofStyle %in% c("Shed")] <- 6

#RoofMatl
house_data$RoofMatl_dum[house_data$RoofMatl %in% c("CompShg")] <- 5
house_data$RoofMatl_dum[house_data$RoofMatl %in% c("Tar&Grv")] <- 4
house_data$RoofMatl_dum[house_data$RoofMatl %in% c("WdShake")] <- 3
house_data$RoofMatl_dum[house_data$RoofMatl %in% c("WdShngl")] <-2
house_data$RoofMatl_dum[house_data$RoofMatl %in% c("ClyTile","Membran","Metal","Roll")] <-1

#Exterior1st
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("WdShing")] <- 1
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("MetalSd")] <- 2
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("VinylSd")] <- 3
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("AsbShng","AsphShn")] <- 4
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("BrkComm")] <- 5
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("BrkFace")] <- 6
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("CBlock","CemntBd")] <- 7
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("HdBoard")] <- 8
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("MetalSd")] <- 9
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("Plywood")] <- 10
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("NA")] <- 11
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("Stucco")] <- 12
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("Wd Sdng")] <- 13
house_data$Exterior1st_dum[house_data$Exterior1st %in% c("WdShing")] <- 14

#Exterior2nd
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("Wd Shng")] <- 1
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("MetalSd")] <- 2
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("VinylSd")] <- 3
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("AsbShng","AsphShn")] <- 4
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("BrkComm")] <- 5
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("BrkFace")] <- 6
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("CBlock","CemntBd")] <- 7
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("HdBoard")] <- 8
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("MetalSd")] <- 9
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("Plywood")] <- 10
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("NA")] <- 11
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("Stucco")] <- 12
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("Wd Sdng")] <- 13
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("WdShing")] <- 14
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("CmentBd")] <- 15
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("ImStucc")] <- 16
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("Stone")] <- 18
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("Other")] <- 19
house_data$Exterior2nd_dum[house_data$Exterior2nd %in% c("Brk Cmn")] <- 20

#MasVnrType
house_data$MasVnrType_dum[house_data$MasVnrType %in% c("BrkCmn")] <- 5
house_data$MasVnrType_dum[house_data$MasVnrType %in% c("BrkFace")] <-4
house_data$MasVnrType_dum[house_data$MasVnrType %in% c("NA")] <-1
house_data$MasVnrType_dum[house_data$MasVnrType %in% c("None")] <-2
house_data$MasVnrType_dum[house_data$MasVnrType %in% c("Stone")] <-3

#ExterQual
house_data$ExterQual_dum[house_data$ExterQual %in% c("Ex")] <-1
house_data$ExterQual_dum[house_data$ExterQual %in% c("Fa")] <-2
house_data$ExterQual_dum[house_data$ExterQual %in% c("Gd")] <-3
house_data$ExterQual_dum[house_data$ExterQual %in% c("TA")] <-4

#ExterCond
house_data$ExterCond_dum[house_data$ExterCond %in% c("Ex")] <-1
house_data$ExterCond_dum[house_data$ExterCond %in% c("Fa")] <-2
house_data$ExterCond_dum[house_data$ExterCond %in% c("Gd")] <-3
house_data$ExterCond_dum[house_data$ExterCond %in% c("TA")] <-4
house_data$ExterCond_dum[house_data$ExterCond %in% c("Po")] <-5

#Foundation
house_data$Foundation_dum[house_data$Foundation %in% c("BrkTil")] <-1
house_data$Foundation_dum[house_data$Foundation %in% c("CBlock")] <-2
house_data$Foundation_dum[house_data$Foundation %in% c("PConc")] <-3
house_data$Foundation_dum[house_data$Foundation %in% c("Slab")] <-4
house_data$Foundation_dum[house_data$Foundation %in% c("Stone")] <-5
house_data$Foundation_dum[house_data$Foundation %in% c("Wood")] <-6

#BsmtQual
house_data$BsmtQual_dum[house_data$BsmtQual %in% c("Ex")] <-5
house_data$BsmtQual_dum[house_data$BsmtQual %in% c("Fa")] <-2
house_data$BsmtQual_dum[house_data$BsmtQual %in% c("Gd")] <-3
house_data$BsmtQual_dum[house_data$BsmtQual %in% c("TA")] <-4
house_data$BsmtQual_dum[house_data$BsmtQual %in% c("NA")] <-1

#BsmtCond
house_data$BsmtCond_dum[house_data$BsmtCond %in% c("NA")] <-1
house_data$BsmtCond_dum[house_data$BsmtCond %in% c("Fa")] <-2
house_data$BsmtCond_dum[house_data$BsmtCond %in% c("Gd")] <-3
house_data$BsmtCond_dum[house_data$BsmtCond %in% c("TA")] <-4
house_data$BsmtCond_dum[house_data$BsmtCond %in% c("Po")] <-5

#BsmtExposure
house_data$BsmtExposure_dum[house_data$BsmtExposure %in% c("Av")] <-5
house_data$BsmtExposure_dum[house_data$BsmtExposure %in% c("Gd")] <-4
house_data$BsmtExposure_dum[house_data$BsmtExposure %in% c("Mn")] <-3
house_data$BsmtExposure_dum[house_data$BsmtExposure %in% c("NA")] <-1
house_data$BsmtExposure_dum[house_data$BsmtExposure %in% c("No")] <-1

#BsmtFinType1
house_data$BsmtFinType1_dum[house_data$BsmtFinType1 %in% c("ALQ")] <-5
house_data$BsmtFinType1_dum[house_data$BsmtFinType1 %in% c("BLQ")] <-4
house_data$BsmtFinType1_dum[house_data$BsmtFinType1 %in% c("GLQ")] <-3
house_data$BsmtFinType1_dum[house_data$BsmtFinType1 %in% c("Unf")] <-2
house_data$BsmtFinType1_dum[house_data$BsmtFinType1 %in% c("Rec")] <-6
house_data$BsmtFinType1_dum[house_data$BsmtFinType1 %in% c("NA")] <-1
house_data$BsmtFinType1_dum[house_data$BsmtFinType1 %in% c("LwQ")] <-7

#BsmtFinType2
house_data$BsmtFinType2_dum[house_data$BsmtFinType2 %in% c("ALQ")] <-5
house_data$BsmtFinType2_dum[house_data$BsmtFinType2 %in% c("BLQ")] <-4
house_data$BsmtFinType2_dum[house_data$BsmtFinType2 %in% c("GLQ")] <-3
house_data$BsmtFinType2_dum[house_data$BsmtFinType2 %in% c("Unf")] <-2
house_data$BsmtFinType2_dum[house_data$BsmtFinType2 %in% c("Rec")] <-6
house_data$BsmtFinType2_dum[house_data$BsmtFinType2 %in% c("NA")] <-1
house_data$BsmtFinType2_dum[house_data$BsmtFinType2 %in% c("LwQ")] <-7

#Heating
house_data$Heating_dum[house_data$Heating %in% c("GasA")] <-6
house_data$Heating_dum[house_data$Heating %in% c("GasW")] <-5
house_data$Heating_dum[house_data$Heating %in% c("Grav")] <-4
house_data$Heating_dum[house_data$Heating %in% c("OthW")] <-3
house_data$Heating_dum[house_data$Heating %in% c("floor")] <-2
house_data$Heating_dum[house_data$Heating %in% c("Wall")] <-1

#HeatingQC
house_data$HeatingQC_dum[house_data$HeatingQC %in% c("Ex")] <-5
house_data$HeatingQC_dum[house_data$HeatingQC %in% c("TA")] <-4
house_data$HeatingQC_dum[house_data$HeatingQC %in% c("Gd")] <-3
house_data$HeatingQC_dum[house_data$HeatingQC %in% c("Po")] <-2
house_data$HeatingQC_dum[house_data$HeatingQC %in% c("Fa")] <-1

#CentralAir
house_data$CentralAir_dum[house_data$CentralAir %in% c("Y")] <-1
house_data$CentralAir_dum[house_data$CentralAir %in% c("N")] <-0

#Electrical
house_data$Electrical_dum[house_data$Electrical %in% c("SBrkr")] <-6
house_data$Electrical_dum[house_data$Electrical %in% c("FuseA")] <-2
house_data$Electrical_dum[house_data$Electrical %in% c("FuseF")] <-4
house_data$Electrical_dum[house_data$Electrical %in% c("FuseP")] <-3
house_data$Electrical_dum[house_data$Electrical %in% c("NA")] <-1
house_data$Electrical_dum[house_data$Electrical %in% c("Mix")] <-5

#KitchenQual
house_data$KitchenQual_dum[house_data$KitchenQual %in% c("TA")] <-5
house_data$KitchenQual_dum[house_data$KitchenQual %in% c("Gd")] <-4
house_data$KitchenQual_dum[house_data$KitchenQual %in% c("Ex")] <-3
house_data$KitchenQual_dum[house_data$KitchenQual %in% c("Fa")] <-2
house_data$KitchenQual_dum[house_data$KitchenQual %in% c("NA")] <-1

#Functional
house_data$Functional_dum[house_data$Functional %in% c("Typ")] <-8
house_data$Functional_dum[house_data$Functional %in% c("Maj1")] <-7
house_data$Functional_dum[house_data$Functional %in% c("Maj2")] <-6
house_data$Functional_dum[house_data$Functional %in% c("Min1")] <-5
house_data$Functional_dum[house_data$Functional %in% c("Min2")] <-4
house_data$Functional_dum[house_data$Functional %in% c("Mod")] <-3
house_data$Functional_dum[house_data$Functional %in% c("Sev")] <-2
house_data$Functional_dum[house_data$Functional %in% c("NA")] <-1

#FireplaceQu
house_data$FireplaceQu_dum[house_data$FireplaceQu %in% c("TA")] <-5
house_data$FireplaceQu_dum[house_data$FireplaceQu %in% c("Gd")] <-4
house_data$FireplaceQu_dum[house_data$FireplaceQu %in% c("Ex")] <-3
house_data$FireplaceQu_dum[house_data$FireplaceQu %in% c("Fa")] <-2
house_data$FireplaceQu_dum[house_data$FireplaceQu %in% c("NA")] <-0
house_data$FireplaceQu_dum[house_data$FireplaceQu %in% c("Po")] <-6
summary(house_data$FireplaceQu_dum)

#GarageType
house_data$GarageType_dum[house_data$GarageType %in% c("BuiltIn")] <-7
house_data$GarageType_dum[house_data$GarageType %in% c("Attchd")] <-6
house_data$GarageType_dum[house_data$GarageType %in% c("Detchd")] <-5
house_data$GarageType_dum[house_data$GarageType %in% c("CarPort")] <-4
house_data$GarageType_dum[house_data$GarageType %in% c("Basment")] <-3
house_data$GarageType_dum[house_data$GarageType %in% c("2Types")] <-2
house_data$GarageType_dum[house_data$GarageType %in% c("NA")] <-1

#GarageFinish
house_data$GarageFinish_dum[house_data$GarageFinish %in% c("Fin")] <-4
house_data$GarageFinish_dum[house_data$GarageFinish %in% c("RFn")] <-3
house_data$GarageFinish_dum[house_data$GarageFinish %in% c("Unf")] <-2
house_data$GarageFinish_dum[house_data$GarageFinish %in% c("NA")] <-1

#GarageQual
house_data$GarageQual_dum[house_data$GarageQual %in% c("Ex")] <-6
house_data$GarageQual_dum[house_data$GarageQual %in% c("TA")] <-5
house_data$GarageQual_dum[house_data$GarageQual %in% c("Gd")] <-4
house_data$GarageQual_dum[house_data$GarageQual %in% c("Po")] <-3
house_data$GarageQual_dum[house_data$GarageQual %in% c("Fa")] <-2
house_data$GarageQual_dum[house_data$GarageQual %in% c("NA")] <-1

#GarageCond
house_data$GarageCond_dum[house_data$GarageCond %in% c("Ex")] <-6
house_data$GarageCond_dum[house_data$GarageCond %in% c("Fa")] <-2
house_data$GarageCond_dum[house_data$GarageCond %in% c("Gd")] <-4
house_data$GarageCond_dum[house_data$GarageCond %in% c("Po")] <-3
house_data$GarageCond_dum[house_data$GarageCond %in% c("NA")] <-1
house_data$GarageCond_dum[house_data$GarageCond %in% c("TA")] <-5
summary(house_data$GarageCond_dum)
#PavedDrive
house_data$PavedDrive_dum[house_data$PavedDrive %in% c("Y")] <-1
house_data$PavedDrive_dum[house_data$PavedDrive %in% c("N")] <-0
house_data$PavedDrive_dum[house_data$PavedDrive %in% c("P")] <-2
summary(house_data$PavedDrive_dum)
#PoolQC
house_data$PoolQC_dum[house_data$PoolQC %in% c("Ex")] <-6
house_data$PoolQC_dum[house_data$PoolQC %in% c("Fa")] <-2
house_data$PoolQC_dum[house_data$PoolQC %in% c("Gd")] <-4
house_data$PoolQC_dum[house_data$PoolQC %in% c("NA")] <-3

summary(house_data$PoolQC_dum)
#Fence
house_data$Fence_dum[house_data$Fence %in% c("GdPrv")] <-5
house_data$Fence_dum[house_data$Fence %in% c("GdWo")] <-4
house_data$Fence_dum[house_data$Fence %in% c("MnPrv")] <-3
house_data$Fence_dum[house_data$Fence %in% c("MnWw")] <-2
house_data$Fence_dum[house_data$Fence %in% c("NA")] <-1

#MiscFeature
house_data$MiscFeature_dum[house_data$MiscFeature %in% c("Shed")] <-5
house_data$MiscFeature_dum[house_data$MiscFeature %in% c("Gar2")] <-4
house_data$MiscFeature_dum[house_data$MiscFeature %in% c("Othr")] <-3
house_data$MiscFeature_dum[house_data$MiscFeature %in% c("TenC")] <-2
house_data$MiscFeature_dum[house_data$MiscFeature %in% c("NA")] <-1

#SaleType
house_data$SaleType_dum[house_data$SaleType %in% c("COD")] <-1
house_data$SaleType_dum[house_data$SaleType %in% c("Con")] <-9
house_data$SaleType_dum[house_data$SaleType %in% c("ConLD")] <-8
house_data$SaleType_dum[house_data$SaleType %in% c("ConLI")] <-7
house_data$SaleType_dum[house_data$SaleType %in% c("ConLw")] <-6
house_data$SaleType_dum[house_data$SaleType %in% c("CWD")] <-5
house_data$SaleType_dum[house_data$SaleType %in% c("New")] <-4
house_data$SaleType_dum[house_data$SaleType %in% c("Oth")] <-3
house_data$SaleType_dum[house_data$SaleType %in% c("WD")] <-2
house_data$SaleType_dum[house_data$SaleType %in% c("NA")] <-0

#SaleCondition
house_data$SaleCondition_dum[house_data$SaleCondition %in% c("Abnorml")] <-1
house_data$SaleCondition_dum[house_data$SaleCondition %in% c("AdjLand")] <-2
house_data$SaleCondition_dum[house_data$SaleCondition %in% c("Alloca")] <-3
house_data$SaleCondition_dum[house_data$SaleCondition %in% c("Family")] <-4
house_data$SaleCondition_dum[house_data$SaleCondition %in% c("Normal")] <-5
house_data$SaleCondition_dum[house_data$SaleCondition %in% c("Partial")] <-6

str(house_data)
house_final <- house_data[,-c(3,6,7,8,9,10,11,12,13,14,15,16,17,22,23,24,25,26,28,29,30,31,32,33,34,36,40,41,43)]
str(house_final)

house_final <- house_final[,-c(14,25,27,29,30,32,35,36,37,44,45,46,50)]
str(house_final)

house_final <- house_final[,-c(80,79,78,38)]
house_final <- house_final[,-c(43)]
str(house_final)
md.pattern(house_final)
colSums(sapply(house_final, is.na))

#1.
house_final$LotFrontage[which(is.na(house_final$LotFrontage))] <- median(house_final$LotFrontage,na.rm = T)
summary(house_final$LotFrontage)
#2.BsmtFinSF2
house_final$BsmtFinSF2[which(is.na(house_final$BsmtFinSF2))] <- median(house_final$BsmtFinSF2,na.rm = T)
str(house_final$BsmtFinSF2)

#3.MasVnrArea
house_final$MasVnrArea[which(is.na(house_final$MasVnrArea))] <- median(house_final$MasVnrArea,na.rm = T)
summary(house_final$MasVnrArea)

#4.BsmtFinSF1
house_final$BsmtFinSF1[which(is.na(house_final$BsmtFinSF1))] <- median(house_final$BsmtFinSF1,na.rm = T)
str(house_final$BsmtFinSF1)

#6.BsmtUnfSF
house_final$BsmtUnfSF[which(is.na(house_final$BsmtUnfSF))] <- median(house_final$BsmtUnfSF,na.rm = T)
str(house_final$BsmtUnfSF)

#7.TotalBsmtSF 
house_final$TotalBsmtSF[which(is.na(house_final$TotalBsmtSF))] <- median(house_final$TotalBsmtSF,na.rm = T)
str(house_final$TotalBsmtSF)

#8. BsmtFullBath
house_final$BsmtFullBath[which(is.na(house_final$BsmtFullBath))] <- median(house_final$BsmtFullBath,na.rm = T)
summary(house_final$BsmtFullBath)

#9.BsmtHalfBath
house_final$BsmtHalfBath[which(is.na(house_final$BsmtHalfBath))] <- median(house_final$BsmtHalfBath,na.rm = T)
summary(house_final$BsmtHalfBath)
#12.GarageCars
house_final$GarageCars[which(is.na(house_final$GarageCars))] <- median(house_final$GarageCars,na.rm = T)
summary(house_final$GarageCars)

#
#12.GarageYrBlt
house_final$GarageYrBlt[which(is.na(house_final$GarageYrBlt))] <- median(house_final$GarageYrBlt,na.rm = T)
summary(house_final$GarageYrBlt)



#13.Utilities_dum
house_final$Utilities_dum[which(is.na(house_final$Utilities_dum))] <- median(house_final$Utilities_dum,na.rm = T)
summary(house_final$Utilities_dum)


#14.Exterior1st_dum
house_final$Exterior1st_dum[which(is.na(house_final$Exterior1st_dum))] <- median(house_final$Exterior1st_dum,na.rm = T)
summary(house_final$Exterior1st_dum)

#15.zone
house_final$zone[which(is.na(house_final$zone))] <- median(house_final$zone,na.rm = T)
summary(house_final$zone)

#16.MasVnrType_dum
house_final$MasVnrType_dum[which(is.na(house_final$MasVnrType_dum))] <- median(house_final$MasVnrType_dum,na.rm = T)
summary(house_final$MasVnrType_dum)


#17.Exterior2nd_dum
house_final$Exterior2nd_dum[which(is.na(house_final$Exterior2nd_dum))] <- median(house_final$Exterior2nd_dum,na.rm = T)
summary(house_final$Exterior2nd_dum)


#18.BsmtQual_dum
house_final$BsmtQual_dum[which(is.na(house_final$BsmtQual_dum))] <- median(house_final$BsmtQual_dum,na.rm = T)
summary(house_final$BsmtQual_dum)

##BsmtExposure_dum
house_final$BsmtExposure_dum[which(is.na(house_final$BsmtExposure_dum))] <- median(house_final$BsmtExposure_dum,na.rm = T)
summary(house_final$BsmtExposure_dum)


#19.BsmtCond_dum
house_final$BsmtCond_dum[which(is.na(house_final$BsmtCond_dum))] <- median(house_final$BsmtCond_dum,na.rm = T)
summary(house_final$BsmtCond_dum)

#20.BsmtFinType1_dum
house_final$BsmtFinType1_dum[which(is.na(house_final$BsmtFinType1_dum))] <- median(house_final$BsmtFinType1_dum,na.rm = T)
summary(house_final$BsmtFinType1_dum)

#21.BsmtFinType2_dum
house_final$BsmtFinType2_dum[which(is.na(house_final$BsmtFinType2_dum))] <- median(house_final$BsmtFinType2_dum,na.rm = T)
summary(house_final$BsmtFinType2_dum)

#22. Heating_dum
house_final$Heating_dum[which(is.na(house_final$Heating_dum))] <- median(house_final$Heating_dum,na.rm = T)
summary(house_final$Heating_dum)

#23. Electrical_dum
house_final$Electrical_dum[which(is.na(house_final$Electrical_dum))] <- median(house_final$Electrical_dum,na.rm = T)
summary(house_final$Electrical_dum)

#24. KitchenQual_dum
house_final$KitchenQual_dum[which(is.na(house_final$KitchenQual_dum))] <- median(house_final$KitchenQual_dum,na.rm = T)
summary(house_final$KitchenQual_dum)

#25. Functional_dum
house_final$Functional_dum[which(is.na(house_final$Functional_dum))] <- median(house_final$Functional_dum,na.rm = T)
summary(house_final$Functional_dum)

#26. GarageType_dum
house_final$GarageType_dum[which(is.na(house_final$GarageType_dum))] <- median(house_final$GarageType_dum,na.rm = T)
summary(house_final$GarageType_dum)

#27. GarageFinish_dum 
house_final$GarageFinish_dum[which(is.na(house_final$GarageFinish_dum))] <- median(house_final$GarageFinish_dum,na.rm = T)
summary(house_final$GarageFinish_dum)

#28. GarageQual_dum
house_final$GarageQual_dum[which(is.na(house_final$GarageQual_dum))] <- median(house_final$GarageQual_dum,na.rm = T)
summary(house_final$GarageQual_dum)

#29. GarageCond_dum
house_final$GarageCond_dum[which(is.na(house_final$GarageCond_dum))] <- median(house_final$GarageCond_dum,na.rm = T)
summary(house_final$GarageCond_dum)

#30. SaleType_dum
house_final$SaleType_dum[which(is.na(house_final$SaleType_dum))] <- median(house_final$SaleType_dum,na.rm = T)
summary(house_final$SaleType_dum)


#30. FireplaceQu_dum
house_final$FireplaceQu_dum[which(is.na(house_final$FireplaceQu_dum))] <- median(house_final$FireplaceQu_dum,na.rm = T)
summary(house_final$FireplaceQu_dum)

#Missing number adjustment
str(house_final)
md.pattern(house_final)

summary(house_final$LotArea)

Missing_NAs <- sapply(house_final, function(x) sum(is.na(x)))
Miss_summary <- data.frame(Index = names(house_final), Missing_values = Missing_NAs )
Miss_summary[Miss_summary$Missing_values > 0,]



#####

Train <- house_final[1:1460,]
Test <- house_final[1461:2919,]

colSums(sapply(Train, is.na))

colSums(sapply(Test, is.na))
md.pattern(Train)

dim(Test)
dim(Train)
str(Train)
str(Test)
## regression
Test$SalePrice <- 0

house_reg <- lm(SalePrice~., Train)
summary(house_reg)

predict.test <- predict(house_reg,Test)
predict.test

write.csv(predict.test, "Predicted.house.value.csv")


## Lasso regression
library(glmnet)
Test$SalePrice <- 0
Test$ GarageArea[which(is.na(Test$ GarageArea))] <- median(Test$ GarageArea,na.rm = T)
summary(Test$GarageArea)
str(Test)
colSums(sapply(Test, is.na))
sum(is.na(Test))
Y <- log(Train$SalePrice)
X <- model.matrix(Id~.,Train)
pred <- model.matrix(Id~.,Test)

lasso.fit<-glmnet(x = X, y = Y, alpha = 1)
crossval <- cv.glmnet(x = X, y = Y, alpha =1)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)
lasso.opt.fit<-glmnet(x = X, y = Y, alpha = 1, lambda = penalty.lasso) 

coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = pred))
lasso.testing
write.csv(lasso.testing,"Final_Predicted_value.csv")


##log
str(Train)



fit.log <- lm(log(SalePrice) ~log(MSSubClass+1)+log(LotFrontage+1)+log(LotArea+1)+log(OverallQual+1)+log(OverallCond+1)+log(YearBuilt+1)+log(YearRemodAdd+1)+
                log(MasVnrArea+1)+log(BsmtFinSF1+1)+log(BsmtFinSF2+1)+log(BsmtUnfSF+1)+log(TotalBsmtSF+1)+log(X1stFlrSF+1)+log(X2ndFlrSF+1)+log(LowQualFinSF+1)+log(GrLivArea+1)
              +log(BsmtFullBath+1)+log(BsmtHalfBath+1)+log(FullBath+1)+log(HalfBath+1)+log( BedroomAbvGr+1)+log(KitchenAbvGr+1)+log( TotRmsAbvGrd+1)+log(Fireplaces+1)+log(GarageYrBlt+1)+
                log(GarageCars+1)+log(GarageArea+1)+log(WoodDeckSF+1)+log(WoodDeckSF+1)+log(OpenPorchSF+1)+log( YrSold+1) +MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF
              +TotalBsmtSF+GarageYrBlt + GarageCars + zone + Street_dum +Alley_dum + LotShape_dum +Utilities_dum +LotConfig_dum + LandSlope_dum+Neighborhood_dum
              +Condition1_dum+Condition2_dum+BldgType_dum+ HouseStyle_dum+RoofStyle_dum+RoofMatl_dum+Exterior1st_dum+Exterior2nd_dum+MasVnrType_dum
              +ExterQual_dum+ExterCond_dum+SaleCondition_dum+Foundation_dum+BsmtQual_dum+BsmtCond_dum+BsmtExposure_dum+BsmtFinType1_dum+BsmtFinType2_dum+Heating_dum +HeatingQC_dum+CentralAir_dum+Electrical_dum
              +KitchenQual_dum + Functional_dum + FireplaceQu_dum + GarageType_dum + GarageFinish_dum + GarageQual_dum+ GarageCond_dum + PavedDrive_dum + SaleType_dum, Train)
              

predict.log <- exp(predict(fit.log,Test))
write.csv(lasso.testing,"log_Final_Predicted_value.csv")

##
Test$SalePrice <- 0
Test$ GarageArea[which(is.na(Test$ GarageArea))] <- median(Test$ GarageArea,na.rm = T)
summary(Test$GarageArea)
str(Test)
colSums(sapply(Test, is.na))
sum(is.na(Test))

Y <- log(Train$SalePrice)
X <- model.matrix(Id~log(MSSubClass+1)+log(LotFrontage+1)+log(LotArea+1)+log(OverallQual+1)+log(OverallCond+1)+log(YearBuilt+1)+log(YearRemodAdd+1)+
                    log(MasVnrArea+1)+log(BsmtFinSF1+1)+log(BsmtFinSF2+1)+log(BsmtUnfSF+1)+log(TotalBsmtSF+1)+log(X1stFlrSF+1)+log(X2ndFlrSF+1)+log(LowQualFinSF+1)+log(GrLivArea+1)
                  +log(BsmtFullBath+1)+log(BsmtHalfBath+1)+log(FullBath+1)+log(HalfBath+1)+log( BedroomAbvGr+1)+log(KitchenAbvGr+1)+log( TotRmsAbvGrd+1)+log(Fireplaces+1)+log(GarageYrBlt+1)+
                    log(GarageCars+1)+log(GarageArea+1)+log(WoodDeckSF+1)+log(WoodDeckSF+1)+log(OpenPorchSF+1)+log( YrSold+1) +MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF
                  +TotalBsmtSF+GarageYrBlt + GarageCars + zone + Street_dum +Alley_dum + LotShape_dum +Utilities_dum +LotConfig_dum + LandSlope_dum+Neighborhood_dum
                  +Condition1_dum+Condition2_dum+BldgType_dum+ HouseStyle_dum+RoofStyle_dum+RoofMatl_dum+Exterior1st_dum+Exterior2nd_dum+MasVnrType_dum
                  +ExterQual_dum+ExterCond_dum+SaleCondition_dum+Foundation_dum+BsmtQual_dum+BsmtCond_dum+BsmtExposure_dum+BsmtFinType1_dum+BsmtFinType2_dum+Heating_dum +HeatingQC_dum+CentralAir_dum+Electrical_dum
                  +KitchenQual_dum + Functional_dum + FireplaceQu_dum + GarageType_dum + GarageFinish_dum + GarageQual_dum+ GarageCond_dum + PavedDrive_dum + SaleType_dum + SalePrice,Train)

pred <- model.matrix(Id~log(MSSubClass+1)+log(LotFrontage+1)+log(LotArea+1)+log(OverallQual+1)+log(OverallCond+1)+log(YearBuilt+1)+log(YearRemodAdd+1)+
                       log(MasVnrArea+1)+log(BsmtFinSF1+1)+log(BsmtFinSF2+1)+log(BsmtUnfSF+1)+log(TotalBsmtSF+1)+log(X1stFlrSF+1)+log(X2ndFlrSF+1)+log(LowQualFinSF+1)+log(GrLivArea+1)
                     +log(BsmtFullBath+1)+log(BsmtHalfBath+1)+log(FullBath+1)+log(HalfBath+1)+log( BedroomAbvGr+1)+log(KitchenAbvGr+1)+log( TotRmsAbvGrd+1)+log(Fireplaces+1)+log(GarageYrBlt+1)+
                       log(GarageCars+1)+log(GarageArea+1)+log(WoodDeckSF+1)+log(WoodDeckSF+1)+log(OpenPorchSF+1)+log( YrSold+1) +MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF
                     +TotalBsmtSF+GarageYrBlt + GarageCars + zone + Street_dum +Alley_dum + LotShape_dum +Utilities_dum +LotConfig_dum + LandSlope_dum+Neighborhood_dum
                     +Condition1_dum+Condition2_dum+BldgType_dum+ HouseStyle_dum+RoofStyle_dum+RoofMatl_dum+Exterior1st_dum+Exterior2nd_dum+MasVnrType_dum
                     +ExterQual_dum+ExterCond_dum+SaleCondition_dum+Foundation_dum+BsmtQual_dum+BsmtCond_dum+BsmtExposure_dum+BsmtFinType1_dum+BsmtFinType2_dum+Heating_dum +HeatingQC_dum+CentralAir_dum+Electrical_dum
                     +KitchenQual_dum + Functional_dum + FireplaceQu_dum + GarageType_dum + GarageFinish_dum + GarageQual_dum+ GarageCond_dum + PavedDrive_dum + SaleType_dum + SalePrice,Test)

lasso.fit<-glmnet(x = X, y = Y, alpha = 1)
crossval <- cv.glmnet(x = X, y = Y, alpha =1)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)
lasso.opt.fit<-glmnet(x = X, y = Y, alpha = 1, lambda = penalty.lasso) 

coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = pred))
lasso.testing
write.csv(lasso.testing,"Final_3_Predicted_value.csv")







