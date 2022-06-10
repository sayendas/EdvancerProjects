#PROJECT 1)-----------------------------------------------------------------------------------------------------------------------------

#data prep----------------------------------------------------------------------

setwd('D:/EDVANCER/R/Project 1')

housing_train=read.csv('housing_train.csv')    #read train data

housing_test=read.csv('housing_test.csv')    #read test data

library(dplyr)


housing_train$data='train'    #add new col for identifying train daa

housing_test$data='test'      #add new col for identifying test data

housing_test$Price=NA      #add new col Price(target variable) as combining data requires same cols in both train and test


housing_all=rbind(housing_train,housing_test)   #combine train and test data

glimpse(housing_all)


sapply(housing_all,function(x) sum(is.na(x)))    #many have NA values


housing_all=housing_all%>%select(-Address)    #as address has so many unique values,treat it as ID col and drop it


glimpse(housing_all)   


housing_all$Postcode=as.character(housing_all$Postcode)   #while postcode is given as int,but technically it should be char,
                                                          #as it does not make sense to do numeric operations there


housing_all=housing_all%>%                                #bedroom2,bathroom,car,landsize,buildingarea in a business context
      group_by(Rooms)%>%                                  #in real life scenerios,depends on number of rooms,so we fill up 
      mutate(Bedroom2=mean(Bedroom2,na.rm=T),             #all NA values with group mean of number of rooms
             Bathroom=mean(Bathroom,na.rm=T),
             Car=mean(Car,na.rm=T),
             Landsize=mean(Landsize,na.rm=T),
             BuildingArea=mean(BuildingArea,na.rm=T)
             )


table(housing_all$CouncilArea)           #we find there are many empty(not NA) values for this col,we have decided to replace them 
                                         #with missing


housing_all=housing_all%>%                #replace empty strings with missing
       mutate(CouncilArea=if_else(CouncilArea=="",'missing',CouncilArea))


sum(is.na(housing_all$YearBuilt))/nrow(housing_all)     #YearBuilt has around 50% NA data,so rather than replace them with median,
                                                        #lets create flag variable,as NA might be an actual business case


housing_all=housing_all%>%                  #creating flag variable for YearBuilt
      mutate(YearBuilt_flag_NA=as.numeric(is.na(YearBuilt),0,1))%>%
      select(-YearBuilt)



CreateDummies=function(data,var,freq_cutoff=0){     #create dummies func
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for(cat in categories){
    name=paste(var,cat,sep='_')
    name=gsub(" ",'',name)
    name=gsub('-','_',name)
    name=gsub('\\?','Q',name)
    name=gsub('<','LT_',name)
    name=gsub('\\+','',name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


glimpse(housing_all)


table(housing_all$Suburb)

table(housing_all$Type)

table(housing_all$Method)

table(housing_all$SellerG)

table(housing_all$Postcode)

table(housing_all$CouncilArea)


sapply(housing_all,function(x) sum(is.na(x)))    #checking once again for NAN values,we see that we have 3 NAN values for 
                                                 #bedroom2,bathroom,car,landsize,buildingArea,we need to handle them.
                                                 #As for Price,those NA values are our desired once,
                                                 #we need to predict values for those



housing_all$Bedroom2[is.nan(housing_all$Bedroom2)]=max(housing_all$Bedroom2,na.rm = T)

housing_all$Bathroom[is.nan(housing_all$Bathroom)]=max(housing_all$Bathroom,na.rm = T)

housing_all$Car[is.nan(housing_all$Car)]=max(housing_all$Car,na.rm = T)

housing_all$Landsize[is.nan(housing_all$Landsize)]=max(housing_all$Landsize,na.rm = T)

housing_all$BuildingArea[is.nan(housing_all$BuildingArea)]=max(housing_all$BuildingArea,na.rm = T)


sapply(housing_all,function(x) sum(is.na(x)))    #Now apart from price,we have no other Vars which have NAs


glimpse(housing_all)


housing_all=CreateDummies(housing_all,'Suburb',100)    #creating dummies

housing_all=CreateDummies(housing_all,'Type',100)

housing_all=CreateDummies(housing_all,'Method',100)

housing_all=CreateDummies(housing_all,'SellerG',100)

housing_all=CreateDummies(housing_all,'Postcode',100)

housing_all=CreateDummies(housing_all,'CouncilArea',100)


glimpse(housing_all)


housing_train=housing_all%>%                    #breaking data preped complete data into train and test
              filter(data=='train')%>%
              select(-data)

housing_test=housing_all%>%
             filter(data=='test')%>%
             select(-data)


#linear regression model--------------------------------------------------------

#we break out train data into 2 parts(70-30) for model building and validation

s=sample(nrow(housing_train),0.7*nrow(housing_train))
housing_train1=housing_train[s,]
housing_train2=housing_train[-s,]

#build model on train1

train1.fit=lm(Price~.,data = housing_train1)

library(car)

sort(vif(train1.fit),decreasing = T)

summary(train1.fit)

train1.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
              -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
              -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
              -Postcode_3072-Postcode_3165-Postcode_3073,data = housing_train1)

sort(vif(train1.fit),decreasing = T)

train1.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
              -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
              -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
              -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2,data = housing_train1)

sort(vif(train1.fit),decreasing = T)

train1.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
              -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
              -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
              -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car,data = housing_train1)

sort(vif(train1.fit),decreasing = T)

train1.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
              -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
              -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
              -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea,data = housing_train1)

sort(vif(train1.fit),decreasing = T)

train1.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
              -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
              -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
              -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea-Rooms,data = housing_train1)

sort(vif(train1.fit),decreasing = T)

train1.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
              -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
              -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
              -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea-Rooms
              -Suburb_MalvernEast,data = housing_train1)

sort(vif(train1.fit),decreasing = T)

train1.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
              -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
              -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
              -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea-Rooms
              -Suburb_MalvernEast-Postcode_3121,data = housing_train1)

sort(vif(train1.fit),decreasing = T)

train1.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124           #we take VIF threshold as <10
              -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
              -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
              -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea-Rooms
              -Suburb_MalvernEast-Postcode_3121-CouncilArea_missing,data = housing_train1)

sort(vif(train1.fit),decreasing = T)

train1.fit=step(train1.fit)    #remove values based on AIC score

formula(train1.fit)

train1.fit=lm(Price ~ Distance + Bathroom + Landsize + YearBuilt_flag_NA + 
                Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + 
                Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + Suburb_BrightonEast + 
                Suburb_Hawthorn + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + 
                Method_PI + Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
                SellerG_RT + SellerG_Fletchers + SellerG_Buxton + SellerG_Marshall + 
                SellerG_Jellis + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
                Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
                Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + 
                Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 + 
                Postcode_3020 + CouncilArea_Brimbank + CouncilArea_Melbourne + 
                CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_MooneeValley + 
                CouncilArea_Moreland + CouncilArea_Boroondara-CouncilArea_GlenEira,data=housing_train1)

summary(train1.fit)

train1.fit=lm(Price ~ Distance + Bathroom + Landsize + YearBuilt_flag_NA + 
                Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + 
                Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + Suburb_BrightonEast + 
                Suburb_Hawthorn + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + 
                Method_PI + Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
                SellerG_RT + SellerG_Fletchers + SellerG_Buxton + SellerG_Marshall + 
                SellerG_Jellis + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
                Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
                Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + 
                Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 + 
                Postcode_3020 + CouncilArea_Brimbank + CouncilArea_Melbourne + 
                CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_MooneeValley + 
                CouncilArea_Moreland + CouncilArea_Boroondara-CouncilArea_GlenEira-SellerG_Buxton,data=housing_train1)

summary(train1.fit)

train1.fit=lm(Price ~ Distance + Bathroom + Landsize + YearBuilt_flag_NA + 
                Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + 
                Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + Suburb_BrightonEast + 
                Suburb_Hawthorn + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + 
                Method_PI + Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
                SellerG_RT + SellerG_Fletchers + SellerG_Buxton + SellerG_Marshall + 
                SellerG_Jellis + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
                Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
                Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + 
                Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 + 
                Postcode_3020 + CouncilArea_Brimbank + CouncilArea_Melbourne + 
                CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_MooneeValley + 
                CouncilArea_Moreland + CouncilArea_Boroondara-CouncilArea_GlenEira-SellerG_Buxton
               -SellerG_McGrath,data=housing_train1)

summary(train1.fit)

train1.fit=lm(Price ~ Distance + Bathroom + Landsize + YearBuilt_flag_NA + 
                Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + 
                Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + Suburb_BrightonEast + 
                Suburb_Hawthorn + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + 
                Method_PI + Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
                SellerG_RT + SellerG_Fletchers + SellerG_Buxton + SellerG_Marshall + 
                SellerG_Jellis + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
                Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
                Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + 
                Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 + 
                Postcode_3020 + CouncilArea_Brimbank + CouncilArea_Melbourne + 
                CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_MooneeValley + 
                CouncilArea_Moreland + CouncilArea_Boroondara-CouncilArea_GlenEira-SellerG_Buxton
              -SellerG_McGrath-Suburb_Footscray,data=housing_train1)

summary(train1.fit)

train1.fit=lm(Price ~ Distance + Bathroom + Landsize + YearBuilt_flag_NA + 
                Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + 
                Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + Suburb_BrightonEast + 
                Suburb_Hawthorn + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + 
                Method_PI + Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
                SellerG_RT + SellerG_Fletchers + SellerG_Buxton + SellerG_Marshall + 
                SellerG_Jellis + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
                Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
                Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + 
                Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 + 
                Postcode_3020 + CouncilArea_Brimbank + CouncilArea_Melbourne + 
                CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_MooneeValley + 
                CouncilArea_Moreland + CouncilArea_Boroondara-CouncilArea_GlenEira-SellerG_Buxton
              -SellerG_McGrath-Suburb_Footscray-Postcode_3147,data=housing_train1)

summary(train1.fit)

train1.fit=lm(Price ~ Distance + Bathroom + Landsize + YearBuilt_flag_NA + 
                Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + 
                Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + Suburb_BrightonEast + 
                Suburb_Hawthorn + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + 
                Method_PI + Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
                SellerG_RT + SellerG_Fletchers + SellerG_Buxton + SellerG_Marshall + 
                SellerG_Jellis + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
                Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
                Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + 
                Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 + 
                Postcode_3020 + CouncilArea_Brimbank + CouncilArea_Melbourne + 
                CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_MooneeValley + 
                CouncilArea_Moreland + CouncilArea_Boroondara-CouncilArea_GlenEira-SellerG_Buxton
              -SellerG_McGrath-Suburb_Footscray-Postcode_3147-Postcode_3145,data=housing_train1)

summary(train1.fit)

train1.fit=lm(Price ~ Distance + Bathroom + Landsize + YearBuilt_flag_NA + 
                Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + 
                Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + Suburb_BrightonEast + 
                Suburb_Hawthorn + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + 
                Method_PI + Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
                SellerG_RT + SellerG_Fletchers + SellerG_Buxton + SellerG_Marshall + 
                SellerG_Jellis + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
                Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
                Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + 
                Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 + 
                Postcode_3020 + CouncilArea_Brimbank + CouncilArea_Melbourne + 
                CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_MooneeValley + 
                CouncilArea_Moreland + CouncilArea_Boroondara-CouncilArea_GlenEira-SellerG_Buxton
              -SellerG_McGrath-Suburb_Footscray-Postcode_3147-Postcode_3145-Method_PI,data=housing_train1)

summary(train1.fit)

train1.fit=lm(Price ~ Distance + Bathroom + Landsize + YearBuilt_flag_NA + 
                Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + 
                Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + Suburb_BrightonEast + 
                Suburb_Hawthorn + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + 
                Method_PI + Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
                SellerG_RT + SellerG_Fletchers + SellerG_Buxton + SellerG_Marshall + 
                SellerG_Jellis + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
                Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
                Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + 
                Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 + 
                Postcode_3020 + CouncilArea_Brimbank + CouncilArea_Melbourne + 
                CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_MooneeValley + 
                CouncilArea_Moreland + CouncilArea_Boroondara-CouncilArea_GlenEira-SellerG_Buxton
              -SellerG_McGrath-Suburb_Footscray-Postcode_3147-Postcode_3145-Method_PI-Postcode_3012,data=housing_train1)

summary(train1.fit)

train1.fit=lm(Price ~ Distance + Bathroom + Landsize + YearBuilt_flag_NA +     #drop vars based on pvalues
                Suburb_Doncaster + Suburb_Footscray + Suburb_Thornbury + 
                Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + Suburb_BrightonEast + 
                Suburb_Hawthorn + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
                Suburb_Brunswick + Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                Suburb_BentleighEast + Suburb_Reservoir + Type_u + Type_h + 
                Method_PI + Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
                SellerG_RT + SellerG_Fletchers + SellerG_Buxton + SellerG_Marshall + 
                SellerG_Jellis + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
                Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
                Postcode_3188 + Postcode_3012 + Postcode_3204 + Postcode_3058 + 
                Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3046 + 
                Postcode_3020 + CouncilArea_Brimbank + CouncilArea_Melbourne + 
                CouncilArea_Banyule + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
                CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_MooneeValley + 
                CouncilArea_Moreland + CouncilArea_Boroondara-CouncilArea_GlenEira-SellerG_Buxton
              -SellerG_McGrath-Suburb_Footscray-Postcode_3147-Postcode_3145-Method_PI-Postcode_3012-Suburb_Brunswick,data=housing_train1)

summary(train1.fit)

train2.pred=predict(train1.fit,newdata = housing_train2)

((train2.pred)-(housing_train2$Price))**2%>%mean()%>%sqrt()    #RMSE on train2 data=404883.5,score on train2=0.524



train.fit=lm(Price~.,data = housing_train)   #building model on entire train data

sort(vif(train.fit),decreasing = T)

summary(train.fit)

train.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
              -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
              -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
              -Postcode_3072-Postcode_3165-Postcode_3073,data = housing_train)

sort(vif(train.fit),decreasing = T)

train.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
             -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
             -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
             -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2,data = housing_train)

sort(vif(train.fit),decreasing = T)

train.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
             -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
             -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
             -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car,data = housing_train)

sort(vif(train.fit),decreasing = T)

train.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
             -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
             -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
             -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea,data = housing_train)

sort(vif(train.fit),decreasing = T)

train.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
             -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
             -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
             -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea-Rooms,data = housing_train)

sort(vif(train.fit),decreasing = T)

train.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
             -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
             -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
             -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea-Rooms-Postcode_3121,data = housing_train)

sort(vif(train.fit),decreasing = T)

train.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
             -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
             -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
             -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea-Rooms-Postcode_3121-Suburb_MalvernEast,data = housing_train)

sort(vif(train.fit),decreasing = T)

train.fit=lm(Price~.-Postcode_3039-Postcode_3071-Postcode_3013-Postcode_3103-Postcode_3124
             -Postcode_3207-Postcode_3044-Postcode_3187-Postcode_3122-Postcode_3104-Postcode_3070
             -Postcode_3101-Postcode_3186-Postcode_3146-Postcode_3056-Postcode_3141-Postcode_3182
             -Postcode_3072-Postcode_3165-Postcode_3073-Bedroom2-Car-BuildingArea-Rooms-Postcode_3121-Suburb_MalvernEast
             -CouncilArea_missing,data = housing_train)

sort(vif(train.fit),decreasing = T)


train.fit=step(train.fit)

summary(train.fit)

formula(train.fit)

train.fit=lm(Price ~ Distance + Bathroom + Landsize + Suburb_Doncaster + Suburb_Footscray + 
               Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
               Suburb_PortMelbourne + Suburb_PascoeVale + Suburb_BrightonEast + 
               Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + 
               Suburb_GlenIris + Suburb_Brunswick + Suburb_SouthYarra + 
               Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
               Suburb_Reservoir + Type_u + Type_h + Method_PI + Method_S + 
               SellerG_Kay + SellerG_Miles + SellerG_Sweeney + SellerG_RT + 
               SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + 
               SellerG_Nelson + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
               Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
               Postcode_3188 + Postcode_3084 + Postcode_3012 + Postcode_3204 + 
               Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + 
               Postcode_3046 + Postcode_3020 + CouncilArea_Whitehorse + 
               CouncilArea_Brimbank + CouncilArea_Melbourne + CouncilArea_PortPhillip + 
               CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
               CouncilArea_GlenEira + CouncilArea_MooneeValley + CouncilArea_Moreland + 
               CouncilArea_Boroondara-Postcode_3084,data = housing_train)

summary(train.fit)

train.fit=lm(Price ~ Distance + Bathroom + Landsize + Suburb_Doncaster + Suburb_Footscray + 
               Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
               Suburb_PortMelbourne + Suburb_PascoeVale + Suburb_BrightonEast + 
               Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + 
               Suburb_GlenIris + Suburb_Brunswick + Suburb_SouthYarra + 
               Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
               Suburb_Reservoir + Type_u + Type_h + Method_PI + Method_S + 
               SellerG_Kay + SellerG_Miles + SellerG_Sweeney + SellerG_RT + 
               SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + 
               SellerG_Nelson + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
               Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
               Postcode_3188 + Postcode_3084 + Postcode_3012 + Postcode_3204 + 
               Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + 
               Postcode_3046 + Postcode_3020 + CouncilArea_Whitehorse + 
               CouncilArea_Brimbank + CouncilArea_Melbourne + CouncilArea_PortPhillip + 
               CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
               CouncilArea_GlenEira + CouncilArea_MooneeValley + CouncilArea_Moreland + 
               CouncilArea_Boroondara-Postcode_3084-Suburb_PascoeVale,data = housing_train)

summary(train.fit)

train.fit=lm(Price ~ Distance + Bathroom + Landsize + Suburb_Doncaster + Suburb_Footscray + 
               Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
               Suburb_PortMelbourne + Suburb_PascoeVale + Suburb_BrightonEast + 
               Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + 
               Suburb_GlenIris + Suburb_Brunswick + Suburb_SouthYarra + 
               Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
               Suburb_Reservoir + Type_u + Type_h + Method_PI + Method_S + 
               SellerG_Kay + SellerG_Miles + SellerG_Sweeney + SellerG_RT + 
               SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + 
               SellerG_Nelson + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
               Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
               Postcode_3188 + Postcode_3084 + Postcode_3012 + Postcode_3204 + 
               Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + 
               Postcode_3046 + Postcode_3020 + CouncilArea_Whitehorse + 
               CouncilArea_Brimbank + CouncilArea_Melbourne + CouncilArea_PortPhillip + 
               CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
               CouncilArea_GlenEira + CouncilArea_MooneeValley + CouncilArea_Moreland + 
               CouncilArea_Boroondara-Postcode_3084-Suburb_PascoeVale-SellerG_Sweeney,data = housing_train)

summary(train.fit)

train.fit=lm(Price ~ Distance + Bathroom + Landsize + Suburb_Doncaster + Suburb_Footscray + 
               Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
               Suburb_PortMelbourne + Suburb_PascoeVale + Suburb_BrightonEast + 
               Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + 
               Suburb_GlenIris + Suburb_Brunswick + Suburb_SouthYarra + 
               Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
               Suburb_Reservoir + Type_u + Type_h + Method_PI + Method_S + 
               SellerG_Kay + SellerG_Miles + SellerG_Sweeney + SellerG_RT + 
               SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + 
               SellerG_Nelson + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
               Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
               Postcode_3188 + Postcode_3084 + Postcode_3012 + Postcode_3204 + 
               Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + 
               Postcode_3046 + Postcode_3020 + CouncilArea_Whitehorse + 
               CouncilArea_Brimbank + CouncilArea_Melbourne + CouncilArea_PortPhillip + 
               CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
               CouncilArea_GlenEira + CouncilArea_MooneeValley + CouncilArea_Moreland + 
               CouncilArea_Boroondara-Postcode_3084-Suburb_PascoeVale-SellerG_Sweeney-SellerG_Nelson,data = housing_train)

summary(train.fit)

train.fit=lm(Price ~ Distance + Bathroom + Landsize + Suburb_Doncaster + Suburb_Footscray + 
               Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
               Suburb_PortMelbourne + Suburb_PascoeVale + Suburb_BrightonEast + 
               Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + 
               Suburb_GlenIris + Suburb_Brunswick + Suburb_SouthYarra + 
               Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
               Suburb_Reservoir + Type_u + Type_h + Method_PI + Method_S + 
               SellerG_Kay + SellerG_Miles + SellerG_Sweeney + SellerG_RT + 
               SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + 
               SellerG_Nelson + Postcode_3147 + Postcode_3145 + Postcode_3127 + 
               Postcode_3081 + Postcode_3031 + Postcode_3181 + Postcode_3015 + 
               Postcode_3188 + Postcode_3084 + Postcode_3012 + Postcode_3204 + 
               Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + 
               Postcode_3046 + Postcode_3020 + CouncilArea_Whitehorse + 
               CouncilArea_Brimbank + CouncilArea_Melbourne + CouncilArea_PortPhillip + 
               CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
               CouncilArea_GlenEira + CouncilArea_MooneeValley + CouncilArea_Moreland + 
               CouncilArea_Boroondara-Postcode_3084-Suburb_PascoeVale-SellerG_Sweeney-SellerG_Nelson-SellerG_hockingstuart,data = housing_train)

summary(train.fit)

train.fit


test.pred=predict(train.fit,newdata = housing_test)   #predict values on test data

sum(is.na(test.pred))      #no NA values on test predictions

sum(test.pred<0)

setwd('D:/EDVANCER/R/Project 1')


#DT model-----------------------------------------------------------------------

library(tree)
library(randomForest)

train.tree.fit=tree::tree(Price~.,data = housing_train)   #build DT model on entire train data

train.tree.fit

plot(train.tree.fit)
text(train.tree.fit)

test.tree.pred=predict(train.tree.fit,newdata = housing_test)   #predict values for test data

test.tree.pred


#RF model-----------------------------------------------------------------------

params=list(mtry=c(40,50,60,80,100,110),               #rf parameters
               ntree=c(200,300,500,700,1000),
               maxnodes=c(5,10,15,20,50),
               nodesize=c(1,2,5,10,20))


para_subset=function(param_list,n=10){
  
             all_comb=expand.grid(param_list)
             
             sam=sample(nrow(all_comb),n)
             
             param_subset=all_comb[sam,]
             
             return(param_subset)
}


num_trials=20

subset_params=para_subset(params,num_trials)    #take 20 comb 

subset_params

library(cvTools)


myerror=999999            #we take a very high value of error

for(i in 1:num_trials){
  
  print(paste0('Starting iteration',i))
  
  current_param=subset_params[i,]
  
  k=cvTuning(randomForest,
             tuning=current_param,
             seed=2,
             Price~.,
             data=housing_train,
             folds = cvFolds(nrow(housing_train),K=10,type='random')
             )
  
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    
    print(current_param)
    
    myerror=score.this
    
    print(myerror)
    
    best_params=current_param
  }
  
  print('DONE')
}



params2=list(mtry=c(60,80),               #rf parameters(round2)
            ntree=c(100,200),
            maxnodes=c(10,50),
            nodesize=c(10,30,50))

num_trials=24

subset_params2=para_subset(params2,num_trials)    #take 24 comb 

subset_params2


myerror2=417474.1            #we take a high value of error(least error from previous round)

for(i in 1:num_trials){
  
  print(paste0('Starting iteration',i))
  
  current_param2=subset_params2[i,]
  
  k=cvTuning(randomForest,
             tuning=current_param2,
             seed=2,
             Price~.,
             data=housing_train,
             folds = cvFolds(nrow(housing_train),K=10,type='random')
  )
  
  score.this2=k$cv[,2]
  
  if(score.this2<myerror2){
    
    print(current_param2)
    
    myerror2=score.this2
    
    print(myerror2)
    
    best_params2=current_param2
  }
  
  print('DONE')
}



params3=list(mtry=c(60,70),               #rf parameters(round3)
             ntree=c(50,100,150),
             maxnodes=c(50,100),
             nodesize=c(50,100))

num_trials=24

subset_params3=para_subset(params3,num_trials)    #take 24 comb 

subset_params3


myerror3=412839.1            #we take a high value of error(least error from previous round)

for(i in 1:num_trials){
  
  print(paste0('Starting iteration',i))
  
  current_param3=subset_params3[i,]
  
  k=cvTuning(randomForest,
             tuning=current_param3,
             seed=2,
             Price~.,
             data=housing_train,
             folds = cvFolds(nrow(housing_train),K=10,type='random')
  )
  
  score.this3=k$cv[,2]
  
  if(score.this3<myerror3){
    
    print(current_param3)
    
    myerror3=score.this3
    
    print(myerror3)
    
    best_params3=current_param3
  }
  
  print('DONE')
}

best_params3    #60   150      100      100

myerror3   #388759.8


best_params3=data.frame(best_params3)


train.rf.final=randomForest(Price~.,                      #build model on entire train dataset
                            mtry=best_params3$mtry,
                            ntree=best_params3$ntree,
                            maxnodes=best_params3$maxnodes,
                            nodesize=best_params3$nodesize,
                            data=housing_train)

test.rf.pred=predict(train.rf.final,newdata = housing_test)

test.rf.pred

setwd('D:/EDVANCER/R/Project 1')

write.csv(test.rf.pred,'Sayen_Das_P1_part2.csv',col.names = T,row.names = F)

write.table(test.rf.pred,file='Sayen_Das_P1_part2.csv',col.names = 'Price',row.names = F)
