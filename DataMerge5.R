#Just GROCERY categories

library(dplyr)
library(caTools)
library(reshape2)
#Importing datasets
getwd()
setwd("/Users/kunlelawal/Desktop/Masters/MGT 6203/Project/Final Table")
campaign_desc = read.csv("campaign_desc.csv")
campaign_table = read.csv("campaign_table.csv")
#causal_data = read.csv("causal_data.csv")
coupon_redempt = read.csv("coupon_redempt.csv")
coupon = read.csv("coupon.csv")
hh_demographic = read.csv("hh_demographic.csv")
product = read.csv("product.csv")
transaction_full = read.csv("transaction_data.csv")

#This code adds 2 indicator variables to the dataset to show whether the household received the campaign (campaignReceived)
#and whether the product purchased was on promo as part of that campaign (productPromo)
#I just did this for the DRUGS GM category, will need to be done again later for the GROCERY category

#Finding campaign that was sent to the most households
#Campaign 18 was sent to the most households (1133), followed by 13 (1077) and then 8 (1076)
as.data.frame(table(campaign_table$CAMPAIGN)) 


transaction_full <- transaction_full[transaction_full$SALES_VALUE <= 600,]

selectedProd <- subset(product, product$DEPARTMENT=="GROCERY", select = c("PRODUCT_ID","BRAND")) #39021 observations
#Merging campaign_table with its description so we can see the details (DAYS, etc.)
campaignDeets<- merge(x=campaign_table, y=campaign_desc, on="CAMPAIGN") 
#Creating table of households that received campaign 18
received18<- subset(campaignDeets, campaignDeets$CAMPAIGN==18)
#Finding which Grocery products were part of campaign and which weren't 
prod18<- subset(coupon, coupon$CAMPAIGN==18) #Coupons that were in campaign 18, 37589 coupons
#Subsetting coupons in campaign 18 to just include those for Drugs GM
couponDrugs<- merge(x=prod18, y=selectedProd, on="PRODUCT") #11486 coupons for Drug GM and Grocery products for campaign 18
#There are duplicates in this dataset (getting rid of COUPON_UPC since not using), also duplicate PRODUCT_IDs
couponDrugsNoDup <- subset(couponDrugs, select = -COUPON_UPC)
couponDrugsNoDup<-unique(couponDrugsNoDup) #10351 DRUG GM and GROCERY products on promo as part of campaign 18
#Adding a column of 1's to dataframe to be indicator variable in final dataset for whether drug product was on promo
couponDrugsNoDup["productPromo"]<-1

#Subsetting to only show transactions that were for Drugs GM
transProd<- merge(x=transaction_full, y=selectedProd, on="PRODUCT_ID") #1646076 transactions
#Creating indicator variable for if the household that made transactions received campaign 18
transCampaign<- merge(x=transProd, y=received18, on="household_key", all.x=TRUE) #1646076 transactions
#If campaign is missing then that household didnt receive campaign
transCampaign$campaignReceived<-ifelse(is.na(transCampaign$CAMPAIGN),0,1) 

#Creating indicator variable for whether product was in campaign by merging datasets
transCampaign2<- merge(x=transCampaign, y=couponDrugsNoDup, on="PRODUCT", all.x=TRUE) #1646076 transactions
#Changing NAs to 0s for indicator variable
transCampaign2$productPromo[is.na(transCampaign2$productPromo)]<-0

transCampaign4<- merge(x=transCampaign2, y=hh_demographic, on="household_key", all.x=TRUE)

#Creating variable to indicate if the transaction was before, during or after the promo period
transCampaign4$campaign_state[transCampaign4$DAY>642]<- "After"
transCampaign4$campaign_state[transCampaign4$DAY<587]<- "Before"
transCampaign4$campaign_state[transCampaign4$DAY>=587 & transCampaign4$DAY<=642]<- "During"

#Checking variable creation
as.data.frame(table(transCampaign4$campaign_state))

#Adding in whether a coupon was used for a transaction
#Merging coupon and coupon_redempt to get more info about the coupon that was redeemed
couponInfo<- merge(x=coupon, y=coupon_redempt, on=c("COUPON_UPC","CAMPAIGN"), all.y=TRUE)
#Getting rid of COUPON_UPC column since not using 
couponRedeemed <- subset(couponInfo, select = -COUPON_UPC)
#Only taking unique observations since duplicates
couponRedeemed<- unique(couponRedeemed)
#Adding a column of 1's to dataframe to be indicator variable in final dataset for whether a coupon was redeemed for this product
couponRedeemed["couponUsed"]<-1

#Merging coupon redeemed dataset with transaction dataset for campaign 18 and 
transCampaign5<- merge(x=transCampaign4, y=couponRedeemed, on=c("household_key", "DAY", "PRODUCT_ID"), all.x=TRUE)
#Running frequency of couponUsed variable
as.data.frame(table(transCampaign5$couponUsed)) #741 coupons were redeemed for DRUGS GM products in campaign 18
#Changing NAs to 0s for couponUsed indicator variable
transCampaign5$couponUsed[is.na(transCampaign5$couponUsed)]<-0
#Running a frequency tables of productPromo and couponUsed to make sure the coupons used were on promo
#table(transCampaign5$productPromo,transCampaign5$couponUsed)

#Examining when most of the coupons were used
#Campaign 18 ran from day 587 - 642
#70% of coupons that got redemmed were used within 28 days (4 weeks) of when the campaign began 
#days 587-615 so we will examine a 28 day period
CouponDay<-subset(transCampaign5, transCampaign5$couponUsed==1)
hist(CouponDay$DAY)
as.data.frame(table(CouponDay$DAY))

#Restricting datasets to the 28 days before the promo began through the 28 days after the promo began and then 28 days
#of the period after (Before: 557-586, During: 587-616, After: 643-672)
transCampaign6<-subset(transCampaign5, 558<=transCampaign5$DAY & transCampaign5$DAY<=615 | 
                         643<=transCampaign5$DAY & transCampaign5$DAY<=671)

#Finding households where coupon was used and applying indicator to all of the transactions for that household
#29 individual households redeemed the 168 coupons
hhUsedCoupon<-unique(subset(transCampaign6, transCampaign6$couponUsed==1, select = c("household_key")))
#Adding hhCoupon to dataset
hhUsedCoupon["hhCouponUsed"]<-1

#Merge with full dataset to create indicator variable for if household ever used a coupon
transCampaign7<- merge(x=transCampaign6, y=hhUsedCoupon, on=c("household_key"), all.x=TRUE)
#Changing NAs to 0s for couponUsed indicator variable
transCampaign7$hhCouponUsed[is.na(transCampaign7$hhCouponUsed)]<-0
as.data.frame(table(transCampaign7$hhCouponUsed))


#Aggregating and casting dataset for analysis

transCampaign8<-dcast(transCampaign7,household_key+campaignReceived+hhCouponUsed+AGE_DESC+MARITAL_STATUS_CODE+INCOME_DESC+
                        HOMEOWNER_DESC+HH_COMP_DESC+KID_CATEGORY_DESC~campaign_state, value.var='SALES_VALUE', fun.aggregate = sum)

#Dividing sales value columns (Before, During, After) by 4 so its the average sales per week since keeping 4 weeks of sales in analysis
transCampaign9<-transform(transCampaign8, beforeWeekAvg=Before/4, duringWeekAvg=During/4, afterWeekAvg=After/4)
#Dropping Before, During and After variables
transCampaign10<-subset(transCampaign9, select=-c(Before, During, After))
#Adding a total sales column, sums averages over 12 weeks
transCampaign10$totSales <-transCampaign10$beforeWeekAvg+transCampaign10$duringWeekAvg+transCampaign10$afterWeekAvg

#Write out dataset to evaluate camapign - Kunle
#write.csv(transCampaign10, file="C13_Grocery")


#Creating training and testing datasets from both the aggregated and individual transaction datasets
# Set Seed so that same sample can be reproduced in future also
set.seed(101) 
# split data into train: 70% and test:30% 
##sample_agg = sample.int(n = nrow(transCampaign10), size = floor(.70*nrow(transCampaign10)), replace = F)

data_train_agg = transCampaign10[sample_agg, ]
#y_train=data_train$SALES_VALUE

data_test_agg  = transCampaign10[-sample_agg, ]
#y_test=data_test$SALES_VALUE

#Transaction Level Data
sample_trans = sample.int(n = nrow(transCampaign7), size = floor(.70*nrow(transCampaign7)), replace = F)

data_train_trans = transCampaign7[sample_trans, ]
#y_train=data_train$SALES_VALUE

data_test_trans  = transCampaign7[-sample_trans, ]
#y_test=data_test$SALES_VALUE

#Output datasets
write.csv(data_train_agg, row.names = F, file="GrocerySumTrain.csv")
write.csv(data_test_agg, row.names = F, file="GrocerySumTest.csv")
write.csv(data_train_trans, row.names = F, file="GroceryTransTrain.csv")
write.csv(data_test_trans, row.names = F, file="GroceryTransTest.csv")


#Not using this
#Summing purchases during each period
#transSummary<- transCampaign7 %>% group_by(household_key,campaign_state, campaignReceived, hhCouponUsed) %>% summarise(avgWeeklySales = (sum(SALES_VALUE))/4)
