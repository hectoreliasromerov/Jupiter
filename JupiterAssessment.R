library(tidyverse)
library(readr)
library(quantmod)
s 
#Define Alibaba aproximate Cost of Product and Initialize variables
demand <-100
inventory<-demand*1.5
n<-1

#Get latest Canadian Currency
from <- "CAD"
to <- "USD"
FXCADtoUS<-getQuote(paste0(from, to, "=X"))

#Read CSV
a<- read_csv("~/Documents/Wildgreentrading/Jupiter/Beauty US/Beauty US - 0-10k.csv")
Results = data.frame(matrix(NA, nrow = nrow(a), ncol = 15))


#LOOP
for(i in 1:nrow(a)) {

# Select  the Product
ASIN<-a$ASIN[i]

#Clean Up the Price (from Character to Numeric)
a$`Amazon: Current`[i] = as.numeric(gsub("\\$", "", a$`Amazon: Current`[i]))
Price<-gsub("//$","",a$`Amazon: Current`[i])

#Clean Up the Fees (from Character to Numeric)
a$`FBA Pick&Pack Fee $: FBA Pick&Pack Fee`[i] = as.numeric(gsub("\\$", "", a$`FBA Pick&Pack Fee $: FBA Pick&Pack Fee`[i]))
AmazonFee<-a$`FBA Pick&Pack Fee $: FBA Pick&Pack Fee`[i]

#Management o NAs in Price and Amazon Fees.
if (is.na(Price) == TRUE ){Price<-0.1}
Price<-as.numeric(Price)
if (is.na(AmazonFee) == TRUE ){AmazonFee<-100}
AmazonFee<-as.numeric(AmazonFee)

# Aproximated Landed Cost (ALC) 
alc<-(inventory*(Price/4)+500)/inventory

#Landed Cost in Canadian dollars
#alcCND<-alc/FXCADtoUS$Last


#Net Profit Margin Calculation (NPM)
#NPM<-Price-alcCND-AmazonFee # Canada
NPM<-Price-alc-AmazonFee # US



#Return on Investment Calculation (ROI)
#ROI<-NPM/alcCND #Canada
ROI<-NPM/alc #US


#Net Profit Calculation (Net Profit)
NetProfit<-NPM/Price




Reco <- "Not Profitable"

if (NetProfit>0.25) {if(ROI>0.9){Reco<-"Request RFQ"}}

#Data for Deep Dive
DD<-c("Product Name:"= a$Title[i],"Date of Review:" = date(),"Category:"=a$`Categories: Root`[i],"SubCategory:"=a$`Categories: Sub`[i],"Product Rank:"=a$`Sales Rank: Current`[i], "Link"= a$`URL: Amazon`[i], "ASIN:"= ASIN, "Price of Product:"= Price,"Expected Demand:" =round(demand,digits = 0),"Needed Inventory:" =round(inventory,digits=0),"Cost:"= Price/3, "Net Profit Margin:"= NPM, "Return on Investment:"= ROI,"Net Profit:"=NetProfit, "Recomendation:"= Reco)
Results[i, ] = DD
}

#Group the Products worth Investing.
b<-Results %>% filter(X15=="Request RFQ")
d<-Results %>% filter(X15=="Request RFQ") %>% separate(X4,c("Cat1","Cat2","Cat3","Cat4"),sep=",")
ggplot(d) + geom_bar(aes(x = Cat1))

write.csv(d,"BeautyUSCandidates.csv")
