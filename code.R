library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)

## Reading in round2.csv as csv and comapnies.txt as TAB deliminted file

rounds2 <- read.csv("rounds2.csv", stringsAsFactors = F)
companies <- read.delim("companies.txt", sep = "\t", stringsAsFactors = F)

## Changing font of round2$company_permalink and company$permalink to lower to allow
## during join operation later

rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)

## Questions Table 1.1
## Unique companies in rounds2? Key is Companies$permalink

noofuniquecompanies <- n_distinct(companies$permalink, na.rm = T)
noofuniquecompanies_rounds2 <- n_distinct(rounds2$company_permalink, na.rm = T)

## Are there any companies in round2 file that are not there in companies file Answer Y 39 companies

companiesnotincompanies <- setdiff(rounds2$company_permalink, companies$permalink)


## Merging two data frames with inner_join in order to map every transaction against list of 
## Companies

master_frame <- inner_join(rounds2, companies, 
                           by = c("company_permalink"="permalink"))

## Replacing Blanks in columns country_code, state_code, region, city with NA 
## As its not possible to interpolate a value for these dimensions

master_frame$country_code[master_frame$country_code == ""]  <- NA_character_
master_frame$state_code[master_frame$state_code == ""]  <- NA_character_
master_frame$region[master_frame$region == ""]  <- NA_character_
master_frame$city[master_frame$city == ""]  <- NA_character_


## Table 2.1 Questions

## Average Investment by funding type and type of funding meeting criteria of
## between 5 and 15 million USD

## Subset for four Investment types

master_frame1 <- subset(master_frame, 
                        funding_round_type == 'venture' | 
                          funding_round_type == 'angel' | 
                          funding_round_type == 'seed' | 
                          funding_round_type == 'private_equity' &
                          !is.na(country_code))

## Group by Investment types and provide mean value of investments in millions of dollars
master_frameinvesttype <- group_by(master_frame1, 
                                   funding_round_type)
avginvestmentbytype <- summarise(master_frameinvesttype, 
                                 meaninvestment = mean(raised_amount_usd, na.rm = T))

## Only one type of Investment meets the criteria 5 - 15 million USD:: "venture"

## Table 3.1 Top nine countries which have recieved highest total funding
## Across Sectors for chosen funding_round_type <- "venture'

# Subset of master_frame with only venture type funding
master_frame1 <- subset(master_frame, 
                        funding_round_type == 'venture')

master_frame1 <- group_by(master_frame1, country_code)
investmentbycountry <- summarise(master_frame1, 
                                 suminvestment = sum(raised_amount_usd, na.rm = T))
investmentbycountry <- na.omit(investmentbycountry)
investmentbycountry <- data.frame(investmentbycountry, row.names = NULL)

## Sort for top nine countries ? and put in dataframe with name top9
investmentbycountry <- investmentbycountry[order(-investmentbycountry$suminvestment),]
top9 <- investmentbycountry[1:9,]


## Reading in mapping.csv as 


#identifying top 3 english speaking countries
#using the pdf shared of english speaking countries to create excel file english_speaking.csv
#please find excel english_speaking.csv file attached too with submission

english_speaking <- read.csv("english_speaking.csv", stringsAsFactors = F)

#merging data frames top9 and english_speaking to find the english speaking countries in top9

top3<-merge(top9, english_speaking, by="country_code")
#sorting top3
top3 <- top3[order(-top3$suminvestment),]
top3 <- top3[1:3,]

## Completed up till this point till q3


#Checkpoint4-Sector Analysis

#Extract the primary sector of each category list from the category_list column

master_frame1<-separate(master_frame1,category_list,into=c("primary_sector","sub_sector1","sub_sector2"),sep="\\|")

#Use the mapping file 'mapping.csv' to map each primary sector to one of the 
#eight main sectors (Note that 'Others' is also considered one of the main sectors)

mapping <- read.csv("mapping.csv", stringsAsFactors = F)
master_frame2<-merge(master_frame1,mapping,by.x="primary_sector",by.y="category_list")
master_frame2<-master_frame2[(!master_frame2$Blanks==1),]
master_frame2<-gather(master_frame2,main_sector,My_Val,Automotive...Sports:Social..Finance..Analytics..Advertising)
master_frame2<-master_frame2[(!master_frame2$My_Val==0),]
master_frame2<-master_frame2[,-19]


#Checkpoint 5: Sector Analysis 2

#Create three separate data frames D1, D2 and D3 for each of the three countries
#containing the observations of funding type FT falling within the 5-15 million
#USD range. The three data frames should contain:
D1<-subset(master_frame2,master_frame2$country_code==top3$country_code[1])
D2<-subset(master_frame2,master_frame2$country_code==top3$country_code[2])
D3<-subset(master_frame2,master_frame2$country_code==top3$country_code[3])

#1. # Table -5.1 (1) Total number of investments (count) for top 3 countries

Investment_decision <- function(p)
{
  decision<-nrow(p)
  return(decision)
}

for(i in 1:nrow(top3))
{
  country <- top3[i,] 
  person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i])
  top3[i,"No_Of_Investment"] <- Investment_decision(person)
} 

# # Table -5.1 (2) Total amount of investment (USD) for top 3 countries
# Total investment For First country

Investment_decision <- function(p)
{
  decision<-sum(p$raised_amount_usd,na.rm = TRUE)
  return(decision)
}

for(i in 1:nrow(top3))
{
  country <- top3[i,] 
  person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i])
  top3[i,"Total_sum_Investment"] <- Investment_decision(person)
} 
# # Table -5.1 (3) Top Sector name (no. of investment-wise)

Investment_decision <- function(p)
{
  max_Tcol <-summarise(group_by(p,main_sector),count=n())
  max_Tcol1 <- max(max_Tcol$count)
  index_max_Tcol <- which(max_Tcol$count == max_Tcol1)
  decision<- max_Tcol$main_sector[index_max_Tcol]
  return(decision)
}

for(i in 1:nrow(top3))
{
  country <- top3[i,] 
  person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i])
  top3[i,"Top_sector"] <- Investment_decision(person)
}

# # Table -5.1 (4) Second Sector name (no. of investment-wise)


Investment_decision <- function(p)
{
  max_Tcol <-summarise(group_by(p,main_sector),count=n())
  max_Tcol1 <- max(max_Tcol$count)
  index_max_Tcol <- which(max_Tcol$count == max_Tcol1)
  max_2_Tcol <- max(max_Tcol$count[-index_max_Tcol]) 
  index_max_2_Tcol <- which(max_Tcol$count == max_2_Tcol)
  decision<- max_Tcol$main_sector[index_max_2_Tcol]
  return(decision)
}

for(i in 1:nrow(top3))
{
  country <- top3[i,] 
  person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i])
  top3[i,"Top_second_sector"] <- Investment_decision(person)
}
## Table -5.1 (5)  Third Sector name (no. of investment-wise)


Investment_decision <- function(p)
{
  max_Tcol <-summarise(group_by(p,main_sector),count=n())
  max_Tcol1 <- max(max_Tcol$count)
  index_max_Tcol <- which(max_Tcol$count == max_Tcol1)
  max_2_Tcol <- max(max_Tcol$count[-index_max_Tcol]) 
  index_max_2_Tcol <- which(max_Tcol$count == max_2_Tcol)
  max_3_Tcol <- max(max_Tcol$count[max_Tcol$count!=max_Tcol1 & max_Tcol$count !=max_2_Tcol])
  index_max_3_Tcol <- which(max_Tcol$count == max_3_Tcol)
  decision<- max_Tcol$main_sector[index_max_3_Tcol]
  return(decision)
}

for(i in 1:nrow(top3))
{
  country <- top3[i,] 
  person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i])
  top3[i,"Top_third_sector"] <- Investment_decision(person)
}
# One function created for all 3 question as below
## Table -5.1 (6) Number of investments in top sector (3)
# # Table -5.1 (7) Number of investments in second sector (4)
# # Table -5.1 (8) Number of investments in third sector (5)

Investment_decision <- function(p)
{
  max_Tcol <-summarise(group_by(p,main_sector),count=n())
  decision<- max_Tcol$count
  return(decision)
}

for(i in 1:nrow(top3))
{
   person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i]
                  &master_frame2$main_sector==top3$Top_sector[i])
   top3[i,"Top_sector_Investment"] <- Investment_decision(person)
  
   person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i]
                  &master_frame2$main_sector==top3$Top_second_sector[i])
   top3[i,"Top_2nd_sector_Investment"] <- Investment_decision(person)
  
  person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i]
                 &master_frame2$main_sector==top3$Top_third_sector[i])
  top3[i,"Top_3rd_sector_Investment"] <- Investment_decision(person)
}



## Table -5.1 (9)  For point 3 (top sector count-wise), which company received the highest 
#investment?
# Table -5.1 (10) For point 4 (second best sector count-wise), 
#which company received the highest investment?

# Below code is used for both the answer 9 and 10, New function designed 

Investment_decision <- function(p)
{
  
  p_company<-aggregate(p$raised_amount_usd,by=list(p$name),FUN=sum,na.rm=TRUE)
  p_comp_max<-max(p_company$x)
  p_comp_max_index<-which(p_company$x==p_comp_max)
  decision<-p_company$Group.1[p_comp_max_index]
  return(decision)
}

for(i in 1:nrow(top3))
{
  person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i]
                 &master_frame2$main_sector==top3$Top_sector[i])
  top3[i,"Top_Company"] <- Investment_decision(person)
  
  person<-subset(master_frame2,master_frame2$country_code==top3$country_code[i]
                 &master_frame2$main_sector==top3$Top_second_sector[i])
  top3[i,"Top_second_Company"] <- Investment_decision(person)
}

# End of Q5 

#Checkpoint 6
#writing required dataframes to .csv files for plotting

write.csv(avginvestmentbytype, file = 'investment_by_type.csv')

write.csv(top9, file = 'top9.csv')

write.csv(top3, file = 'top3.csv')
