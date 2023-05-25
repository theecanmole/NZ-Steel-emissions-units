# NZ Steel free industrial allocation of emissions units 2010 to 2021
# https://www.stuff.co.nz/business/130935028/tasman-steel-posts-340m-profit-with-benefit-of-117m-of-free-carbon-credits
# https://www.stuff.co.nz/business/114961557/very-real-risk-nz-steel-could-be-forced-to-pull-out-of-auckland
# https://www.stuff.co.nz/business/farming/83230321/companies-revealed-for-buying-fraudulent-carbon-credits?rm=a
# EPA industrial allocation data https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/industrial-allocations/decisions/
# link @ 11/01/2023 
https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions_2022.xlsx

# LINK ROT https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions.xlsx
# Companies office https://app.companiesoffice.govt.nz/companies/app/ui/pages/companies/421913/detail
# Climate Change (Stationary Energy and Industrial Processes) Regulations 2009 https://www.legislation.govt.nz/regulation/public/2009/0285/latest/DLM2394207.html 
# navigate to folder /NZsteel, select file NZ-Steel-code.r and right click and open with RKward
# what version of R is this?
R.version.string
[1] "R version 4.2.0 (2022-04-22)"
# load packages 
library(readxl)
library(dplyr)
library(RColorBrewer)
library(tidyr)

# check and/or reset working folder
getwd()
[1] "/home/user/R/NZsteel"
setwd("/home/user/R/NZsteel")

# obtain 2010 to 2021 emission unit allocation to industry data from EPA

#download.file("https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions_2022.xlsx","Industrial-Allocations-Final-Decisions_2022.xlsx")

# check names of work sheets
excel_sheets("Industrial-Allocations-Final-Decisions_2022.xlsx")
[1] "IA Final Decisions"

# read in allocation of emissions units data
Allocations <- read_excel("Industrial-Allocations-Final-Decisions_2022.xlsx", sheet = "IA Final Decisions",skip=3)
# check dataframe variables
str(Allocations) 
tibble [1,286 × 4] (S3: tbl_df/tbl/data.frame)
 $ Activity        : chr [1:1207] "Aluminium smelting" "Burnt lime" "Burnt lime" "Burnt lime" ...
 $ Applicant’s name: chr [1:1207] "New Zealand Aluminium Smelters Limited" "Graymont (NZ) Limited" "Holcim (New Zealand) Limited" "Perry Resources (2008) Ltd" ...
 $ Year            : num [1:1207] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Final Allocation: num [1:1207] 210421 47144 3653 4712 948 ...

# revise and shorten column names
colnames(Allocations) <- c("Activity", "Applicant", "Year", "Allocation") 
# check range of variables 
summary(Allocations)   
  Activity          Applicant              Year        Allocation       
 Length:1286        Length:1286        Min.   :2010   Min.   :      1.0  
 Class :character   Class :character   1st Qu.:2012   1st Qu.:    180.8  
 Mode  :character   Mode  :character   Median :2014   Median :   1142.0  
                                       Mean   :2015   Mean   :  47896.6  
                                       3rd Qu.:2018   3rd Qu.:   7580.0  
                                       Max.   :2021   Max.   :2145482.0 
                                       
# separate allocations of emissions units data into years
nzu2010 <- filter(Allocations, Year =="2010")
nzu2011 <- filter(Allocations, Year =="2011")
nzu2012 <- filter(Allocations, Year =="2012")
nzu2013 <- filter(Allocations, Year =="2013")
nzu2014 <- filter(Allocations, Year =="2014")
nzu2015 <- filter(Allocations, Year =="2015")
nzu2016 <- filter(Allocations, Year =="2016")
nzu2017 <- filter(Allocations, Year =="2017")
nzu2018 <- filter(Allocations, Year =="2018")
nzu2019 <- filter(Allocations, Year =="2019") 
nzu2020 <- filter(Allocations, Year =="2020")  
nzu2021 <- filter(Allocations, Year =="2021")

# check just the 2010 data
str(nzu2010)
tibble [141 × 4] (S3: tbl_df/tbl/data.frame)
 $ Activity  : chr [1:141] "Aluminium smelting" "Burnt lime" "Burnt lime" "Burnt lime" ...
 $ Applicant : chr [1:141] "New Zealand Aluminium Smelters Limited" "Graymont (NZ) Limited" "Holcim (New Zealand) Limited" "Perry Resources (2008) Ltd" ...
 $ Year      : num [1:141] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Allocation: num [1:141] 210421 47144 3653 4712 948 ...
 
# Each year, usually in May, the EPA makes a 'provisional' allocation of emssion units to selected industries. see https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/industrial-allocations/ I want to estimate the market value of free allocation of units. I understand that the deadline for a provisional allocation is 30 April of each year so I assume the transfer of the allocation to NZ Steel is made in May of each year. There is an online 'open data' Github repository of New Zealand Unit (NZU) prices going back to May 2010. https://github.com/theecanmole/nzu
# The NZU repository has it's own citation and DOI: Theecanmole. (2016). New Zealand emission unit (NZU) monthly prices 2010 to 2016: V1.0.01 [Data set]. Zenodo. http://doi.org/10.5281/zenodo.221328
# I will add a NZU market price value at the May average price from 2010 to 2021
nzu2010[["Value"]] <- nzu2010[["Allocation"]]*17.58
nzu2011[["Value"]] <- nzu2011[["Allocation"]]*19.84
nzu2012[["Value"]] <- nzu2012[["Allocation"]]*6.23
nzu2013[["Value"]] <- nzu2013[["Allocation"]]*1.94
nzu2014[["Value"]] <- nzu2014[["Allocation"]]*4.08
nzu2015[["Value"]] <- nzu2015[["Allocation"]]*5.34
nzu2016[["Value"]] <- nzu2016[["Allocation"]]*14.63
nzu2017[["Value"]] <- nzu2017[["Allocation"]]*16.96
nzu2018[["Value"]] <- nzu2018[["Allocation"]]*21.28
nzu2019[["Value"]] <- nzu2019[["Allocation"]]*25.29 
nzu2020[["Value"]] <- nzu2020[["Allocation"]]*24.84
nzu2021[["Value"]] <- nzu2021[["Allocation"]]*37.14
# add the average May spot prices
# Mayprice <-c(17.58, 19.84,6.23,1.94,4.08,5.34,  14.63, 16.96 , 21.28 , 25.29 , 24.84 ,37.14)

# combine all the year data together into 1 dataframe - I use rbind as all the column names are consistent
Allocations <- rbind(nzu2010,nzu2011,nzu2012,nzu2013,nzu2014,nzu2015,nzu2016,nzu2017,nzu2018,nzu2019,nzu2020,nzu2021)

# check the new dataframe
str(Allocations)
 
# read my csv data file back into R if needed
Allocations <- read.csv("Allocations.csv") 
tibble [1,286 × 5] (S3: tbl_df/tbl/data.frame)
 $ Activity  : chr [1:1286] "Aluminium smelting" "Burnt lime" "Burnt lime" "Burnt lime" ...
 $ Applicant : chr [1:1286] "New Zealand Aluminium Smelters Limited" "Graymont (NZ) Limited" "Holcim (New Zealand) Limited" "Perry Resources (2008) Ltd" ...
 $ Year      : num [1:1286] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Allocation: num [1:1286] 210421 47144 3653 4712 948 ...
 $ Value     : num [1:1286] 3699201 828792 64220 82837 16666 ... 
# Create a .csv formatted data file
write.csv(Allocations, file = "Allocations.csv", row.names = FALSE)
 
filter(Allocations, Applicant =="New Zealand Steel Development Limited") 
# A tibble: 12 × 5
   Activity                                    Applicant  Year Allocation  Value
   <chr>                                       <chr>     <dbl>      <dbl>  <dbl>
 1 Iron and steel manufacturing from iron sand New Zeal…  2010     494704 8.70e6
 2 Iron and steel manufacturing from iron sand New Zeal…  2011     989304 1.96e7
 3 Iron and steel manufacturing from iron sand New Zeal…  2012    1003730 6.25e6
 4 Iron and steel manufacturing from iron sand New Zeal…  2013    1029352 2.00e6
 5 Iron and steel manufacturing from iron sand New Zeal…  2014    1073489 4.38e6
 6 Iron and steel manufacturing from iron sand New Zeal…  2015    1067501 5.70e6
 7 Iron and steel manufacturing from iron sand New Zeal…  2016    1048116 1.53e7
 8 Iron and steel manufacturing from iron sand New Zeal…  2017    1432496 2.43e7
 9 Iron and steel manufacturing from iron sand New Zeal…  2018    1782366 3.79e7
10 Iron and steel manufacturing from iron sand New Zeal…  2019    2118983 5.36e7
11 Iron and steel manufacturing from iron sand New Zeal…  2020    2030166 5.04e7
12 Iron and steel manufacturing from iron sand New Zeal…  2021    2145482 7.97e7 

# create dataframe for the allocation of units to NZ Steel
NZsteelunits <- filter(Allocations, Applicant =="New Zealand Steel Development Limited")  

str(NZsteelunits) 
tibble [12 × 5] (S3: tbl_df/tbl/data.frame)
$ Activity  : chr [1:12] "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" ...
$ Applicant : chr [1:12] "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" ...
$ Year      : num [1:12] 2010 2011 2012 2013 2014 ...
$ Allocation: num [1:12] 494704 989304 1003730 1029352 1073489 ...
$ Value     : num [1:12] 8696896 19627791 6253238 1996943 4379835 ...
 
# omit first two columns as they are redundant
NZsteelunits <- NZsteelunits[,3:5]

str(NZsteelunits)
tibble [12 × 3] (S3: tbl_df/tbl/data.frame)
 $ Year      : num [1:12] 2010 2011 2012 2013 2014 ...
 $ Allocation: num [1:12] 494704 989304 1003730 1029352 1073489 ...
 $ Value     : num [1:12] 8696896 19627791 6253238 1996943 4379835 ... 

 # Create a .csv formatted data file
write.csv(NZsteelunits, file = "NZsteelunits.csv", row.names = FALSE)
# read csv file back in
NZsteelunits <- read.csv("NZsteelunits.csv") 

------------------------------------------------------------------------------
# 21 May 2023 Sunday download latest detailed emissions data by category from Ministry for the Environment
# https://environment.govt.nz/publications/new-zealands-greenhouse-gas-inventory-1990-2021/

download.file("https://environment.govt.nz/assets/publications/climate-change/Time-series-emissions-data-by-category-presented-in-AR4-Excel-xlsx.xlsx","Time-series-emissions-data-by-category-2021.xlsx")
trying URL 'https://environment.govt.nz/assets/publications/climate-change/Time-series-emissions-data-by-category-presented-in-AR4-Excel-xlsx.xlsx'
trying URL 'https://environment.govt.nz/assets/publications/climate-change/Time-series-emissions-data-by-category-presented-in-AR4-Excel-xlsx.xlsx'
Content type 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' length 1497400 bytes (1.4 MB)
==================================================
downloaded 1.4 MB

# DATED obtain Greenhouse Gas Inventory 1990 to 2020 more detailed time series data from MfE
# download.file("https://environment.govt.nz/assets/publications/GhG-Inventory/Time-series-emissions-data-by-category.xlsx","Time-series-emissions-data-1990-to-2020.xlsx") 
#trying URL 'https://environment.govt.nz/assets/publications/GhG-Inventory/Time-series-emissions-data-by-category.xlsx'
#Content type 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' length 1567250 bytes (1.5 MB)
# ==================================================
#downloaded 1.5 MB
# check number and name of worksheets
excel_sheets("Time-series-emissions-data-by-category-2021.xlsx") 
 [1] "AR4 - All gases" "AR4 - CO2"       "AR4 - CH4"       "AR4 - N2O"      
[5] "AR4 - HFCs"      "AR4 - PFCs"      "AR4 - SF6" 

# read in inventory emissions data
emissions <- read_excel("Time-series-emissions-data-by-category-2021.xlsx", sheet = "AR4 - All gases",skip=10)

# filter out only steel sector emissions being row 371 from column 3 1990 to 2020
steel <- as.numeric(emissions[371,3:34]) 
# check steel vector
str(steel)
 num [1:32] 1307 1421 1499 1535 1417 ...
# print to screen
steel
 [1] 1306.735 1421.220 1498.642 1535.022 1416.737 1493.641 1455.552 1295.650
 [9] 1395.999 1441.968 1429.077 1500.045 1464.779 1658.224 1670.604 1623.167
[17] 1625.343 1645.659 1539.205 1513.433 1696.438 1685.892 1664.743 1708.143
[25] 1732.380 1767.870 1712.053 1758.300 1694.406 1661.611 1578.554 1718.126
# convert from Kilotonnes to tonnes to match free allocation 
steel <-c(steel * 10^3)
str(steel)
num [1:32] 1306735 1421220 1498642 1535022 1416737 ...
# select 2010 to 2021 emissions
steel <- steel[21:32]  
steel
[1] 1696438 1685892 1664743 1708143 1732380 1767870 1712053 1758300 1694406
[10] 1661611 1578554 1718126
length(steel)
[1] 12
# the inventory steel emissions has 12 values which matchs the allocations dataframe 12 rows 2010 to 2021 the 

# OUTDATED add 2021 steel emissions from Participant report 1889288 tonnes
# OUTDATED steel <- c(steel, 1889288)

# add actual steel sector emissions 2010 to 2020 allocation dataframe 
NZsteelunits[["Emissions"]] <- steel  
# check dataframe
str(NZsteelunits)
'data.frame':	12 obs. of  9 variables:
 $ Year          : int  2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 ...
 $ Allocation    : int  494704 989304 1003730 1029352 1073489 1067501 1048116 1432496 1782366 2118983 ...
 $ Value         : num  8696896 19627791 6253238 1996943 4379835 ...
 $ Emissions     : num  1696438 1685892 1664743 1708143 1732380 ...
 $ twoforone     : num  0.25 0.5 0.5 0.5 0.5 0.5 0.5 0.67 0.83 1 ...
 $ ETSliability  : num  424110 842946 832372 854072 866190 ...
 $ Overallocation: num  70594 146358 171358 175280 207299 ...
 $ ERU           : int  NA NA NA 1022527 1001714 32534 NA NA NA NA ...
 $ Stockpile     : num  70594 216953 388311 1586118 2795131 ...


# OUTDATED Are the Inventory steel emissions a good proxy for NZ Steels emissions? Check EPA 2021 "ETS Participant Emissions" report,
#Table 1: Reported ETS emissions and removals by activity, 2020/21 reporting year, Industrial processes Producing iron or steel 54431 (page 12)
# Table 5: Reported participant emissions stationary energy, 2020, New Zealand Steel Limited 762038 (page 20)
# Producing iron or steel 54431 New Zealand Steel Development Limited 54431 (page 25)
# There are 5 opt-in participants who purchase stationary energy. Table 17 details their emissions for the 2020 calendar year. 
# Table 17: Reported participant emissions stationary energy opt-in participants, 2020
# Purchasing coal New Zealand Steel Limited 736875 (page 39)
# 54431 + 762038 + 736875
# [1] 1553344
# Yes the 2020 NZ Steel actual emissions = 1553344 tonnes, 2020 GHG Inventory steel emissions = 1578554, so they are near enough

#Check EPA 2022 "ETS Participant Emissions" report
#page 9 foot note 2 Note: For all sectors except forestry, the emissions and removals reported in the 2021/22 reporting year occurred in #the 2021 calendar year. In the forestry sector, emissions returns approved in the 2021/22 reporting year may correspond with other/longer periods.
#Table 1: Reported ETS emissions and removals by activity, 2021 calendar year, Industrial processes Producing iron or steel 43466 (page 11)
#Table 5: Reported participant emissions stationary energy, 2021, New Zealand Steel Limited 989459 (page 17)
#Table 7: Producing iron or steel New Zealand Steel Development Limited 43466 (page 21)
#There are 5 opt-in participants who purchase stationary energy. Table 17 details their emissions for the 2020 calendar year. 
#Table 17: Reported participant emissions stationary energy opt-in participants 2021
#Purchasing coal New Zealand Steel Limited 856363 (page 39)
#43466 + 989459 + 856363 = 1889288 tonnes 

# Yes the 2021 NZ Steel actual emissions = 1889288 tonnes, 2021 GHG Inventory steel emissions = ??, so they are near enough

# create variable that is the 'two-for-one' discount - 2 tonnes = 1 unit to surrender  https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/participating-in-the-ets/surrendering-units/

# Phase out of the '1 for 2' surrender obligation, Prior to 2017, non-forestry participants had to surrender one eligible unit for every two tonnes of emissions they reported in their annual emissions return, effectively a 50% surrender obligation. 2017 - 1 unit for each 1.5 whole tonnes of emissions 2018 - 1 unit for each 1.2 whole tonnes of emissions 2019 - 1 unit for each 1 whole tonne of emissions"
# 2010 was a half year for ETS so the 'discount' for calculating emissions liability under the ETS is .5 x .5 = 0.25

unitdiscount <- c(0.25,0.5,0.5,0.5,0.5,0.5,0.5,0.67,0.83,1,1,1)
str(unitdiscount)
num [1:12] 0.25 0.5 0.5 0.5 0.5 0.5 0.5 0.67 0.83 1 ... 

# Add unit discount variable to the data frame
NZsteelunits[["twoforone"]] <- unitdiscount

# add variable that is the ETS unit surrender liability - 'Emissions' x 'twoforone' 
NZsteelunits[["ETSliability"]] <- NZsteelunits[["Emissions"]]*NZsteelunits[["twoforone"]]

# add variable that is the emissions footprint of unit allocation 'Allocation' / 2 for 1 unit discount
NZsteelunits[["Footprint"]] <- NZsteelunits[["Allocation"]] / NZsteelunits[["twoforone"]]

# add variable that is the annual surplus, the annual sum of allocated units less the estimate of units surrendered (ETS liability)
NZsteelunits[["Overallocation"]] <- NZsteelunits[["Allocation"]] - NZsteelunits[["ETSliability"]] 

# what is the cumulative over allocation by the end of 2021?
sum(NZsteelunits[["Overallocation"]]/10^6)
[1] 3.113329 # million units

# add internationally sourced emission reduction units (ERUs) held by NZ Steel Limited from the Emissions Unit Register https://www.emissionsregister.govt.nz/Common/ViewPublicReport.aspx?rt=fa64ead4-70b7-4d41-9633-616965b4e2fb
2013 1022527
2014 1001714
2015 32534

# create a vector of emission reduction units (ERUs) of length 12 for years 2010 to 2021
ERU <- c(rep(NA,3) ,c(1022527,1001714,32534) , rep(NA,6)) 
str(ERU)
 num [1:12] NA NA NA 1022527 1001714  
# add ERU to dataframe
NZsteelunits[["ERU"]] <- ERU 

NZsteelunits[["ERU"]]
[1]      NA      NA      NA 1022527 1001714   32534      NA      NA      NA
[10]      NA      NA      NA 

# create new dummy variable 'Stockpile'  12 x NA
Stockpile <- c(rep(NA,12) )

# add up Overallocation values and ERUs into a cumulative Stockpile variable
NZsteelunits[["Stockpile"]][1] <- NZsteelunits[["Overallocation"]][1]
NZsteelunits[["Stockpile"]][2] <- NZsteelunits[["Stockpile"]][1]+NZsteelunits[["Overallocation"]][2]
NZsteelunits[["Stockpile"]][3] <- NZsteelunits[["Stockpile"]][2]+NZsteelunits[["Overallocation"]][3]
NZsteelunits[["Stockpile"]][4] <- NZsteelunits[["Stockpile"]][3]+NZsteelunits[["Overallocation"]][4] + NZsteelunits[["ERU"]][4]
NZsteelunits[["Stockpile"]][5] <- NZsteelunits[["Stockpile"]][4]+NZsteelunits[["Overallocation"]][5] + NZsteelunits[["ERU"]][5]
NZsteelunits[["Stockpile"]][6] <- NZsteelunits[["Stockpile"]][5]+NZsteelunits[["Overallocation"]][6] + NZsteelunits[["ERU"]][6]
NZsteelunits[["Stockpile"]][7] <- NZsteelunits[["Stockpile"]][6]+NZsteelunits[["Overallocation"]][7]
NZsteelunits[["Stockpile"]][8] <- NZsteelunits[["Stockpile"]][7]+NZsteelunits[["Overallocation"]][8]
NZsteelunits[["Stockpile"]][9] <- NZsteelunits[["Stockpile"]][8]+NZsteelunits[["Overallocation"]][9]
NZsteelunits[["Stockpile"]][10] <- NZsteelunits[["Stockpile"]][9]+NZsteelunits[["Overallocation"]][10]
NZsteelunits[["Stockpile"]][11] <- NZsteelunits[["Stockpile"]][10]+NZsteelunits[["Overallocation"]][11]
NZsteelunits[["Stockpile"]][12] <- NZsteelunits[["Stockpile"]][11]+NZsteelunits[["Overallocation"]][12]

# How big is the estimated stockpile after the 2021 calendar year
NZsteelunits[["Stockpile"]][12]
[1] 5170104 # at 21 May 2023
# [1] 4998943 
# or 5,170,104 units or 5.2 million units at end of calendar year 2021

# What is most recent NZU price? From 23 December 2022 NZU spot price? https://www.carbonnews.co.nz/story.asp?storyID=26800

Jan 24, 2023, 18:30:39	https://www.carbonnews.co.nz/story.asp?storyID=26805
carbonspotprice <- 73.00
# What is the market value (if stockpile is still owned by NZ Steel and is an asset on their balance sheet) It's ~ $380 million
NZsteelunits[["Stockpile"]][12] * carbonspotprice
1] 377417623 #  or $377,417,623

# save dataframe as a .csv file 
write.table(NZsteelunits, file = "NZsteelunits.csv", sep = ",", col.names = TRUE, qmethod = "double",row.names = FALSE)

# to read in NZ Steel data again 
NZsteelunits <- read.csv("NZsteelunits.csv")

# check the dataframe
str(NZsteelunits)
'data.frame':	12 obs. of  10 variables:
 $ Year          : int  2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 ...
 $ Allocation    : int  494704 989304 1003730 1029352 1073489 1067501 1048116 1432496 1782366 2118983 ...
 $ Value         : num  8696896 19627791 6253238 1996943 4379835 ...
 $ Emissions     : num  1696438 1685892 1664743 1708143 1732380 ...
 $ twoforone     : num  0.25 0.5 0.5 0.5 0.5 0.5 0.5 0.67 0.83 1 ...
 $ ETSliability  : num  424110 842946 832372 854072 866190 ...
 $ Overallocation: num  70594 146358 171358 175280 207299 ...
 $ ERU           : num  NA NA NA 1022527 1001714 ...
 $ Stockpile     : num  70594 216953 388311 1586118 2795131 ...
 $ Footprint     : num  1978816 1978608 2007460 2058704 2146978 ...  

# select some colours for charting 
display.brewer.all(n=NULL, type="qual", select=NULL, exact.n=TRUE, colorblindFriendly=TRUE)
# plot of 3 palletes, Set2, Paired, Dark2 
display.brewer.all(n=4, type="qual", select="Paired", exact.n=TRUE, colorblindFriendly=TRUE) # light blue,  blue, light green dark green 
display.brewer.pal(7,"Accent")
display.brewer.pal("Dark2",n=8)

brewer.pal("Dark2",n=8)
[1] "#1B9E77"   "#D95F02"   "#7570B3"   "#E7298A"   "#66A61E"   "#E6AB02"   "#A6761D" 
teal/Mountain Meadow russet/Bamboo blue/Deluge pink/Cerise green/Vida Loca mustard/Corn tan\Mandalay

[8] "#666666" Dove gray
# #ff0066 "Rose" (pinky/purple!)
# #ff0000 "red" 
# #6600cc "purple"
# #660066 "pompadour" (another purple)
brewer.pal("Dark2",n=3)
[1] "#1B9E77" "#D95F02" "#7570B3"  # teal khaki mauve
brewer.pal("Dark2",n=4)
[1] "#1B9E77" "#D95F02" "#7570B3" "#E7298A" # teal khaki mauve shocking pink 

# chart of actual NZ Steel emissions
#svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v1.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v1.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1 ,tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,cex=1,pch=16, col="#1B9E77")
legend(2011, 1.2, bty = "n",cex=1.1, "Steel emissions MfE Greenhouse Gas Inventory 2021",col = "#1B9E77", text.col = 1,lty = 1, pch=16 ) 
legend(2011, 1.0, bty = "n",cex=1.1, "NZ Steel emissions EPA Participant Reports 2020 & 2021", col="#E7298A",text.col=1,lty = 1, pch=16)
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Limited greenhouse gas emissions \n2010 to 2021")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2021\nETS Participant Emissions EPA 2020 & 2021")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
points(2020,1553344/10^6,cex=1,pch=16, col="#E7298A")
points(2021,1889288/10^6,cex=1,pch=16, col="#E7298A")
lines(c(2020,2021),c(1553344/10^6,1889288/10^6),lwd=1,col="#E7298A")
dev.off() 

# add units allocated to chart of actual emissions
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v2.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v2.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1,tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
legend(2013, 1, bty = "n",cex=1.1, c("Actual emissions","Emission units allocated"), col =  c("#1B9E77","#D95F02") , text.col = 1, lty = 1, pch = c(16,17))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Limited greenhouse gas emissions and\nemission unit allocation 2010 to 2021"))) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2021, EPA Industrial Allocations")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

# chart actual emissions, units allocated and the ETS liability 
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v3.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v3.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
legend(2011, 2.5, cex=1.1, bty = "n", c("Actual emissions","Emission units allocated","Emissions x 'two for one' equals ETS liability"), col =  c("#1B9E77","#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(16,17,15))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Limited annual allocation of emissions units\n always exceeds the ETS liability 2010 to 2021")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2021\nEPA Industrial Allocations 2021")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

# chart only units allocated and the ETS liability 
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v3a.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v3a.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
#lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
#points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
legend(2011, 2.5, cex=1.1, bty = "n", c("Emission units allocated","Emissions x 'two for one' equals ETS liability"), col =  c("#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(17,15))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Limited annual allocation of emissions units\n always exceeds the ETS liability 2010 to 2021")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2021\nEPA Industrial Allocations 2021")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# chart actual emissions, units allocated and ets liability and overallocation
#svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v4.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))
png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v4.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
legend(2011, 2.5, cex=1.1, bty = "n", c("Actual emissions","Emission units allocated","Emissions x 'two for one' equals ETS liability"), col =  c("#1B9E77","#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(16,17,15))
legend(2011, 0.7, cex=1.1, bty = "n", c("Allocations less ETS liability equals annual surplus of units"), col =  c("#E7298A") , text.col = 1, lty = 1, pch = c(18))
mtext(side=3,cex=1.5,line=-3.5,expression(paste("NZ Steel Limited overallocation of emissions units\n above ETS liability 2010 to 2021"))) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 2021, EPA Industrial Allocations 2021")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# chart actual emissions, units allocated and ets liability and overallocation and add emissions reduction units (ERU) owned
#svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v5.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel")) 
png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v5.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#66A61E",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#66A61E",cex=1.25,lwd=1)
legend(2011, 2.5, cex=1.1, bty = "n", c("Actual emissions","Emission units allocated","Emissions x 'two for one' equals ETS liability","Emission Removal Units ERUs"), col=c("#1B9E77","#D95F02","#7570B3","#66A61E") , text.col = 1, lty = 1, pch = c(16,17,15,16))
legend(2011, 0.65, cex=1.1, bty = "n", c("Allocation less ETS liability equals surplus units"), col=c("#E7298A"), text.col = 1, lty = 1, pch = c(18))
mtext(side=3,cex=1.5,line=-3.5, expression(paste("NZ Steel Limited overallocation of emissions units\n above ETS liability 2010 to 2021"))) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2021, EPA Industrial Allocations 2021")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

# add cumulative stockpile
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v6.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v6.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,ylim=c(0,5.8),tck=0.01,axes=FALSE,ann=FALSE,col="#A6761D",type="l",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
points(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,col="#A6761D",cex=1,pch=19)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#66A61E",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#66A61E",cex=1.25,lwd=1)
legend(2010, 5.25, cex=1, bty = "n", c("Actual Emissions","Emission units allocated","Emissions x 'two for one' equals ETS liability","Emission Reduction Units add to surplus units","Allocation less ETS liability = annual surplus of units","surplus + ERUs =  stockpile of ~ 5 million units 2021?"), col=c("#1B9E77","#D95F02","#7570B3","#66A61E","#E7298A","#A6761D") , text.col = 1, lty = 1, pch = c(16,17,15,16,18,19))
mtext(side=3,cex=1.5,line=-3.5, expression(paste("NZ Steel Limited cumulative stockpile of \nemissions units 2010 2021")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: NZ Greenhouse Gas Inventory 2021, EPA Industrial Allocations 2021")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()


# chart with darkgray for all variables except the cumulative stock pile of emission units
#svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v7.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v7.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,ylim=c(0,5.8),tck=0.01,axes=FALSE,ann=FALSE,col="#A6761D",type="l",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
points(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,col="#A6761D",cex=1,pch=19)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="darkgray",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="darkgray",cex=1.1,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="darkgray",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="darkgray",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="darkgray",cex=1.25,lwd=2)
legend(2010, 5.25, cex=1, bty = "n", c("Actual emissions","Emission units allocated","Emissions x 'two for one' equals ETS Liability","Emission Reduction Units","Allocation less ETS liability = annual surplus of units","surplus + ERUs =  stockpile of ~ 5 million units 2021?"), col=c("darkgray","darkgray","darkgray","darkgray","darkgray","#A6761D") , text.col = 1, lty = 1, pch = c(16,17,15,16,18,19))
mtext(side=3,cex=1.5,line=-3.5, expression(paste("NZ Steel Limited cumulative stockpile of \nemissions units 2010 2021")) )
mtext(side=1,line=-1.25,cex=1, "Data: NZ Greenhouse Gas Inventory 2021 EPA Industrial Allocations 2021")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# chart the actual emissions , the allocation and the footprint of allocation
"#E6AB02" # mustard/Corn

svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v8.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v8.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9), tck=0.01,axes=FALSE,ann=FALSE,col="#A6761D",type="l",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["Footprint"]]/10^6,col="#E6AB02", lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Footprint"]]/10^6,col="#E6AB02",cex=0.9,pch=15 )
legend(2016, 1, cex=1, bty = "n", c("Actual Emissions","Emission units allocated","Emissions footprint of allocation"), col=c("#1B9E77","#D95F02","#E6AB02") , text.col = 1, lty = 1, pch = c(16,17,15))
mtext(side=3,cex=1.5,line=-3.5, expression(paste("NZ Steel Limited emissions footprint of \nallocation of emissions units 2010 2021")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: NZ Greenhouse Gas Inventory 2021, EPA Industrial Allocations 2021")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

