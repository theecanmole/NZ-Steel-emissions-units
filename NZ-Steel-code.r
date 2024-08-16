# NZ Steel free industrial allocation of emissions units 2010 to 2021

# https://www.stuff.co.nz/business/130935028/tasman-steel-posts-340m-profit-with-benefit-of-117m-of-free-carbon-credits
# https://www.stuff.co.nz/business/114961557/very-real-risk-nz-steel-could-be-forced-to-pull-out-of-auckland
# https://www.stuff.co.nz/business/farming/83230321/companies-revealed-for-buying-fraudulent-carbon-credits
# EPA industrial allocation data https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/industrial-allocations/decisions/
# link 15/08/2024
https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions-Report-Aug-2024.xlsx
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

# obtain emission unit allocation to industry data from EPA
download.file("https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions-Report-Aug-2024.xlsx","Industrial-Allocations/Industrial-Allocations-Final-Decisions-Report-Aug-2024.xlsx")
Warning messages:
1: In download.file("https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions-Report-Aug-2024.xlsx",  :
  URL https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions-Report-Aug-2024.xlsx: cannot open destfile 'Industrial-Allocations/Industrial-Allocations-Final-Decisions-Report-Aug-2024.xlsx', reason 'No such file or directory'
2: In download.file("https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions-Report-Aug-2024.xlsx",  :
  download had nonzero exit status
# I downloaded manually   
#download.file("https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions_2022.xlsx","Industrial-Allocations-Final-Decisions_2022.xlsx")

# check names of work sheets
excel_sheets("Industrial-Allocations-Final-Decisions-Report-Aug-2024.xlsx")
[1] "IA Final Decisions"
# read in allocation of emissions units data
Allocations <- read_excel("Industrial-Allocations-Final-Decisions-Report-Aug-2024.xlsx", sheet = "IA Final Decisions",skip=3)
# read in allocation of emissions units data
#Allocations <- read_excel("Industrial-Allocations-Final-Decisions_2022.xlsx", sheet = "IA Final Decisions",skip=3)
# check dataframe variables
str(Allocations) 
tibble [1,472 × 4] (S3: tbl_df/tbl/data.frame)
 $ Activity        : chr [1:1472] "Fresh tomatoes" "Protein meal" "Protein meal" "Tissue paper" ...
 $ Applicant’s name: chr [1:1472] "A1 TOMS LIMITED" "Affco New Zealand Limited" "Alliance Group Limited" "Asaleo Care New Zealand Limited" ...
 $ Year            : num [1:1472] 2023 2023 2023 2023 2023 ...
 $ Final Allocation: num [1:1472] 290 12139 10604 36823 577 ...

# revise and shorten column names
colnames(Allocations) <- c("Activity", "Applicant", "Year", "Allocation") 
# check range of variables 
summary(Allocations)   
   Activity          Applicant              Year        Allocation     
 Length:1472        Length:1472        Min.   :2010   Min.   :      1  
 Class :character   Class :character   1st Qu.:2012   1st Qu.:    198  
 Mode  :character   Mode  :character   Median :2015   Median :   1182  
                                       Mean   :2016   Mean   :  49857  
                                       3rd Qu.:2019   3rd Qu.:   8148  
                                       Max.   :2023   Max.   :2145482 
                                       
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
nzu2022 <- filter(Allocations, Year =="2022")
nzu2023 <- filter(Allocations, Year =="2023")
# Each year, usually in May, the EPA makes a 'provisional' allocation of emssion units to selected industries. see https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/industrial-allocations/ I want to estimate the market value of free allocation of units. I understand that the deadline for a provisional allocation is 30 April of each year so I assume the transfer of the allocation to NZ Steel is made in May of each year. There is an online 'open data' Github repository of New Zealand Unit (NZU) prices going back to May 2010. https://github.com/theecanmole/nzu

# The NZU repository has it's own citation and DOI: Theecanmole. (2016). New Zealand emission unit (NZU) monthly prices 2010 to 2016: V1.0.01 [Data set]. Zenodo. http://doi.org/10.5281/zenodo.221328

# add the average May spot prices
# Mean May prices  17.58, 19.84,6.23,1.94,4.08,5.34,  14.63, 16.96 , 21.28 , 25.29 , 24.84 ,37.14

# add a NZU market price and a value at the May average price from 2010
nzu2010[["MeanMayprice"]] <- 17.58
nzu2010[["Value"]] <- nzu2010[["Allocation"]] * 17.58
nzu2011[["MeanMayprice"]] <- 19.84
nzu2011[["Value"]] <- nzu2011[["Allocation"]] * 19.84
nzu2012[["MeanMayprice"]] <-6.23
nzu2012[["Value"]] <- nzu2012[["Allocation"]]* 6.23
nzu2013[["MeanMayprice"]] <- 1.94
nzu2013[["Value"]] <- nzu2013[["Allocation"]]* 1.94
nzu2014[["MeanMayprice"]] <- 4.08
nzu2014[["Value"]] <- nzu2014[["Allocation"]]* 4.08
nzu2015[["MeanMayprice"]] <- 5.34
nzu2015[["Value"]] <- nzu2015[["Allocation"]]* 5.34
nzu2016[["MeanMayprice"]] <- 14.54
nzu2016[["Value"]] <- nzu2016[["Allocation"]]* 14.54
nzu2017[["MeanMayprice"]] <- 16.96
nzu2017[["Value"]] <- nzu2017[["Allocation"]]* 16.96
nzu2018[["MeanMayprice"]] <- 21.28
nzu2018[["Value"]] <- nzu2018[["Allocation"]]* 21.28
nzu2019[["MeanMayprice"]] <- 25.29 
nzu2019[["Value"]] <- nzu2019[["Allocation"]]* 25.29
nzu2020[["MeanMayprice"]] <- 24.84
nzu2020[["Value"]] <- nzu2020[["Allocation"]]* 24.84
nzu2021[["MeanMayprice"]] <- 37.14
nzu2021[["Value"]] <- nzu2021[["Allocation"]]* 37.14
nzu2022[["MeanMayprice"]] <- 76.55
nzu2022[["Value"]] <- nzu2022[["Allocation"]]* 76.55
nzu2023[["MeanMayprice"]] <- 53.81
nzu2023[["Value"]] <- nzu2023[["Allocation"]]* 53.81
str(nzu2022) 

# combine all the year data together into 1 dataframe - I use rbind as all the column names are consistent
Allocations <- rbind(nzu2010,nzu2011,nzu2012,nzu2013,nzu2014,nzu2015,nzu2016,nzu2017,nzu2018,nzu2019,nzu2020,nzu2021,nzu2022,nzu2023)

# check the new dataframe
str(Allocations)
tibble [1,472 × 6] (S3: tbl_df/tbl/data.frame)
 $ Activity    : chr [1:1472] "Glass containers" "Protein meal" "Protein meal" "Fresh cucumbers" ...
 $ Applicant   : chr [1:1472] "ACI OPERATIONS NZ LIMITED" "Affco New Zealand Limited" "Alliance Group Limited" "Alwyn Ernest Inger, Anne Marie Inger" ...
 $ Year        : num [1:1472] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Allocation  : num [1:1472] 14454 2798 2119 168 3654 ...
 $ MeanMayprice: num [1:1472] 17.6 17.6 17.6 17.6 17.6 ...
 $ Value       : num [1:1472] 254101 49189 37252 2953 64237 ... 
 
# Create a .csv formatted data file
write.csv(Allocations, file = "Allocations.csv", row.names = FALSE)
# read my csv data file back into R if needed later
Allocations <- read.csv("Allocations.csv") 

# look at NZ Steel allocations
filter(Allocations, Applicant =="New Zealand Steel Development Limited") 
# A tibble: 14 × 6
   Activity                       Applicant  Year Allocation MeanMayprice  Value
   <chr>                          <chr>     <dbl>      <dbl>        <dbl>  <dbl>
 1 Iron and steel manufacturing … New Zeal…  2010     494704        17.6  8.70e6
 2 Iron and steel manufacturing … New Zeal…  2011     989304        19.8  1.96e7
 3 Iron and steel manufacturing … New Zeal…  2012    1003730         6.23 6.25e6
 4 Iron and steel manufacturing … New Zeal…  2013    1029352         1.94 2.00e6
 5 Iron and steel manufacturing … New Zeal…  2014    1073489         4.08 4.38e6
 6 Iron and steel manufacturing … New Zeal…  2015    1067501         5.34 5.70e6
 7 Iron and steel manufacturing … New Zeal…  2016    1048116        14.5  1.52e7
 8 Iron and steel manufacturing … New Zeal…  2017    1432496        17.0  2.43e7
 9 Iron and steel manufacturing … New Zeal…  2018    1782366        21.3  3.79e7
10 Iron and steel manufacturing … New Zeal…  2019    2118983        25.3  5.36e7
11 Iron and steel manufacturing … New Zeal…  2020    2030166        24.8  5.04e7
12 Iron and steel manufacturing … New Zeal…  2021    2145482        37.1  7.97e7
13 Iron and steel manufacturing … New Zeal…  2022    1910503        76.6  1.46e8
14 Iron and steel manufacturing … New Zeal…  2023    1830000        53.8  9.85e7

# create dataframe for the allocation of units to NZ Steel
NZsteelunits <- filter(Allocations, Applicant =="New Zealand Steel Development Limited")  

str(NZsteelunits) 
tibble [14 × 6] (S3: tbl_df/tbl/data.frame)
 $ Activity    : chr [1:14] "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" ...
 $ Applicant   : chr [1:14] "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" ...
 $ Year        : num [1:14] 2010 2011 2012 2013 2014 ...
 $ Allocation  : num [1:14] 494704 989304 1003730 1029352 1073489 ...
 $ MeanMayprice: num [1:14] 17.58 19.84 6.23 1.94 4.08 ...
 $ Value       : num [1:14] 8696896 19627791 6253238 1996943 4379835 ..
 
# omit first two columns as they are redundant
NZsteelunits <- NZsteelunits[,c(3:6)]

str(NZsteelunits)
tibble [14 × 4] (S3: tbl_df/tbl/data.frame)
 $ Year        : num [1:14] 2010 2011 2012 2013 2014 ...
 $ Allocation  : num [1:14] 494704 989304 1003730 1029352 1073489 ...
 $ MeanMayprice: num [1:14] 17.58 19.84 6.23 1.94 4.08 ...
 $ Value       : num [1:14] 8696896 19627791 6253238 1996943 4379835 ...

# Create a .csv formatted data file
write.csv(NZsteelunits, file = "NZsteelunits.csv", row.names = FALSE)
# read csv file back in
NZsteelunits <- read.csv("NZsteelunits.csv") 

------------------------------------------------------------------------------
The AR4 emissions data
download.file("https://environment.govt.nz/assets/publications/GhG-Inventory/GHG-inventory-2024/Time-series-emissions-data-in-AR4-1990-to-2022-from-NZGHGI-2024.xlsx, "TimeseriesemissionsdatainAR41990to2022fromNZGHGI2024.xlsx")
url <- "https://environment.govt.nz/assets/publications/GhG-Inventory/GHG-inventory-2024/Time-series-emissions-data-in-AR4-1990-to-2022-from-NZGHGI-2024.xlsx" 

# 21 May 2023 Sunday download latest detailed emissions data by category from Ministry for the Environment
# https://environment.govt.nz/publications/new-zealands-greenhouse-gas-inventory-1990-2021/

#download.file("https://environment.govt.nz/assets/publications/climate-change/Time-series-emissions-data-by-category-presented-in-AR4-Excel-xlsx.xlsx","Time-series-emissions-data-by-category-2021.xlsx")
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
excel_sheets("Time-series-emissions-data-in-AR4-1990-to-2022-from-NZGHGI-2024.xlsx") 
[1] "All gases" "CO2"       "CH4"       "N2O"       "HFCs"      "PFCs"     
[7] "SF6"

# read in inventory emissions data
emissions <- read_excel("Time-series-emissions-data-in-AR4-1990-to-2022-from-NZGHGI-2024.xlsx", sheet = "All gases",skip=10)

# filter out only steel sector emissions being row 371 from column 3 1990 to 2022
steel <- as.numeric(emissions[371,3:35]) 
# check steel vector
str(steel)
num [1:33] 1307 1421 1499 1535 1417 ...
# print to screen
steel
[1] 1306.735 1421.220 1498.642 1535.022 1416.737 1493.641 1455.552 1295.650
 [9] 1395.999 1441.968 1429.077 1500.045 1464.779 1658.224 1670.604 1623.167
[17] 1625.343 1645.659 1539.205 1513.433 1696.438 1685.892 1664.743 1708.143
[25] 1732.380 1767.870 1712.053 1758.300 1694.406 1661.611 1578.554 1718.126
[33] 1541.555
# convert from Kilotonnes to tonnes to match free allocation 
steel <-c(steel * 10^3)
str(steel)
 num [1:33] 1306735 1421220 1498642 1535022 1416737 ...
# select only 2010 to 2022 emissions
steel <- steel[21:33]  
steel
[1] 1696438 1685892 1664743 1708143 1732380 1767870 1712053 1758300 1694406
[10] 1661611 1578554 1718126
length(steel)
[1] 13
# add a 'NA' for 2023 steel emissions as the 2023 GHGI not due until April 2025
steel <- append(steel,NA) 
str(steel)
 num [1:14] 1696438 1685892 1664743 1708143 1732380 ... 
tail(steel)
[1] 1694406 1661611 1578554 1718126 1541555      NA 
# https://www.rnz.co.nz/news/business/525278/nz-steel-reports-making-1-point-3m-tonnes-of-carbon-emissions-claims-1-point-8m-tonnes-of-free-carbon-credits
# Eloise Gibson says NZ STeel emissions to EPA are 1.8 million tonnes
# EPS spreadsheet says NZ Steel 2023 importing coal 417070 purchasing coal 904186 producing steel 37826
417070 + 904186 + 37826
[1] 1359082
# add EPA 2023 steel emissions 
steel[14] <- 1359082

# the inventory steel emissions has 14 values which matchs the allocations dataframe 14 rows 2010 to 2023 the 
# what is mass of emissions? (sum of 2010 to 2022 as 2023 is 'NA'
sum(steel) 
[1] 23279154

# OUTDATED add 2021 steel emissions from Participant report 1889288 tonnes
# OUTDATED steel <- c(steel, 1889288)

# add actual steel sector emissions 2010 to 2023 allocation dataframe 
NZsteelunits[["Emissions"]] <- steel  
# check dataframe
str(NZsteelunits)
tibble [14 × 5] (S3: tbl_df/tbl/data.frame)
 $ Year        : num [1:14] 2010 2011 2012 2013 2014 ...
 $ Allocation  : num [1:14] 494704 989304 1003730 1029352 1073489 ...
 $ MeanMayprice: num [1:14] 17.58 19.84 6.23 1.94 4.08 ...
 $ Value       : num [1:14] 8696896 19627791 6253238 1996943 4379835 ...
 $ Emissions   : num [1:14] 1696438 1685892 1664743 1708143 1732380 ...

tail(NZsteelunits,2)
# A tibble: 2 × 9
   Year Allocation MeanMayprice      Value Emissions twoforone ETSliability
  <dbl>      <dbl>        <dbl>      <dbl>     <dbl>     <dbl>        <dbl>
1  2022    1910503         76.6 146249005.  1541555.         1     1541555.
2  2023    1830000         53.8  98472300        NA          1          NA 

# OUTDATED Are the Inventory steel emissions a good proxy for NZ Steels emissions? Check EPA 2021 "ETS Participant Emissions" report,
#Table 1: Reported ETS emissions and removals by activity, 2020/21 reporting year, Industrial processes Producing iron or steel 54431 (page 12)
# Table 5: Reported participant emissions stationary energy, 2020, New Zealand Steel Limited 762038 (page 20)
# Producing iron or steel 54431 New Zealand Steel Development Limited 54431 (page 25) and p 12 Table 1 2020-2021
# There are 5 opt-in participants who purchase stationary energy. Table 17 details their emissions for the 2020 calendar year. 
# Table 17: Reported participant emissions stationary energy opt-in participants, 2020
# Purchasing coal New Zealand Steel Limited 736875 (page 39) {p 12 1,507,273}
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

unitdiscount <- c(0.25,0.5,0.5,0.5,0.5,0.5,0.5,0.67,0.83,1,1,1,1,1)
str(unitdiscount)
num [1:14] 0.25 0.5 0.5 0.5 0.5 0.5 0.5 0.67 0.83 1
# Add unit discount variable to the data frame
NZsteelunits[["twoforone"]] <- unitdiscount

# add variable that is the ETS unit surrender liability - 'Emissions' x 'twoforone' 
NZsteelunits[["ETSliability"]] <- NZsteelunits[["Emissions"]]*NZsteelunits[["twoforone"]]

# add variable that is the emissions footprint of unit allocation 'Allocation' / 2 for 1 unit discount
NZsteelunits[["Footprint"]] <- NZsteelunits[["Allocation"]] / NZsteelunits[["twoforone"]]

# add variable that is the annual surplus, the annual sum of allocated units less the estimate of units surrendered (ETS liability)
NZsteelunits[["Overallocation"]] <- NZsteelunits[["Allocation"]] - NZsteelunits[["ETSliability"]] 
# What is the sum of the annual over allocations?
sum(NZsteelunits[["Overallocation"]])
[1] 3953195 # 3113329 #  3,113,329
# what is the cumulative over allocation by the end of 2023?
sum(NZsteelunits[["Overallocation"]]/10^6)
[1] 3.953195 # 3.113329 # almost 4 million units

# add internationally sourced emission reduction units (ERUs) held by NZ Steel Limited from the Emissions Unit Register https://www.emissionsregister.govt.nz/Common/ViewPublicReport.aspx?rt=fa64ead4-70b7-4d41-9633-616965b4e2fb
2013 1022527
2014 1001714
2015 32534

# create a vector of emission reduction units (ERUs) of length 14 for years 2010 to 2023
ERU <- c(rep(NA,3) ,c(1022527,1001714,32534) , rep(NA,8)) 
str(ERU)
 num [1:14] NA NA NA 1022527 1001714  
# add ERU to dataframe
NZsteelunits[["ERU"]] <- ERU 

NZsteelunits[["ERU"]]
 [1]      NA      NA      NA 1022527 1001714   32534      NA      NA      NA
[10]      NA      NA      NA      NA      NA

# create new dummy variable 'Stockpile'  12 x NA
Stockpile <- c(rep(NA,14) )

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
NZsteelunits[["Stockpile"]][13] <- NZsteelunits[["Stockpile"]][12]+NZsteelunits[["Overallocation"]][13]
NZsteelunits[["Stockpile"]][14] <- NZsteelunits[["Stockpile"]][13]+NZsteelunits[["Overallocation"]][14]

# How big is the estimated stockpile after the 2023 calendar year
NZsteelunits[["Stockpile"]][14]
[1] 6009970
# [1] 5170104 # at 21 May 2023
# [1] 4998943 
# or 5,170,104 units or 5.2 million units at end of calendar year 2021

# What is most recent NZU price? and what is market value of NZ Steel stockpile ?
https://www.carbonnews.co.nz/story.asp?storyID=32304 53.17 16 August 2024
NZsteelunits[["Stockpile"]][14] * 53.17
[1] 319550121 
# or $320 million

#Jan 24, 2023, 18:30:39	https://www.carbonnews.co.nz/story.asp?storyID=26805
# carbonspotprice <- 73.00
# What is the market value (if stockpile is still owned by NZ Steel and is an asset on their balance sheet) It's ~ $380 million
# NZsteelunits[["Stockpile"]][12] * carbonspotprice
# 1] 377417623 #  or $377,417,623

# save dataframe as a .csv file 
write.table(NZsteelunits, file = "NZsteelunits.csv", sep = ",", col.names = TRUE, qmethod = "double",row.names = FALSE)

# to read in NZ Steel data again 
NZsteelunits <- read.csv("NZsteelunits.csv")

# check the dataframe
str(NZsteelunits)
tibble [14 × 11] (S3: tbl_df/tbl/data.frame)
 $ Year          : num [1:14] 2010 2011 2012 2013 2014 ...
 $ Allocation    : num [1:14] 494704 989304 1003730 1029352 1073489 ...
 $ MeanMayprice  : num [1:14] 17.58 19.84 6.23 1.94 4.08 ...
 $ Value         : num [1:14] 8696896 19627791 6253238 1996943 4379835 ...
 $ Emissions     : num [1:14] 1696438 1685892 1664743 1708143 1732380 ...
 $ twoforone     : num [1:14] 0.25 0.5 0.5 0.5 0.5 0.5 0.5 0.67 0.83 1 ...
 $ ETSliability  : num [1:14] 424110 842946 832372 854072 866190 ...
 $ Footprint     : num [1:14] 1978816 1978608 2007460 2058704 2146978 ...
 $ Overallocation: num [1:14] 70594 146358 171358 175280 207299 ...
 $ ERU           : num [1:14] NA NA NA 1022527 1001714 ...
 $ Stockpile     : num [1:14] 70594 216953 388311 1586118 2795131 ...

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
# #D13D19 'orange flame'    / Thunderbird
# #530D29 'windsor wine' / Maroon Oak
brewer.pal("Dark2",n=3)
[1] "#1B9E77" "#D95F02" "#7570B3"  # teal khaki mauve
brewer.pal("Dark2",n=4)
[1] "#1B9E77" "#D95F02" "#7570B3" "#E7298A" # teal khaki mauve shocking pink 

# chart of actual NZ Steel emissions
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v1.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v1.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2023), labels = c(2010:2023), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1 ,tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,cex=1,pch=16, col="#1B9E77")
legend(2011, 1.2, bty = "n",cex=1.1, "Steel emissions MfE Greenhouse Gas Inventory 2023",col = "#1B9E77", text.col = 1,lty = 1, pch=16 ) 
legend(2011, 1.0, bty = "n",cex=1.1, "NZ Steel emissions EPA Participant Reports 2020", col="red",text.col=1, pch=19)
mtext(side=3,cex=1.5, line=-3.5, expression(paste("New Zealand Steel Limited greenhouse gas emissions \n2010 to 2023")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2021\nETS Participant Emissions EPA 2020 2021 2024")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
points(2020,1553344/10^6,cex=1.5,pch=19, col="red")
#points(2021,1889288/10^6,cex=1,pch=19, col="red")
#lines(c(2020,2021),c(1553344/10^6,1889288/10^6),lwd=1,col="#E7298A")
dev.off() 

# add units allocated to chart of actual emissions
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v2.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v2.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2023), labels = c(2010:2023), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1,tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
legend(2013, 1, bty = "n",cex=1.1, c("Actual emissions","Emission units allocated"), col =  c("#1B9E77","#D95F02") , text.col = 1, lty = 1, pch = c(16,17))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("New Zealand Steel Limited greenhouse gas emissions and\nemission unit allocation 2010 to 2021"))) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2022, EPA Industrial Allocations")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

# chart actual emissions, units allocated and the ETS liability 
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v3.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v3.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2023), labels = c(2010:2023), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
legend(2011, 2.5, cex=1.1, bty = "n", c("Actual emissions","Emission units allocated","Emissions x 'two for one' equals ETS liability"), col =  c("#1B9E77","#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(16,17,15))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("New Zealand Steel Limited annual allocation of emissions units\n always exceeds the ETS liability 2010 to 2023")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2022\nEPA Industrial Allocations 2023")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

# chart only units allocated and the ETS liability 
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v3a.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))
#png("NZsteel-Allocation-GHGs-line-2010-2020-600by600-v3a.png", bg="white", width=600, height=600,pointsize = 12)
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v3a.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2023), labels = c(2010:2023), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
#lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
#points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
legend(2011, 2.5, cex=1.1, bty = "n", c("Emission units allocated","Emissions x 'two for one' equals ETS liability"), col =  c("#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(17,15))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("New Zealland Steel Limited annual allocation of emissions units\n always exceeds the ETS liability 2010 to 2023")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2022\nEPA Industrial Allocations 2023")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# chart actual emissions, units allocated and ets liability and overallocation
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v4.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v4.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2023), labels = c(2010:2023), tick = TRUE)
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
mtext(side=3,cex=1.5,line=-3.5,expression(paste("New Zealand Steel Limited overallocation of emissions units\n above ETS liability 2010 to 2023"))) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 2023, EPA Industrial Allocations 2024")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# chart actual emissions, units allocated and ets liability and overallocation and add emissions reduction units (ERU) owned
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v5.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel")) 
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v5.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2023), labels = c(2010:2023), tick = TRUE)
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
mtext(side=3,cex=1.5,line=-3.5, expression(paste("NZ Steel Limited overallocation of emissions units\n above ETS liability 2010 to 2023"))) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2022, EPA Industrial Allocations 2023")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

# add cumulative stockpile
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v6.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v6.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,ylim=c(0,5.8),tck=0.01,axes=FALSE,ann=FALSE,col="#A6761D",type="l",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2023), labels = c(2010:2023), tick = TRUE)
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
mtext(side=3,cex=1.5,line=-3.5, expression(paste("NZ Steel Limited cumulative stockpile of \nemissions units 2010 2023")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: NZ Greenhouse Gas Inventory 2022, EPA Industrial Allocations 2024")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# chart with darkgray for all variables except the cumulative stock pile of emission units
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v7.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v7.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,ylim=c(0,5.8),tck=0.01,axes=FALSE,ann=FALSE,col="#A6761D",type="l",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2023), labels = c(2010:2023), tick = TRUE)
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
legend(2010, 5.25, cex=1, bty = "n", c("Actual emissions","Emission units allocated","Emissions x 'two for one' equals ETS Liability","Emission Reduction Units","Allocation less ETS liability = annual surplus of units","surplus + ERUs =  stockpile of + 5 million units 2021?"), col=c("darkgray","darkgray","darkgray","darkgray","darkgray","#A6761D") , text.col = 1, lty = 1, pch = c(16,17,15,16,18,19))
mtext(side=3,cex=1.5,line=-3.5, expression(paste("NZ Steel Limited cumulative stockpile of \nemissions units 2010 2021")) )
mtext(side=1,line=-1.25,cex=1, "Data: NZ Greenhouse Gas Inventory 2022, EPA Industrial Allocations 2023")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# chart the actual emissions , the allocation and the emissions footprint of allocation
"#E6AB02" # mustard/Corn
#A6761D Mandalay brown
# what is the total mass of emissions pemitted by free emission units?  29 million
sum(NZsteelunits[["Footprint"]]) 
[1] 28722417
# [1] 24981914
# what is the total Inventory steel sector emissions?       23 m
sum(NZsteelunits[["Emissions"]])
[1] 23279154
# [1] 20378517 
# what are the total steel sector allocation of emissions?      20 m
sum(NZsteelunits[["Allocation"]])
1] 19956192
# [1] 16215689
# chart
svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v8.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v8.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9), tck=0.01,axes=FALSE,ann=FALSE,col="#A6761D",type="l",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2023), labels = c(2010:2023), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["Footprint"]]/10^6,col="#E6AB02", lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Footprint"]]/10^6,col="#E6AB02",cex=0.9,pch=15 )
legend(2013, 0.8, cex=1, bty = "n", c("Emissions footprint of allocation","Actual emissions","Emission units allocated"), col=c("#E6AB02","#1B9E77","#D95F02") , text.col = 1, lty = 1, pch = c(15,16,17))
mtext(side=3,cex=1.5,line=-3.5, expression(paste("New Zealand Steel Limited emissions footprint of \nfree allocation of emissions units 2010 to 2023")) ) 
mtext(side=3,cex=1.1,line=-6, expression(paste("The 20m free units allowed an emissions footprint \nof 29m tonnes when steel emissions were 23m tonnes")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: NZ Greenhouse Gas Inventory 2022, EPA Industrial Allocations 2023")
mtext(side=2,cex=1, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()
