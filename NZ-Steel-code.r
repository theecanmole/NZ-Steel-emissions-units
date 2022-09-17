# NZ Steel free industrial allocation of emissions units 2010 to 2020
# https://www.stuff.co.nz/business/114961557/very-real-risk-nz-steel-could-be-forced-to-pull-out-of-auckland
# https://www.stuff.co.nz/business/farming/83230321/companies-revealed-for-buying-fraudulent-carbon-credits?rm=a
# EPA industrial allocation data https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions.xlsx

# navigate to folder /media/user/RED/NZsteel, select file NZ-Steel-code.r and right click and open with RKward
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

# obtain 2010 to 2020 emission unit allocation to industry data from EPA
download.file("https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions.xlsx","Industrial-Allocations-Final-Decisions.xlsx")
trying URL 'https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Reports/Industrial-Allocations/Industrial-Allocations-Final-Decisions.xlsx'
Content type 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' length 56684 bytes (55 KB)
==================================================
downloaded 55 KB 
# check work sheets
excel_sheets("Industrial-Allocations-Final-Decisions.xlsx")
[1] "IA Final Decisions"

# read in allocation of emissions units data
Allocations <- read_excel("Industrial-Allocations-Final-Decisions.xlsx", sheet = "IA Final Decisions",skip=3)

str(Allocations) 
tibble [1,207 × 4] (S3: tbl_df/tbl/data.frame)
 $ Activity        : chr [1:1207] "Aluminium smelting" "Burnt lime" "Burnt lime" "Burnt lime" ...
 $ Applicant’s name: chr [1:1207] "New Zealand Aluminium Smelters Limited" "Graymont (NZ) Limited" "Holcim (New Zealand) Limited" "Perry Resources (2008) Ltd" ...
 $ Year            : num [1:1207] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Final Allocation: num [1:1207] 210421 47144 3653 4712 948 ...

# revise and shorten column names
colnames(Allocations) <- c("Activity", "Applicant", "Year", "Allocation") 

# separate allocation of emissions units data into years
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

# check just the 2010 data
str(nzu2010)
Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	141 obs. of  4 variables:
 $ Activity  : chr  "Aluminium smelting" "Burnt lime" "Burnt lime" "Burnt lime" ...
 $ Applicant : chr  "New Zealand Aluminium Smelters Limited" "Graymont (NZ) Limited" "Holcim (New Zealand) Limited" "Perry Resources (2008) Ltd" ...
 $ Year      : num  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Allocation: num  210421 47144 3653 4712 948 ...   
 
# Each year, from May, the EPA makes a 'provisional' allocation of emssion units to selected industries. see https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/industrial-allocations/ I want to estimate the market value of free allocation of units. I understand that the deadline for a provisional allocation is 30 April of each year so I assume the transfer of allocation is made in May of each year. There is an online 'open data' Github repository of New Zealand Unit (NZU) prices going back to May 2010. https://github.com/theecanmole/nzu
# The NZU repository has it's own citation and DOI: Theecanmole. (2016). New Zealand emission unit (NZU) monthly prices 2010 to 2016: V1.0.01 [Data set]. Zenodo. http://doi.org/10.5281/zenodo.221328
# I will add a NZU market price value at the May average price from 2010 to 2019 

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

# combine all year data together into 1 dataframe - I use rbind as all the column names are consistent
Allocations <- rbind(nzu2010,nzu2011,nzu2012,nzu2013,nzu2014,nzu2015,nzu2016,nzu2017,nzu2018,nzu2019,nzu2020)

# check the new dataframe
str(Allocations)
tibble [1,207 × 5] (S3: tbl_df/tbl/data.frame)
 $ Activity  : chr [1:1207] "Aluminium smelting" "Burnt lime" "Burnt lime" "Burnt lime" ...
 $ Applicant : chr [1:1207] "New Zealand Aluminium Smelters Limited" "Graymont (NZ) Limited" "Holcim (New Zealand) Limited" "Perry Resources (2008) Ltd" ...
 $ Year      : num [1:1207] 2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ Allocation: num [1:1207] 210421 47144 3653 4712 948 ...
 $ Value     : num [1:1207] 3699201 828792 64220 82837 16666 ... 
 
# read my csv data file back into R if needed
Allocations <- read.csv("Allocations.csv") 

# Create a .csv formatted data file
write.csv(Allocations, file = "Allocations.csv", row.names = FALSE)
 
filter(Allocations, Applicant =="New Zealand Steel Development Limited") 
# A tibble: 11 × 5
   Activity                                    Applicant  Year Allocation  Value
   <chr>                                       <chr>     <dbl>      <dbl>  <dbl>
 1 Iron and steel manufacturing from iron sand New Zeal…  2010     494704 8.70e6
 2 Iron and steel manufacturing from iron sand New Zeal…  2011     989304 1.96e7
 3 Iron and steel manufacturing from iron sand New Zeal…  2012    1003730 6.25e6
 4 Iron and steel manufacturing from iron sand New Zeal…  2013    1029352 2.00e6
 5 Iron and steel manufacturing from iron sand New Zeal…  2014    1073489 4.38e6

# create dataframe for allocation of units to NZ Steel
NZsteelunits <- filter(Allocations, Applicant =="New Zealand Steel Development Limited")  

str(NZsteelunits) 
tibble [11 × 5] (S3: tbl_df/tbl/data.frame)
 $ Activity  : chr [1:11] "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" ...
 $ Applicant : chr [1:11] "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" ...
 $ Year      : num [1:11] 2010 2011 2012 2013 2014 ...
 $ Allocation: num [1:11] 494704 989304 1003730 1029352 1073489 ...
 $ Value     : num [1:11] 8696896 19627791 6253238 1996943 4379835 ...  

# omit first two columns as they are redundant
NZsteelunits <- NZsteelunits[,3:5]

str(NZsteelunits)
tibble [11 × 3] (S3: tbl_df/tbl/data.frame)
 $ Year      : num [1:11] 2010 2011 2012 2013 2014 ...
 $ Allocation: num [1:11] 494704 989304 1003730 1029352 1073489 ...
 $ Value     : num [1:11] 8696896 19627791 6253238 1996943 4379835 ...

------------------------------------------------------------------------------
# obtain Greenhouse Gas Inventory 1990 to 2020 more detailed time series data from MfE
download.file("https://environment.govt.nz/assets/publications/GhG-Inventory/Time-series-emissions-data-by-category.xlsx","Time-series-emissions-data-1990-to-2020.xlsx") 
trying URL 'https://environment.govt.nz/assets/publications/GhG-Inventory/Time-series-emissions-data-by-category.xlsx'
Content type 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' length 1567250 bytes (1.5 MB)
==================================================
downloaded 1.5 MB
# check number of worksheets
excel_sheets("Time-series-emissions-data-1990-to-2020.xlsx") 
 
[1] "All gases" "CO2"       "CH4"       "N2O"       "HFCs"      "PFCs"     
[7] "SF6"  

# read in inventory emissions data
emissions <- read_excel("Time-series-emissions-data-1990-to-2020.xlsx", sheet = "All gases",skip=10)

# filter out only steel sector emissions being row 371 from column 3 1990 to 2020
steel <- as.numeric(emissions[371,3:33]) 
steel
> steel
 [1] 1306.735 1421.220 1498.642 1535.022 1416.737 1493.641 1455.552 1295.650
 [9] 1395.999 1441.968 1429.077 1500.045 1464.779 1658.224 1670.604 1623.167
[17] 1625.343 1645.659 1539.205 1513.433 1696.438 1685.892 1664.743 1708.143
[25] 1732.380 1767.870 1712.053 1758.300 1694.406 1661.611 1578.554

# convert from Kilotonnes to tonnes to match free allocation 
steel <-c(steel * 10^3)

str(steel)
num [1:31] 1306735 1421220 1498642 1535022 1416737 ...
# select 2010 to 2020 emissions
steel <- steel[21:31]  
steel
[1] 1696438 1685892 1664743 1708143 1732380 1767870 1712053 1758300 1694406
[10] 1661611 1578554
length(steel)
[1] 11
# add actual steel sector emissions 2010 to 2020 allocation dataframe 
NZsteelunits[["Emissions"]] <- steel  

# check dataframe
str(NZsteelunits)
tibble [11 × 4] (S3: tbl_df/tbl/data.frame)
 $ Year      : num [1:11] 2010 2011 2012 2013 2014 ...
 $ Allocation: num [1:11] 494704 989304 1003730 1029352 1073489 ...
 $ Value     : num [1:11] 8696896 19627791 6253238 1996943 4379835 ...
 $ Emissions : num [1:11] 1696438 1685892 1664743 1708143 1732380 ... 

# Are the Inventory steel emissions a good proxy for NZ Steels emissions? Check EPA 2021 "ETS Participant Emissions" report,
Table 1: Reported ETS emissions and removals by activity, 2020/21 reporting year, Industrial processes Producing iron or steel 54431 (page 12)
Table 5: Reported participant emissions stationary energy, 2020, New Zealand Steel Limited 762038 (page 20)
Producing iron or steel 54431 New Zealand Steel Development Limited 54431 (page 25)
There are 5 opt-in participants who purchase stationary energy. Table 17 details their emissions for the 2020 calendar year. 
Table 17: Reported participant emissions stationary energy opt-in participants, 2020
Purchasing coal New Zealand Steel Limited 736875 (page 39)
54431 + 762038 + 736875
[1] 1553344
# Yes the 2020 NZ Steel actual emissions = 1553344 tonnes, 2020 GHG Inventory steel emissions = 1578554, so they are near enough

# select some colours for charting 
display.brewer.all(n=NULL, type="qual", select=NULL, exact.n=TRUE, colorblindFriendly=TRUE)
# plot of 3 palletes, Set2, Paired, Dark2 
display.brewer.all(n=4, type="qual", select="Paired", exact.n=TRUE, colorblindFriendly=TRUE) # light blue,  blue, light green dark green 
display.brewer.pal(7,"Accent")
display.brewer.pal("Dark2",n=8)

brewer.pal("Dark2",n=8)
[1] "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D" teal russet mauve pink green mustard tan gray
[8] "#666666"
brewer.pal("Dark2",n=3)
[1] "#1B9E77" "#D95F02" "#7570B3"  # teal khaki mauve
brewer.pal("Dark2",n=4)
[1] "#1B9E77" "#D95F02" "#7570B3" "#E7298A" # teal khaki mauve shocking pink 

png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v1.png", bg="white", width=560, height=420,pointsize = 14)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1 ,tick = TRUE)
#axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, at = c(0,1,2,3), labels = c(0,1,2,3),tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,cex=1,pch=16, col="#1B9E77")
legend(2013, 1.2, bty = "n", "Steel Emissions GHGI", col = "#1B9E77"  , text.col = 1, lty = 1, pch = 16 ) 
legend(2013, 1.0, bty = "n", "NZ Steel Emissions EPA", col = "#E7298A"  , text.col = 1, lty = 1, pch = 16 )
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd greenhouse gas emissions \n2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2020\nETS Participant Emissions EPA October 2021")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
points(2020,1553344/10^6,cex=1,pch=16, col="#E7298A")
dev.off() 

svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v1.svg", width = 8, height = 6, pointsize = 14, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1 ,tick = TRUE)
#axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, at = c(0,1,2,3), labels = c(0,1,2,3),tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,cex=1,pch=16, col="#1B9E77")
legend(2013, 1.2, bty = "n", "Steel Emissions GHGI", col = "#1B9E77"  , text.col = 1, lty = 1, pch = 16 ) 
legend(2013, 1.0, bty = "n", "NZ Steel Emissions EPA", col = "#E7298A"  , text.col = 1, lty = 1, pch = 16 )
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd greenhouse gas emissions \n2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2020\nETS Participant Emissions EPA October 2021")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
points(2020,1553344/10^6,cex=1,pch=16, col="#E7298A")
dev.off() 

# create png chart of value of units allocated
png("NZsteel-Allocation-Value-2010-2020-560by420-v1.png", bg="white", width=560, height=420,pointsize = 14)
par(mar=c(2.7,2.7,1,1)+0.1) 
plot(NZsteelunits[["Year"]],NZsteelunits[["Value"]]/10^6,tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1 ,tick = TRUE)
#axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, at = c(0,1,2,3), labels = c(0,1,2,3),tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Value"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Value"]]/10^6,cex=1,pch=16, col="#E7298A")
#legend(2013, 1.2, bty = "n", "Steel Emissions GHGI", col = "#1B9E77"  , text.col = 1, lty = 1, pch = 16 ) 
legend(2010, 45, bty = "n", "Value of free emissions units allocated by EPA", col = "#E7298A"  , text.col = 1, lty = 1, pch = 16 )
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel value of free emissions units\n2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2020\nETS Participant Emissions EPA October 2021")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("$NZD million")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# create svg chart of value of units allocated
svg(filename="NZsteel-Allocation-Value-2010-2020-560by420-v1.svg", width = 8, height = 6, pointsize = 14, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))
par(mar=c(2.7,2.7,1,1)+0.1) 
plot(NZsteelunits[["Year"]],NZsteelunits[["Value"]]/10^6,tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1 ,tick = TRUE)
#axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, at = c(0,1,2,3), labels = c(0,1,2,3),tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Value"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Value"]]/10^6,cex=1,pch=16, col="#E7298A")
#legend(2013, 1.2, bty = "n", "Steel Emissions GHGI", col = "#1B9E77"  , text.col = 1, lty = 1, pch = 16 ) 
legend(2010, 45, bty = "n", "Value of free emissions units allocated by EPA", col = "#E7298A"  , text.col = 1, lty = 1, pch = 16 )
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel value of free emissions units\n2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2020\nETS Participant Emissions EPA October 2021")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("$NZD million")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v2.png", bg="white", width=560, height=420,pointsize = 14)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1,tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
legend(2013, 0.9, bty = "n", c("Actual Emissions","Units Allocated"), col =  c("#1B9E77","#D95F02") , text.col = 1, lty = 1, pch = c(16,17))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd greenhouse gas emissions and \nunit allocation 2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2020, EPA Industrial Allocations 2020")
#mtext(side=2,cex=0.9, line=0,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v2.svg", width = 8, height = 6, pointsize = 14, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1,tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
legend(2013, 0.9, bty = "n", c("Actual Emissions","Units Allocated"), col =  c("#1B9E77","#D95F02") , text.col = 1, lty = 1, pch = c(16,17))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd greenhouse gas emissions and \nunit allocation 2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2020\nEPA Industrial Allocations 2020")
#mtext(side=2,cex=0.9, line=0,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

# create variable that is the 'two-for-one' discount - 2 tonnes = 1 unit to surrender  https://www.epa.govt.nz/industry-areas/emissions-trading-scheme/participating-in-the-ets/surrendering-units/

# "Phase out of the '1 for 2' surrender obligation, Prior to 2017, non-forestry participants had to surrender one eligible unit for every two tonnes of emissions they reported in their annual emissions return, effectively a 50% surrender obligation. 2017 - 1 unit for each 1.5 whole tonnes of emissions 2018 - 1 unit for each 1.2 whole tonnes of emissions 2019 - 1 unit for each 1 whole tonne of emissions"
# 2010 was a half year for ETS so the 'discount' for calculating emissions liability under the ETS is .5 x .5 = 0.25

unitdiscount <- c(0.25,0.5,0.5,0.5,0.5,0.5,0.5,0.67,0.83,1,1)
str(unitdiscount)
num [1:11] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.67 0.83 1 1 

# Add unit discount variable to the data frame
NZsteelunits[["twoforone"]] <- unitdiscount

# add variable that is the ETS unit surrender liability - 'Emissions' x 'twoforone' 
NZsteelunits[["ETSliability"]] <- NZsteelunits[["Emissions"]]*NZsteelunits[["twoforone"]]

# add variable that is the overallocation 'Allocation' (free units) less 'ETSliability" (units surrendered to Govt)

NZsteelunits[["Overallocation"]] <- NZsteelunits[["Allocation"]] - NZsteelunits[["ETSliability"]] 

png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v3.png", bg="white", width=560, height=420,pointsize = 14)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
#lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
#points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
legend(2012, 2.5, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated","Emissions x 'two for one' equals ETS Liability","Allocation less ETS Liability equals Surplus Units"), col =  c("#1B9E77","#D95F02","#7570B3","#E7298A") , text.col = 1, lty = 1, pch = c(16,17,15,18))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd surplus allocation of emissions units\nETS unit allocation and ETS liability 2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2020\nEPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
#lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
#points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
legend(2012, 2.5, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated","Emissions x 'two for one' equals ETS Liability","Allocation less ETS Liability equals Surplus Units"), col =  c("#1B9E77","#D95F02","#7570B3","#E7298A") , text.col = 1, lty = 1, pch = c(16,17,15,18))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd surplus allocation of emissions units\nETS unit allocation and ETS liability 2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2020\nEPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()

# de-emphasize variables except ets liability and allocation and over allocation -> darkgray for

png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v3.png", bg="white", width=560, height=420,pointsize = 14)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
#lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
#points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
legend(2012, 2.5, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated","Emissions x 'two for one' equals ETS Liability"), col =  c("darkgray","#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(16,17,15))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd allocation of emissions units\n exceeds ETS liability 2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2020\nEPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v3.svg", width = 8, height = 6, pointsize = 14, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
#lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
#points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
legend(2012, 2.5, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated","Emissions x 'two for one' equals ETS Liability"), col =  c("darkgray","#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(16,17,15))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd allocation of emissions units\n exceeds ETS liability 2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2020\nEPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# cumulative over allocation by 2020
sum(NZsteelunits[["Overallocation"]]/10^6)
[1] 2.685974 

# de-emphasize variables except ets liability and allocation and over allocation -> darkgray for
png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v4.png", bg="white", width=560, height=420,pointsize = 14)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
legend(2012, 2.5, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated","Emissions x 'two for one' equals ETS Liability"), col =  c("darkgray","#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(16,17,15))
legend(2012, 0.6, cex=0.8, bty = "n", c("Allocation less ETS Liability equals Surplus Units\n(2,685,974 units total by 2021)"), col =  c("#E7298A") , text.col = 1, lty = 1, pch = c(18))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd over allocation of emissions units\n above ETS Liability 2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 2020, EPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v5.png", bg="white", width=560, height=420,pointsize = 14)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,lwd=2)
legend(2012, 2.5, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated","Emissions x 'two for one' equals ETS Liability","Emission Removal Units"), col=c("darkgray","#D95F02","#7570B3","#1B9E77") , text.col = 1, lty = 1, pch = c(16,17,15,16))
legend(2012, 0.65, cex=0.8, bty = "n", c("Allocation less ETS Liability equals\nSurplus Units 2,685,974 units by 2020)"), col=c("#E7298A"), text.col = 1, lty = 1, pch = c(18))
#legend(2012, 1.25, cex=0.8, bty = "n", c("Emission Removal Units"), col=c("#1B9E77"), text.col = 1, lty = 1, pch = c(16))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd over allocation of emissions units\n above ETS Liability 2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2020, EPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off() 

svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v4.svg", width = 8, height = 6, pointsize = 14, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
legend(2012, 2.5, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated","Emissions x 'two for one' equals ETS Liability"), col=c("darkgray","#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(16,17,15))
legend(2012, 0.65, cex=0.8, bty = "n", c("Allocation less ETS Liability equals\nSurplus Units (2,685,974 tonnes)"), col=c("#E7298A"), text.col = 1, lty = 1, pch = c(18))
legend(2012, 1.25, cex=0.8, bty = "n", c("Emission Removal Units"), col=c("#1B9E77"), text.col = 1, lty = 1, pch = c(21))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd over allocation of emissions units\n above ETS liability 2010 to 2020")) ) 
#mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2020\nEPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v5.svg", width = 8, height = 6, pointsize = 14, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,lwd=2)
legend(2012, 2.5, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated","Emissions x 'two for one' equals ETS Liability","Emission Removal Units"), col=c("darkgray","#D95F02","#7570B3","#1B9E77") , text.col = 1, lty = 1, pch = c(16,17,15,16))
legend(2012, 0.65, cex=0.8, bty = "n", c("Allocation less ETS Liability equals\nSurplus Units (2,685,974 tonnes)"), col=c("#E7298A"), text.col = 1, lty = 1, pch = c(18))
#legend(2012, 1.25, cex=0.8, bty = "n", c("Emission Removal Units"), col=c("#1B9E77"), text.col = 1, lty = 1, pch = c(16))
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel Ltd over allocation of emissions units\n above ETS liability 2010 to 2020")) ) 
mtext(side=1,line=-1.25,cex=0.8,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2020, EPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

str(NZsteelunits)
tibble [11 × 6] (S3: tbl_df/tbl/data.frame)
 $ Year          : num [1:11] 2010 2011 2012 2013 2014 ... 
 
# add ERUs held by NZ Steel Ltd from the Emissions Unit Register https://www.emissionsregister.govt.nz/Common/ViewPublicReport.aspx?rt=fa64ead4-70b7-4d41-9633-616965b4e2fb
2013 1022527
2014 1001714
2015 32534

# create a vector of ERUs
ERU <- c(rep(NA,3) ,c(1022527,1001714,32534) , rep(NA,5)) 
str(ERU)
 num [1:11] NA NA NA 1022527 1001714  
# add ERU to dataframe
NZsteelunits[["ERU"]] <- ERU 

# in spreadsheet add a column of the cumulative stockpile of units (last year surplus + this year surplus + ERU
# create new variable 'Stockpile' 11 x NA
Stockpile <- c(rep(NA,11) )
NZsteelunits[["ERU"]] 
 NZsteelunits[["ERU"]]
 [1]      NA      NA      NA 1022527 1001714   32534      NA      NA      NA
[10]      NA      NA

# add up Overallocation values into Stockpile variable
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

# save dataframe as a .csv file 
write.table(NZsteelunits, file = "NZsteelunits-2010-2020.csv", sep = ",", col.names = TRUE, qmethod = "double",row.names = FALSE)

# to read in NZ Steel data again 
NZsteelunits <- read.csv("NZsteelunits-2010-2020.csv")
str(NZsteelunits)
'data.frame':	11 obs. of  8 variables:
 $ Year          : int  2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 ...
 $ Allocation    : int  494704 989304 1003730 1029352 1073489 1067501 1048116 1432496 1782366 2118983 ...
 $ Emissions     : num  1696438 1685892 1664743 1708143 1732380 ...
 $ twoforone     : num  0.25 0.5 0.5 0.5 0.5 0.5 0.5 0.67 0.83 1 ...
 $ ETSliability  : num  424110 842946 832372 854072 866190 ...
 $ Overallocation: num  70594 146358 171358 175280 207299 ...
 $ ERU           : int  NA NA NA 1022527 1001714 32534 NA NA NA NA ...
 $ Stockpile     : num  70594 216953 388311 1586118 2795131 ... 

NZsteelunits[["Stockpile"]][11]
[1] 4742749 
# or 4,742,749 units or 4.7 million units
# How many cumulative emissions units are there (if not sold earlier) 
NZsteelunits[["Stockpile"]][11]
[1] 4742749 
# What is Monday 23 May 2022 NZU spot price? https://www.carbonnews.co.nz/story.asp?storyID=23959
carbonspotprice <- 76.90
# What is the market value (if stockpile still owned)
NZsteelunits[["Stockpile"]][11] * carbonspotprice
[1] 364717388 
# $364,717,388

svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v6.svg", width = 8, height = 6, pointsize = 14, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,ylim=c(0,5),tck=0.01,axes=FALSE,ann=FALSE, type="l",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
points(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,cex=1,pch=19)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,lwd=2)
legend(2010.5, 4.7, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated (always exceed ETS liability)","Emissions x 'two for one' equals ETS Liability","Emission Reduction Units unneeded in ETS","Allocation less ETS liability = surplus units","Surplus + ERUs =  Stockpile 4.7 million units 2020?"), col=c("darkgray","#D95F02","#7570B3","#1B9E77","#E7298A","black") , text.col = 1, lty = 1, pch = c(16,17,15,16,18,19))
mtext(side=3,cex=1.5, line=-2, expression(paste("NZ Steel Ltd stockpile of emissions units")) ) 
mtext(side=1,line=-1.25,cex=0.8,col="gray","Data: NZ Greenhouse Gas Inventory 2020 EPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# png
png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v6.png", bg="white", width=560, height=420,pointsize = 14)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,ylim=c(0,5),tck=0.01,axes=FALSE,ann=FALSE, type="l",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
points(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,cex=1,pch=19)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,lwd=2)
legend(2010.5, 4.7, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated (always exceed ETS Liability)","Emissions x 'two for one' equals ETS Liability","Emission Reduction Units unneeded in ETS","Allocation less ETS liability = surplus units","Surplus + ERUs = Stockpile 4.7 million units 2020?"), col=c("darkgray","#D95F02","#7570B3","#1B9E77","#E7298A","black") , text.col = 1, lty = 1, pch = c(16,17,15,16,18,19))
mtext(side=3,cex=1.5, line=-2, expression(paste("NZ Steel Ltd stockpile of emissions units")) ) 
mtext(side=1,line=-1.25,cex=0.8,col="gray","Data: NZ Greenhouse Gas Inventory 2020 EPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v7.svg", width = 8, height = 6, pointsize = 14, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,ylim=c(0,5),tck=0.01,axes=FALSE,ann=FALSE, type="l",las=1,col="2",lwd=2)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
points(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,col="2",cex=1,pch=19)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="darkgray",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="darkgray",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="darkgray",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="darkgray",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="darkgray",cex=1.25,lwd=2)
legend(2010.5, 4.7, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated (always exceed ETS liability)","Emissions x 'two for one' equals ETS Liability","Emission Reduction Units unneeded in ETS","Allocation less ETS liability = surplus units","Surplus + ERUs =  Stockpile 4.7 million units 2020?"), col=c("darkgray","darkgray","darkgray","darkgray","darkgray","2") , text.col = 1, lty = 1, pch = c(16,17,15,16,18,19))
mtext(side=3,cex=1.5, line=-2, expression(paste("NZ Steel Ltd stockpile of emissions units")) ) 
mtext(side=1,line=-1.25,cex=0.8,col="gray","Data: NZ Greenhouse Gas Inventory 2020 EPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# png
png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v7.png", bg="white", width=560, height=420,pointsize = 14)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,ylim=c(0,5),tck=0.01,axes=FALSE,ann=FALSE, type="l",las=1,col="2",lwd=2)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
points(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,col="2",cex=1,pch=19)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="darkgray",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="darkgray",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="darkgray",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="darkgray",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="darkgray",cex=1.25,lwd=2)
legend(2010.5, 4.7, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated (always exceed ETS liability)","Emissions x 'two for one' equals ETS Liability","Emission Reduction Units unneeded in ETS","Allocation less ETS liability = surplus units","Surplus + ERUs =  Stockpile 4.7 million units 2020?"), col=c("darkgray","darkgray","darkgray","darkgray","darkgray","2") , text.col = 1, lty = 1, pch = c(16,17,15,16,18,19))
mtext(side=3,cex=1.5, line=-2, expression(paste("NZ Steel Ltd stockpile of emissions units")) ) 
mtext(side=1,line=-1.25,cex=0.8,col="gray","Data: NZ Greenhouse Gas Inventory 2020 EPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()


plot(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,ylim=c(0,5),tck=0.01,axes=FALSE,ann=FALSE, type="l",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
points(NZsteelunits[["Year"]],NZsteelunits[["Stockpile"]]/10^6,cex=1,pch=19)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="darkgray",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=0.9,pch=15 )
lines(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Overallocation"]]/10^6,col="#E7298A",cex=1.1,pch=18 ) 
points(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["ERU"]]/10^6,col="#1B9E77",cex=1.25,lwd=2)
legend(2010.5, 4.7, cex=0.8, bty = "n", c("Actual Emissions","Units Allocated (always exceed ETS Liability)","Emissions x 'two for one' equals ETS Liability","Emission Reduction Units unneeded in ETS","Allocation less ETS liability = surplus units","Surplus + ERUs = Stockpile 4.7 million units 2020?"), col=c("darkgray","#D95F02","#7570B3","#1B9E77","#E7298A","black") , text.col = 1, lty = 1, pch = c(16,17,15,16,18,19))
mtext(side=3,cex=1.5, line=-2, expression(paste("NZ Steel Ltd stockpile of emissions units")) ) 
mtext(side=1,line=-1.25,cex=0.8,col="gray","Data: NZ Greenhouse Gas Inventory 2020 EPA Industrial Allocations 2020")
mtext(side=2,cex=0.9, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# do some linear regression of NZ Steel's emissions 
NZsteelunits <- read.csv("NZsteelunits-2010-2020.csv")

str(NZsteelunits)
'data.frame':	11 obs. of  9 variables:
 $ Year          : int  2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 ...
 $ Allocation    : int  494704 989304 1003730 1029352 1073489 1067501 1048116 1432496 1782366 2118983 ...
 $ Value         : num  8696896 19627791 6253238 1996943 4379835 ...
 $ Emissions     : num  1696438 1685892 1664743 1708143 1732380 ...
 $ twoforone     : num  0.25 0.5 0.5 0.5 0.5 0.5 0.5 0.67 0.83 1 ...
 $ ETSliability  : num  424110 842946 832372 854072 866190 ...
 $ Overallocation: num  70594 146358 171358 175280 207299 ...
 $ ERU           : int  NA NA NA 1022527 1001714 32534 NA NA NA NA ...
 $ Stockpile     : num  70594 216953 388311 1586118 2795131 ... 
 
 lm(formula, data, subset, weights, na.action,
   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
   singular.ok = TRUE, contrasts = NULL, offset, ...)
   
steelregression <- lm(Emissions~Year,data=NZsteelunits)
summary(steelregression)
Call:
lm(formula = Emissions ~ Year, data = NZsteelunits)
Coefficients:
(Intercept)         Year  
   11177338        -4705
summary(steelregression)

Call:
lm(formula = Emissions ~ Year, data = NZsteelunits)

Residuals:
   Min     1Q Median     3Q    Max 
-94319 -26407   2334  25818  71471 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) 11177338   10002745   1.117    0.293
Year           -4705       4964  -0.948    0.368

Residual standard error: 52060 on 9 degrees of freedom
Multiple R-squared:  0.09076,	Adjusted R-squared:  -0.01027 
F-statistic: 0.8984 on 1 and 9 DF,  p-value: 0.368 

plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]],ylim=c(0,2.9*10^6),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]],col="darkgray",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]],col="darkgray",cex=1,pch=16)
lines(abline(steelregression))
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2020), labels = c(2010:2020), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)

years <-NZsteelunits[["Year"]]- 2009
emissions <- NZsteelunits[["Emissions"]]/10^6
steelregression <- lm(emissions~years)


summary(steelregression)
Call:
lm(formula = emissions ~ years)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.094319 -0.026407  0.002334  0.025818  0.071471 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.724630   0.033668  51.224 2.07e-12 ***
years       -0.004705   0.004964  -0.948    0.368    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.05206 on 9 degrees of freedom
Multiple R-squared:  0.09076,	Adjusted R-squared:  -0.01027 
F-statistic: 0.8984 on 1 and 9 DF,  p-value: 0.368 

max(emissions) 
[1] 1.76787

plot(years,emissions,ylim=c(0,1.8),xlim=c(0,12),tck=0.01,axes=TRUE,ann=TRUE, type="l",las=1)
lines(years,emissions,col="darkgray",lwd=1)
points(years,emissions,col="darkgray",cex=1,pch=16)
lines(abline(steelregression))
axis(side=1, tck=0.01, las=0, lwd = 1, tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)

summary(steelregression)
Call:
lm(formula = emissions ~ years)

(Intercept)  1.724630
years       -0.004705
y = 1.724630 + -0.004705 (x)
x = years = 11 {2021} 
predict11 <- 1.724630 + (11 *-0.004705)
predict12 <- 1.724630 + (12 *-0.004705)

predict11
[1] 1.672875
predict12
[1] 1.66817
points(11,predict11,col="darkgray",cex=1,pch=16)
points(12,predict12,col="darkgray",cex=1,pch=16)
