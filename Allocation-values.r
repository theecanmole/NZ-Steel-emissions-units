## NZ Steel

getwd()

# read the csv file of incoming/internationally sourced emission units into R
units <-read.csv("Allocations.csv",skip=0)
NZsteelunits <- units[units[["Applicant"]]=="New Zealand Steel Development Limited",]
str(NZsteelunits)
tibble [15 × 11] (S3: tbl_df/tbl/data.frame)
 $ Year          : num [1:15] 2010 2011 2012 2013 2014 ...
 $ Allocation    : num [1:15] 494704 989304 1003730 1029352 1073489 ...
 $ MeanMayprice  : num [1:15] 17.58 19.84 6.23 1.94 4.08 ...
 $ Value         : num [1:15] 8696896 19627791 6253238 1996943 4379835 ...
 $ Emissions     : num [1:15] 1696438 1685892 1664743 1708143 1732380 ...
 $ twoforone     : num [1:15] 0.25 0.5 0.5 0.5 0.5 0.5 0.5 0.67 0.83 1 ...
 $ ETSliability  : num [1:15] 424110 842946 832372 854072 866190 ...
 $ Footprint     : num [1:15] 1978816 1978608 2007460 2058704 2146978 ...
 $ Overallocation: num [1:15] 70594 146358 171358 175280 207299 ...
 $ ERU           : num [1:15] NA NA NA 1022527 1001714 ...
 $ Stockpile     : num [1:15] 70594 216952 388310 1586117 2795130 ...

NZsteelunits[,2:4]
  Allocation MeanMayprice      Value
        <dbl>        <dbl>      <dbl>
 1     494704        17.6    8696896.
 2     989304        19.8   19627791.
 3    1003730         6.23   6253238.
 4    1029352         1.94   1996943.
 5    1073489         4.08   4379835.
 6    1067501         5.34   5700455.
 7    1048116        14.5   15239607.
 8    1432496        17.0   24295132.
 9    1782366        21.3   37928748.
10    2118983        25.3   53589080.
11    2030166        24.8   50429323.
12    2145482        37.1   79683201.
13    1910503        76.6  146249005.
14    1830000        53.8   98472300
15    1594941        51.4     239015.

# check length of values of NZ steel units
length(NZsteelunits[["Value"]]) 
[1] 15
# print to screen
NZsteelunits[["MeanMayprice"]]
 [1] 17.58 19.84  6.23  1.94  4.08  5.34 14.54 16.96 21.28 25.29 24.84 37.14
[13] 76.55 53.81 51.39

NZsteelunits[["Value"]]
 [1]   8696896.3  19627791.4   6253237.9   1996942.9   4379835.1   5700455.3
 [7]  15239606.6  24295132.2  37928748.5  53589080.1  50429323.4  79683201.5
[13] 146249004.7  98472300.0    239014.9

NZsteelunits[15,]
# A tibble: 1 × 11
   Year Allocation MeanMayprice   Value Emissions twoforone ETSliability
  <dbl>      <dbl>        <dbl>   <dbl>     <dbl>     <dbl>        <dbl>
1  2024    1594941         51.4 239015.  1524587.         1      1524587
# ℹ 4 more variables: Footprint <dbl>, Overallocation <dbl>, ERU <dbl>,
#   Stockpile <dbl>
# check allocation @ may price
1594941 * 51.4
[1] 81979967
# check all values
NZsteelunits[["Allocation"]] * NZsteelunits[["MeanMayprice"]]
 [1]   8696896  19627791   6253238   1996943   4379835   5700455  15239607
 [8]  24295132  37928748  53589080  50429323  79683201 146249005  98472300
[15]  81964018

NZsteelunits[["Value"]] <- NZsteelunits[["Allocation"]] * NZsteelunits[["MeanMayprice"]]
sum(Allocations[["Value"]])

[1] 2189558872

NZsteelunits[["Value"]][15]
[1] 239014.9
<-
NZsteelunits[["Allocation"]][15]
[1] 1594941
*
NZsteelunits[["MeanMayprice"]][15]
[1] 51.39

NZsteelunits[["Allocation"]][15] * NZsteelunits[["MeanMayprice"]][15]
[1] 81964018

# print to screen in million dollars
NZsteelunits[["Value"]]/10^6 
 [1]   8.696896  19.627791   6.253238   1.996943   4.379835   5.700455
 [7]  15.239607  24.295132  37.928748  53.589080  50.429323  79.683201
[13] 146.249005  98.472300  81.964018

# how many emissions units allocated over the 15 years?  22m
sum(NZsteelunits[["Allocation"]])
[1] 21551133
# what is the total value of the allocated units
sum(NZsteelunits[["Value"]])
[1] 634505574
# [1] 552780571
# [1] 552541556           # 552 million
# [1] 307914582 # $308 million.....
# How many tonnes of iron sand steel was smelted?
sum(NZsteelunits[["Emissions"]])
[1] 24941540

# create a matrix to draw barplots with
datamatrix <- matrix(c( NZsteelunits[["Value"]]/10^6), nrow = 1, ncol=15, byrow=TRUE, dimnames = list(c("Market Value"),
c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024")))
datamatrix
                 2010     2011     2012     2013     2014     2015     2016
Market Value 8.696896 19.62779 6.253238 1.996943 4.379835 5.700455 15.23961
                 2017     2018     2019     2020    2021    2022    2023
Market Value 24.29513 37.92875 53.58908 50.42932 79.6832 146.249 98.4723
                 2024
Market Value 81.96402

svg(filename="NZsteel-units-marketvalue-720by540.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
par(mar=c(4, 3, 4, 1)+0.1)
barplot(datamatrix,ylim=c(0,150),las=1,space=0.5, beside = FALSE, col=c("#5035D2"))
mtext(side=1,line=2.5,cex=1,expression(paste("Source: EPA Industrial Allocations 2024")))
#legend("left", inset=c(0.0,0.0) ,bty="n",c("Annual market value of allocated units"),fill=c("#5035D2"))
mtext(side=2,cex=1, line=-1.8,expression(paste("$million")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
mtext(side=3,cex=1.5, line=1.2,expression(paste("NZ Steel market value of emission units allocated from 2010 to 2024")) )
mtext(side=3,cex=1, line=-0.4,expression(paste("The 22 million free emission units allocated to NZ Steel had a market value of $634 million")) )
dev.off() 

# or read in data from csv file
NZsteelunits <-  read.csv("NZsteelunits.csv") 
# assume zero allocation 2022 2023 2024...
NZsteelunitsvalue <-append(NZsteelunits[["Value"]]/10^6,c(0,0,0))
str(NZsteelunitsvalue)
num [1:15] 8.7 19.63 6.25 2 4.38 ... 
 
# assume NZ Steel GIDI grant of 140M is 1/3s over 2023 2024 2025 and zero 2010 to 2021
gidigrant <- rep(0,12) 
gidigrant <- append(gidigrant,rep(140/3,3))
str(gidigrant) 
num [1:15] 0 0 0 0 0 0 0 0 0 0 ... 
# create matrix for a barplot
datamatrix1 <- matrix(c( NZsteelunitsvalue,gidigrant), nrow = 2, ncol=15, byrow=TRUE, dimnames = list(c("Increase emissions","Reduce emissions"), c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"))) 
datamatrix1 
                       2010     2011     2012     2013     2014     2015
Increase emissions 8.696896 19.62779 6.253238 1.996943 4.379835 5.700455
Reduce emissions   0.000000  0.00000 0.000000 0.000000 0.000000 0.000000
                       2016     2017     2018     2019     2020    2021
Increase emissions 15.33394 24.29513 37.92875 53.58908 50.42932 79.6832
Reduce emissions    0.00000  0.00000  0.00000  0.00000  0.00000  0.0000
                       2022     2023     2024
Increase emissions  0.00000  0.00000  0.00000
Reduce emissions   46.66667 46.66667 46.66667 

svg(filename="NZsteel-freeunits-GIDI-720by540.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-units-marketvalue-565by420.png", bg="white", width=570, height=428,pointsize = 12)
#png("NZsteel-units-marketvalue-600by450.png", bg="white", width=600, height=450,pointsize = 11)
#png("NZsteel-units-marketvalue-600by600.png", bg="white", width=600, height=600,pointsize = 11)
#png("NZsteel-units-marketvalue-720by540.png", bg="white", width=720, height=540,pointsize = 14)
par(mar=c(4, 3, 4, 1)+0.1)
barplot(datamatrix1,las=1,space=0.5, beside = FALSE, col= c("#A60727","#74C812")) 
mtext(side=1,line=2.5,cex=1.2,expression(paste("Source: EPA Industrial Allocation decisions")))
legend("topleft", inset=c(0.0,0.0),cex=1.2 ,bty="n",c("NZETS subsidy to increase emissions $308 million","GIDI subsidy to reduce emissions $140 million"),fill=c("#A60727","#74C812"))
mtext(side=2,cex=1.2, line=-1.8,expression(paste("$million")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
mtext(side=3,cex=1.7, line=1.2,expression(paste("NZ Steel NZETS unit allocation subsidy and GIDI subsidy")) )
#mtext(side=3,cex=1, line=-0.4,expression(paste("Free emission units worth $308 million subsidise emissions, GIDI grant of $140m subsidy reduce emissions")) )
dev.off() 

# square charts
svg(filename="NZsteel-freeunits-GIDI-720by540.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-units-marketvalue-565by420.png", bg="white", width=570, height=428,pointsize = 12)
#png("NZsteel-units-marketvalue-600by450.png", bg="white", width=600, height=450,pointsize = 11)
#png("NZsteel-units-marketvalue-600by600.png", bg="white", width=600, height=600,pointsize = 11)
#png("NZsteel-units-marketvalue-720by540.png", bg="white", width=720, height=540,pointsize = 14)
#png("NZsteel-units-marketvalue-600by600.png", bg="white", width=600, height=600,pointsize = 11)
#png("NZsteel-units-marketvalue-720by720.png", bg="white", width=720, height=720,pointsize = 12)
par(mar=c(4, 3, 4, 1)+0.1)
barplot(datamatrix1,las=1,space=0.5, beside = FALSE, col= c("#A60727","#74C812")) 
mtext(side=1,line=2.5,cex=1.3,expression(paste("Source: EPA Industrial Allocation decisions")))
legend("topleft", inset=c(0.0,0.0),cex=1.3 ,bty="n",c("NZETS subsidy to increase emissions $308 million","GIDI subsidy to reduce emissions $140 million"),fill=c("#A60727","#74C812"))
mtext(side=2,cex=1.3, line=-1.8,expression(paste("$million")))
mtext(side=4,cex=1, line=0.05,R.version.string)
mtext(side=3,cex=1.8, line=1.2,expression(paste("NZ Steel NZETS unit allocation subsidy and GIDI subsidy")) )
dev.off() 

# square chart only units allocated and the ETS liability 
#svg(filename="NZsteel-Allocation-GHGs-line-2010-2020-720by540v3a.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))
png("NZsteel-Allocation-GHGs-line-2010-2020-600by600-v3a.png", bg="white", width=600, height=600,pointsize = 12)
#png("NZsteel-Allocation-GHGs-line-2010-2020-560by420-v3a.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1)
plot(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,ylim=c(0,2.9),tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1, tick = TRUE)
#lines(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",lwd=1)
#points(NZsteelunits[["Year"]],NZsteelunits[["Emissions"]]/10^6,col="#1B9E77",cex=1,pch=16)
lines(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",lwd=2)
points(NZsteelunits[["Year"]],NZsteelunits[["Allocation"]]/10^6,col="#D95F02",cex=1.3,pch=17)
lines(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",lwd=2)
points(NZsteelunits[["Year"]],NZsteelunits[["ETSliability"]]/10^6,col="#7570B3",cex=1.3,pch=15 )
legend(2011, 2.6, cex=1.4, bty = "n", c("Emission units allocated to NZ Steel","Steel emissions x 'two for one' equals ETS liability"), col =  c("#D95F02","#7570B3") , text.col = 1, lty = 1, pch = c(17,15))
mtext(side=3,cex=1.7, line=-3.9, expression(paste("NZ Steel Limited annual allocation of emissions units\n always exceeds the ETS liability 2010 to 2021")) ) 
mtext(side=1,line=-1.25,cex=1.2,"Data: New Zealand’s Greenhouse Gas Inventory 1990 - 2021\nEPA Industrial Allocations 2021")
mtext(side=2,cex=1.2, line=-1.2,expression(paste("million tonnes C", O[2], "-e")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()
