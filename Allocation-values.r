getwd()

# read the csv file of incoming/internationally sourced emission units into R
units <-read.csv("Allocations.csv",skip=0)
NZsteelunits <- units[units[["Applicant"]]=="New Zealand Steel Development Limited",]
str(NZsteelunits)
'data.frame':	12 obs. of  5 variables:
 $ Activity  : chr  "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" "Iron and steel manufacturing from iron sand" ...
 $ Applicant : chr  "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" "New Zealand Steel Development Limited" ...
 $ Year      : int  2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 ...
 $ Allocation: int  494704 989304 1003730 1029352 1073489 1067501 1048116 1432496 1782366 2118983 ...
 $ Value     : num  8696896 19627791 6253238 1996943 4379835 ... 


# check length of values of NZ steel units
length(NZsteelunits[["Value"]]) 
[1] 12 
# print to screen
NZsteelunits[["Value"]]
[1]  8696896 19627791  6253238  1996943  4379835  5700455 15333937 24295132
[9] 37928748 53589080 50429323 79683201 
# print to screen in million dollars
NZsteelunits[["Value"]]/10^6 

# what is the total value of the allocated units

sum(NZsteelunits[["Value"]])
[1] 307914582 # $308 million..... 
# how many emissions units allocated over the 12 years?
sum(NZsteelunits[["Allocation"]]) 
[1] 16215689                        # 16.215689 million emission units

# create a matrix to draw barplots with
datamatrix <- matrix(c( NZsteelunits[["Value"]]/10^6), nrow = 1, ncol=12, byrow=TRUE, dimnames = list(c("Market Value"), c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")))

svg(filename="NZsteel-units-marketvalue-720by540.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("NZsteel-units-marketvalue-565by420.png", bg="white", width=570, height=428,pointsize = 12)
#png("NZsteel-units-marketvalue-600by450.png", bg="white", width=600, height=450,pointsize = 12)
#png("NZsteel-units-marketvalue-720by540.png", bg="white", width=720, height=540,pointsize = 14)
par(mar=c(4, 3, 4, 1)+0.1)
barplot(datamatrix,las=1,space=0.5, beside = FALSE, col=c("#5035D2")) 
mtext(side=1,line=2.5,cex=1,expression(paste("Source: EPA ETS unit movement report")))
#legend("left", inset=c(0.0,0.0) ,bty="n",c("Annual market value of allocated units"),fill=c("#5035D2"))
mtext(side=2,cex=1, line=-1.8,expression(paste("$million")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
mtext(side=3,cex=1.5, line=1.2,expression(paste("NZ Steel market value of emission units allocated from 2010 to 2021")) )
mtext(side=3,cex=1, line=-0.4,expression(paste("The 16 million free emission units allocated to NZ Steel have a market value of $308 million")) )
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
