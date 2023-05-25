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

#svg(filename="Emissions-units-surrendered-2010-2022-720by540.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
#png("Emissions-units-surrendered-2010-2022-560by420.png", bg="white", width=560, height=420,pointsize = 12)
#png("Emissions-units-surrendered-2010-2022-600by450.png", bg="white", width=600, height=450,pointsize = 12)
png("NZsteel-2010-2022-720by540.png", bg="white", width=720, height=540,pointsize = 14)
par(mar=c(4, 3, 4, 1)+0.1)
barplot(datamatrix,las=1,space=0.5, beside = FALSE, col=c("#5035D2","#38BEB6","#E2A001","#73C90E","#A60727")) 
mtext(side=1,line=2.5,cex=1,expression(paste("Source: EPA ETS unit movement report")))
legend("topright", inset=c(0.0,0.0) ,bty="n",c(
"Imported Kyoto units ERUs CERs RMUs",
"Fixed Price Option $25 or $35 per unit",
"NZ Assigned Amount Units",
"Forestry NZUs",
"NZUs"),fill=c("#5035D2","#38BEB6","#E2A001","#73C90E","#A60727"))
mtext(side=2,cex=1, line=-1.8,expression(paste("million units")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
mtext(side=3,cex=1.5, line=1,expression(paste("NZETS types of emission units surrendered from 2010 to 2022")) )
dev.off() 

# create png chart of value of units allocated
# create svg chart of value of units allocated # I should replace with a barplot
svg(filename="NZsteel-Allocation-Value-2010-2020-560by420-v1.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))
png("NZsteel-Allocation-Value-2010-2020-560by420-v1.png", bg="white", width=560, height=420,pointsize = 12)
par(mar=c(2.7,2.7,1,1)+0.1) 
plot(NZsteelunits[["Year"]],NZsteelunits[["Value"]]/10^6,tck=0.01,axes=FALSE,ann=FALSE, type="n",las=1)
axis(side=1, tck=0.01, las=0, lwd = 1, at = c(2010:2021), labels = c(2010:2021), tick = TRUE)
axis(side=2, tck=0.01, las=2, line = NA,lwd = 1 ,tick = TRUE)
lines(NZsteelunits[["Year"]],NZsteelunits[["Value"]]/10^6,col="#E7298A",lwd=1)
points(NZsteelunits[["Year"]],NZsteelunits[["Value"]]/10^6,cex=1,pch=16, col="#E7298A")
legend(2010, 45, bty = "n", "Value of free emissions units allocated by EPA", col = "#E7298A"  , text.col = 1, lty = 1, pch = 16 )
mtext(side=3,cex=1.5, line=-3.5, expression(paste("NZ Steel value of free emissions units\n2010 to 2021")) ) 
mtext(side=1,line=-1.25,cex=1,"Data: New Zealand’s Greenhouse Gas Inventory 1990 – 2020\nETS Participant Emissions EPA October 2021")
mtext(side=2,cex=1.1, line=-1.2,expression(paste("$NZD million")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
box()
dev.off()

# check length of values of NZ steel units
length(NZsteelunits[["Value"]]) 
[1] 12 
# print to screen
NZsteelunits[["Value"]]
[1]  8696896 19627791  6253238  1996943  4379835  5700455 15333937 24295132
[9] 37928748 53589080 50429323 79683201 
NZsteelunits[["Value"]]/10^6 

# what is the total value of the allocated units
sum(NZsteelunits[["Value"]])
sum(NZsteelunits[["Value"]])
[1] 307914582 # $308 million..... 
# how many emissions units allocated over the 12 years?
sum(NZsteelunits[["Allocation"]]) 
[1] 16215689                        # 16.215689 million emission units

# create a matrix to draw barplots with
datamatrix <- matrix(c( NZsteelunits[["Value"]]/10^6), nrow = 1, ncol=12, byrow=TRUE, dimnames = list(c("Market Value"), c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")))

#svg(filename="NZsteel-units-marketvalue-720by540.svg", width = 8, height = 6, pointsize = 12, onefile = FALSE, family = "sans", bg = "white", antialias = c("default", "none", "gray", "subpixel"))  
png("NZsteel-units-marketvalue-565by420.png", bg="white", width=570, height=428,pointsize = 12)
#png("NZsteel-units-marketvalue-600by450.png", bg="white", width=600, height=450,pointsize = 12)
#png("NZsteel-units-marketvalue-720by540.png", bg="white", width=720, height=540,pointsize = 14)
par(mar=c(4, 3, 4, 1)+0.1)
barplot(datamatrix,las=1,space=0.5, beside = FALSE, col=c("#5035D2")) 
mtext(side=1,line=2.5,cex=1,expression(paste("Source: EPA ETS unit movement report")))
legend("topleft", inset=c(0.0,0.0) ,bty="n",c("Annual market value of allocated units"),fill=c("#5035D2"))
mtext(side=2,cex=1, line=-1.8,expression(paste("$million")))
mtext(side=4,cex=0.75, line=0.05,R.version.string)
mtext(side=3,cex=1.5, line=1.2,expression(paste("NZ Steel market value of emission units allocated from 2010 to 2021")) )
mtext(side=3,cex=1, line=-0.4,expression(paste("The 16 million free emission units allocated to NZ Steel have a market value of $308 million")) )
dev.off() 
