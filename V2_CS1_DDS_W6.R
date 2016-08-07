# file WITHOUT DataCamp Interactive


rm(list = ls()) 
objects()
getwd() 
setwd("C:\\Users\\najee\\Documents\\R\\MSDS6306-W6-HW")

library(ggplot2)
library(stats)
library(dtplyr)
library(tabplot)
library(rmarkdown)


# Data reading:
# File 1
####GDPData 
GDPData <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv ", header = TRUE, sep = ",", stringsAsFactors = FALSE)
GDPData2 <- GDPData[-c(1:4,195:330), -c(3,6,7:10)]
names(GDPData2) <- c("ID","Ranking","Country", "GDP")
head(GDPData2, 13)
tail(GDPData2, 13)
dim(GDPData2)[1]

# File 2
EduData <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
names(EduData) <- c("ID","Country","Income_Group", "Region", "Lending_Group", "Other")
dim(EduData)[1] 
str(EduData)
head(EduData, 13)
tail(EduData, 13)

# ********************************************
# find Exclusions of countries whose IDs do not match between 2 files

GDPDataList <- GDPData2[,c(1,3)]
row.names(GDPDataList) <- NULL
B <- GDPDataList[order(GDPDataList$ID),]

EduDataList <- EduData[,c(1,2)]
row.names(EduDataList) <- NULL
A <- EduDataList[order(EduDataList$ID),]
dim(B)
dim(A)
N <- dim(A)[1] 
N
n <- dim(B)[1] 
n
# Implementing a double loop by column to check for equal values in two tables and write check results in a new DF table with Country Names and Check columns 
DF <- data.frame(ID = rep("", N), Country = rep("", N), Check = rep("", N),stringsAsFactors = FALSE)
for (i in 1:N)  {
  j <- 1
  while (j <= n)
  {
    if (A[i,1] == B[j,1]) 
    {
      DF[i,1] = A[i,1]
      DF[i,2] = A[i,2]
      DF[i,3] = "yes"
      i <- i + 1
      break
    } else 	
    {
      DF[i,1] = A[i,1]
      DF[i,2] = A[i,2]
      DF[i,3] = "no"
      j <- j + 1
    } 
  }
}
# fix(DF) #And close
Exclusion = subset(DF, Check == "no")
row.names(Exclusion) <- NULL
Exclusion2 <- Exclusion[,c(1,2)]
Exclusion2
# ******************************************

# Q1 Match on cty-code. How many match ?

EduData2 <- EduData[,c(1:3)]
row.names(EduData2) = NULL
Data <- merge(GDPData2, EduData2, by = "ID") 

dim(Data)[1]
answ1 <- length(Data$ID)
message("Number of countries for which both data type are available is: ", answ1)
# 189 records with country code but names are still mixed strings

# *********************************************

# Question 2: Sort the data frame in ascending order by GDP rank. 
# What is the 13th country in the resulting data frame?  
# Ensuring GDP is captured as numeric vector and is not in 
# scientific format in the dataframe 

sapply(Data, class) 
typeof(Data$GDP)
Data$GDP <- gsub("\\,","", Data$GDP)
GDP <- Data$GDP
Data2 <- transform(Data, GDP = as.numeric(as.character(GDP)))
typeof(Data2$GDP)

# Sorting data frame as requested and displaying and storing answer 2
sort.Data2 <- Data2[order(Data2$GDP), ]

#Store sorted GDP Data in a dataframe
SortedGdpRankData2 <- data.frame(sort.Data2)
head(SortedGdpRankData2, 15)

answ2 <- SortedGdpRankData2[13,5]
message("The 13th country by GDP ranked in ascending order is: ", answ2)

# *******************************************************

# Question 3: What are the average GDP rankings 
# for the "High income: OECD" and "High income: nonOECD" groups?  
# Ensuring also the Ranking attribute is captured as numeric 
# vector and verifying that no further missing values are for 
# the attributes in scope for the rest of the exercise	

# SortedGdpRankData2

Ranking <- SortedGdpRankData2$Ranking
Data3 <- transform(SortedGdpRankData2, Ranking = as.numeric(as.character(Ranking)))
head(Data3, 15)


answ3.1 <- with(Data3, mean(Ranking[Income_Group == "High income: OECD"]))
message("The avg GDP Ranking for High income: OECD countries is: ", round(answ3.1, 2))
answ3.2 <- with(Data3, mean(Ranking[Income_Group == "High income: nonOECD"]))
message("The avg GDP Ranking for High income: Non-OECD countries is: ", round(answ3.2, 2))

# *************************************

# Question 4: Plot the GDP for all of the countries. Use ggplot2 to color your plot by Income Group

head(SortedGdpRankData2, 15)
str(SortedGdpRankData2)
library(tabplot)


tableplot(SortedGdpRankData2)

tableplot(SortedGdpRankData2, select = c(GDP))
# Convert GDP Sacaling
GdpScaledToBillion <- SortedGdpRankData2$GDP/1000000


# Chart 1: 
c1 <- plot(SortedGdpRankData2$GDP, bty = "l", col = "green", pch = 19, main = "GDP distribution", xlab = "Countries (asc. order)", ylab = "GDP in $tr")


# Chart plot with Corrected GDP Scale and identifying top 4 outliers

op <- par(mar = c(5,5,4,2) + 0.5)
plot(GdpScaledToBillion, bty = "l", col = "blue", pch = 18, axes = FALSE, xlab = "", ylab = "")
title(main = "GDP Per Country", col.main = "red", lines.main = 14, sub = "Source: Elaboration on World Bank Data", col.sub = "grey") ##lines.sub = 4, font.sub = 3
axis(1)
pts <- pretty(GdpScaledToBillion, n = 10)
axis(2, at = pts, las = 1)
box()
title(ylab = "GDP ($bn)", line = 2)
title(xlab = "Sequence of Countries", line = 2)
par(op)
text(182, 16.1, "US", pos = 3, xpd = F, col = "black", vfont = NULL, font = 2, cex = 0.6, offset = 0)
text(180, 8.1, "CHN", pos = 3, xpd = F, col = "black", vfont = NULL, font = 2, cex = 0.6, offset = 0)
text(178, 5.85, "JPN", pos = 3, xpd = F, col = "black", vfont = NULL, font = 2, cex = 0.6, offset = 0)
text(176, 3.35, "DEU", pos = 3, xpd = F, col = "black", vfont = NULL, font = 2, cex = 0.6, offset = 0)
text(8, 0.3, "Tuvalu ($40mm)", pos = 3, xpd = F, col = "black", vfont = NULL, font = 2, cex = 0.6, offset = 0, adj = 1)

colour <- c("grey", "yellow","orange","red","black")
col.list <- rep(0, length(SortedGdpRankData2$Income_Group))
col.list[SortedGdpRankData2$Income_Group == "Low income"] <- 1
col.list[SortedGdpRankData2$Income_Group == "Lower middle income"] <- 2
col.list[SortedGdpRankData2$Income_Group == "Upper middle income"] <- 3
col.list[SortedGdpRankData2$Income_Group == "High income: nonOECD"] <- 4
col.list[SortedGdpRankData2$Income_Group == "High income: OECD"] <- 5
p <- plot(GdpScaledToBillion, pch = 19, main = "GDP per Income Group", xlab = "Countries (asc. Order)", ylab = "GDP in $m", col = c(colour[col.list]))
legend(x = "topleft", cex = 0.7, c(" Low income in Grey ",
                                   " Lower middle income in Yellow ",
                                   " Upper middle income in Orange ",
                                   " High_income_Non_OECD in Red ",
                                   " High_income_OECD in Black "))
# And close chart





#################################################################
# Formatting and adding text / labels to this last chart as above 
colour <- c("khaki1","khaki3","khaki4","indianred2","firebrick1")
col.list <- rep(0, length(Data3$Income_Group))
col.list[Data3$Income_Group=="Low income"] <- 1
col.list[Data3$Income_Group=="Lower middle income"] <-2
col.list[Data3$Income_Group=="Upper middle income"] <- 3
col.list[Data3$Income_Group=="High income: nonOECD"] <- 4
col.list[Data3$Income_Group=="High income: OECD"] <- 5
op<-par(mar=c(3.0, 3.0, 1.5, 1.5))
plot(GDP/1000000, bty="l", col=c(colour[col.list]), pch=19, axes = FALSE, xlab="", ylab="")
title(main="Distribution of GDP Countries by Income Group", col.main="indianred4", sub="Source: Elaboration on World Bank Data", col.sub="grey", font.sub=3) 
pts <- pretty(GDP / 1000000, n=10)
axis(2, at = pts, las = 1)
box()
title(ylab = "GDP ($bn)", line = 2)
par(op)
text(20, 15, "Low Income", pos = 3, xpd = F, col="khaki1", vfont = NULL, font = 2, cex=1,2, offset=0, adj = c(0,0))
text(20, 14.5, "Lower Middle Income", pos = 3, xpd = F, col="khaki3", vfont = NULL, font = 2, cex=1.2, offset=0, adj = c(0,0))
text(20, 14, "Upper Middle Income", pos = 3, xpd = F, col="khaki4", vfont = NULL, font = 2, cex=1,2, offset=0, adj = c(0,0))
text(20, 13.5, "High Income [Non OECD]", pos = 3, xpd = F, col="indianred2", vfont = NULL, font = 2, cex=1.2, offset=0, adj = c(0,0))
text(20, 13, "High Income [OECD]", pos = 3, xpd = F, col="firebrick1", vfont = NULL, font = 2, cex=1.2, offset=0, adj = c(0,0))
textbox(c(54,60),1.8, "[54] Malta ", cex=1, col="indianred2", border="black", font=2)
textbox(c(68,78),1.8, "[68] Iceland ", cex=1, col="indianred2", border="black", font=2)
textbox(c(103,112),1.8, "[103] Kenya", cex=1, col=" khaki1", border="black", font=2)
textbox(c(131,145),1.8, "[131] Bangladesh", cex=1, col=" khaki1", border="black", font=2)
textbox(c(136,144),3.2, "[136] Qatar", cex=1, col=" indianred2", border="black", font=2)
textbox(c(177,188),1.8, "[177] Indonesia", cex=1, col=" khaki3", border="black", font=2)
textbox(c(150,160),2, "[150] Thailand", cex=1, col=" khaki3", border="black", font=2)
textbox(c(188,195),8.2, "[188] China", cex=1, col=" khaki3", border="black", font=2)
# And close chart
# Adding tables with Top and Bottom 10 countries by GDP
# This would provide space for a 3X2 chart table with first row merged
# nf <- layout(matrix(c(1,1,2,3), 2, 2, byrow=TRUE), respect=TRUE)
# layout.show(nf)
# This instead is just what we need for our purposes
Data4 <- Data3[order(Data3$Ranking), ]
TopData4 <- Data4[c(1:10), c(2:4,6)]
row.names(TopData4)<-NULL
names(TopData4)<-c("Ranking","Country","GDP ($m)", "Income Group")
WorstData4 <- Data4[c(180:180), c(2:4,6)]
row.names(WorstData4)<-NULL
names(WorstData4)<-c("Ranking","Country","GDP ($m)", "Income Group")
par(mfrow=c(2,1), oma=c(0,0,2,0))
plot(1,type='n', axes=F, xlab="", ylab="")

title(main="Highest", col.main="indianred4") 
addtable2plot(0.6,0.2,TopData4,bty="o",display.rownames=FALSE,hlines=TRUE,vlines=TRUE)
plot(1,type='n', axes=F, xlab="", ylab="")
title(main="Lowest", col.main="indianred4") 
addtable2plot(0.6 ,0.2,TopData4,bty="o",display.rownames=TRUE,hlines=TRUE,vlines=TRUE)
title("Highest and Lowest GDP Countries ??? Ranking & Income Group Status", outer=TRUE, cex=1.2, col="blue", font=3)
# And close



