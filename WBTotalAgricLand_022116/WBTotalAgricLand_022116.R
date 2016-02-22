getwd()
setwd("C:/Users/erica/Desktop/ResearchMethods/R-projects/Soil-data/WorldBankData/WorldBankLandArea")
#load csv file from working directory
#use sep="/t" for tab delimited files
test <- read.csv("WBLandArea_countries.csv", header = TRUE, sep=",")

#data file sorted alphabetically, even though the original file is sorted by value.

##------------------------------------------------
##Try to make plot show up for data
##------------------------------------------------

str(test)
library(ggplot2)
library(reshape)
library(scales)

#trim off identifying columns - leaves just data!
#testcut<-test[,6:59]
rm(testcut)

#create a years array from 1961-2014 to match column headers
years<-c(1961:2014)

#works - plots two columns of test dataset
ggplot(test, aes(X1961,X1962))+geom_point(color="blue")

#looks like I may need to restructure the data into a 
#single column before I can use it. To do that, use melt.
melted<-melt(test,id.vars="Country.Name", measure.vars=6:59, variable_name = "year")
ggplot(test, aes(X1961,X1962))+geom_point(color="blue")

#get rid of X in year column, by using a substring of the 2nd-5th char of the time variable. Copy
melted$year2<-substr(melted$year,2,5)
#now convert the character into a number
melted$year <-as.numeric(melted$year2)
#and delete the temp column
melted$year2<-NULL

#no longer need years
rm(years)

#lines were actually changing in the example w/ boxes above! 
#Needed to suppress legend (boxes) so that I could see them
ggplot(melted, aes(x=year,y=value,group=Country.Name,color=Country.Name))+geom_line()+theme(legend.position="none")

##------------------------------------------------
##Trim initial data to limit lines plotted
##------------------------------------------------

#Look at top 50 countries
#Need to pre-filter data in test array before melting,
#use filtered/sorted data to create melt

# varToSort[order(-varToSortBy),]
#neg means sort in descending order
sorted<-test[order(-test$X2014),]    

top50<-sorted[1:50,]

top50Melt<-melt(top50,id.vars="Country.Name", measure.vars=6:59, variable_name = "year")

#get rid of X, by using a substring of the 2nd-5th char of the time variable. Copy
top50Melt$year2<-substr(top50Melt$year,2,5)
#now convert the character into a number
top50Melt$year <-as.numeric(top50Melt$year2)
#and delete the temp column
top50Melt$year2<-NULL

#plot land area of top fifty countries as line graph
ggplot(top50Melt, aes(x=year,y=value,group=Country.Name,color=Country.Name))+geom_line()+theme(legend.position="none")

#plot land area bar charts for...1961? Why not in order??
ggplot(data=top50Melt[1:50,], aes(x=Country.Name[1:50], y=value[1:50])) +geom_bar(stat="identity")

top50Melt$value[2651:2700]

#still not in order, but I think this is the right year
#without the top50melt$ in the aes factors, get error:'data' must be of a vector type, was 'NULL'
ggplot(data=top50Melt[2651:2700,], aes(x=top50Melt$Country.Name[2651:2700], y=top50Melt$value[2651:2700])) +geom_bar(stat="identity")

top50in2014<-top50Melt[2651:2700,]

#need to reorder the factors; otherwise defaults to alphabetical: https://kohske.wordpress.com/2010/12/29/faq-how-to-order-the-factor-variables-in-ggplot2/
reordered <-reorder(top50in2014$Country.Name,top50in2014$value)

#plot again using reordered factors for x (coord flip rotates plot sideways)
#Load scales library, then scale y (horizontal, because rotated) to get non-scientific notation
#use theme to change size of axis and tick mark labels
ggplot(data=top50in2014, aes(x=reordered, y=top50in2014$value)) +geom_bar(width=.75,stat="identity")+coord_flip()+labs(title="Countries with largest land area", x="Country", y = "Land area (sq. km)")+scale_y_continuous(labels=comma)+ theme(axis.title.x = element_text(size=12),axis.title.y = element_text(size=12), axis.text.x = element_text(size =10), axis.text.y = element_text(size =7), plot.title=element_text(size=16))


#***********Import second dataset as csv

agric <- read.csv("C:/Users/erica/Desktop/ResearchMethods/R-projects/Soil-data/WorldBankData/WorldBankAgricLand/WBAgricLand_countries.csv", header = TRUE, sep=",")
#trim to same size (ends on 2013)
agric <- agric[,1:59]

#copy test array with land area info from above
landArea<-test

#merge the two into one dataframe (must be same size)
#this stacks the two sets of data on top of one another (vertically)
combined <- rbind(landArea,agric)

#since I'm only plotting 2013 data for now (2014 has no agric land data), make a smaller version with just the 2013 data
combinedSmall <- combined
#did the first few manually; should start at 58, go down to 2?
for (i in 57:4){
combinedSmall[,i]<-NULL
}
combinedSmall[,2]<-NULL

ggplot(data=combinedSmall, aes(x=combinedSmall$Country.Name, y=combinedSmall$X2013,fill=combinedSmall$Indicator.Name)) +geom_bar(width=.75,stat="identity")+coord_flip()

#sorts all elements by the value desired - better to put second array in as a new column in combined, rather than using rbind?
combinedSorted<-combinedSmall[order(-combinedSmall$X2013),]    

#**********************************************************

#Try again, making combined from a copy of landarea, and adding agric column to it 
#works with one year's worth of data, not sure what to do with multi-year data

#re-import original land area data (make sure that sorting and headers are correct!!)
combined<-test
combined$agric <-agric[,58]

#clean out extra years (use only 2013 data) 
combinedSmall <- combined
#did the first few manually; should start at 58, go down to 2?
for (i in 57:2){
  combinedSmall[,i]<-NULL
}

#sort the countries by total land area (verified that this works)
combinedSorted<-combinedSmall[order(-combinedSmall$X2013),]    

#take the top 50
combinedTop50<-combinedSorted[1:50,]

#reorder the factors to link country names to land area values; otherwise defaults to alphabetical: https://kohske.wordpress.com/2010/12/29/faq-how-to-order-the-factor-variables-in-ggplot2/
combinedReordered <-reorder(combinedTop50$Country.Name,combinedTop50$X2013)

#plot to check data - looks good
ggplot(data=combinedTop50, aes(x=combinedReordered, y=combinedTop50$X2013)) +geom_bar(width=.75,stat="identity")+coord_flip()

#plot percent agric land for each country (same order)
ggplot(data=combinedTop50, aes(x=combinedReordered, y=combinedTop50$agric)) +geom_bar(width=.75,stat="identity")+coord_flip()+labs(title="% agricultural land for 50 largest countries", x="Country", y = "% Agricultural Land")+scale_y_continuous(labels=comma)+ theme(axis.title.x = element_text(size=12),axis.title.y = element_text(size=12), axis.text.x = element_text(size =10), axis.text.y = element_text(size =7), plot.title=element_text(size=16))

#****************************************************

#calculate the actual agric land area per country
absAgricAreaTop50 <- combinedTop50
absAgricAreaTop50$absAgricArea <- absAgricAreaTop50$X2013*absAgricAreaTop50$agric/100

#melt into one column with two factors: LandArea and AgricLandArea
absAgricMelt<-melt(absAgricAreaTop50,id.vars="Country.Name", measure.vars=2:4, variable_name = "x2013", na.rm = FALSE)

help(melt)


ggplot(data=absAgricMelt[51:150,], aes(x=absAgricMelt$Country.Name[51:150], y=absAgricMelt$value[51:150],fill=absAgricMelt$X2013[51:150])) +geom_bar(width=.75,stat="identity")+coord_flip()

#need non-contiguous chunks of the melted array
ggplot(data=absAgricMelt[51:150,], aes(x=absAgricMelt$Country.Name[51:150], y=absAgricMelt$value[51:150],fill=absAgricMelt$x2013[51:150]))+geom_bar(width=.5,stat="identity")+coord_flip()

#cut out the agricultural % data
cutMelt <-absAgricMelt
cutMelt[51:100,]<-cutMelt[101:150,]
cutMelt<-cutMelt[1:100,]

#plot stacked bar chart with agricultural and total land areas
ggplot(data=cutMelt, aes(x=cutMelt$Country.Name, y=cutMelt$value,fill=cutMelt$x2013))+geom_bar(width=.5,stat="identity")+coord_flip()+labs(title="Total and agricultural land area for 50 largest countries", x="Country", y = "Land area (sq. km)")+scale_y_continuous(labels=comma)+ theme(axis.title.x = element_text(size=12),axis.title.y = element_text(size=12), axis.text.x = element_text(size =10), axis.text.y = element_text(size =7), plot.title=element_text(size=16))+scale_fill_manual(values=c("#E69F00","#999999"), name="Land Type", breaks=c("X2013", "absAgricArea"),  labels=c("Agricultural", "Total"))

#add a new column called factor to store the ordered factors (can't seem to use the raw combinedReordered data to get the same result...)
cutMelt$factor <-0
cutMelt$factor<-combinedReordered

#replot, with countries in order.
ggplot(data=cutMelt, aes(x=cutMelt$factor, y=cutMelt$value,fill=cutMelt$x2013))+geom_bar(width=.75,stat="identity",position="dodge")+coord_flip()+labs(title="Total and agricultural land area for 50 largest countries", x="Country", y = "Land area (sq. km)")+scale_y_continuous(labels=comma,expand = c(0, 0), limits = c(-5,17500000))+ theme(axis.title.x = element_text(size=12),axis.title.y = element_text(size=12), axis.text.x = element_text(size =10), axis.text.y = element_text(size =7), plot.title=element_text(size=16))+scale_fill_manual(values=c("#999999","#E69F00"), name="Land Type", breaks=c("X2013", "absAgricArea"),  labels=c("Total","Agricultural"))
