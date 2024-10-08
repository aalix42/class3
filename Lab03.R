##install.packages(c("dplyr", "lubridate", "ggplot2"))
##install.packages(c("dplyr"))
library(dplyr)
library(lubridate)
library(ggplot2)

datC02 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

#change column names
colnames(datC02)[4] <- "CO2"
colnames(datC02)
plot(datC02$Year, datC02$CO2,
     xlab = " Year", ylab = "C02 Emissions")

ggplot(datC02, aes(x = Year, y = CO2, color = Entity)) + geom_line()

NA_CO <- datC02 %>%
  filter(Entity == "United States"|
           Entity == "Mexico"|Entity == "Canada")

ggplot(NA_CO, aes(x =Year, y = CO2, color = Entity)) + 
  geom_line() + 
  labs(x = "Year", y = "CO2 emisssions (tons CO2)") + 
  theme_light() + 
  scale_color_manual(values = c("#7FB3D555","#34495E55", "#E7B80055"))


compCO2 <- datC02[datC02$Year >= 1950 & datC02$Entity == "France" |
                    datC02$Year >= 1950 & datC02$Entity == "India" |
                    datC02$Year >= 1950 &  datC02$Entity == "Russia" , ]

ggplot(data = compCO2 , aes(x=Entity, y=CO2))+ # look at CO2 by country
  geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
  geom_boxplot(width=0.03,size=0.15, fill="grey90")+ # add grey 
  labs(x = "Country", y="Annual emissions (tons CO2)") 

ClimateChange <- read.csv("/cloud/project/activity03/climate-change.csv")

ymd_hm(exampleDate, tz="America/New_York")

ClimateChange$dateF <- ymd(ClimateChange$Day, tz="America/New_York")

#US <- datCO2[datCO2$Entity == "United States",]

Northern <- ClimateChange[ClimateChange$Entity == "Northern Hemisphere", ]
Southern <- ClimateChange[ClimateChange$Entity == "Southern Hemisphere", ]  

plot(Northern$dateF, # x data
     Northern$temperature_anomaly, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Temperature Anamoly", #y axis label
     xlab = "Date",) 

points(Southern$dateF,
       Southern$temperature_anomaly,
       type = "b", 
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")

#inGGPLOT


ggplot(ClimateChange, aes(x = dateF, y = temperature_anomaly, color = Entity)) + 
  geom_line() +
  labs(x = "Year", y = "Temperature Anomoly") + 
  theme_light() + 
  scale_color_manual(values = c("#7FB3D555","#34495E55", "#E7B80055"))

#homework prompt 1
CompareData <- datC02 %>%
  filter(Entity == "United States"|
           Entity == "Denmark"|Entity == "Russia")

#finding the maximum CO2 emissions to add to a label 
Denmark <- CompareData[CompareData$Entity == "Denmark",]
max(Denmark$CO2)


max(CompareData$Entity)
HW1 <- ggplot(data = CompareData,
       aes(x = Year, ymin = 0, ymax = CO2, y = CO2, fill = Entity)) + 
  geom_ribbon(alpha = 0.5) + 
  labs(x="Year", y = "Annual emissions (tons CO2)")

HW1 + annotate("segment",
               y = 74851794,
               x = 1750, 
               xend = 2020) +
  annotate("text",
         x=1980,
         y = 187841794,
         label = "maximum CO2 emissions in Denmark")

#prompt2
world <- datC02 %>%
  filter(Entity == "World")

CO2Graph2 <- ggplot(world, aes(x = Year, y = CO2)) + 
  geom_area() + 
  labs(x = "Year", y = "Annual emissions (tons CO2)")


CO2Graph2

#prompt3 
#reading in the data 
datDisasters <- read.csv("/cloud/project/activity03/natural-disasters.csv")

#filtering for world 
worldDrought <- datDisasters %>%
  filter(Country.name == "World")

#plotting
worldDroughtPlot <- ggplot(worldDrought, aes(x = Year, y = Number.of.deaths.from.drought, color = Area)) + 
  geom_point() +
  geom_line() + 
  labs(x = "Year", y = "Annual Drought Deaths") + 
  scale_color_manual(values = c("red"))


worldDroughtPlot

colnames(worldDrought)[1] <- "Area" 

