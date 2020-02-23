attach(ebola1.cleaned.by.country)
library(tidyverse)
library(dplyr)
str(ebola1.cleaned.by.country)
str(ebola1.cleaned.by.country$Value)
#Note Value means that this many people have been infected on that day 
#Add in a column where you show which country closes its border or not
str(ebola1.cleaned.by.country$Country)
view(ebola1.cleaned.by.country)
View(ebola1.cleaned.by.country)


#Add in a column on the average growth rate of each cases per country, then combine the different rates into one column
sum(ebola1.cleaned.by.country$Value[Country=="Guinea"])
sum(ebola1.cleaned.by.country$Value)
ebola1.cleaned.by.country$Value <- as.integer(ebola1.cleaned.by.country$Value) #Turn the value into an integer
sum(ebola1.cleaned.by.country$Value)


sum(as.integer(ebola1.cleaned.by.country$Category)) 
ebola1.cleaned.by.country$Category <- as.integer(ebola1.cleaned.by.country$Category)
sum(ebola1.cleaned.by.country$Category)# total number of cases in the entire dataset
sum(ebola1.cleaned.by.country$Category[ebola1.cleaned.by.country$Country == "Guinea"]) #Total number of cases in Guinea
sum(ebola1.cleaned.by.country$Category[ebola1.cleaned.by.country$Country == "Liberia"]) #Total number of cases in Liberia
sum(ebola1.cleaned.by.country$Category[ebola1.cleaned.by.country$Country == "Senegal"]) #Total number of cases in Senegal
sum(ebola1.cleaned.by.country$Category[ebola1.cleaned.by.country$Country == "Nigeria"]) #Total number of cases in Nigeria
sum(ebola1.cleaned.by.country$Category[ebola1.cleaned.by.country$Country == "Mali"]) #Total number of cases in Mali
sum(ebola1.cleaned.by.country$Category[ebola1.cleaned.by.country$Country == "Sierra Leone"]) #Total number of cases in Sierra Leone
ebola1.cleaned.by.country$Country <- as.integer(ebola1.cleaned.by.country$Country)
mean(ebola1.cleaned.by.country$Country)

healthcare_quality <- ebola1.cleaned.by.country(as.factor("0","1","2"))



#ebola1.cleaned.by.country["healthcare_quality"] <- NA

#ebola1.cleaned.by.country$healthcare_quality <- ebola1.cleaned.by.country$Country["Guinea"==2, "Senegal"==1, "Nigeria"==2, "Sierra Leone"==0, "Liberia"==0,"Mali"==0]

#names(ebola1.cleaned.by.country$Country)

#ebola1.cleaned.by.country$healthcare_quality <- ebola1.cleaned.by.country$Country[]

#Two sampled dependent T test
ebola1.cleaned.by.country$Date <- as.integer(ebola1.cleaned.by.country$Date)

#Sierra Leone before border close
slbefore <- 4
slafter <- 211 #Sierra Leone
t.test(slbefore,slafter,paired=T, mu=0, alternative="less")
gbefore <-42
gafter <-175 #Guinea

senegalbefore <- 0
senegalafter <- 16 #senegal

malibefore <- 14 #mali
maliafter <-0

nigeriabefore <- 28
nigeriaafter <- 0 #nigeria

libbefore <- 57
libafter <- 118 #Liberia

borderopen <- cbind(slbefore,gbefore,senegalbefore,malibefore,nigeriabefore,libbefore)
borderclosed <- cbind(slafter,gafter,senegalafter,maliafter,nigeriaafter,libafter)

#Result
t.test(borderopen,borderclosed,paired=T, mu=0, alternative="less") #this is when were doing the less than symbol
t.test(borderopen,borderclosed,paired=T, mu=0, alternative="less", conf.level = 0.90)





#Rough Draft
summary(ebola1.cleaned.by.country)
View(ebola1.cleaned.by.country)
#Ill have to make another column where it has the before cases when the border was open and after cases when the border was closed
ebola1.cleaned.by.country[""] <- NA
ebola1.cleaned.by.country[order(ebola1.cleaned.by.country$Date, decreasing = TRUE),] 
ebola1.cleaned.by.country$Date

ebola1.cleaned.by.country$datesss <- as.Date(ebola1.cleaned.by.country$Date, format= "%d-%m-%Y")
subset(ebola1.cleaned.by.country, datesss> "30-3-2014" & date < "12-6-2014") #doesnt work
filter(ebola1.cleaned.by.country, between(datesss, as.Date("31-3-2014"), as.Date("13-6-2014")))

t.test(ebola1.cleaned.by.country$Category,ebola1.cleaned.by.country$Date,paired=T, mu=0, alternative="less")
str(ebola1.cleaned.by.country)


chisq.test(borderclosed,borderopen)








