#install.packages("RODBC")
library(RODBC)
#install.packages("sqldf")
library(sqldf)
myconn<- odbcConnect("dartmouth","gprice","gprice@qbs181")

Phonecall_Enrollment_Changes <- sqlQuery(myconn, "select * from Phonecall_Encounter")

#install.packages("tidyverse")
library(tidyverse)

#Create a new column "Enrollment_Group"

Phonecall_Enrollment_Changes <- sqldf(c("ALTER table Phonecall_Enrollment_Changes add Enrollment_Group nvarchar", "select * from Phonecall_Enrollment_Changes"))

#Fill Enrollment_Group according to specified codes

Phonecall_Enrollment_Changes <- sqldf(c("UPDATE Phonecall_Enrollment_Changes SET Enrollment_Group = 'Clinical Alert' WHERE EncounterCode = 125060000", "select * from Phonecall_Enrollment_Changes"))

Phonecall_Enrollment_Changes <- sqldf(c("UPDATE Phonecall_Enrollment_Changes SET Enrollment_Group = 'Health Coaching' WHERE EncounterCode = 125060001", "select * from Phonecall_Enrollment_Changes"))

Phonecall_Enrollment_Changes <- sqldf(c("UPDATE Phonecall_Enrollment_Changes SET Enrollment_Group = 'Technical Question' WHERE EncounterCode = 125060002", "select * from Phonecall_Enrollment_Changes"))

Phonecall_Enrollment_Changes <- sqldf(c("UPDATE Phonecall_Enrollment_Changes SET Enrollment_Group = 'Administrative' WHERE EncounterCode = 125060003", "select * from Phonecall_Enrollment_Changes"))

Phonecall_Enrollment_Changes <- sqldf(c("UPDATE Phonecall_Enrollment_Changes SET Enrollment_Group = 'Other' WHERE EncounterCode = 125060004", "select * from Phonecall_Enrollment_Changes"))

Phonecall_Enrollment_Changes <- sqldf(c("UPDATE Phonecall_Enrollment_Changes SET Enrollment_Group = 'Lack of Engagement' WHERE EncounterCode = 125060005", "select * from Phonecall_Enrollment_Changes"))

#Display first 10 columns of DF
(Phonecall_Enrollment_Changes[sample(nrow(Phonecall_Enrollment_Changes),10),])

#Create a table showing the # of records for each enrollment group
table(Phonecall_Enrollment_Changes$Enrollment_Group)

Call_Duration_Changes <- sqlQuery(myconn, "select * from Callduration")

#Merge tables Call_Duration_Changes and Phonecall_Enrollment_Changes
Merged.DF <- merge(Call_Duration_Changes,Phonecall_Enrollment_Changes, by.x = "tri_CustomerIDEntityReference", by.y = "CustomerId")

#Display first 10 columns of DF
(Merged.DF[sample(nrow(Merged.DF),10),])

#Number of records for call types
Call_Type <- Merged.DF %>% 
  group_by(CallType) %>% 
  summarize(table(CallType))

colnames(Call_Type) <- c("Call Type","Number of records")
rownames(Call_Type) <- c("Inbound","Outbound")
view(Call_Type)

#Number of records for call outcomes
Call_Outcome <- Merged.DF %>% 
  group_by(CallOutcome) %>% 
  summarize(table(CallOutcome))

colnames(Call_Outcome) <- c("Call Outcome","Number of records")
rownames(Call_Outcome) <- c("No Response","Left Voicemail","Successful")
View(Call_Outcome)

#Call Duration for each enrollment group
Call_Duration <- Merged.DF %>% 
  group_by(Enrollment_Group) %>% 
  summarize(Callduration = sum(CallDuration), MeanCallDuration = mean(CallDuration))
view(Call_Duration)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("lubridate")
library(lubridate)

#Select required tables
Demographics <- sqlQuery(myconn, "select * from Demographics")
Conditions <- sqlQuery(myconn, "select * from Conditions")
TextMessages <- sqlQuery(myconn, "select * from TextMessages")

#Merge Demographics and Conditions Tables
Dem_Con <- merge(Demographics,Conditions, by.x = "contactid", by.y = "tri_patientid")

#Merge Dem_Con and TextMessages tables
Text_trend <- merge(Dem_Con,TextMessages, by.x = "contactid", by.y = "tri_contactId")

#Checking for NA's within the column
anyNA(Text_trend$TextSentDate)

#Checking the structure of the column
str(Text_trend$TextSentDate)

Sys.getlocale()

#Checking date format
strptime(Text_trend$TextSentDate, format = "%m/%d/$y")

#as.Date function does not work and turns column into NA's

#use "floor_date" function from lubridate

Text_trend$TextSentDate <- mdy(Text_trend$TextSentDate)
Text_trend$TextSentDate <- floor_date(Text_trend$TextSentDate, unit = "week")

Text_Plot <- Text_trend %>% 
  group_by(SenderName, week = floor_date(TextSentDate)) %>% 
  summarize("Number_of_Texts" = n())

#Plot Text_trend
ggplot(Text_Plot, aes(fill=SenderName,x=week,y=Number_of_Texts)) +
  geom_bar(position="stack", stat="identity")+labs(x = "Week", y = "Number of Texts", fill = "Sender Type", title = "Number of Texts Sent per Week by Sender Type")

#Group Text_trend based on Conditions
Condition_trend <- Text_trend %>% 
  group_by(tri_name, week = floor_date(TextSentDate)) %>% 
  summarize("Number_of_Texts" = n())

#Plot Condition_trend
ggplot(Condition_trend, aes(fill=tri_name,x=week,y=Number_of_Texts)) + geom_bar(position="stack", stat="identity")+labs(x = "Week", y = "Number of Texts", fill = "Chronic Conditions", title = "Number of Texts Sent per Week by Chronic Condition")
