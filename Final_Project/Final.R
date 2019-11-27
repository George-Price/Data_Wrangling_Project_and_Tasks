---
  title: "Data Wrangling Final"
author: "George Price"
date: "11/25/2019"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
  
  
  Question 1.

a) Consider the following blood pressure dataset (IC_BP_v2.csv). Perform the following operations:
  
  Convert BP alerts to BP status
```{r}

library(tidyverse)
library(readr)
library(lubridate)
library(plyr)

IC_BP <- read_csv("~/Desktop/IC_BP_v2.csv")
Demographics <- read_csv("~/Desktop/Demographics.csv")

colnames(IC_BP)[colnames(IC_BP)=="BPAlerts"] <- 'BP_Status'

(IC_BP[sample(nrow(IC_BP),10),])

```
b)

Define: 
  Hypotension-1 & Normal as Controlled blood pressure 

Hypotension-2, Hypertension-1, Hypertension-2 & Hypertension-3 as Uncontrolled blood pressure 
Controlled & Uncontrolled blood pressure as 1 or 0 (Dichotomous Outcomes)

```{r}

# Hypotension-1 & Normal as Controlled blood pressure
IC_BP$BP_Status <- revalue(IC_BP$BP_Status, c("Hypo1"="Controlled blood pressure"))
IC_BP$BP_Status <- revalue(IC_BP$BP_Status, c("Normal"="Controlled blood pressure"))

# Hypotension-2, Hypertension-1, Hypertension-2 & Hypertension-3 as Uncontrolled blood pressure 
IC_BP$BP_Status <- revalue(IC_BP$BP_Status, c("Hypo2"="Uncontrolled blood pressure"))
IC_BP$BP_Status <- revalue(IC_BP$BP_Status, c("HTN1"="Uncontrolled blood pressure"))
IC_BP$BP_Status <- revalue(IC_BP$BP_Status, c("HTN2"="Uncontrolled blood pressure"))
IC_BP$BP_Status <- revalue(IC_BP$BP_Status, c("HTN3"="Uncontrolled blood pressure"))

(IC_BP[sample(nrow(IC_BP),10),])

# Create empty column for dichotomous outcome
IC_BP["BP Dichotomous"] <- NA

# Controlled & Uncontrolled blood pressure as 1 or 0 (Dichotomous Outcomes)

index <- c('Controlled blood pressure','Uncontrolled blood pressure')
values <- c(1,0)

IC_BP$`BP Dichotomous` <- values[match(IC_BP$BP_Status, index)]

(IC_BP[sample(nrow(IC_BP),10),])


```
c) 

Merge this table with demographics (SQL table) to obtain their enrollment dates

```{r}

IC_Dem <- merge(IC_BP, Demographics, by.x="ID", by.y="contactid")

#Enrollment Start
colnames(IC_Dem)[colnames(IC_Dem)=="tri_imaginecareenrollmentemailsentdate"] <- 'Enrollment'
IC_Dem$Enrollment<-mdy(IC_Dem$Enrollment)

#Enrollment End
colnames(IC_Dem)[colnames(IC_Dem)=="tri_enrollmentcompletedate"] <- 'Enrollment_End'
IC_Dem$Enrollment_End<-mdy(IC_Dem$Enrollment_End)

(IC_Dem[sample(nrow(IC_Dem),10),])

```
d)

Create a 12-week interval of averaged scores of each customer.

```{r}

# Create a matrix with a loop to give the lowest Observed Time for each ID

patient.bp <- matrix(ncol=2, nrow = length(unique(IC_Dem$ID)))

for (i in 1:length(unique(IC_Dem$ID))){
  patient.bp[i,1] = unique(IC_Dem$ID)[i]
  patient.bp[i,2] = min(IC_Dem[which(IC_Dem$ID == unique(IC_Dem$ID)[i]),"ObservedTime"])
}

colnames(patient.bp)=c("ID","Min.ObservedTime")

patient.bp <- data.frame(patient.bp)

# Add a new column the lowest Observed Time for each ID
IC_Dem <- merge(IC_Dem, patient.bp, by = 'ID')

# Convert observed time values to numeric
IC_Dem$ObservedTime <- as.numeric(IC_Dem$ObservedTime)

# Converting minimum observed time to numeric alone does not work, need to do numeric and character
IC_Dem$Min.ObservedTime <- as.numeric(as.character(IC_Dem$Min.ObservedTime))

# Find the difference between the min and each enrollment value by patient ID

IC_Dem$ObservedDiff <- (IC_Dem$ObservedTime - IC_Dem$Min.ObservedTime)

# Convert the Observed Difference to weeks but use floor in order to aggregate
IC_Dem$ObservedDiff <- floor(IC_Dem$ObservedDiff/7)

# Find max number of weeks
max(IC_Dem$ObservedDiff)

# Create new DF to aggregate

Average.Week.BP <- cbind(IC_Dem$ID,IC_Dem$SystolicValue,IC_Dem$Diastolicvalue,IC_Dem$ObservedDiff, IC_Dem$`BP Dichotomous`)
colnames(Average.Week.BP) <- c("ID", "SYSBP", "DYSBP", "Observed.Time.Difference","BP Dichotomous")


Average.Week.BP <- data.frame(Average.Week.BP)

Average.Week.BP$SYSBP <- as.numeric(as.character(Average.Week.BP$SYSBP))
Average.Week.BP$DYSBP <- as.numeric(as.character(Average.Week.BP$DYSBP))
Average.Week.BP$Observed.Time.Difference <- as.numeric(as.character(Average.Week.BP$Observed.Time.Difference))
Average.Week.BP$BP.Dichotomous <- as.numeric(as.character(Average.Week.BP$BP.Dichotomous))


Average.Week.BP <- aggregate(.~ID + Observed.Time.Difference, data = Average.Week.BP, mean)

# I have decided to floor the BP Dichotomous values to most easily find the change from baseline to week 12
Average.Week.BP$BP.Dichotomous <- floor(Average.Week.BP$BP.Dichotomous)

# 12 week interval of averaged scores 
week.12.interval <- Average.Week.BP %>% 
  filter(Observed.Time.Difference < 12)

(week.12.interval[sample(nrow(week.12.interval),10),])

```
e)

Compare the scores from baseline (first week) to follow-up scores (12 weeks)

Both the systolic and diastolic scores drop (slightly) from baseline to week 12. 
```{r}

week.0 <- week.12.interval %>% 
  filter(Observed.Time.Difference == 0)

summary(week.0)

(week.0[sample(nrow(week.0),10),])

week.11 <- week.12.interval %>% 
  filter(Observed.Time.Difference == 11)

summary(week.11)

(week.11[sample(nrow(week.11),10),])

# Systolic Blood Pressure Graph Comparing Baseline and Week 12

Systolic.Comparison <- boxplot(week.0$SYSBP,week.11$SYSBP, ylab = "Systolic Blood Pressure", xlab = "Baseline vs. Week 12")

# Diastolic Blood Pressure Graph Comparing Baseline and Week 12

Diastolic.Comparison <- boxplot(week.0$DYSBP,week.11$DYSBP, ylab = "Diastolic Blood Pressure", xlab = "Baseline vs. Week 12")


```
f)

How many customers were brought from uncontrolled regime to controlled regime after 12 weeks of intervention?
  
  There appear to be 9 customers whom were brought from uncontrolled regime to controlled regime after 12 weeks of intervention.

```{r}

# Find the patients at baseline with uncontrolled regime
week.0.Uncontrolled <- week.0 %>% 
  filter(BP.Dichotomous == 0)
length(week.0.Uncontrolled$BP.Dichotomous)
# 336

# Find the patients at week 12 with uncontrolled regime
week.11.controlled <- week.11 %>% 
  filter(BP.Dichotomous == 1)
length(week.11.controlled$BP.Dichotomous)
# 74

# See where the ID's are the same for the same two scenarios. 
count(week.11.controlled$ID == week.0.Uncontrolled$ID)
# 9

count(week.0.Uncontrolled$ID == week.11.controlled$ID)
# 9 

```

3. Obtain the final dataset such that we have 1 Row per ID by choosing on the latest date when the text was sent (if sent on multiple days)

```{r}

library(tidyverse)
library(readr)
library(lubridate)

getwd() 
setwd("/Users/georgedprice/desktop")

Demographics <- read_csv("~/Desktop/Demographics.csv")
Conditions <- read_csv("~/Desktop/Conditions.csv")
Text_Messages <- read_csv("TextMessages.csv")

# Merge Tables Demographics and Conditions
Dem.Con <- merge(Demographics,Conditions, by.x = "contactid", by.y = "tri_patientid")

#Display first 10 columns of DF
(Dem.Con[sample(nrow(Dem.Con),10),])

# Merge Tables DemCon and Text_Messages
Merged <- merge(Text_Messages,Dem.Con, by.x = "tri_contactId",by.y = "contactid")

#Display first 10 columns of DF
(Merged[sample(nrow(Merged),10),])

# Alter Merged to remove NA columns

Merged$X3 <- NULL
Merged$X4.x <- NULL
Merged$X4.y <- NULL
Merged$X5.x <- NULL
Merged$X5.y <- NULL

#Checking date format
Merged$TextSentDate <- mdy(Merged$TextSentDate)

# Group by ID and use slice to only view the last text date 
Merged.Updated <- Merged %>% 
  group_by(tri_contactId) %>% 
  slice(which.max(TextSentDate))

#Display first 10 columns of DF
(Merged.Updated[sample(nrow(Merged.Updated),10),])

```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
