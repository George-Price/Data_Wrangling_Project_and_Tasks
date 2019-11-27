---
title: "Data Wrangling Midterm"
author: "George Price"
date: "10/27/2019"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


1.

Outline of Approach:

(a)

Data Related Issues:

- Columns are a mix of continuous and categorical variables.
- Most columns contain NA's.
- Columns containing NA's need to be either broken up into columns without NA's or filled with a replacement value.
- Some columns contain units of measure with varying units (weeks, months, years, etc.)

(b and c)

In order to address the problems listed above in the data set, I will review column by column to: 

- Asses data type (categorical or continuous).  
- For columns with units of measure, verify that these were binned as categorical variables (Ex. 1-12 months coded as 1; rather than 1-12 months coded as 1-12). Assuming this is the case, I will not attempt to convert all responses into one unit of measurement as that would be making assumptions about the responses that are not provided in the data set (Ex. If you convert a range of 1-12 months as 1 year, you are equating different amounts of time, which provides misleading information).
- Confirm presence of missing values.
- Apply correct implementation of one-hot-encoding for categorical variables or mean-imputation for continuous variables for missing values.
- Verify counts of re-constructed or altered columns with CDC website.
- Confirm replacement of NA's / missing data.

Specific Approach for Categorical Data:

- Confirm data as categorical based on CDC website for specific column.
- Implement for loop that will create a new column for each distinct response, which will remove NA's as each column will become binary (0 or 1).
- Rename columns based on question and response of column.
- Sum column and confirm counts with CDC website, which will ensure no data was lost despite restructuring of the columns.
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.

Specific Approach for Continuous Data: 

- Confirm data as continuous based on CDC website for specific column.
- Address each response of the question and code these codes/values appropriately (this will vary column to column and specific justification will be given). However, it should be noted that the CDC provides an outline for suggested coding (provided below) of certain response types and this was adhered to:

https://www.cdc.gov/nchs/tutorials/NHANES/Preparing/CleanRecode/Info1.htm 

- Calculate the mean and perform mean imputation on rows missing values (mean imputation discussed below)
- Confirm mean imputation by checking for values of mean with SQL, and ensure that all NA's were correctly replaced with the calculated mean
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfuly.


Mean imputation is a method in which the missing value on a certain variable is replaced by the mean of the available cases. This method maintains the sample size, but has limitations as the variability in the data is reduced, resulting in the standard deviations and the variance estimates tend to be underestimated. While correct implementation of mean implementation of appears subjective, there is literature (provided below) to suggest that if > 60% of data is missing utilizing mean imputation would be ill-advised. 

In the case of this midterm, the percent of missing data for columns I categorized as continuous were mostly, if not all, missing greater than 60% of the data; however, I still utilized mean-imputation as a coding exercise and practice of implementation of a method we learned in class. Some of the suggested methods such as creating a new data frame for the specific variable seemed unneccessary so I avoided such methods. 

https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3701793/pdf/40064_2013_Article_296.pdf
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3668100/pdf/kjae-64-402.pdf

2. (code below)

```{r}
library(data.table)
library(sqldf)

getwd()
setwd("/Users/georgedprice/Desktop")
getwd()

#install.packages("SASxport")
library(SASxport)
DIQ_df <- read.xport("DIQ_I.XPT.txt")

```
Column 1: This is just the respondent sequence number, so I am just going to confirm there are no missing values, then move forward to Column 2.

Column 2: Doctor told you have diabetes

Code: 1 -> Yes (Count = 856)
Code: 2 -> No (Count = 8568)
Code: 3 -> Borderline (Count = 147)
Code: 7 -> Refused (Count = 0)
Code: 9 -> Don't Know (Count = 4)
Code: . -> Missing (Count = 0)

- Confirm data as categorical based on CDC website for specific column
- Set NA's to zero
- Avoid creating columns for Code 7 and Code NA as there are no counts for these.
- Implement for loop that will create a new column for each distinct response
- Rename columns based on if Doctor told you have diabetes and response of column
- Sum column and confirm counts with CDC website
- Confirms no NA's exist
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}

#Check for NA's in Column 1
sum(is.na(DIQ_df$SEQN))
#Returns zero, no missing values for column 1

# Doctor told you have diabetes

#Loop to create distinct columns
for(unique_value in unique(DIQ_df$DIQ010)){
  DIQ_df[paste("Diabetes_Status", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ010 == unique_value, 1, 0)
}

# Rename columns and check counts based on website
colnames(DIQ_df)[colnames(DIQ_df)=="Diabetes_Status.1"] <- "Doctor told you have diabetes: Yes"
sum(DIQ_df$`Doctor told you have diabetes: Yes`)
# Returns 856, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Diabetes_Status.2"] <- "Doctor told you have diabetes: No"
sum(DIQ_df$`Doctor told you have diabetes:No`)
# Returns 8568, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Diabetes_Status.3"] <- "Doctor told you have diabetes: Borderline"
sum(DIQ_df$`Doctor told you have diabetes: Borderline`)
# Returns 147, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Diabetes_Status.9"] <- "Doctor told you have diabetes: Don't_know"
sum(DIQ_df$`Doctor told you have diabetes: Don't_know`)
# Returns 4, in agreement with the website

# No columns created for code "7" or "." as there were no values which is in accordance with the counts of the website

#Remove original data column as it is no longer in use
DIQ_df$DIQ010 <- c()
```
Column 3: Age when first told you had diabetes (DID040)

Code: 2-78 -> Range of Values (Count = 833)
Code: 80 -> 80 years or older (Count = 7)
Code: 666 -> Less than 1 year (Count = 1)
Code: 777 -> Refused (Count = 0)
Code: 999 -> Don't Know (Count = 12)
Code: . -> Missing (Count = 8722)

- Confirm data as continuous based on CDC website for specific column
- Avoid creating columns for Code 777 as there is no count for this column. 
- Code 1 for less than 1 year; this may be slightly inaccurate as the age is listed as less than one year but seems to be the best trade of to maintain the continuity of the data for age ranges.
- Do not alter Code for 80 years or older, although this may be slightly inaccurate as there is a chance that the participant is older than 80 years, with a count of 7 this should have a neglible effect on the calculated mean. (Ex. If participant is 81 but listed as 80, you would be losing a year when calculating the mean age)
- Treat Don't Know column as Missing column per the CDC website. 
- Address each response of the question and code these codes/values appropriately.
- Calculate the mean and perform mean imputation on rows missing values (mean imputation discussed below)
- Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfuly
- Confirm that all missing values were replaced with the mean.

```{r}
# Age when first told you had diabetes

#Check number of NA's
sum(is.na(DIQ_df$DID040))

#Plug in 1 for code 666 and NA for code 999
DIQ_df$DID040[DIQ_df$DID040==666] <- 1
DIQ_df$DID040[DIQ_df$DID040==999] <- NA

#Perform Mean Imputation
mean(DIQ_df$DID040, na.rm = TRUE)
DIQ_df$DID040[is.na(DIQ_df$DID040)] <- 48.40428

#Check number of mean imputations added
sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID040 = '48.40428'")
#8734, a summation of the NA's from the Missing and Don't Know categories as given by the website

table(DIQ_df$DID040)
sum(is.na(DIQ_df$DID040))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean

```
Column 4: Ever told you have prediabetes (DIQ160)

Code: 1 -> Yes (Count = 513)
Code: 2 -> No (Count = 5521)
Code: 7 -> Refused (Count = 0)
Code: 9 -> Don't Know (Count = 11)
Code: . -> Missing (Count = 3530)

- Confirm data as categorical based on CDC website for specific column
- Set NA's to 0
- Implement for loop that will create a new column for each distinct response. 
- Avoid creating columns for Code 7 as there is no count for this column.
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}
# Ever told you have prediabetes (DIQ160)

# Set NA Values to 0
DIQ_df$DIQ160[is.na(x = DIQ_df$DIQ160)] <- 0

#Loop to create distinct columns
for(unique_value in unique(DIQ_df$DIQ160)){
  DIQ_df[paste("Age of Prediabetes", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ160 == unique_value, 1, 0)
}

#Rename columns and return counts
colnames(DIQ_df)[colnames(DIQ_df)=="Age of Prediabetes.1"] <- "Told you have prediabetes: Yes"
sum(DIQ_df$`Told you have prediabetes: Yes`)
# Returns 513, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Age of Prediabetes.2"] <- "Told you have prediabetes: No"
sum(DIQ_df$`Told you have prediabetes: No`)
# Returns 5521, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Age of Prediabetes.9"] <- "Told you have prediabetes: Don't know"
sum(DIQ_df$`Told you have prediabetes: Don't know`)
# Returns 11, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Age of Prediabetes.0"] <- "Told you have prediabetes: Missing"
sum(DIQ_df$`Told you have prediabetes: Missing`)
# Returns 3530, in agreement with the website

# No columns created for code "7" as there were no values which is in accordance with the counts of the website

#Remove original and missing data column as it is no longer in use
DIQ_df$DIQ160 <- c()
DIQ_df$`Told you have prediabetes: Missing` <- c()

```
Column 5: Ever told have health risk for diabetes (DIQ170)

Code: 1 -> Yes (Count = 899)
Code: 2 -> No (Count = 5268)
Code: 7 -> Refused (Count = 0)
Code: 9 -> Don't Know (Count = 19)
Code: . -> Missing (Count = 3389)

- Confirm data as categorical based on CDC website for specific column
- Avoid creating columns for Code 7 as there is no count for this column.
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}

#Ever told have health risk for diabetes (DIQ170)

#Set NA's to zero
DIQ_df$DIQ170[is.na(x = DIQ_df$DIQ170)] <- 0

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ170)){
  DIQ_df[paste("Health Risk", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ170 == unique_value, 1, 0)
}

#Rename columns and check values with website
colnames(DIQ_df)[colnames(DIQ_df)=="Health Risk.1"] <- "health risk for diabetes: Yes"
sum(DIQ_df$`health risk for diabetes: Yes`)
# Returns 899, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Health Risk.2"] <- "health risk for diabetes: no"
sum(DIQ_df$`health risk for diabetes: no`)
# Returns 5268, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Health Risk.9"] <- "health risk for diabetes: Don't know"
sum(DIQ_df$`health risk for diabetes: Don't know`)
# Returns 19, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Health Risk.0"] <- "health risk for diabetes: Missing"
sum(DIQ_df$`health risk for diabetes: Missing`)
# Returns 3389, in agreement with the website

# No columns created for code "7" as there were no values which is in accordance with the counts of the website

#Remove original column as it is no longer in use
DIQ_df$DIQ170 <- c()
DIQ_df$'health risk for diabetes: Missing' <- c()

```
Column 6: Feel could be at risk for diabetes

Code: 1 -> Yes (Count = 1588)
Code: 2 -> No (Count = 4510)
Code: 7 -> Refused (Count = 1)
Code: 9 -> Don't Know (Count = 87)
Code: . -> Missing (Count = 3389)

- Confirm data as categorical based on CDC website for specific column
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}
 # Feel could be at risk for diabetes (DIQ172)

#Set NA's to 0
DIQ_df$DIQ172[is.na(x = DIQ_df$DIQ172)] <- 0

#Implement for loop to separate columns
for(unique_value in unique(DIQ_df$DIQ172)){
  DIQ_df[paste("At Risk", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ172 == unique_value, 1, 0)
}

#Rename columns and confirm counts with website
colnames(DIQ_df)[colnames(DIQ_df)=="At Risk.1"] <- "feel at risk for diabetes: Yes"
sum(DIQ_df$`feel at risk for diabetes: Yes`)
# Returns 1588, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="At Risk.2"] <- "feel at risk for diabetes: no"
sum(DIQ_df$`feel at risk for diabetes: no`)
# Returns 4510, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="At Risk.7"] <- "feel at risk for diabetes: Refused"
sum(DIQ_df$`feel at risk for diabetes: Refused`)
# Returns 1, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="At Risk.9"] <- "feel at risk for diabetes: Don't know"
sum(DIQ_df$`feel at risk for diabetes: Don't know`)
# Returns 87, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="At Risk.0"] <- "feel at risk for diabetes: Missing"
sum(DIQ_df$`feel at risk for diabetes: Missing`)
# Returns 3389, in agreement with the website

#Remove original column and missing data column as it is no longer in use
DIQ_df$DIQ172 <- c()
DIQ_df$`feel at risk for diabetes: Missing` <- c()
```
Column 7-30: Reason for risk of diabetes/prediabetes (DIQ175)

DIQ175A: Family History
Code: 10 -> Family History (Count = 1186)

DIQ175B: Overweight
Code: 11 -> Overweight (Count = 601)

DIQ175C: Age
Code: 12 -> Age (Count = 160)

DIQ175D: Poor diet
Code: 13 -> Poor diet (Count = 459)

DIQ175E: Race
Code: 14 -> Race (Count = 155)

DIQ175F: Had a baby weighed over 9 lbs. at birth
Code: 15 -> Had a baby weighed over 9 lbs. at birth (Count = 53)

DIQ175G: Lack of physical activity
Code: 10 -> Lack of physical activity (Count = 280)

DIQ175H: High blood pressure
Code: 17 -> High blood pressure (Count = 258)

DIQ175I: High blood sugar
Code: 18 -> High blood sugar (Count = 77)

DIQ175J: High cholesterol
Code: 10 -> High cholesterol (Count = 150)

DIQ175K: Hypoglycemic
Code: 20 -> Hypoglycemic (Count = 31)

DIQ175L: Extreme hunger
Code: 21 -> Extreme hunger (Count = 59)

DIQ175M: Tingling/numbness in hands or feet
Code: 22 -> Tingling/numbness in hands or feet (Count = 138)

DIQ175N: Blurred vision
Code: 23 -> Blurred vision (Count = 109)

DIQ175O: Increased fatigue
Code: 24 -> Increased fatigue (Count = 134)

DIQ175P: Anyone could be at risk
Code: 25 -> Anyone could be at risk (Count = 117)

DIQ175Q: Doctor warning
Code: 26 -> Doctor warning (Count = 136)

DIQ175R: Other, specify
Code: 27 ->  Other, specify (Count = 23)

DIQ175S: Gestastional diabetes
Code: 28 ->  Gestastional diabetes (Count = 31)

DIQ175T: Frequent Urination
Code: 29 ->  Frequent Urination (Count = 105)

DIQ175U: Thirst
Code: 30 -> Thirst (Count = 79)

DIQ175V: Craving for sweet/eating a lot of sugar
Code: 31 -> Craving for sweet/eating a lot of sugar (Count = 11)

DIQ175W: Medication 
Code: 32 -> Medication (Count = 4)

DIQ175X: Polycystic ovarian syndrome 
Code: 33 -> Polycystic ovarian syndrome (Count = 1)

- Confirm data as categorical based on CDC website for specific column
- Set NA’s to zero
- Implement for loop each condition that will create a new column
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}
# Reason for risk of diabetes/prediabetes (DIQ175)

# Set NA's to 0
DIQ_df$DIQ175A[is.na(x = DIQ_df$DIQ175A)] <- 0
DIQ_df$DIQ175B[is.na(x = DIQ_df$DIQ175B)] <- 0
DIQ_df$DIQ175C[is.na(x = DIQ_df$DIQ175C)] <- 0
DIQ_df$DIQ175D[is.na(x = DIQ_df$DIQ175D)] <- 0
DIQ_df$DIQ175E[is.na(x = DIQ_df$DIQ175E)] <- 0
DIQ_df$DIQ175F[is.na(x = DIQ_df$DIQ175F)] <- 0
DIQ_df$DIQ175G[is.na(x = DIQ_df$DIQ175G)] <- 0
DIQ_df$DIQ175H[is.na(x = DIQ_df$DIQ175H)] <- 0
DIQ_df$DIQ175I[is.na(x = DIQ_df$DIQ175I)] <- 0
DIQ_df$DIQ175J[is.na(x = DIQ_df$DIQ175J)] <- 0
DIQ_df$DIQ175K[is.na(x = DIQ_df$DIQ175K)] <- 0
DIQ_df$DIQ175L[is.na(x = DIQ_df$DIQ175L)] <- 0
DIQ_df$DIQ175M[is.na(x = DIQ_df$DIQ175M)] <- 0
DIQ_df$DIQ175N[is.na(x = DIQ_df$DIQ175N)] <- 0
DIQ_df$DIQ175O[is.na(x = DIQ_df$DIQ175O)] <- 0
DIQ_df$DIQ175P[is.na(x = DIQ_df$DIQ175P)] <- 0
DIQ_df$DIQ175Q[is.na(x = DIQ_df$DIQ175Q)] <- 0
DIQ_df$DIQ175R[is.na(x = DIQ_df$DIQ175R)] <- 0
DIQ_df$DIQ175S[is.na(x = DIQ_df$DIQ175S)] <- 0
DIQ_df$DIQ175T[is.na(x = DIQ_df$DIQ175T)] <- 0
DIQ_df$DIQ175U[is.na(x = DIQ_df$DIQ175U)] <- 0
DIQ_df$DIQ175V[is.na(x = DIQ_df$DIQ175V)] <- 0
DIQ_df$DIQ175W[is.na(x = DIQ_df$DIQ175W)] <- 0
DIQ_df$DIQ175X[is.na(x = DIQ_df$DIQ175X)] <- 0

#Family History

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175A)){
  DIQ_df[paste("Family history", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175A == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Family history.10"] <- "reason for risk: Family history"
sum(DIQ_df$`reason for risk: Family history`)
# Returns 1186, in agreement with the website

#Remove original column and missing data column as it is no longer in use
DIQ_df$DIQ175A <- c()
DIQ_df$`Family history.0` <- c()

#Overweight

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175B)){
  DIQ_df[paste("Overweight", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175B == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Overweight.11"] <- "reason for risk: overweight"
sum(DIQ_df$`reason for risk: overweight`)
# Returns 601, in agreement with the website

#Remove original column and missing data column as it is no longer in use
DIQ_df$DIQ175B <- c()
DIQ_df$`Overweight.0` <- c()

#Age

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175C)){
  DIQ_df[paste("Age", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175C == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Age.12"] <- "reason for risk: age"
sum(DIQ_df$`reason for risk: age`)
# Returns 160, in agreement with the website

#Remove original column and missing data column as it is no longer in use
DIQ_df$DIQ175C <- c()
DIQ_df$`Age.0` <- c()

#Poor diet

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175D)){
  DIQ_df[paste("Poor Diet", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175D == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Poor Diet.13"] <- "reason for risk: poor diet"
sum(DIQ_df$`reason for risk: poor diet`)
# Returns 459, in agreement with the website

#Remove original column and missing data column as it is no longer in use
DIQ_df$DIQ175D <- c()
DIQ_df$`Poor Diet.0` <- c()

#Race

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175E)){
  DIQ_df[paste("Race", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175E == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Race.14"] <- "reason for risk: race"
sum(DIQ_df$`reason for risk: race`)
# Returns 155, in agreement with the website

#Remove original column and missing data column as it is no longer in use
DIQ_df$DIQ175E <- c()
DIQ_df$`Race.0` <- c()

#Had baby > 9 lbs. at birth

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175F)){
  DIQ_df[paste("Baby", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175F == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Baby.15"] <- "reason for risk: baby > 9lbs at birth"
sum(DIQ_df$`reason for risk: baby > 9lbs at birth`)
# Returns 53, in agreement with the website

#Remove original column and missing data column as it is no longer in use
DIQ_df$DIQ175F <- c()
DIQ_df$`Baby.0` <- c()

#Lack of physical activity

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175G)){
  DIQ_df[paste("Activity", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175G == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Activity.16"] <- "reason for risk: lack of physical activity"
sum(DIQ_df$`reason for risk: lack of physical activity`)
# Returns 280, in agreement with the website

#Remove original column and missing data column as it is no longer in use
DIQ_df$DIQ175G <- c()
DIQ_df$`Activity.0` <- c()

#High blood pressure

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175H)){
  DIQ_df[paste("BP", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175H == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="BP.17"] <- "reason for risk: high blood pressure"
sum(DIQ_df$`reason for risk: high blood pressure`)
# Returns 258, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175H <- c()
DIQ_df$`BP.0` <- c()

#High blood sugar

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175I)){
  DIQ_df[paste("Sugar", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175I == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Sugar.18"] <- "reason for risk: high blood sugar"
sum(DIQ_df$`reason for risk: high blood sugar`)
# Returns 77, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175I <- c()
DIQ_df$`Sugar.0` <- c()

#High cholesterol

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175J)){
  DIQ_df[paste("chol", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175J == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="chol.19"] <- "reason for risk: high cholesterol"
sum(DIQ_df$`reason for risk: high cholesterol`)
# Returns 150, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175J <- c()
DIQ_df$`chol.0` <- c()

#Hypoglycemic

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175K)){
  DIQ_df[paste("Hypoglycemic", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175K == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Hypoglycemic.20"] <- "reason for risk: Hypoglycemic"
sum(DIQ_df$`reason for risk: Hypoglycemic`)
# Returns 31, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175K <- c()
DIQ_df$`Hypoglycemic.0` <- c()

#Extreme hunger

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175L)){
  DIQ_df[paste("Extreme hunger", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175L == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Extreme hunger.21"] <- "reason for risk: Extreme hunger"
sum(DIQ_df$`reason for risk: Extreme hunger`)
# Returns 59, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175L <- c()
DIQ_df$`Extreme hunger.0` <- c()

#Tingling/numbness in hands or feet

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175M)){
  DIQ_df[paste("Tingling/numbness", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175M == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Tingling/numbness.22"] <- "reason for risk: Tingling/numbness"
sum(DIQ_df$`reason for risk: Tingling/numbness`)
# Returns 138, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175M <- c()
DIQ_df$`Tingling/numbness.0` <- c()

#Blurred vision

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175N)){
  DIQ_df[paste("Blurred vision", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175N == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Blurred vision.23"] <- "reason for risk: Blurred vision"
sum(DIQ_df$`reason for risk: Blurred vision`)
# Returns 109, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175N <- c()
DIQ_df$`Blurred vision.0` <- c()

#Increased fatigue

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175O)){
  DIQ_df[paste("Increased fatigue", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175O == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Increased fatigue.24"] <- "reason for risk: Increased fatigue"
sum(DIQ_df$`reason for risk: Increased fatigue`)
# Returns 134, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175O <- c()
DIQ_df$`Increased fatigue.0` <- c()

#Anyone could be at risk

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175P)){
  DIQ_df[paste("Anyone could be at risk", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175P == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Anyone could be at risk.25"] <- "reason for risk: Anyone could be at risk"
sum(DIQ_df$`reason for risk: Anyone could be at risk`)
# Returns 117, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175P <- c()
DIQ_df$`Anyone could be at risk.0` <- c()

#Doctor warning

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175Q)){
  DIQ_df[paste("Doctor warning", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175Q == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Doctor warning.26"] <- "reason for risk: Doctor warning"
sum(DIQ_df$`reason for risk: Doctor warning`)
# Returns 136, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175Q <- c()
DIQ_df$`Doctor warning.0` <- c()

#Other, specify

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175R)){
  DIQ_df[paste("Other", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175R == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Other.27"] <- "reason for risk: Other"
sum(DIQ_df$`reason for risk: Other`)
# Returns 23, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175R <- c()
DIQ_df$`Other.0` <- c()

#Gestational diabetes

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175S)){
  DIQ_df[paste("Gestational diabetes", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175S == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Gestational diabetes.28"] <- "reason for risk: Gestational diabetes"
sum(DIQ_df$`reason for risk: Gestational diabetes`)
# Returns 31, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175S <- c()
DIQ_df$`Gestational diabetes.0` <- c()

#Frequent urination

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175T)){
  DIQ_df[paste("Frequent urination", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175T == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Frequent urination.29"] <- "reason for risk: Frequent urination"
sum(DIQ_df$`reason for risk: Frequent urination`)
# Returns 105, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175T <- c()
DIQ_df$`Frequent urination.0` <- c()

#Thirst

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175U)){
  DIQ_df[paste("Thirst", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175U == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Thirst.30"] <- "reason for risk: Thirst"
sum(DIQ_df$`reason for risk: Thirst`)
# Returns 79, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175U <- c()
DIQ_df$`Thirst.0` <- c()

#Craving for sweet/eating a lot of sugar

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175V)){
  DIQ_df[paste("Craving sweets", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175V == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Craving sweets.31"] <- "reason for risk: Craving sweets"
sum(DIQ_df$`reason for risk: Craving sweets`)
# Returns 11, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175V <- c()
DIQ_df$`Craving sweets.0` <- c()

#Medication

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175W)){
  DIQ_df[paste("Medication", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175W == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Medication.32"] <- "reason for risk: Medication"
sum(DIQ_df$`reason for risk: Medication`)
# Returns 4, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175W <- c()
DIQ_df$`Medication.0` <- c()

#Polycystic ovarian syndrome

#For loop to separate columns
for(unique_value in unique(DIQ_df$DIQ175X)){
  DIQ_df[paste("Polycystic ovarian syndrome", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ175X == unique_value, 1, 0)
}

colnames(DIQ_df)[colnames(DIQ_df)=="Polycystic ovarian syndrome.33"] <- "reason for risk: Polycystic ovarian syndrome"
sum(DIQ_df$`reason for risk: Polycystic ovarian syndrome`)
# Returns 1, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ175X <- c()
DIQ_df$`Polycystic ovarian syndrome.0` <- c()

```
Column 31: Had blood tested past three years

Code: 1 -> Yes (Count = 2836)
Code: 2 -> No (Count = 3167)
Code: 7 -> Refused (Count = 2)
Code: 9 -> Don't Know (Count = 181)
Code: . -> Missing (Count = 3389)

- Confirm data as categorical based on CDC website for specific column
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.

```{r}

# Had blood tested past three years (DIQ180)

# Set NA's to 0
DIQ_df$DIQ180[is.na(x = DIQ_df$DIQ180)] <- 0

# Implement for loop that will create a new column for each distinct response
for(unique_value in unique(DIQ_df$DIQ180)){
  DIQ_df[paste("blood tested in past 3 years", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ180 == unique_value, 1, 0)
}

# Rename columns based on question and response of column
colnames(DIQ_df)[colnames(DIQ_df)=="blood tested in past 3 years.1"] <- "blood tested in past 3 years: yes"
sum(DIQ_df$`blood tested in past 3 years: yes`)
# Returns 2836, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="blood tested in past 3 years.2"] <- "blood tested in past 3 years: No"
sum(DIQ_df$`blood tested in past 3 years: No`)
# Returns 3167, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="blood tested in past 3 years.7"] <- "blood tested in past 3 years: Refused"
sum(DIQ_df$`blood tested in past 3 years: Refused`)
# Returns 2, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="blood tested in past 3 years.9"] <- "blood tested in past 3 years: Don't know"
sum(DIQ_df$`blood tested in past 3 years: Don't know`)
# Returns 181, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="blood tested in past 3 years.0"] <- "blood tested in past 3 years: Missing"
sum(DIQ_df$`blood tested in past 3 years: Missing`)
# Returns 3389, in agreement with the website

#Remove original column and missing values column
DIQ_df$DIQ180 <- c()
DIQ_df$`blood tested in past 3 years: Missing` <- c()
```
Column 32: Taking insulin now

Code: 1 -> Yes (Count = 256)
Code: 2 -> No (Count = 9316)
Code: 7 -> Refused (Count = 1)
Code: 9 -> Don't Know (Count = 2)
Code: . -> Missing (Count = 0)

- Confirm data as categorical based on CDC website for specific column
- Avoid creating columns for Code NA as there is no count for this column.
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}

# Taking insulin now (DIQ050)


# Implement for loop that will create a new column for each distinct response
for(unique_value in unique(DIQ_df$DIQ050)){
  DIQ_df[paste("Taking insulin now", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ050 == unique_value, 1, 0)
}

# Rename columns based on question and response of column

colnames(DIQ_df)[colnames(DIQ_df)=="Taking insulin now.1"] <- "Taking insulin now: yes"
sum(DIQ_df$`Taking insulin now: yes`)
# Returns 256, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Taking insulin now.2"] <- "Taking insulin now: No"
sum(DIQ_df$`Taking insulin now: No`)
# Returns 9316, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Taking insulin now.7"] <- "Taking insulin now: Refused"
sum(DIQ_df$`Taking insulin now: Refused`)
# Returns 1, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Taking insulin now.9"] <- "Taking insulin now: Don't know"
sum(DIQ_df$`Taking insulin now: Don't know`)
# Returns 2, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ050 <- c()

```
Column 33: How long taking insulin (DID060)

Code: 1 to 55 -> Range of Values (Count = 249)
Code: 666 -> Less than 1 month (Count = 5)
Code: 777 -> Refused (Count = 0)
Code: 999 -> Don't Know (Count = 2)
Code: . -> Missing (Count = 9319)

- Confirm data as continuous based on CDC website for specific column
- Avoid creating columns for Code 777 as there is no count for this column.
- Decision to place value of 0 for code 666 as it is under 1 year and will keep the data continuous (leaving 666 would obviously skew mean and would be inaccurate)
- Address each response of the question and code these codes/values appropriately.
- Calculate the mean and perform mean imputation on rows missing values (mean imputation discussed below)
- Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfuly
- Confirm that all missing values were replaced with the mean.

Column 34: Unit of measure (month/year) (DID060U)

Code: 1 -> Months (Count = 30)
Code: 2 -> Years (Count = 219)
Code: . -> Missing (Count = 9326)

- Confirm data as categorical based on CDC website for specific column
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}

# How long taking insuling (DID060)

# Number of NA's
sum(is.na(DIQ_df$DID060))
# 9319, agrees with website

# Set 666 to 0 and 999 to NA
DIQ_df$DID060[DIQ_df$DID060==666] <- 0
DIQ_df$DID060[DIQ_df$DID060==999] <- NA

# Calculate the mean and perform mean imputation on rows missing values 
mean(DIQ_df$DID060, na.rm = TRUE)
DIQ_df$DID060[is.na(DIQ_df$DID060)] <- 29.25044

# Check means included in NA columns 
sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID060 = '29.25044'")
# returns 9321, a combination of codes NA and 999 (which was turned to NA), which agrees with website

table(DIQ_df$DID060)
sum(is.na(DIQ_df$DID060))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean

# Unit of measure(month/years)

# Set NA's to 0
DIQ_df$DIQ060U[is.na(x = DIQ_df$DIQ060U)] <- 0

#Implement for loop
for(unique_value in unique(DIQ_df$DIQ060U)){
  DIQ_df[paste("Insulin Use Unit of measure", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ060U == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website

colnames(DIQ_df)[colnames(DIQ_df)=="Insulin Use Unit of measure.1"] <- "Insulin Use Unit of measure: Months"
sum(DIQ_df$`Insulin Use Unit of measure: Months`)
# Returns 30, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Insulin Use Unit of measure.2"] <- "Insulin Use Unit of measure: Years"
sum(DIQ_df$`Insulin Use Unit of measure: Years`)
# Returns 219, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Insulin Use Unit of measure.0"] <- "Insulin Use Unit of measure: Missing"
sum(DIQ_df$`Insulin Use Unit of measure: Missing`)
# Returns 9326, in agreement with the website

#Remove original column and missing values column
DIQ_df$DIQ060U <- c()
DIQ_df$`Insulin Use Unit of measure: Missing` <- c()

```
Column 35: Take diabetic pills to lower blood sugar (DIQ070)

Code: 1 -> Yes (Count = 643)
Code: 2 -> No (Count = 870)
Code: 7 -> Refused (Count = 1)
Code: 9 -> Don't Know (Count = 1)
Code: . -> Missing (Count = 8060)

- Confirm data as categorical based on CDC website for specific column
-Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}

# Take diabetic pills to lower blood sugar
DIQ_df$DIQ070[is.na(x = DIQ_df$DIQ070)] <- 0

# Implement for loop
for(unique_value in unique(DIQ_df$DIQ070)){
  DIQ_df[paste("Take diabetic pills", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ070 == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website

colnames(DIQ_df)[colnames(DIQ_df)=="Take diabetic pills.1"] <- "Take diabetic pills: Yes"
sum(DIQ_df$`Take diabetic pills: Yes`)
# Returns 643, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Take diabetic pills.2"] <- "Take diabetic pills: No"
sum(DIQ_df$`Take diabetic pills: No`)
# Returns 870, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Take diabetic pills.7"] <- "Take diabetic pills: Refused"
sum(DIQ_df$`Take diabetic pills: Refused`)
# Returns 1, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Take diabetic pills.9"] <- "Take diabetic pills: Don't know"
sum(DIQ_df$`Take diabetic pills: Don't know`)
# Returns 1, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Take diabetic pills.0"] <- "Take diabetic pills: Missing"
sum(DIQ_df$`Take diabetic pills: Missing`)
# Returns 8060, in agreement with the website

#Remove original column and missing values column
DIQ_df$DIQ070 <- c()
DIQ_df$`Take diabetic pills: Missing` <- c()

```
Column 36: How long ago saw a diabetes specialist

Code: 1 -> 1 year ago or less (Count = 276)
Code: 2 -> More than 1 year ago but no more than 2 years ago (Count = 64)
Code: 3 -> More than 2 years ago but no more than 5 years ago (Count = 86)
Code: 4 -> More than 5 years ago (Count = 85)
Code: 5 -> Never (Count = 330)
Code: 7 -> Refused (Count = 0)
Code: 9 -> Don't know (Count = 12)
Code: . -> Missing (Count = 8060)

- Confirm data as categorical based on CDC website for specific column
- Avoid creating columns for Code 7 as there is no count for this column.
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}
# How long ago saw a diabetes specialist (DIQ230)

# Take diabetic pills to lower blood sugar
DIQ_df$DIQ230[is.na(x = DIQ_df$DIQ230)] <- 0

# Implement for loop
for(unique_value in unique(DIQ_df$DIQ230)){
  DIQ_df[paste("Time since specialist", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ230 == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website

colnames(DIQ_df)[colnames(DIQ_df)=="Time since specialist.1"] <- "Time since specialist: 1 year ago or less"
sum(DIQ_df$`Time since specialist: 1 year ago or less`)
# Returns 276, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Time since specialist.2"] <- "Time since specialist: More than 1 year ago but no more than 2 years ago"
sum(DIQ_df$`Time since specialist: More than 1 year ago but no more than 2 years ago`)
# Returns 64, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Time since specialist.3"] <- "Time since specialist: More than 2 years ago but no more than 5 years ago"
sum(DIQ_df$`Time since specialist: More than 2 years ago but no more than 5 years ago`)
# Returns 86, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Time since specialist.4"] <- "Time since specialist: more than 5 years ago"
sum(DIQ_df$`Time since specialist: more than 5 years ago`)
# Returns 85, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Time since specialist.5"] <- "Time since specialist: Never"
sum(DIQ_df$`Time since specialist: Never`)
# Returns 330, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Time since specialist.9"] <- "Time since specialist: Don't know"
sum(DIQ_df$`Time since specialist: Don't know`)
# Returns 12, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Time since specialist.0"] <- "Time since specialist: Missing"
sum(DIQ_df$`Time since specialist: Missing`)
# Returns 8722, in agreement with the website

# No columns created for code "7" as there were no values which is in accordance with the counts of the website

#Remove original column and missing values column
DIQ_df$DIQ230 <- c()
DIQ_df$`Time since specialist: Missing` <- c()

```
Column 37: Is there one Dr you see for diabetes

Code: 1 -> Yes (Count = 643)
Code: 2 -> No (Count = 210)
Code: 7 -> Refused (Count = 0)
Code: 9 -> Don't know (Count = 0)
Code: . -> Missing (Count = 8722)

- Confirm data as categorical based on CDC website for specific column
- Avoid creating columns for Code 7 and Code 9 as there are no counts for these columns
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}
# Take diabetic pills to lower blood sugar (DIQ240)

# Set NA values to 0
DIQ_df$DIQ240[is.na(x = DIQ_df$DIQ240)] <- 0

#Implement for loop
for(unique_value in unique(DIQ_df$DIQ240)){
  DIQ_df[paste("See Dr for diabetes", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ240 == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website
colnames(DIQ_df)[colnames(DIQ_df)=="See Dr for diabetes.1"] <- "See Dr for diabetes: Yes"
sum(DIQ_df$`See Dr for diabetes: Yes`)
# Returns 643, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="See Dr for diabetes.2"] <- "See Dr for diabetes: No"
sum(DIQ_df$`See Dr for diabetes: No`)
# Returns 210, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="See Dr for diabetes.0"] <- "See Dr for diabetes: Missing"
sum(DIQ_df$`See Dr for diabetes: Missing`)
# Returns 8722, in agreement with the website

# No columns created for code "7" and "9" as there were no values which is in accordance with the counts of the website

#Remove original column and missing values column
DIQ_df$DIQ240 <- c()
DIQ_df$`See Dr for diabetes: Missing` <- c()

```
Column 38: Past year how many times seen doctor

Code: 1 to 60 -> Range of Values (Count = 627)
Code: 0 -> None (Count = 13)
Code: 7777 -> Refused (Count = 0)
Code: 9999 -> Don't know (Count = 3)
Code: . -> Missing (Count = 8932)

- Confirm data as continuous based on CDC website for specific column
- Avoid creating columns for Code 7777 as there is no count for this column.
- Change 9999 code to NA's per CDC guidance addressed in intro. 
- Calculate the mean and perform mean imputation on rows missing values.
- Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfuly
- Confirm that all missing values were replaced with the mean.
```{r}

#Calculate number of NA's
sum(is.na(DIQ_df$DID250))
#8932, in agreement with website

#Add NA to 9999 column for mean imputation
DIQ_df$DID250[DIQ_df$DID250==9999] <- NA

#Calculate mean and apply mean imputation 
mean(DIQ_df$DID250, na.rm = TRUE)
DIQ_df$DID250[is.na(DIQ_df$DID250)] <- 4.507812

#Confirm mean imputation by checking for values of mean with SQL
sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID250 = '4.507812'")
# 8935, this equals the combination of the values for the Don't Know and Missing columns

table(DIQ_df$DID250)
sum(is.na(DIQ_df$DID250))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean

```
Column 39: How often check blood for glucose/sugar (DID260)

Code: 1 to 15 -> Range of Values (Count = 661)
Code: 0 -> Never (Count = 188)
Code: 7777 -> Refused (Count = 0)
Code: 9999 -> Don't know (Count = 0)
Code: . -> Missing (Count = 8726)

- Confirm data as continuous based on CDC website for specific column.
- Avoid creating columns for Code 7777 and Code 9999 as there are no counts for this column.
- Address each response of the question and code these codes/values appropriately.
- Calculate the mean and perform mean imputation on rows missing values
 - Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfuly
- Confirm that all missing values were replaced with the mean.

Column 40: Unit of measure (day/week/month/year) (DIQ260U)
Code: 1 -> Per day (Count = 412)
Code: 2 -> Per week (Count = 150)
Code: 3 -> Per month (Count = 65)
Code: 4 -> Per year (Count = 34)
Code: . -> Missing (Count = 8914)

- Confirm data as categorical based on CDC website for specific column
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.

```{r}

# How often check blood for glucose/sugar (DID260) 

# Count of NA's in column
sum(is.na(DIQ_df$DID260))
#8726

#Calculate mean and apply mean imputation 
mean(DIQ_df$DID260, na.rm = TRUE)
DIQ_df$DID260[is.na(DIQ_df$DID260)] <- 1.657244

#Confirm mean imputation by checking for values of mean with SQL
sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID260 = '1.657244'")
# 8726, this equals the Missing columns from the website

table(DIQ_df$DID260)
sum(is.na(DIQ_df$DID260))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean


# Unit of measure (day/week/month/year) (DIQ260U)
DIQ_df$DIQ260U[is.na(x = DIQ_df$DIQ260U)] <- 0

# Implement for loop 
for(unique_value in unique(DIQ_df$DIQ260U)){
  DIQ_df[paste("Check Blood for Glucose Unit of measure", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ260U == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website
colnames(DIQ_df)[colnames(DIQ_df)=="Check Blood for Glucose Unit of measure.1"] <- "Check Blood for Glucose Unit of measure: Per day"
sum(DIQ_df$`Check Blood for Glucose Unit of measure: Per day`)
# Returns 412, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Check Blood for Glucose Unit of measure.2"] <- "Check Blood for Glucose Unit of measure: Per week"
sum(DIQ_df$`Check Blood for Glucose Unit of measure: Per week`)
# Returns 150, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Check Blood for Glucose Unit of measure.3"] <- "Check Blood for Glucose Unit of measure: Per month"
sum(DIQ_df$`Check Blood for Glucose Unit of measure: Per month`)
# Returns 65, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Check Blood for Glucose Unit of measure.4"] <- "Check Blood for Glucose Unit of measure: Per year"
sum(DIQ_df$`Check Blood for Glucose Unit of measure: Per year`)
# Returns 34, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Check Blood for Glucose Unit of measure.0"] <- "Check Blood for Glucose Unit of measure: Missing"
sum(DIQ_df$`Check Blood for Glucose Unit of measure: Missing`)
# Returns 8914, in agreement with the website


#Remove original column and missing values column
DIQ_df$DIQ260U <- c()
DIQ_df$`Check Blood for Glucose Unit of measure: Missing` <- c()


```
Column 41: Past year DR checked for A1C

Code: 1 -> Yes (Count = 641)
Code: 2 -> No (Count = 156)
Code: 7 -> Refused (Count = 0)
Code: 9 -> Don't know (Count = 56)
Code: . -> Missing (Count = 8722)

- Confirm data as categorical based on CDC website for specific column
- Avoid creating columns for Code 7 as there is no count for this column.
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}
# Past year DR checked for A1C (DIQ275)

# Set NA's to 0
DIQ_df$DIQ275[is.na(x = DIQ_df$DIQ275)] <- 0

# Implement for loop
for(unique_value in unique(DIQ_df$DIQ275)){
  DIQ_df[paste("DR checked for A1C", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ275 == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website

colnames(DIQ_df)[colnames(DIQ_df)=="DR checked for A1C.1"] <- "DR checked for A1C: Yes"
sum(DIQ_df$`DR checked for A1C: Yes`)
# Returns 641, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="DR checked for A1C.2"] <- "DR checked for A1C: No"
sum(DIQ_df$`DR checked for A1C: No`)
# Returns 156, in agreement with the website 

colnames(DIQ_df)[colnames(DIQ_df)=="DR checked for A1C.9"] <- "DR checked for A1C: Don't know"
sum(DIQ_df$`DR checked for A1C: Don't know`)
# Returns 56, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="DR checked for A1C.0"] <- "DR checked for A1C: Missing"
sum(DIQ_df$`DR checked for A1C: Missing`)
# Returns 8722, in agreement with the website

# No columns created for code "7" as there were no values which is in accordance with the counts of the website

#Remove original column and missing values column 
DIQ_df$DIQ275 <- c()
DIQ_df$`DR checked for A1C: Missing` <- c()

```
Column 42: What was your last A1C level

Code: 2 to 18.5 -> Range of Values (Count = 404)
Code: 777 -> Refused (Count = 2)
Code: 999 -> Don't know (Count = 235)
Code: . -> Missing (Count = 8934)

- Confirm data as continuous based on CDC website for specific column
- Address each response of the question and code these codes/values appropriately.
- For Codes 777 and 999 assign to NA to allow for mean imputation per CDC website suggestion provided in introduction.
- Calculate the mean and perform mean imputation on rows missing values
- Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfuly
- Confirm that all missing values were replaced with the mean.
```{r}

# What was your last A1C level (DIQ280)

# Sum NA values
sum(is.na(DIQ_df$DIQ280))
#8934

# Assign NA values
DIQ_df$DIQ280[DIQ_df$DIQ280==777] <- NA
DIQ_df$DIQ280[DIQ_df$DIQ280==999] <- NA


# Calculate mean and apply mean imputation 
mean(DIQ_df$DIQ280, na.rm = TRUE)
DIQ_df$DIQ280[is.na(DIQ_df$DIQ280)] <- 7.334158

# Check number of mean imputations 
sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DIQ280 = '7.334158'")
# 9171, this is a combination of the Missing Values, Don't Know, and Refused columns and this is in agreement with the website


table(DIQ_df$DIQ280)
sum(is.na(DIQ_df$DIQ280))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean


```
Column 43: What does Dr say A1C should be (DIQ291)

Code: 1 -> Less than 6 (Count = 213)
Code: 2 -> Less than 7 (Count = 182)
Code: 3 -> Less than 8 (Count = 32)
Code: 4 -> Less than 9 (Count = 5)
Code: 5 -> Less than 10 (Count = 9)
Code: 6 -> Provider did not specify goal (Count = 88)
Code: 77 -> Refused (Count = 2)
Code: 99 -> Don't know (Count = 110)
Code: . -> Missing (Count = 8934)

- Confirm data as categorical based on CDC website for specific column
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}
#  What does Dr say A1C should be (DIQ291)

# Assign NA values to zero
DIQ_df$DIQ291[is.na(x = DIQ_df$DIQ291)] <- 0

# Implement for loop
for(unique_value in unique(DIQ_df$DIQ291)){
  DIQ_df[paste("Dr say A1C should be", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ291 == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website
colnames(DIQ_df)[colnames(DIQ_df)=="Dr say A1C should be.1"] <- "Dr say A1C should be: Less than 6"
sum(DIQ_df$`Dr say A1C should be: Less than 6`)
# Returns 213, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Dr say A1C should be.2"] <- "Dr say A1C should be: Less than 7"
sum(DIQ_df$`Dr say A1C should be: Less than 7`)
# Returns 182, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Dr say A1C should be.3"] <- "Dr say A1C should be: Less than 8"
sum(DIQ_df$`Dr say A1C should be: Less than 8`)
# Returns 32, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Dr say A1C should be.4"] <- "Dr say A1C should be: Less than 9"
sum(DIQ_df$`Dr say A1C should be: Less than 9`)
# Returns 5, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Dr say A1C should be.5"] <- "Dr say A1C should be: Less than 10"
sum(DIQ_df$`Dr say A1C should be: Less than 10`)
# Returns 9, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Dr say A1C should be.6"] <- "Dr say A1C should be: Provider did not specify goal"
sum(DIQ_df$`Dr say A1C should be: Provider did not specify goal`)
# Returns 88, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Dr say A1C should be.77"] <- "Dr say A1C should be: Refused"
sum(DIQ_df$`Dr say A1C should be: Refused`)
# Returns 2, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Dr say A1C should be.99"] <- "Dr say A1C should be: Don't know"
sum(DIQ_df$`Dr say A1C should be: Don't know`)
# Returns 110, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Dr say A1C should be.0"] <- "Dr say A1C should be: Missing"
sum(DIQ_df$`Dr say A1C should be: Missing`)
# Returns 8934, in agreement with the website


#Remove original column and missing values column
DIQ_df$DIQ291 <- c()
DIQ_df$`Dr say A1C should be: Missing` <- c()


```
Column 44: What was your recent SBP (DIQ300S)

Code: 80 to 201 -> Range of Values (Count = 539)
Code: 7777 -> Refused (Count = 1)
Code: 9999 -> Don't know (Count = 305)
Code: . -> Missing (Count = 8730)

- Confirm data as continuous based on CDC website for specific column
- Address each response of the question and code these codes/values appropriately.
- Assign code 7777 and code 9999 to NA to allow for mean imputation per CDC website suggestion provided in introduction.
- Calculate the mean and perform mean imputation on rows missing values
 - Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfully
- Confirm that all missing values were replaced with the mean.
```{r}

# What was your recent SBP (DIQ300S)

# Check number of NA's
sum(is.na(DIQ_df$DIQ300S))
#8730

# Change columns to NA
DIQ_df$DIQ300S[DIQ_df$DIQ300S==7777] <- NA
DIQ_df$DIQ300S[DIQ_df$DIQ300S==9999] <- NA

# Calculate mean and apply mean imputation
mean(DIQ_df$DIQ300S, na.rm = TRUE)
DIQ_df$DIQ300S[is.na(DIQ_df$DIQ300S)] <- 130.4935

sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DIQ300S = '130.4935'")
#9036, this is a combination of the Missing Values, Refused, and Don't Know columns and this agrees with the website

table(DIQ_df$DIQ300S)
sum(is.na(DIQ_df$DIQ300S))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean

```
Column 45: What was your recent DBP (DIQ300D)

Code: 17 to 251 -> Range of Values (Count = 539)
Code: 7777 -> Refused (Count = 1)
Code: 9999 -> Don't know (Count = 305)
Code: . -> Missing (Count = 8730)

- Confirm data as continuous based on CDC website for specific column
- Address each response of the question and code these codes/values appropriately.
- Assign code 7777 and code 9999 to NA to allow for mean imputation per CDC website suggestion provided in introduction.
- Calculate the mean and perform mean imputation on rows missing values
 - Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfully
- Confirm that all missing values were replaced with the mean.
```{r}
# What was your recent DBP (DIQ300D)

# Check number of NA's
sum(is.na(DIQ_df$DIQ300D))
#8730

# Change columns to NA
DIQ_df$DIQ300D[DIQ_df$DIQ300D==7777] <- NA
DIQ_df$DIQ300D[DIQ_df$DIQ300D==9999] <- NA

# Calculate mean and apply mean imputation
mean(DIQ_df$DIQ300D, na.rm = TRUE)
DIQ_df$DIQ300D[is.na(DIQ_df$DIQ300D)] <- 78.22913

sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DIQ300D = '78.22913'")
#9060, this is a combination of the Missing Values, Refused, and Don't Know columns and this agrees with the website

table(DIQ_df$DIQ300S)
sum(is.na(DIQ_df$DIQ300S))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean
```
Column 46: What does Dr say SBP should be (DID310S)

Code: 80 to 175 -> Range of Values (Count = 336)
Code: 6666 -> Provider did not specify goal (Count = 308)
Code: 7777 -> Refused (Count = 2)
Code: 9999 -> Don't know (Count = 200)
Code: . -> Missing (Count = 8729)

- Confirm data as continuous based on CDC website for specific column
- Address each response of the question and code these codes/values appropriately.
- Assign code 6666, code 7777 and code 9999 to NA to allow for mean imputation per CDC website suggestion provided in introduction.
- Calculate the mean and perform mean imputation on rows missing values
- Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfully
- Confirm that all missing values were replaced with the mean.

```{r}

# What does Dr say SBP should be (DID310S)

# Find number of NA's in column
sum(is.na(DIQ_df$DID310S))
#8729

# Change codes to NA for mean imputation
DIQ_df$DID310S[DIQ_df$DID310S==6666] <- NA
DIQ_df$DID310S[DIQ_df$DID310S==7777] <- NA
DIQ_df$DID310S[DIQ_df$DID310S==9999] <- NA

# Calculate mean and apply mean imputation
mean(DIQ_df$DID310S, na.rm = TRUE)
DIQ_df$DID310S[is.na(DIQ_df$DID310S)] <- 123.0595

sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID310S = '123.0595'")
#9239, this is a combination of the Missing Values, Provider did not specify goal, Refused, and Don't Know columns and this agrees with the website

table(DIQ_df$DID310S)
sum(is.na(DIQ_df$DID310S))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean

```
Column 47: What does Dr say DBP should be (DID310D)

Code: 18 to 140 -> Range of Values (Count = 315)
Code: 6666 -> Provider did not specify goal (Count = 308)
Code: 7777 -> Refused (Count = 2)
Code: 9999 -> Don't know (Count = 221)
Code: . -> Missing (Count = 8729)

- Confirm data as continuous based on CDC website for specific column
- Address each response of the question and code these codes/values appropriately.
- Assign code 6666, code 7777 and code 9999 to NA to allow for mean imputation per CDC website suggestion provided in introduction.
- Calculate the mean and perform mean imputation on rows missing values
- Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfully
- Confirm that all missing values were replaced with the mean.
```{r}

# What does Dr say SBP should be (DID310D)

# Find number of NA's in column
sum(is.na(DIQ_df$DID310D))
#8729

# Change codes to NA for mean imputation
DIQ_df$DID310D[DIQ_df$DID310D==6666] <- NA
DIQ_df$DID310D[DIQ_df$DID310D==7777] <- NA
DIQ_df$DID310D[DIQ_df$DID310D==9999] <- NA

# Calculate mean and apply mean imputation
mean(DIQ_df$DID310D, na.rm = TRUE)
DIQ_df$DID310D[is.na(DIQ_df$DID310D)] <- 76.7746

sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID310D = '76.7746'")
#9260, this is a combination of the Missing Values, Provider did not specify goal, Refused, and Don't Know columns and this agrees with the website

table(DIQ_df$DID310D)
sum(is.na(DIQ_df$DID310D))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean

```
Column 48: What was most recent LDL number (DID320)

Code: 4 to 520 -> Range of Values (Count = 115)
Code: 5555 -> Never heard of LDL (Count = 59)
Code: 6666 -> Never had cholesterol test (Count = 34)
Code: 7777 -> Refused (Count = 5)
Code: 9999 -> Don't know (Count = 633)
Code: . -> Missing (Count = 8729)

- Confirm data as continuous based on CDC website for specific column
- Address each response of the question and code these codes/values appropriately.
- Assign code 5555, code 6666, code 7777 and code 9999 to NA to allow for mean imputation as these categories provide no information towards a range and this decision is in accordance with the CDC website suggestion provided in introduction.
- Calculate the mean and perform mean imputation on rows missing values
- Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfully
- Confirm that all missing values were replaced with the mean.
```{r}
# What was most recent LDL number (DID320)

# Find number of NA's in column
sum(is.na(DIQ_df$DID320))
#8729

# Change codes to NA for mean imputation
DIQ_df$DID320[DIQ_df$DID320==5555] <- NA
DIQ_df$DID320[DIQ_df$DID320==6666] <- NA
DIQ_df$DID320[DIQ_df$DID320==7777] <- NA
DIQ_df$DID320[DIQ_df$DID320==9999] <- NA

# Calculate mean and apply mean imputation
mean(DIQ_df$DID320, na.rm = TRUE)
DIQ_df$DID320[is.na(DIQ_df$DID320)] <- 122.9826

sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID320 = '122.9826'")
#9460, this is a combination of the Never heard of LDL, Never had cholesterol test, Refused, Don't Know, and Missing columns and this agrees with the website

table(DIQ_df$DID320)
sum(is.na(DIQ_df$DID320))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean

```
Column 49: What does DR say LDL should be (DID330)

Code: 6 to 205 -> Range of Values (Count = 87)
Code: 6666 -> Provider did not specify goal (Count = 242)
Code: 7777 -> Refused (Count = 3)
Code: 9999 -> Don't know (Count = 421)
Code: . -> Missing (Count = 8822)

- Confirm data as continuous based on CDC website for specific column
- Assign code 6666, code 7777 and code 9999 to NA to allow for mean imputation per CDC website suggestion provided in introduction.
- Address each response of the question and code these codes/values appropriately.
- Calculate the mean and perform mean imputation on rows missing values
 - Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfully
- Confirm that all missing values were replaced with the mean.

```{r}

# What was most recent LDL number (DID330)

# Find number of NA's in column
sum(is.na(DIQ_df$DID330))
#8822

# Change codes to NA for mean imputation
DIQ_df$DID330[DIQ_df$DID330==6666] <- NA
DIQ_df$DID330[DIQ_df$DID330==7777] <- NA
DIQ_df$DID330[DIQ_df$DID330==9999] <- NA

# Calculate mean and apply mean imputation
mean(DIQ_df$DID330, na.rm = TRUE)
DIQ_df$DID330[is.na(DIQ_df$DID330)] <- 116.1609

sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID330 = '116.1609'")
#9488, this is a combination of the Provider did not specify goal, Refused, Don't Know, and Missing columns and this agrees with the website

table(DIQ_df$DID330)
sum(is.na(DIQ_df$DID330))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean


```
Column 50: Past year time Dr check feet for sores (DID341)

Code: 1 to 34 -> Range of Values (Count = 590)
Code: 0 -> None (Count = 242)
Code: 7777 -> Refused (Count = 2)
Code: 9999 -> Don't know/not sure (Count = 9)
Code: . -> Missing (Count = 8732)

- Confirm data as continuous based on CDC website for specific column
- Assign code 7777 and code 9999 to NA to allow for mean imputation per CDC website suggestion provided in introduction.
- Address each response of the question and code these codes/values appropriately.
- Calculate the mean and perform mean imputation on rows missing values
 - Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfully
- Confirm that all missing values were replaced with the mean.
```{r}

# Past year time Dr check feet for sores (DID341)

# Find number of NA's in column
sum(is.na(DIQ_df$DID341))
#8732

# Change codes to NA for mean imputation
DIQ_df$DID341[DIQ_df$DID341==7777] <- NA
DIQ_df$DID341[DIQ_df$DID341==9999] <- NA

# Calculate mean and apply mean imputation
mean(DIQ_df$DID341, na.rm = TRUE)
DIQ_df$DID341[is.na(DIQ_df$DID341)] <- 2.403846

sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID341 = '2.403846'")
#8743, this is a combination of Refused, Don't Know/not sure, and Missing columns and this agrees with the website

table(DIQ_df$DID341)
sum(is.na(DIQ_df$DID341))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean
```
Column 51: How often do you check your feet (DID350)

Code: 1 to 20 -> Range of Values (Count = 699)
Code: 0 -> None (Count = 139)
Code: 7777 -> Refused (Count = 0)
Code: 9999 -> Don't know/not sure (Count = 5)
Code: . -> Missing (Count = 8732)

- Confirm data as continuous based on CDC website for specific column
- Assign code 9999 to NA to allow for mean imputation per CDC website suggestion provided in introduction.
- Address each response of the question and code these codes/values appropriately.
- Calculate the mean and perform mean imputation on rows missing values
 - Confirm mean imputation by checking for values of mean with SQL
- Use the table function as a "sanity-check" that your calculated mean seems accurate with the values of the column and that your mean-imputation ran successfully
- Confirm that all missing values were replaced with the mean.

Column 52: Unit of measure (day/week/month/year) (DIQ350U)
Code: 1 -> Per day (Count = 487)
Code: 2 -> Per week (Count = 152)
Code: 3 -> Per month (Count = 46)
Code: 4 -> Per year (Count = 14)
Code: . -> Missing (Count = 8876)

- Confirm data as categorical based on CDC website for specific column
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}

# Past year time Dr check feet for sores (DID350)

# Find number of NA's in column
sum(is.na(DIQ_df$DID350))
#8732

# Change codes to NA for mean imputation
DIQ_df$DID350[DIQ_df$DID350==9999] <- NA

# Calculate mean and apply mean imputation
mean(DIQ_df$DID350, na.rm = TRUE)
DIQ_df$DID350[is.na(DIQ_df$DID350)] <- 1.220764

sqldf("SELECT COUNT(*) FROM DIQ_df WHERE DID350 = '1.220764'")
#8743, this is a combination of Refused, Don't Know/not sure, and Missing columns and this agrees with the website

table(DIQ_df$DID350)
sum(is.na(DIQ_df$DID350))
# 0 NA's returned, confirming that the NA's were appropriately replaced with the mean

# Unit of measure (day/week/month/year) (DIQ250U)

# Set NA’s to zero
DIQ_df$DIQ350U[is.na(x = DIQ_df$DIQ350U)] <- 0

# Implement for loop that will create a new column for each distinct response
for(unique_value in unique(DIQ_df$DIQ350U)){
  DIQ_df[paste("Feet Check Unit of measure", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ350U == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website

colnames(DIQ_df)[colnames(DIQ_df)=="Feet Check Unit of measure.1"] <- "Feet Check Unit of measure: Per day"
sum(DIQ_df$`Feet Check Unit of measure: Per day`)
# Returns 487, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Feet Check Unit of measure.2"] <- "Feet Check Unit of measure: Per week"
sum(DIQ_df$`Feet Check Unit of measure: Per week`)
# Returns 152, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Feet Check Unit of measure.3"] <- "Feet Check Unit of measure: Per month"
sum(DIQ_df$`Feet Check Unit of measure: Per month`)
# Returns 46, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Feet Check Unit of measure.4"] <- "Feet Check Unit of measure: Per year"
sum(DIQ_df$`Feet Check Unit of measure: Per year`)
# Returns 14, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Feet Check Unit of measure.0"] <- "Feet Check Unit of measure: Missing"
sum(DIQ_df$`Feet Check Unit of measure: Missing`)
# Returns 8876, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ350U <- c()
DIQ_df$`Feet Check Unit of measure: Missing` <- c()

```
Column 53: Last time had pupils dilated for exam (DIQ360)
Code: 1 -> Less than 1 month (Count = 82)
Code: 2 -> 1-12 months (Count = 440)
Code: 3 -> 13-24 months (Count = 129)
Code: 4 -> Greater than 2 years (Count = 116)
Code: 5 -> Never (Count = 64)
Code: 7 -> Refused (Count = 0)
Code: 9 -> Don't know (Count = 15)
Code: . -> Missing (Count = 8729)

- Confirm data as categorical based on CDC website for specific column
- Use table to ensure values are coded based on code and not on month which is unclear from website
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.

```{r}

table(DIQ_df$DIQ360)

# Last time had pupils dilated for exam (DIQ360)

# Set NA’s to zero
DIQ_df$DIQ360[is.na(x = DIQ_df$DIQ360)] <- 0
# 8729

# Implement for loop
for(unique_value in unique(DIQ_df$DIQ360)){
  DIQ_df[paste("Pupils dilated last", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ360 == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website

colnames(DIQ_df)[colnames(DIQ_df)=="Pupils dilated last.1"] <- "Pupils dilated last: Less than 1 month"
sum(DIQ_df$`Pupils dilated last: Less than 1 month`)
# Returns 82, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Pupils dilated last.2"] <- "Pupils dilated last: 1-12 months"
sum(DIQ_df$`Pupils dilated last: 1-12 months`)
# Returns 440, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Pupils dilated last.3"] <- "Pupils dilated last: 13-24 months"
sum(DIQ_df$`Pupils dilated last: 13-24 months`)
# Returns 129, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Pupils dilated last.4"] <- "Pupils dilated last: Greater than 2 years"
sum(DIQ_df$`Pupils dilated last: Greater than 2 years`)
# Returns 116, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Pupils dilated last.5"] <- "Pupils dilated last: Never"
sum(DIQ_df$`Pupils dilated last: Never`)
# Returns 64, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Pupils dilated last.9"] <- "Pupils dilated last: Don't know"
sum(DIQ_df$`Pupils dilated last: Don't know`)
# Returns 15, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Pupils dilated last.0"] <- "Pupils dilated last: Missing"
sum(DIQ_df$`Pupils dilated last: Missing`)
# Returns 8729, in agreement with the website


#Remove original column as it is no longer in use
DIQ_df$DIQ360 <- c()
DIQ_df$`Pupils dilated last: Missing` <- c()
```
Column 54: Diabetes affected eyes/had retinopathy (DIQ080)
Code: 1 -> Yes (Count = 194)
Code: 2 -> No (Count = 644)
Code: 7 -> Refused (Count = 0)
Code: 9 -> Don't know (Count = 8)
Code: . -> Missing (Count = 8729)

- Confirm data as categorical based on CDC website for specific column
- Avoid creating columns for Code 7 as there is no count for this column.
- Set NA’s to zero
- Implement for loop that will create a new column for each distinct response
- Rename columns based on question and response of column
- Sum column and confirm counts with CDC website
- Remove original column and "missing data" column as these would not provide useful data if implementing this cleaned data set into a machine learning model.
```{r}
# Diabetes affected eyes/had retinopathy (DIQ080)

# Set NA's to 0
DIQ_df$DIQ080[is.na(x = DIQ_df$DIQ080)] <- 0
# 8729

# Implement for loop

for(unique_value in unique(DIQ_df$DIQ080)){
  DIQ_df[paste("Diabetes affected eyes", unique_value, sep = ".")] <- ifelse(DIQ_df$DIQ080 == unique_value, 1, 0)
}

# Rename columns based on question and response of column and sum column and confirm counts with CDC website
colnames(DIQ_df)[colnames(DIQ_df)=="Diabetes affected eyes.1"] <- "Diabetes affected eyes: Yes"
sum(DIQ_df$`Diabetes affected eyes: Yes`)
# Returns 194, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Diabetes affected eyes.2"] <- "Diabetes affected eyes: No"
sum(DIQ_df$`Diabetes affected eyes: No`)
# Returns 644, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Diabetes affected eyes.9"] <- "Diabetes affected eyes: Don't know"
sum(DIQ_df$`Diabetes affected eyes: Don't know`)
# Returns 8, in agreement with the website

colnames(DIQ_df)[colnames(DIQ_df)=="Diabetes affected eyes.0"] <- "Diabetes affected eyes: Missing"
sum(DIQ_df$`Diabetes affected eyes: Missing`)
# Returns 8729, in agreement with the website

#Remove original column as it is no longer in use
DIQ_df$DIQ080 <- c()
DIQ_df$`Diabetes affected eyes: Missing` <- c()

```
