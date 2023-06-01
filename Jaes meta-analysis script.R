---
#title: "JCDD Meta-Analysis Script"
#output: html_document
---

##loads “readxl” package (included with R Studio)
library(readxl)

##Lines 10-17 loades the Excel file “Sample_data.xlsx” from GitHub
# URL of the raw Excel file on GitHub
url <- "https://github.com/mread1865/JCDDMeta/raw/main/Sample_data.xlsx"
# Temporary file to download the Excel sheet to
temp <- tempfile()
# Download the file
download.file(url, temp, mode = "wb")
# Read the Excel file into a data frame
sample <- read_excel(temp) 
# You can remove the temporary file after reading the data
unlink(temp)

##If using personal data, can use the code below with the file location added
#sample <- read_excel("Sample_data.xlsx")

##view the “sample” dataset
View(sample)
install.packages("meta")
install.packages("data.table")
library(meta)  
library(data.table)

##extracts variables “Author”, “Subgroup”, “Survival_yr_mean” and 
#“Survival_year_SE” from the dataset “sample” and compiles them into a new dataset “data1’. 
#These variables will be categorized to V1, V2, V3 and V4 respectively. 
data1<-data.table(sample$Author, sample$Subgroup, sample$Survival_yr_mean, 
                  sample$Survival_year_SE) 

#Omits blank values in studies (as not all studies will report certain values), 
data1<-na.omit(data1) 
data1$V3 <- as.numeric(as.character(data1$V3))
#Changes the values that have been erroneously input as characters to 
#numeric values (in this case, the mean and SE) that can be analyzed 
#by the statistical software.
data1$V4 <- as.numeric(as.character(data1$V4)) 
#displays the dataset “data1”
View(data1) 


#creates a meta-analysis data by defining the treatment effect 
#(TE, which is mean survival in this case) and standard error (SE) of TE. 
#The command “studlab” defines a study label which will be displayed for 
#each row representing an individual article, which is an 
#author name in this case. The analysis results created by “metagen” is 
#then defined as an object “meta1”. 
meta1 <- metagen(TE = V3, seTE = V4, data = data1, byvar = NULL, studlab = V1) 

#If using subgroup analysis, run the code below instead
meta1 <- metagen(TE = V3, seTE = V4, data = data1, byvar = V2, studlab = V1)

#Display the generated results table
summary(meta1)

#Generate forest plot
forest(meta1)

#Binary variables using Diabetes (DM) as an example
data2<-data.table(sample$Author, sample$Subgroup, sample$Patient_number, 
                  sample$DM)
data2<-na.omit(data2)
data2$V3 <- as.numeric(as.character(data2$V3))
data2$V4 <- as.numeric(as.character(data2$V4))

#Creates meta-analysis using binary variable
#Defines the numerator “event” and denominator “n” as according variables.
meta2 <- metaprop(event = V4, n = V3, data = data2, byvar = V2, studlab = V1) 
#Showing the results of the metaprop function
summary(meta2) 
#showing a forest plot of the meta1 function
forest(meta2) 

#Create Funnel plot to assess bias

#Funnel Plot for CONTINUOUS variable
meta3 <- metagen(TE = V3, seTE = V4, data = data1, byvar = NULL, studlab = V1, 
                 comb.fixed=FALSE)
funnel(meta3, xlab="Mean survival, years")
       
#Assess bias using Egger's linear regresion test
metabias(meta3)

#Funnel Plot for BINARY variable
meta4 <- metaprop(event = V4, n = V3, data = data2, byvar = NULL, studlab = V1, 
                  comb.fixed=FALSE, sm="PRAW")
funnel(meta4, xlab="Mean survival, years")
#Assess bias using Egger's linear regression test. 
#This will provide an error as you need at least 10 study's to utilize. 
metabias(meta4)
