library(rio)
library(tidyverse)
library(rvest)
library(iepg)

#01 Set Up----------------------------------------------------------------------    done
#Set up your. Rprofile
 #f_editProfile: Function for editing the .Rprofile file
#Install the iepg package
#Set up a new project in github and Rstudio
#Clone a repository from github into Rstudio
#Commit and push changes you make to a project in rstudio
#Pull changes to see what someone else has done to your project

#02 Getting Data----------------------------------------------------------------    need to learn
#Reading data in from a .csv or .rds file using the rio import() function
csv_students <- import("C:/Users/ChloeYarnell/Downloads/students.csv")
str(csv_students)

#Reading data in from an .xlsx file, extracting a specific sheet using the rio import() function
xlsx_students <- import("C:/Users/ChloeYarnell/Downloads/students excel.xlsx", sheet = 2)
##xlsx_students2 <- read_excel("C:/Users/ChloeYarnell/Downloads/students excel.xlsx", sheet = 2)
str(xlsx_students)

#Searching for data in the IEP database
#Getting data from the IEP database
#Getting ACLED data from the IEP database using the iepg_acled() function
#Getting data for a specific country in the ACLED database using the clause argument
#eg iepg_acled(clause = ....)
#Getting data from an API
#Scraping data from a website
#Exporting data as a .csv or .rds file using the export() function from the rio package
csv_interns <- data.frame(
  ID = c(1, 2, 3),
  Name = c("chloe", "parker", "gianna")
)
export(csv_interns, "C:/Users/ChloeYarnell/Downloads/interns.csv")

#03 Manipulating Data-----------------------------------------------------------    fine with
#Assigning a dataframe to a name using the <- or = operators
#Using the pipe operator (%>%) to chain lists of commands together
#Creating a new variable using the *mutate() *function
#Selecting only certain columns of a dataframe using select()
#Renaming variables using the rename() function
#selecting only certain rows or groups using the filter() function
#joining one dataframe to another one using left_join()
#using conditional operators in combination with mutate() to create new variables based on certain conditions
#eg. if_else() or case_when()
#summarize datasets by using group_by() in conjunction with summarise()
#Remove duplicated rows using distinct()
#Making longer strings of texts using paste0()

#04 Visualizing Data------------------------------------------------------------    fine with
#Making base plots using ggplot()
#Changing plot features likes axis titles, font size etc.

#05 Variables, Loops, Functions-------------------------------------------------    cleaning data def?
#Using variables for standard project features like setting the report year, setting filepaths etc
#Using variables in conjunctions with other functions to create dynamic names, files etc.
#Using a loop to output multiple charts or tables at once
#Writing a function to clean dataset and make a chart, that takes a country and a year as its arguments

#06 IEP Projects----------------------------------------------------------------    need to learn
#Setting up projects with the standard files: 
#projectControl.R, projectVariables.R, projectFunctions.R, projectExport.R
#Making self-contained scripts
#Using standard IEP functions and variables to: load libraries, 
#make a chart information variable, make a chart plot, export the plot and 
#its associated data in three different sizes etc.
