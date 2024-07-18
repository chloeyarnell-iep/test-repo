#01 Set Up----------------------------------------------------------------------
#Set up your. Rprofile
#Install the iepg package
#Set up a new project in github and Rstudio
#Clone a repository from github into Rstudio
#Commit and push changes you make to a project in rstudio
#Pull changes to see what someone else has done to your project

#02 Getting Data----------------------------------------------------------------
#Reading data in from a .csv or .rds file using the rio import() function
#Reading data in from an .xlsx file, extracting a specific sheet using the rio import() function
#Searching for data in the IEP database
#Getting data from the IEP database
#Getting ACLED data from the IEP database using the iepg_acled() function
#Getting data for a specific country in the ACLED database using the clause argument
#eg iepg_acled(clause = ....)
#Getting data from an API
#Scraping data from a website
#Exporting data as a .csv or .rds file using the export() function from the rio package

#03 Manipulating Data-----------------------------------------------------------
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

#04 Visualizing Data------------------------------------------------------------
#Making base plots using ggplot()
#Changing plot features likes axis titles, font size etc.

#05 Variables, Loops, Functions-------------------------------------------------
#Using variables for standard project features like setting the report year, setting filepaths etc
#Using variables in conjunctions with other functions to create dynamic names, files etc.
#Using a loop to output multiple charts or tables at once
#Writing a function to clean dataset and make a chart, that takes a country and a year as its arguments

#06 IEP Projects----------------------------------------------------------------
#Setting up projects with the standard files: projectControl.R, projectVariables.R, projectFunctions.R, projectExport.R
#Making self-contained scripts
#Using standard IEP functions and variables to: load libraries, make a chart information variable, make a chart plot, export the plot and its associated data in three different sizes etc.
