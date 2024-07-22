library(rio)
library(tidyverse)
library(rvest)
library(iepg)

#01 Set Up----------------------------------------------------------------------    done (Rprofile??)
#Set up your. Rprofile
# f_editProfile: Function for editing the .Rprofile file
f_editProfile <- function() {
  file.edit("~/.Rprofile")
}
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

##### ----- IEP CUSTOM FUNCTIONS
#' Eventually these functions will be incorporated into a stand-alone IEP R package
#' For the moment, using an include() function in an .Rprofile is enough for the package to load automatically
##### -----

### --- Set up folder structure in a new project
IEP_ProjectFolders <- function(base_path = getwd()) {
  # Define the directory structure
  folders <- list(
    "01_documentation" = NULL,
    "02_data" = c("raw", "processed"),
    "03_scripts" = c("01_cleaning","02_standardOutputs","03_analysis","04_outputs"),
    "04_outputs" = c("charts", "maps", "tables")
  )
  
  # Function to create the directory structure
  create_folders <- function(base_path, folders) {
    for (main_folder in names(folders)) {
      main_folder_path <- file.path(base_path, main_folder)
      if (!file.exists(main_folder_path)) {
        dir.create(main_folder_path)
      }
      file.create(file.path(main_folder_path, ".gitkeep"))  # Create .gitkeep in main folder
      
      if (main_folder == "03_scripts") {
        file.create(file.path(main_folder_path, "projectControl.R"))  # Create ProjectControl.R in 03_scripts folder
        file.create(file.path(main_folder_path, "functions.R"))  # Create ProjectControl.R in 03_scripts folder
      }
      
      if (!is.null(folders[[main_folder]])) {
        for (sub_folder in folders[[main_folder]]) {
          sub_folder_path <- file.path(main_folder_path, sub_folder)
          if (!file.exists(sub_folder_path)) {
            dir.create(sub_folder_path)
          }
          file.create(file.path(sub_folder_path, ".gitkeep"))  # Create .gitkeep in sub folder
        }
      }
    }
  }
  
  # Call the function to create the directory structure
  create_folders(base_path, folders)
  
  cat("Folder structure created or updated successfully!\n")
}

### --- Open the new ideas excel file

IEP_Ideas <- function() {
  if (!exists("RESEARCH_PIPELINE")) {
    stop("Variable 'RESEARCH_PIPELINE' does not exist.")
  }
  
  if (Sys.info()["sysname"] == "Windows") {
    shell.exec(RESEARCH_PIPELINE)
  } else if (Sys.info()["sysname"] == "Darwin") { # For macOS
    system(paste("open", shQuote(RESEARCH_PIPELINE)))
  } else {
    stop("This function is designed for Windows and macOS only.")
  }
}
