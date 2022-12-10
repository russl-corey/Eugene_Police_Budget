library(readr)
library(stringr)
library(dplyr)

# set working directory to data folder
setwd('/home/russell/Data')
folder <- '~/Documents/Eugene_Police/data/'

# Load All department data

budget <- c() 

for(year in 2018:2023){
  # Format file
  file <- paste0(folder, 'All_', year, '.csv')
  
  # update user
#  print(paste('processing: ', file))
  
  # read csv file
  data <- read_csv(file)
  
  data$year <-  as.Date(paste0(year,'-01-01'))
  
  budget <- rbind(budget, data)
}

# Create total budget column
budget$total_budget <-  budget$`Supplemental Budget`

# Load police department budget
budget_police <- c() 

for(year in 2018:2022){
  # Format file
  file <- paste0(folder, 'Department_', year, '.csv')
  
  # update user
#  print(paste('processing: ', file))
  
  # read csv file
  data <- read_csv(file)
  
  data$year <- as.Date(paste0(year,'-01-01'))
  
  budget_police <- rbind(budget_police, data)
}
rm(data)
# Calculate percentage change 

# Create list of departments in data 
departments <- unique(budget$Department)

# Create id column by adding Department to Year
budget$id <- paste(budget$year, budget$Department, sep='_')

# create empty array to hold merged results
tmp_budget <- NA

for(dept in departments){
  #print(dept)
  
  # Make a table of years and adopted budget for dept
  stuff <- budget[budget$Department == dept,] %>%
    arrange(year) %>%
    dplyr::select('id', 'Adopted Budget')
  
  # extract vector of adopted budget
  amounts <-  stuff %>% dplyr::select('Adopted Budget')
  
  # Create a row of budgets shifted up and fill first row with 0
  shifted <- cbind(head(stuff$`Adopted Budget`, -1))
  shifted <- rbind(0, shifted)
  
  # Create difference column and set first row 0 again
  budget_increase <- amounts - shifted
  budget_increase[1,] <- 0
  
  # Add calculated column to department df and rename cols
  stuff <- cbind(stuff, budget_increase)
  colnames(stuff) = c('id', 'Adopted Budget', 'budget_increase')
  
  # Merge on id
  tmp = merge(x=budget, y=stuff, by=c('id'), all=FALSE)
  
  # Rename columns after merge
  tmp <- tmp %>%
    dplyr::select(c('id', 'Adopted Budget.x', 'budget_increase'))
  colnames(tmp) = c('id', 'Adopted Budget', 'budget_increase')

  # collect results
  tmp_budget <- rbind(tmp, tmp_budget)
  
  # Clean temp vars
  rm(budget_increase)
  rm(stuff)
  rm(shifted)
  rm(tmp)
}


# Add results to budget
budget <- merge(budget, tmp_budget, by='id', all=TRUE) %>%
  rename('Adopted Budget' = 'Adopted Budget.x') %>%
  dplyr::select(-c('Adopted Budget.y'))
rm(tmp_budget)

# Calculate increase as percentage
budget$budget_increase_percnt <- round(100 * budget$budget_increase /  budget$`Adopted Budget`,
                                       digits = 2)





operating_budget_2022_12_01 <- read_csv("~/Documents/Eugene_Police/data/operating_budget-2022-12-01.csv")
#View(operating_budget_2022_12_01)


budget_police %>%
  dplyr::select('Service', "Proposed Budget", 'Adopted Budget', 'Supplemental Budget', 'year' ) %>%
  filter(year == '2021-01-01') %>%
  View()













