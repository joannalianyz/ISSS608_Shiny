library(tidyverse)
library(lubridate)

# Load CC data 

cc_df <-read_csv('who_is_suspicious/Data/Final.csv')

# aggregate CC data

pivot_weekend <- cc_df %>% 
  group_by(Name, Week) %>% 
  summarize(Price = sum(Price)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Week, 
    values_from = Price, 
    values_fill = 0
  ) %>% 
  rename("Total Expenditure During Weekdays" = Weekday, "Total Expenditure During Weekends" = Weekend)

pivot_food_cat <- cc_df %>% 
  group_by(Name, Category) %>% 
  summarize(Price = sum(Price)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Category, 
    values_from = Price, 
    values_fill = 0
  ) %>% 
  rename("Total Expenditure For Food" = Food, 
         "Total Expenditure For Leisure" = Leisure, 
         "Total Expenditure For Retail" = Retail, 
         "Total Expenditure For Company" = Company, 
         "Total Expenditure For Gas" = Gas)


# Cleaning gastech_employee

gastech_employee <- read_csv('who_is_suspicious/Data/EmployeeRecords.csv')

glimpse(gastech_employee)

gastech_employee_new <- gastech_employee %>%
  rename(Citizenship = CitizenshipCountry,
         Department = CurrentEmploymentType,
         Military = MilitaryServiceBranch) %>%
  mutate(Name = paste(FirstName, LastName),
         BirthDate = as.Date(BirthDate,format="%d/%m/%Y"),
         EmploymentDate = as.Date(CurrentEmploymentStartDate, format="%d/%m/%Y"),
         MilitaryDischargeDate = as.Date(MilitaryDischargeDate, format= "%d/%m/%Y")) %>%
  mutate(Age = 2014 - year(BirthDate),
         EmploymentYears = 2014 - year(EmploymentDate),
         YearsAftDischarge = year(EmploymentDate) - year(MilitaryDischargeDate)) %>%
  select(Name, Gender, Department, Citizenship, Age, EmploymentYears, YearsAftDischarge) %>%
  mutate(Age_bin = cut_interval(Age, length = 10, width=10),
         EmploymentYears_bin = cut_interval(EmploymentYears, length = 10, width=10),
         YearsAftDischarge_bin = cut_interval(YearsAftDischarge, length = 5, width=10))

# Merge cc agg data 
gastech_employee_new_cc <- gastech_employee_new %>% 
  left_join(pivot_weekend, by = c("Name" = "Name")) %>% 
  left_join(pivot_food_cat, by = c("Name" = "Name")) %>% 
  replace_na(list(
    `Total Expenditure During Weekdays` = 0,
    `Total Expenditure During Weekends` = 0,
    `Total Expenditure For Food` = 0, 
    `Total Expenditure For Leisure` = 0, 
    `Total Expenditure For Retail` = 0, 
    `Total Expenditure For Company` = 0, 
    `Total Expenditure For Gas` = 0, 
    YearsAftDischarge = 'No Military'
    # YearsAftDischarge_bin = 'No Military'
  )) %>% 
  mutate(`Total Expenditure During Weekdays`= cut_interval(`Total Expenditure During Weekdays`, 4), 
         `Total Expenditure During Weekends`= cut_interval(`Total Expenditure During Weekends`, 4), 
         `Total Expenditure For Food`= cut_interval(`Total Expenditure For Food`, 4), 
         `Total Expenditure For Leisure`= cut_interval(`Total Expenditure For Leisure`, 4), 
         `Total Expenditure For Company`= cut_interval(`Total Expenditure For Retail`, 4), 
         `Total Expenditure For Retail`= cut_interval(`Total Expenditure For Retail`, 4), 
         `Total Expenditure For Gas`= cut_interval(`Total Expenditure For Gas`, 4) 
  )

glimpse(gastech_employee_new_cc)

write_rds(gastech_employee_new_cc, 'who_is_suspicious/Data/EmployeeRecords_Clean.rds')


# Add additional employee data to cc_df 

cc_df_employee <- cc_df %>% 
  left_join(gastech_employee_new %>% select(-Department)) %>% 
  replace_na(list(
    YearsAftDischarge = 'No Military',
    YearsAftDischarge_bin = 'No Military'
  ))
  
write_rds(cc_df_employee, 'who_is_suspicious/Data/final_cc_employee.rds')





  