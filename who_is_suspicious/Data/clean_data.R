library(tidyverse)
library(lubridate)

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
  select(Name, Gender, Department, Citizenship, Age, EmploymentYears, YearsAftDischarge)

glimpse(gastech_employee_new)

write_rds(gastech_employee_new, 'who_is_suspicious/Data/EmployeeRecords_Clean.rds')