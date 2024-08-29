

names(assignment) = c("ID", "Customer ID", "Month", "Name", "Age", "SSN", "Occupation",
                      "Annual Income", "Monthly Income", "Bank Account", "Credit Card", 
                      "Interest Rate", "Loan Number", "Loan", "Delay from due date", 
                      "Delayed Payment", "Credit Limit Changed", "Number Credit Inqueries", 
                      "Credit Mix", "Outstanding Debts", "Credit Utilization Ratio", 
                      "Credit History", "Payment of Min Amount", "EMI per month", 
                      "Monthly Investment", "Payment Behaviour", "Monthly Balance", 
                      "Customer Credit Score")

# Set working Directory 
setwd("C:\\Users\\User\\Desktop\\Apu y2 sem1\\P for data analysis")

assignment<- read.csv("C:\\Users\\User\\Desktop\\Apu y2 sem1\\P for data analysis\\assignment.data.csv")


library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)

assignment <- read.csv("C:\\Users\\User\\Desktop\\Apu y2 sem1\\P for data analysis\\assignment.data.csv",
                       na.strings = c("", "", "", "","",
                                      "#F%$D@*&8","_______","","","",
                                      "","","","","",
                                      "","","_","","_",
                                      "","","NA","NM","",
                                      "!@9#%8","",""))





#Convert data type to the correct one 

#Change Annual Income from Character to Numeric 
assignment$Annual_Income=as.numeric(assignment$Annual_Income)

#Change Num_of_Delayed_Payment from character to integer 
assignment$Num_of_Delayed_Payment=as.integer(assignment$Num_of_Delayed_Payment)

#Change Outstanding_Debt from character to numeric 
assignment$Outstanding_Debt=as.numeric(assignment$Outstanding_Debt)

#Change Amount_invested_monthly from character to numeric 
assignment$Amount_invested_monthly=as.numeric(assignment$Amount_invested_monthly)





#Cleaning Age

assignment$Age <- gsub("_", "",assignment$Age)

# This code is use to remove Bad value for age collum but it 
#might be wrong as there are some weird value such as 28_  also got remove to become NA
assignment$Age[!(assignment$Age >= 0)] <- NA
assignment$Age[as.numeric(assignment$Age) > 123] <- NA
assignment$Age=as.integer(assignment$Age)

#Cleaning Num Of Loan
#Num of loan have negative

assignment$Num_of_Loan <- gsub("_", "",assignment$Num_of_Loan)
assignment$Num_of_Loan=as.integer(assignment$Num_of_Loan)

assignment$Num_of_Loan <- assignment$Num_of_Loan
assignment$Num_of_Loan[!(assignment$Num_of_Loan >= 0)] <- NA





# Cleaning Num_of_Delayed_Payment
assignment$Num_of_Delayed_Payment <- gsub("_", "",assignment$Num_of_Delayed_Payment)
assignment$Num_of_Delayed_Payment=as.integer(assignment$Num_of_Delayed_Payment)
assignment$Num_of_Delayed_Payment[!(assignment$Num_of_Delayed_Payment >= 0)] <- NA


assignment$Delay_from_due_date[!(assignment$Delay_from_due_date >= 0)] <- NA



assignment$Num_Bank_Accounts[!(assignment$Num_Bank_Accounts >= 0)] <- NA


#Cleaning Occupation
#Fill in occupation 
assignment <- assignment %>%
  group_by(Customer_ID) %>%
  fill(Occupation, .direction = "downup")

#Cleaning Monthly_Inhand_Salary
assignment <- assignment %>%
  group_by(Customer_ID) %>%
  fill(Monthly_Inhand_Salary, .direction = "downup")



#Cleaning Amount_invested_monthly

assignment$Amount_invested_monthly[assignment$Amount_invested_monthly == "__10000__"] <- NA






#------------------------------------------------------------------------------------------------------------------------------------------#
# Data Preprocessing


#Montly in hand salary should be two decimal
assignment$Monthly_Inhand_Salary <- round(assignment$Monthly_Inhand_Salary,2)



# Changing Credit_History_Age into numeric month form
credit_history_year <- str_extract(assignment$Credit_History_Age, pattern = "[0-9]+")
credit_history_month <- str_extract(assignment$Credit_History_Age, pattern =  "(?<=and )\\d+")

credit_history_year <- as.numeric(credit_history_year)
credit_history_month <- as.numeric(credit_history_month)

total_month <- credit_history_year * 12 + credit_history_month
assignment$Credit_History_Age <- total_month


#Converting Payment Behavior into suitable format


library(tidyr)
assignment <- separate(assignment, Payment_Behaviour, 
                       into = c("Payment_Behaviour", "spent_level", "payment_type", "payment_detail"), sep = "_")

# Combine Payment_Behaviour and spent_level columns
assignment$Payment_Behaviour <- ifelse(is.na(assignment$Payment_Behaviour) | is.na(assignment$spent_level), 
                                       NA, 
                                       paste(assignment$Payment_Behaviour, "_", assignment$spent_level, sep = ""))

# Combine payment_type and payment_detail columns
assignment$spent_level <- ifelse(is.na(assignment$payment_type) | is.na(assignment$payment_detail), 
                                 NA, 
                                 paste(assignment$payment_type, "_", assignment$payment_detail, sep = ""))

# Remove unnecessary columns
assignment <- subset(assignment, select = -c(payment_type, payment_detail))



#Cleaning Payment_Behaviour
#Fill in  Payment_Behaviour
assignment <- assignment %>%
  group_by(Customer_ID) %>%
  fill(Payment_Behaviour, .direction = "downup")




# Remove outlier code
Q1 <- quantile(assignment$Interest_Rate, 0.25, na.rm = TRUE)
Q3 <- quantile(assignment$Interest_Rate, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
# Remove outliers
assignment$Interest_Rate[assignment$Interest_Rate < lower_bound] <- Q1
assignment$Interest_Rate[assignment$Interest_Rate > upper_bound] <- Q3


# Remove outlier code
summary_result <- summary(assignment$Age)
first_quartile <- summary_result[2]
third_quartile <- summary_result[5]
Q1 <- first_quartile[[1]]
Q3 <- third_quartile[[1]]
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
assignment$Age[assignment$Age < lower_bound] <- Q1
assignment$Age[assignment$Age > upper_bound] <- Q3




# Remove outlier code
summary_result <- summary(assignment$Num_Bank_Accounts)
first_quartile <- summary_result[2]
third_quartile <- summary_result[5]
Q1 <- first_quartile[[1]]
Q3 <- third_quartile[[1]]
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
assignment$Num_Bank_Accounts[assignment$Num_Bank_Accounts < lower_bound] <- Q1
assignment$Num_Bank_Accounts[assignment$Num_Bank_Accounts > upper_bound] <- Q3



# Remove outlier code  
Q1 <- quantile(assignment$Monthly_Inhand_Salary, 0.25, na.rm = TRUE)
Q3 <- quantile(assignment$Monthly_Inhand_Salary, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
assignment$Monthly_Inhand_Salary[assignment$Monthly_Inhand_Salary < lower_bound] <- Q1
assignment$Monthly_Inhand_Salary[assignment$Monthly_Inhand_Salary > upper_bound] <- Q3


# Remove outlier code  
Q1 <- quantile(assignment$Delay_from_due_date, 0.25, na.rm = TRUE)
Q3 <- quantile(assignment$Delay_from_due_date, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
assignment$Delay_from_due_date[assignment$Delay_from_due_date < lower_bound] <- Q1
assignment$Delay_from_due_date[assignment$Delay_from_due_date > upper_bound] <- Q3




# Remove na by replacing with mean
columns_to_process <- c("Age", "Num_Bank_Accounts",
                        "Delay_from_due_date", "Monthly_Inhand_Salary")
for (col in columns_to_process) {
  mean_value <- mean(assignment[[col]], na.rm = TRUE)
  assignment[[col]][is.na(assignment[[col]])] <- mean_value
}
assignment$Age <- round(assignment$Age)
assignment$Delay_from_due_date <- round(assignment$Delay_from_due_date)
assignment$Monthly_Inhand_Salary <- round(assignment$Monthly_Inhand_Salary,2)
assignment$Num_Bank_Accounts <- round(assignment$Num_Bank_Accounts)


#------------------------------------------------------------------------------------------------------------------------------------------#
#Run the code to here for complete file



#head of the data set
head(assignment,10)


tail(assignment,10)


# Dimension fo the data set
dim(assignment)


#unique
selected_data <- assignment[c("Age", "Num_Bank_Accounts", "Delay_from_due_date", "Monthly_Inhand_Salary")]
selected_data <- na.omit(selected_data)
names(selected_data)=c("Age", "Num_Bank_Accounts", "Delay_from_due_date", "Monthly_Inhand_Salary")
selected_data <- unique(selected_data)
print(selected_data)




# Calculate standard deviations
sd_age <- sd(assignment$Age)
sd_bank_accounts <- sd(assignment$Num_Bank_Accounts)
sd_delay <- sd(assignment$Delay_from_due_date)
sd_salary <- sd(assignment$Monthly_Inhand_Salary)

# Round standard deviations
rounded_sd_age <- round(sd_age, 2)
rounded_sd_bank_accounts <- round(sd_bank_accounts, 2)
rounded_sd_delay <- round(sd_delay, 2)
rounded_sd_salary <- round(sd_salary, 2)

# Print rounded standard deviations
print(paste("Standard deviation for Age:", rounded_sd_age))
print(paste("Standard deviation for Num_Bank_Accounts:", rounded_sd_bank_accounts))
print(paste("Standard deviation for Delay_from_due_date:", rounded_sd_delay))
print(paste("Standard deviation for Monthly_Inhand_Salary:", rounded_sd_salary))

str(assignment)

#To check whether Missing value still exist after cleaning
table(is.na(assignment$Monthly_Inhand_Salary))



#
summarys <- summary(assignment$Age)







#------------------------------------------------------------------------------------------------------------------------------------------#

#Visualization

hist(assignment$Monthly_Inhand_Salary)

assignment$Age <- as.numeric(assignment$Age)
ggplot(assignment, aes(x=Age)) + geom_histogram()



boxplot(assignment$Monthly_Inhand_Salary)


#Scatterplot
#plot()

#------------------------------------------------------------------------------------------------------------------------------------------#

#Hypothesis 
# Filter the dataframe that fullfill the requiement Delay from duedate <>5 and Payment behavior is low spent
assignment_duedate <- assignment %>%
  filter(Delay_from_due_date > 5, Payment_Behaviour == "Low_spent")

assignment_duedate1 <- assignment %>%
  filter(Delay_from_due_date < 5, Payment_Behaviour == "Low_spent")

# Count the number of people meeting both criteria
num_people <- nrow(assignment_duedate)
num_people1 <- nrow(assignment_duedate1)

# Further filter the data for people with age less than or more than 22
assignment_duedate_age <- assignment_duedate %>%
  filter(Age > 22)

assignment_duedate_age1 <- assignment_duedate %>%
  filter(Age < 22)

# Count the number of people meeting all criteria
num_people_age <- nrow(assignment_duedate_age)
num_people_age1 <- nrow(assignment_duedate_age1)

# Filter the data with Num of bank accounts that are less than and more than four
assignment_duedate_age_account <- assignment_duedate_age %>%
  filter(Num_Bank_Accounts < 4)

assignment_duedate_age_account1 <- assignment_duedate_age %>%
  filter(Num_Bank_Accounts > 4)

# Count the number of people meeting all criteria
num_people_age_22_account <- nrow(assignment_duedate_age_account)
num_people_age_22_account1 <- nrow(assignment_duedate_age_account1)

# Filter the data with monthly in-hand salary
assignment_duedate_age_account_salary <- assignment_duedate_age_account1 %>%
  filter(Monthly_Inhand_Salary < 3000)

assignment_duedate_age_account_salary1 <- assignment_duedate_age_account1 %>%
  filter(Monthly_Inhand_Salary > 3000)

num_people_age_22_account_salary <- nrow(assignment_duedate_age_account_salary)
num_people_age_22_account_salary1 <- nrow(assignment_duedate_age_account_salary1)


# Combine the result
Hypothesis4 <- data.frame(
  criteria = c("Delay > 5 & Low spent", "Delay < 5 & Low spent", "Age > 22", "Age < 22",
               "Num Bank Accounts < 4", "Num Bank Accounts > 4", "Monthly Salary < 3000", "Monthly Salary > 3000"),
  count = c(num_people, num_people1, num_people_age, num_people_age1, num_people_age_22_account,
            num_people_age_22_account1, num_people_age_22_account_salary ,num_people_age_22_account_salary1)
)

color = c("red", "blue", "red", "blue", "red", "blue", "red", "blue")
Hypothesis4_graph <- data.frame(Hypothesis4,color)
Hypothesis4_graph$criteria <- factor(Hypothesis4_graph$criteria, levels = Hypothesis4_graph$criteria)

#print out the result
print(Hypothesis4)


# Result for No of People with low spent and Due Date <> 5
num_people_graph1 <- data.frame(
  Criteria = c("Delay > 5 days", "Delay < 5 days"),
  Count = c(num_people, num_people1),
  Color = c("red", "blue")
)
num_people_graph1$Criteria <- factor(num_people_graph1$Criteria, levels = num_people_graph1$Criteria)

ggplot(num_people_graph1, aes(x = Criteria, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity()+
  labs(title = "No of People with low spent and Due Date <> 5",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")


# Result for No of People with Age <> 22
hypothesis_graph <- data.frame(
  Criteria = c("Age > 22", "Age < 22"),
  Count = c(num_people_age, num_people_age1),
  Color = c("red", "blue")
)
hypothesis_graph$Criteria <- factor(hypothesis_graph$Criteria, levels = hypothesis_graph$Criteria)

ggplot(hypothesis_graph, aes(x = Criteria, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity()+
  labs(title = "No of People with Age <> 22",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")


# Result for No of People with account <> 4
hypothesis_graph <- data.frame(
  Criteria = c("Account > 4", "Account < 4"),
  Count = c(num_people_age_22_account, num_people_age_22_account1),
  Color = c("red", "blue")
)
hypothesis_graph$Criteria <- factor(hypothesis_graph$Criteria, levels = hypothesis_graph$Criteria)

ggplot(hypothesis_graph, aes(x = Criteria, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity()+
  labs(title = "No of poeple with salary <> 4",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")



# Result for No of People with salary <> 3000
hypothesis_graph <- data.frame(
  Criteria = c("Salary < 3000", "Salary > 3000"),
  Count = c(num_people_age_22_account_salary, num_people_age_22_account_salary1),
  Color = c("red", "blue")
)
hypothesis_graph$Criteria <- factor(hypothesis_graph$Criteria, levels = hypothesis_graph$Criteria)

ggplot(hypothesis_graph, aes(x = Criteria, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity()+
  labs(title = "No of people with <> 3000 salary",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")



# Combine all graph for better view

ggplot(Hypothesis4_graph, aes(x = Criteria, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Count the number of rows where Age is greater than 22 and low spent
age_greater_22 = sum(assignment$Age > 22, assignment$Payment_Behaviour == "Low_spent")

# Count the number of rows where Age is less than or equal to 22 and low spent
age_lesser_22 = sum(assignment$Age <= 22, assignment$Payment_Behaviour == "Low_spent")

# Create a vector of counts
a = c(age_greater_22, age_lesser_22)

# Create labels for the pie chart
l = c("Age > 22 & Low spent", "Age <= 22 & Low spent")

# Create the pie chart
pie(a, labels = l, radius = 1, main = "Number of low spent people by Age Group", col = c("green", "blue"), clockwise = TRUE)


# Count the number of rows where Delay from duedate >5 and low spent
duedate_greater_5 = sum(assignment$Delay_from_due_date > 5, assignment$Payment_Behaviour == "Low_spent")

# Count the number of rows where Delay from duedate less than or equal to 5 and low spent
duedate_lesser_5 = sum(assignment$Delay_from_due_date <= 5, assignment$Payment_Behaviour == "Low_spent")

# Create a vector of counts
a = c(duedate_greater_5, duedate_lesser_5)

# Create labels for the pie chart
l = c("Delay > 5 & Low spent", "Delay <= 5 & Low spent")

# Create the pie chart
pie(a, labels = l, radius = 1, main = "Number of low spent people by Delay_from_due_date", col = c("green", "blue"), clockwise = TRUE)


# Count the number of rows where  Num of bank accounts less than 4 and low spent
accounts_lesser_4 = sum(assignment$Delay_from_due_date < 4, assignment$Payment_Behaviour == "Low_spent")

# Count the number of rows where  Num of bank accounts more than or equal to 4 and low spent
accounts_greater_4 = sum(assignment$Delay_from_due_date >= 4, assignment$Payment_Behaviour == "Low_spent")

# Create a vector of counts
a = c( accounts_lesser_4, accounts_greater_4)

# Create labels for the pie chart
l = c("Num Bank Accounts < 4 & Low spent", "Num Bank Accounts >= 4 & Low spent")

# Create the pie chart
pie(a, labels = l, radius = 1, main = "Number of low spent people by Delay_from_due_date", col = c("green", "blue"), clockwise = TRUE)



# Count the number of rows where  Num of bank accounts less than 4 and low spent
salary_lesser_3000 = sum(assignment$Monthly_Inhand_Salary < 3000, assignment$Payment_Behaviour == "Low_spent")

# Count the number of rows where  Num of bank accounts more than or equal to 4 and low spent
salary_greater_3000 = sum(assignment$Monthly_Inhand_Salary >= 3000, assignment$Payment_Behaviour == "Low_spent")

# Create a vector of counts
a = c( salary_lesser_3000, salary_greater_3000)

# Create labels for the pie chart
l = c("Monthly Salary < 3000 & Low spent", "Monthly Salary >= 3000 & Low spent")

# Create the pie chart
pie(a, labels = l, radius = 1, main = "Number of low spent people by Monthly Salary", col = c("green", "blue"), clockwise = TRUE)






#==================================================================================================
#===================================================================================================
#Anova
anova_model <- aov(Age ~ Payment_Behaviour, data = assignment)
summary(anova_model)



# Further filter the data for people with age less than or more than 22
assignment_age <- assignment %>%
  filter(Age > 22, Payment_Behaviour == "Low_spent")

assignment_age1 <- assignment %>%
  filter(Age <= 22, Payment_Behaviour == "Low_spent")

# Count the number of people meeting all criteria
num_people <- nrow(assignment_age)
num_people1 <- nrow(assignment_age1)


# Filter the dataframe that fullfill the requiement Delay from duedate <>5 and Payment behavior is low spent
assignment_age_duedate <- assignment_age %>%
  filter(Delay_from_due_date > 5)

assignment_age_duedate1 <- assignment_age %>%
  filter(Delay_from_due_date <= 5)

# Count the number of people meeting both criteria
num_people_duedate <- nrow(assignment_age_duedate)
num_people_duedate1 <- nrow(assignment_age_duedate1)


# Filter the data with Num of bank accounts that are less than and more than four
assignment_age_duedate_account <- assignment_age_duedate %>%
  filter(Num_Bank_Accounts < 4)

assignment_age_duedate_account1 <- assignment_age_duedate %>%
  filter(Num_Bank_Accounts >= 4)

# Count the number of people meeting all criteria
num_people_duedate_account <- nrow(assignment_age_duedate_account)
num_people_duedate_account1 <- nrow(assignment_age_duedate_account1)

# Filter the data with monthly in-hand salary
assignment_age_duedate_account_salary <- assignment_age_duedate_account1 %>%
  filter(Monthly_Inhand_Salary < 3000)

assignment_age_duedate_account_salary1 <- assignment_age_duedate_account %>%
  filter(Monthly_Inhand_Salary >= 3000)

num_people_duedate_account_salary <- nrow(assignment_age_duedate_account_salary)
num_people_duedate_account_salary1 <- nrow(assignment_age_duedate_account_salary1)


# Combine the result
Hypothesis2 <- data.frame(
  criteria = c("Age > 22 & Low spent", "Age <= 22 & Low spent", "Delay > 5& Low spent", "Delay <= 5 & Low spent", 
               "Num Bank Accounts < 4 & Low spent", "Num Bank Accounts >= 4 & Low spent", "Monthly Salary < 3000 & Low spent", "Monthly Salary >= 3000 & Low spent"),
  count = c(num_people, num_people1, num_people_duedate, num_people_duedate1, num_people_duedate_account,
            num_people_duedate_account1, num_people_duedate_account_salary ,num_people_duedate_account_salary1)
)

color = c("red", "blue", "red", "blue", "red", "blue", "red", "blue")
Hypothesis2_graph <- data.frame(Hypothesis2,color)
Hypothesis2_graph$criteria <- factor(Hypothesis2_graph$criteria, levels = Hypothesis2_graph$criteria)


#print out the result
print(Hypothesis2)

# Result for No of People with with low spent and Age <> 22
num_people_graph1 <- data.frame(
  Criteria = c("Age > 22", "Age <= 22"),
  Count = c(num_people, num_people1),
  Color = c("red", "cyan")
)
num_people_graph1$Criteria <- factor(num_people_graph1$Criteria, levels = num_people_graph1$Criteria)

ggplot(num_people_graph1, aes(x = Criteria, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity()+
  labs(title = "No of People with low spent and Age <> 22",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

#3D pie
pie3D(num_people_graph1$Count, labels = num_people_graph1$Count, explode = 0.1,
      main = "No of People with low spent and Age <> 22", col = num_people_graph1$Color, labelcex = 1.2, radius = 1.2, labelcol = "black")

# Allow elements to be drawn outside the plot region
par(xpd = TRUE)

legend(x = 1.2, y = 1.2,  # Replace with desired x and y coordinates
       legend = paste(num_people_graph1$Criteria, num_people_graph1$Count),
       fill = num_people_graph1$Color, title = "Criteria", cex = 0.8, bty = "n")

# Reset xpd to avoid unexpected behavior in future plots
par(xpd = FALSE)


# Result for No of People with Due Date <> 5
hypothesis_graph <- data.frame(
  Criteria = c("Delay > 5 days", "Delay <= 5 days"),
  Count = c(num_people_duedate, num_people_duedate1),
  Color = c("red", "cyan")
)
hypothesis_graph$Criteria <- factor(hypothesis_graph$Criteria, levels = hypothesis_graph$Criteria)

ggplot(hypothesis_graph, aes(x = Criteria, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity()+
  labs(title = "No of People with low spent and Due Date <> 5",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")


#3D pie
pie3D(hypothesis_graph$Count, labels = hypothesis_graph$Count, explode = 0.1,
      main = "No of People with low spent and Due Date <> 5", col = hypothesis_graph$Color, labelcex = 1.2, radius = 1.2, labelcol = "black")

# Allow elements to be drawn outside the plot region
par(xpd = TRUE)

legend(x = 1.2, y = 1.2,  # Replace with desired x and y coordinates
       legend = paste(hypothesis_graph$Criteria, hypothesis_graph$Count),
       fill = hypothesis_graph$Color, title = "Criteria", cex = 0.8, bty = "n")

# Reset xpd to avoid unexpected behavior in future plots
par(xpd = FALSE)



# Result for No of People with account <> 4
hypothesis_graph <- data.frame(
  Criteria = c("Account > 4", "Account <= 4"),
  Count = c(num_people_duedate_account, num_people_duedate_account1),
  Color = c("red", "cyan")
)
hypothesis_graph$Criteria <- factor(hypothesis_graph$Criteria, levels = hypothesis_graph$Criteria)

ggplot(hypothesis_graph, aes(x = Criteria, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity()+
  labs(title = "No of poeple with low spent and number of account <> 4",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

#3D pie
pie3D(hypothesis_graph$Count, labels = hypothesis_graph$Count, explode = 0.1,
      main = "No of poeple with low spent and number of account <> 4", col = hypothesis_graph$Color, labelcex = 1.2, radius = 1.2, labelcol = "black")

# Allow elements to be drawn outside the plot region
par(xpd = TRUE)

legend(x = 1.2, y = 1.2,  # Replace with desired x and y coordinates
       legend = paste(hypothesis_graph$Criteria, hypothesis_graph$Count),
       fill = hypothesis_graph$Color, title = "Criteria", cex = 0.8, bty = "n")

# Reset xpd to avoid unexpected behavior in future plots
par(xpd = FALSE)



# Result for No of People with salary <> 3000
hypothesis_graph <- data.frame(
  Criteria = c("Salary < 3000", "Salary >= 3000"),
  Count = c(num_people_duedate_account_salary, num_people_duedate_account_salary1),
  Color = c("red", "cyan")
)
hypothesis_graph$Criteria <- factor(hypothesis_graph$Criteria, levels = hypothesis_graph$Criteria)

ggplot(hypothesis_graph, aes(x = Criteria, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity()+
  labs(title = "No of people with low spent and <> 3000 salary",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")


#3D pie
pie3D(hypothesis_graph$Count, labels = hypothesis_graph$Count, explode = 0.1,
      main = "No of people with low spent and <> 3000 salary", col = hypothesis_graph$Color, labelcex = 1.2, radius = 1.2, labelcol = "black")

# Allow elements to be drawn outside the plot region
par(xpd = TRUE)

legend(x = 1.2, y = 1.2,  # Replace with desired x and y coordinates
       legend = paste(hypothesis_graph$Criteria, hypothesis_graph$Count),
       fill = hypothesis_graph$Color, title = "Criteria", cex = 0.8, bty = "n")

# Reset xpd to avoid unexpected behavior in future plots
par(xpd = FALSE)

# Combine all graph for better view
ggplot(Hypothesis2_graph, aes(x = criteria, y = count, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  geom_text(aes(label = count), vjust = -0.5) +  # Add values on top of the bars
  labs(title = "Summary of Hypotheses",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#FUTHER ANALYSIS***********************************************
#filter the data for people with age less than or more than 22
assignment_age <- assignment %>%
  filter(Age > 20, Payment_Behaviour == "Low_spent")

assignment_age1 <- assignment %>%
  filter(Age <= 20, Payment_Behaviour == "Low_spent")

# Count the number of people meeting all criteria
num_people <- nrow(assignment_age)
num_people1 <- nrow(assignment_age1)


# Filter the dataframe that fullfill the requiement Delay from duedate <>5 and Payment behavior is low spent
assignment_age_duedate <- assignment_age %>%
  filter(Delay_from_due_date > 5)

assignment_age_duedate1 <- assignment_age %>%
  filter(Delay_from_due_date <= 5)

# Count the number of people meeting both criteria
num_people_duedate <- nrow(assignment_age_duedate)
num_people_duedate1 <- nrow(assignment_age_duedate1)


# Filter the data with Outstanding_Debt<>80
assignment_age_duedate_debt <- assignment_age_duedate %>%
  filter(Outstanding_Debt>80)

assignment_age_duedate_debt1 <- assignment_age_duedate %>%
  filter(Outstanding_Debt <=80)

# Count the number of people meeting all criteria
num_people_duedate_debt <- nrow(assignment_age_duedate_debt)
num_people_duedate_debt1 <- nrow(assignment_age_duedate_debt1)

# Filter the data with Num_of_Delayed_Payment
assignment_age_duedate_debt_delay <- assignment_age_duedate_debt %>%
  filter(Num_of_Delayed_Payment >3)

assignment_age_duedate_debt_delay1 <- assignment_age_duedate_debt %>%
  filter(Num_of_Delayed_Payment <=3 )

num_people_duedate_debt_delay <- nrow(assignment_age_duedate_debt_delay)
num_people_duedate_debt_delay1 <- nrow(assignment_age_duedate_debt_delay1)


# Combine the result
Hypothesis2 <- data.frame(
  criteria = c("Age > 20 & Low spent", "Age <= 20 & Low spent", "Delay > 5& Low spent", "Delay <= 5& Low spent", 
               "Outstanding_Debt > 80 & Low spent", "Outstanding_Debt <=80 & Low spent", "Num_of_Delayed_Payment >3 & Low spent", "Num_of_Delayed_Payment <=3 & Low spent"),
  count = c(num_people, num_people1, num_people_duedate, num_people_duedate1, num_people_duedate_debt,
            num_people_duedate_debt1, num_people_duedate_debt_delay,num_people_duedate_debt_delay1)
)

color = c("red", "blue", "red", "blue", "red", "blue", "red", "blue")
Hypothesis2_graph <- data.frame(Hypothesis2,color)
Hypothesis2_graph$criteria <- factor(Hypothesis2_graph$criteria, levels = Hypothesis2_graph$criteria)


#print out the result
print(Hypothesis2)

# Combine all graph for better view
ggplot(Hypothesis2_graph, aes(x = criteria, y = count, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  geom_text(aes(label = count), vjust = -0.5) +  # Add values on top of the bars
  labs(title = "Summary of Hypothesis",
       x = "Criteria",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#------------------------------------------------------------------------------------------------------------------------------------------#

#Extra Feature
#1

extra_feature <- as.data.frame(list(Monthly_Inhand_Salary = assignment$Monthly_Inhand_Salary,
                                    Num_of_Delayed_Payment = assignment$Num_of_Delayed_Payment,
                                    Age= assignment$Age))
extra_feature <- na.omit(extra_feature)

ef1 <- lm(Num_of_Delayed_Payment ~ Monthly_Inhand_Salary , data = extra_feature)
summary(ef1)

# The result show that low R-squared value, meaning that age explains only a very small portion
# The 3 star suggesting the relation for both the variable is not 0
# The low p value low indicate we can reject the null hypothesis that they the both variable have no relation
#The result also show that as num of delay increase, the Montly in hand salary decrease





#2
# To find relationship between delay form due date and payment beheviour
extra_feature2  <- as.data.frame(list(Payment_Behaviour= assignment$Payment_Behaviour,
                                      Num_Bank_Accounts = assignment$Num_Bank_Accounts))
extra_feature2$Num_Bank_Accounts <- as.numeric(extra_feature2$Num_Bank_Accounts)
extra_feature2  <- na.omit(extra_feature2)


extra_feature2$Payment_Behaviour <- as.numeric(factor(extra_feature2$Payment_Behaviour,
                                                      levels = c("Low_spent", "High_spent")))


extra_feature2[grepl("Low_spent",extra_feature2)]<- 0
extra_feature2[grepl("High_spent",extra_feature2)]<-1

extra_feature2_data <- data.frame(
  Payment_Behaviour = extra_feature2$Payment_Behaviour,
  Num_Bank_Accounts = extra_feature2$Num_Bank_Accounts
)

cor(extra_feature2)


#A negative correlation indicates an inverse relationship between the two variables. 
#it suggests that as the payment behaviour increase , the Num_Bank_Accounts tends to decrease.


lm_model <- lm(Num_Bank_Accounts ~ Payment_Behaviour, data = extra_feature2_data)

# Print the summary of the linear regression model
summary(lm_model)



# Extra feature 3
#BoxPlot

plot2 <- assignment
plot2$Payment_Behaviour <- factor(plot2$Payment_Behaviour, levels = c("Low_spent", "High_spent"))
ggplot(plot2, aes(x=Payment_Behaviour, y=Num_Bank_Accounts)) +
  geom_boxplot(fill="cornflowerblue",
               color="black", notch=TRUE)



#Extra feature 4

extra_feature4 <- as.data.frame(list(Month = assignment$Month, 
                                     Monthly_Inhand_Salary = assignment$Monthly_Inhand_Salary))
extra_feature4$Monthly_Inhand_Salary <- as.numeric(extra_feature4$Monthly_Inhand_Salary)
extra_feature4 <- na.omit(extra_feature4)


#Seasonal plot
library(forecast)
extra_feature4 <- aggregate(Monthly_Inhand_Salary~ Month,data = extra_feature4,FUN = mean)


# Convert Month column to factor with ordered levels
extra_feature4$Month <- factor(extra_feature4$Month, levels = month.name, ordered = TRUE)

# Convert the data to a time series object
extra_feature4_time <- ts(extra_feature4$Monthly_Inhand_Salary, frequency = 12)

# Create the seasonal plot
seasonplot(extra_feature4_time, main = "Seasonal Plot of Monthly in hand salary")

#The seasonal plot show that the month Mar have the highest salary


extra_feature5_behaviour[grepl("Low_spent",extra_feature5_behaviour)]<- 0
extra_feature5_behaviour[grepl("High_spent",extra_feature5_behaviour)]<-1
extra_feature5_behaviour <- as.numeric(extra_feature5_behaviour)
#Extra Feature 5

extra_feature5_salary <- assignment$Monthly_Inhand_Salary
extra_feature5_behaviour <- assignment$Payment_Behaviour
extra_feature5_Num_Bank_Accounts <- assignment$Num_Bank_Accounts
extra_feature5_Delay_from_due_date<- assignment$Delay_from_due_date
extra_feature5_Age <- assignment$Age



extra_feature5 <- data.frame(extra_feature5_salary,extra_feature5_behaviour,
                             extra_feature5_Num_Bank_Accounts,extra_feature5_Age,
                             extra_feature5_Delay_from_due_date)


logistic <- glm(extra_feature5_behaviour  ~ extra_feature5_salary ,
                data = extra_feature5,  family = binomial)


logistic <- glm(extra_feature5_behaviour  ~ extra_feature5_Num_Bank_Accounts ,
                data = extra_feature5,  family = binomial)

logistic <- glm(extra_feature5_behaviour  ~ extra_feature5_Delay_from_due_date ,
                data = extra_feature5,  family = binomial)

logistic <- glm(extra_feature5_behaviour  ~ extra_feature5_Age,
                data = extra_feature5,  family = binomial)

summary(logistic)


logistic <- glm(extra_feature5_behaviour  ~ extra_feature5_salary + extra_feature5_Num_Bank_Accounts+extra_feature5_Delay_from_due_date+extra_feature5_Age,
                data = extra_feature5,  family = binomial)






#Extra feature 6


# Combine data into a data frame
extra_feature_6 <- data.frame(payment_behavior = assignment$Payment_Behaviour, Age = assignment$Age, 
                              Delay_from_due_date = assignment$Delay_from_due_date,
                              Monthly_Inhand_Salary = assignment$Monthly_Inhand_Salary,
                              Num_Bank_Accounts = assignment$Num_Bank_Accounts)

extra_feature_6 <- na.omit(extra_feature_6)
extra_feature_6 <- subset(extra_feature_6, payment_behavior != "High_spent")
extra_feature_6 <- extra_feature_6 %>% sample_frac(0.01, replace = FALSE)

colors <- ifelse(extra_feature_6$payment_behavior == "Low_spent", "blue", "red")
# Define colors based on payment behavior

# Custom panel function to add points and abline
panel_with_abline <- function(x, y, col, pch) {
  points(x, y, col = col, pch = pch)
  abline(lm(y ~ x), col = 'darkgray')  # Add regression line
}

# Create scatterplot matrix with custom panel function
pairs(extra_feature_6[, c("Age", "Delay_from_due_date", "Monthly_Inhand_Salary","Num_Bank_Accounts")], 
      col = colors, pch = 19,
      main = "Scatterplot Matrix",
      lower.panel = NULL,
      upper.panel = panel_with_abline)  # Use custom panel function

# Add legend for payment behavior
legend("bottomleft", legend = unique(extra_feature_6$payment_behavior), 
       col = c("blue", "red"), pch = 19,
       title = "Payment Behavior",
       xpd = TRUE)



#Extra Feature 7


library(lattice)
histogram(~Monthly_Inhand_Salary | Occupation, data = assignment,
          main="Occupation",
          xlab="Montly in hand salary")


#Extra Feature 8

assigment_sample <- assignment %>%
  sample_frac(0.7)

#Extract the two table use for analyse from the assignment data frame
assigment_due <- assigment_sample$Delay_from_due_date
assigment_payment <- assigment_sample$Payment_Behaviour
assigment_hypothesis <- data.frame(Delay_from_due_date = assigment_due, Payment_behaviour = assigment_payment)

#Sort out all the delay due date with more then 5
assigment_hypothesis <- assigment_hypothesis[assigment_hypothesis$Delay_from_due_date > 5, ]

#Get the result for the amount of delay for each credit score
result <- table(assigment_hypothesis$Payment_behaviour)
result <- as.data.frame(result)
names(result) <- c("Payment_behaviour", "Delay_from_due_date")
result

# Convert the result into percentage form
total_delay <- sum(result$Delay_from_due_date)
result$Delay_from_due_date <- result$Delay_from_due_date / total_delay * 100
result$Delay_from_due_date <- round(result$Delay_from_due_date,0)
result$Delay_from_due_date <- paste(result$Delay_from_due_date,"%", sep = "")
result


# Convert the result into percentage form
total_delay <- sum(result$Delay_from_due_date)
result$Delay_from_due_date <- result$Delay_from_due_date / total_delay * 100
result$Delay_from_due_date <- round(result$Delay_from_due_date,0)
result$Delay_from_due_date <- paste(result$Delay_from_due_date,"%", sep = "")
result









# use to view the entire assignment table
fix(assignment)





