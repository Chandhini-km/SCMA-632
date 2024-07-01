setwd('C:\\Users\\Chand\\Downloads\\A3')

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA","glue")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("C:\\Users\\Chand\\Downloads\\A3\\NSSO68 (3).csv")
dim(data)

unique(data$Religion)

# Filtering for Mizoram
MIZ<- data %>%
  filter(state == "15")
# Display dataset info
cat("Dataset Information:\n")
print(names(MIZ))
print(head(MIZ))
print(dim(MIZ))

# Finding missing values
missing_info <- colSums(is.na(MIZ))
cat("Missing Values Information:\n")
print(missing_info)

# Sub-setting the data
miznew <- MIZ %>%
  select(state_1,Religion, District, Region, Sector,emftt_q, emftt_v)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(miznew)))

dim(miznew)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
miznew$emftt_q <- impute_with_mean(miznew$emftt_q)
miznew$emftt_v <- impute_with_mean(miznew$emftt_v)

dim(miznew)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(miznew)))

MIZ$Religion

miznew$emftt_v

MIZ$Religion

unique(MIZ$Religion)
str(MIZ$Religion)

# Fitting a probit regression to identify non-vegetarians. 

religion_mapping <- c("Hinduism", "Islam", "Christianity","Sikkhism","Buddhism")
MIZ$Religion <- factor(MIZ$Religion, labels = religion_mapping)
table(MIZ$Religion)

columns <- c('emftt_v','emftt_q')
data1 <- MIZ[columns]
data1$target <- ifelse(data1$emftt_v>0,1,0) 
probit_modet <- glm(target~., data = data1, family = binomial(link = "probit"))
summary(probit_modet)

# Performorming a Tobit regression analysis on "NSSO68.csv" 
df_MIZ = data[data$state_1 == 'MIZ',]
vars <- c("state_1","Religion", "District", "Region", "Sector","emftt_q", "emftt_v")

df_MIZ_p = df_MIZ[vars]
names(df_MIZ_p)

df_MIZ_p$price = df_MIZ_p$emftt_v / df_MIZ_p$emftt_q
names(df_MIZ_p)

summary(df_MIZ_p)

head(table(df_MIZ_p$emftt_q))

dim(df_MIZ_p)

names(MIZ)

#  dependent variable and independent variables
y <- MIZ$foodtotal_v
X <- MIZ[, c("sauce_jam_v", "Othrprocessed_v", "Beveragestotal_v", "fv_tot")]

#  data for Tobit regression
y_tobit <- pmin(pmax(y, 0), 1)  
X_tobit <- cbind(1, X) 

install.packages("censReg")
library(censReg)
# Fitting the Tobit model
X_tobit_df <- as.data.frame(X_tobit)
model <- censReg(y_tobit ~ ., data = X_tobit_df[, -1])

# Printing model summary
summary(model)

# Performorming a Tobit regression analysis on "NSSO68.csv" 
df_MIZ = data[data$state_1 == 'MIZ',]
vars <- c("state_1","Religion", "District", "Region", "Sector","emftt_q", "emftt_v")

df_MIZ_p = df_MIZ[vars]
names(df_MIZ_p)

df_MIZ_p$price = df_MIZ_p$emftt_v / df_MIZ_p$emftt_q
names(df_MIZ_p)

summary(df_MIZ_p)

head(table(df_MIZ_p$emftt_q))

dim(df_MIZ_p)

names(MIZ)

#  dependent variable and independent variables
y <- MIZ$foodtotal_v
X <- MIZ[, c("sauce_jam_v", "Othrprocessed_v", "Beveragestotal_v", "fv_tot")]

#  data for Tobit regression
y_tobit <- pmin(pmax(y, 0), 1)  
X_tobit <- cbind(1, X) 

install.packages("censReg")
library(censReg)
# Fitting the Tobit model
X_tobit_df <- as.data.frame(X_tobit)
model <- censReg(y_tobit ~ ., data = X_tobit_df[, -1])

# Printing model summary
summary(model)

