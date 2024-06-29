#Name : Ignatius Harry Cahiadharma 柯昱安
#UID : 112266019
#Task-2 Causal Inferences, Prof. Tzu-Ting Yang

# Question 2.1 -----------------------------------------------------------
#Use commands to transform your term paper data into R’s data format

library(tidyverse)
library(readxl)
library(writexl)

setwd("/Users/ignatiusharry/Library/CloudStorage/Dropbox/Data/S2/NCCU/2nd Semester/Causal Inference And Data Science/Task 2")
data <- read_excel("Indonesia Tourism.xlsx")
str(data)

#variable 2
ggplot(data, aes(x = tourism_expenditure)) +
  geom_histogram()

# Question 2.2 -----------------------------------------------------------
#Use is.na to check any missing value in your dataset

colSums(is.na(data))
#no missing data


str(data)
# Question 3.1 --------------------------------------------------------------
#Use mutate() to generate a variable for your empirical analysis
data_log <- data %>%
  mutate(log_unemployment = log(data$unemployment),
  log_FDI = log(data$FDI),
  log_DDI = log(data$DDI),
  log_foreign_tourist = log(data$foreign_tourist),
  log_domestic_tourist = log(data$domestic_tourist),
  log_tourism_employment = log(data$tourism_employment),
  log_GRDP = log(data$GRDP),
  log_accommodation = log(data$accommodation),
  log_pop_density = log(data$pop_density),
  log_revenue = log(data$tourism_revenue),
  log_tourist_attraction = log(data$number_tourist_attraction),
  log_expenditure = log(data$tourism_expenditure))


str(data_log)

# Question 3.2 ------------------------------------------------------------
# Use summarise() to create a new dataset for your empirical analysis

data_mean <- data_log %>%
  group_by(province) %>%
  summarise(
    mean_HDI = mean(HDI, na.rm = TRUE),
    mean_unemployment = mean(log_unemployment, na.rm = TRUE),
    mean_FDI = mean(log_FDI, na.rm = TRUE),
    mean_DDI = mean(log_DDI, na.rm = TRUE),
    mean_foreign_tourist = mean(log_foreign_tourist, na.rm = TRUE),
    mean_domestic_tourist = mean(log_domestic_tourist, na.rm = TRUE),
    mean_tourism_employment = mean(log_tourism_employment, na.rm = TRUE),
    mean_GRDP = mean(log_GRDP, na.rm = TRUE),
    mean_accommodation = mean(log_accommodation, na.rm = TRUE),
    mean_pop_density = mean(log_pop_density, na.rm = TRUE),
    mean_revenue = mean(log_revenue, na.rm = TRUE),
    mean_tourist_attraction = mean(log_tourist_attraction, na.rm = TRUE),
    mean_expenditure = mean(log_expenditure, na.rm = TRUE)
  )

str(data_mean)


# Question 3.3 -----------------------------------------------------------
# Preliminary Research ----------------------------------------------------
str(data_mean)

# Sorting and plotting the provinces by mean foreign tourist arrivals
data_foreign_tourist <- data_mean %>%
  arrange(desc(mean_foreign_tourist))

ggplot(data_foreign_tourist, aes(x = reorder(province, -mean_foreign_tourist), y = mean_foreign_tourist)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Provinces Ranked by Mean Foreign Tourist Arrivals",
       x = "Province",
       y = "Mean Foreign Tourist Arrivals") +
  theme_minimal()

# Sorting and plotting the provinces by mean domestic tourist arrivals
data_domestic_tourist <- data_mean %>%
  arrange(desc(mean_domestic_tourist))

ggplot(data_domestic_tourist, aes(x = reorder(province, -mean_domestic_tourist), y = mean_domestic_tourist)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Provinces Ranked by Mean Domestic Tourist Arrivals",
       x = "Province",
       y = "Mean Domestic Tourist Arrivals") +
  theme_minimal()

# Question 3.4 s -------------------------------------------------------------------------
#Create a table to display summary statistic

summary_statistics <- data_log %>%
  reframe(
    variable = names(.)[-c(1,2)], # Exclude non-numeric columns like 'province' and 'year'
    observations = sapply(.[-c(1,2)], function(x) sum(!is.na(x))),
    mean = sapply(.[-c(1,2)], mean, na.rm = TRUE),
    sd = sapply(.[-c(1,2)], sd, na.rm = TRUE),
    min = sapply(.[-c(1,2)], min, na.rm = TRUE),
    max = sapply(.[-c(1,2)], max, na.rm = TRUE)
  )

# Convert to data frame for better formatting
summary_df <- as.data.frame(summary_statistics)
summary_df

# Create a cleaner table using kable
library(knitr)
kable(summary_df, caption = "Summary Statistics for Estimation Sample",
      digits = 2, na = "-")

# 4. Visualize Data -------------------------------------------------------

# Question 4.1 --------------------------------------------------------------------
# Use any command that in R’s graphical packages, such as ggplot2 or others, to create a graph that can represent one of the findings in your term paper

# Scatter Plot ------------------------------------------------------------

# MEAN # Scatter Plot ------------------------------------------------------------
library(ggrepel)
library(gridExtra)

# scatter plots with regression lines
str(data_mean)

create_scatter_plot <- function(data_log, x_var, y_var) {
  ggplot(data_log, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", color = "red") +
    geom_text_repel(aes(label = province), size = 3) +
    labs(title = paste(y_var, "vs", x_var),
         x = x_var,
         y = y_var) +
    theme_minimal()
}

# List of independent variables
independent_vars <- c("mean_foreign_tourist", "mean_domestic_tourist")

# Creating plot for each variables
plots <- lapply(independent_vars, function(var) {
  create_scatter_plot(data_mean, var, "mean_GRDP")
})

# print
do.call(grid.arrange, c(plots, ncol = 2))


# List of independent variables
independent_vars2 <- c("mean_foreign_tourist", "mean_domestic_tourist")

# Creating plot for each variables
plots2 <- lapply(independent_vars2, function(var) {
  create_scatter_plot(data_mean, var, "mean_unemployment")
})

# print
do.call(grid.arrange, c(plots2 , ncol = 2))


# -------------------------------------------------------------------------


# List of independent variables
independent_vars <- c("log_foreign_tourist", "log_domestic_tourist")

# Scatter Plot ------------------------------------------------------------
# Load necessary libraries
library(ggplot2)

# Assuming 'data_log' is your data frame with the variables
# List of independent variables
independent_vars <- c("log_foreign_tourist", "log_domestic_tourist")

# Function to create scatter plot with regression line
create_scatter_plot <- function(data_log, x_var, y_var, x_label, y_label) {
  ggplot(data_log, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", color = "red") +
    geom_text_repel(aes(label = province), size = 3) +
    labs(title = paste(y_label, "vs", x_label),
         x = x_label,
         y = y_label) +
    theme_minimal()
}

# Create plots for each independent variable
plots <- lapply(independent_vars, function(var) {
  if (var == "log_foreign_tourist") {
    x_label <- "Foreign Tourist Arrivals"
  } else {
    x_label <- "Domestic Tourist Arrivals"
  }
  create_scatter_plot(data_log, var, "log_unemployment", x_label, "Unemployment")
})

# Combine and display the plots together
grid.arrange(plots[[1]], plots[[2]], ncol = 2)

# Print the plots
plots[[1]]
plots[[2]]
# Display all plots
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))

# -------------------------------------------------------------------------



# List of independent variables
independent_vars <- c("log_foreign_tourist", "log_domestic_tourist")

# Function to create scatter plot with regression line
create_scatter_plot <- function(data_log, x_var, y_var, x_label, y_label) {
  ggplot(data_log, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", color = "red") +
    labs(title = paste(y_label, "vs", x_label),
         x = x_label,
         y = y_label) +
    theme_minimal()
}

# Create plots for each independent variable
plots <- lapply(independent_vars, function(var) {
  if (var == "log_foreign_tourist") {
    x_label <- "Foreign Tourist Arrivals"
  } else {
    x_label <- "Domestic Tourist Arrivals"
  }
  create_scatter_plot(data_log, var, "log_GRDP", x_label, "GRDP")
})

# Combine and display the plots together
grid.arrange(plots[[1]], plots[[2]], ncol = 2)

# Print the plots
plots[[1]]
plots[[2]]
# Display all plots
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))


# 5. Empricial Analysis ---------------------------------------------------
# Question 5.2 --------------------------------------------------------------------
#Based on your research question and empirical method, write down the equation that you will use for estimating causal effect


# Lasso -------------------------------------------------------------------


str(data_log)
library(glmnet)
library(dplyr)

# Data preparation: Selecting the required variables
# Using log variables for Lasso regression
#data_lasso1 <- data_log %>%
#  select(log_GRDP, domestic_tourist,foreign_tourist, accommodation, HDI, FDI, DDI, pop_density,tourism_revenue,  tourism_expenditure, tourism_employment, number_tourist_attraction)


# Select relevant columns and convert to matrix (glmnet requires input in matrix form)
data_lasso1 <- data_log %>%
  select(log_GRDP, log_domestic_tourist, log_foreign_tourist, HDI, log_accommodation, log_FDI, log_DDI, log_pop_density, log_tourist_attraction)

# Separate predictors (X) and response variable (y)
X <- as.matrix(data_lasso1 %>% select(-log_GRDP))
y <- data_lasso1$log_GRDP

# Run Lasso Regression
lasso_model1 <- glmnet(X, y, alpha = 1)

# Plot of coefficients
plot(lasso_model1, xvar = "lambda", label = TRUE)

# Choose the optimal lambda using cross-validation
cv_lasso1 <- cv.glmnet(X, y, alpha = 1)

# Cross-validation results
plot(cv_lasso1)

# The best Lambda
best_lambda1 <- cv_lasso1$lambda.min
cat("Optimal lambda: ", best_lambda1, "\n")

# Model coefficients with optimal lambda
lasso_coefs1 <- coef(cv_lasso1, s = "lambda.min")
print(lasso_coefs1)


# ------------------------------------------------------------------------
# -------------------------------------------------------------------------

#data_lasso2 <- data_log %>%
#  select(unemployment, domestic_tourist,foreign_tourist, HDI, accommodation, FDI, DDI, pop_density,tourism_revenue, number_tourist_attraction, tourism_expenditure)

data_lasso2 <- data_log %>%
  select(unemployment, log_domestic_tourist, log_foreign_tourist, HDI, log_accommodation, log_FDI, log_DDI, log_pop_density,log_tourist_attraction)

# Convert to matrix (glmnet requires input in matrix form)
X <- as.matrix(data_lasso2 %>% select(-unemployment))
y <- data_lasso2$unemployment

#run Lasso Regression
lasso_model <- glmnet(X, y, alpha = 1)

# plot of coefficients
plot(lasso_model, xvar = "lambda", label = TRUE)

# Choose the optimal lambda using cross-validation
cv_lasso <- cv.glmnet(X, y, alpha = 1)

# Plot hasil cross-validation
plot(cv_lasso)

# The best Lambda
best_lambda <- cv_lasso$lambda.min
cat("Lambda optimal: ", best_lambda, "\n")

# Model coefficients with optimal lambda
lasso_coefs <- coef(cv_lasso, s = "lambda.min")
print(lasso_coefs)


# FIXED EFFECT PANEL REGRESSION-------------------------------------------------------------------------

# Panel Regression
library(plm)
library(sandwich)
library(broom)
library(lmtest)

pdata <- pdata.frame(data_log, index = c("province", "year"))

## Utilized Fixed Effect (with robust)
model_grdp  <- plm(formula = log_GRDP ~ 
                     log_domestic_tourist + 
                     log_foreign_tourist + 
                     HDI + 
                     log_accommodation + 
                     log_FDI + 
                     log_DDI + 
                     log_pop_density +
                     log_revenue +
                     #log_tourism_employment +
                     #log_expenditure +
                     log_tourist_attraction, model = "within", data = pdata)
                     
summary(model_grdp)

# Calculate robust standard errors
robust_se <- vcovHC(model_grdp, type = "HC4")

# Use coeftest to get robust standard errors
robust_summary <- coeftest(model_grdp, vcov = robust_se)

# Print the robust summary
print(robust_summary)


# 2nd model ------------------------------------------------------------
model_unemployment  <- plm(formula = log_unemployment ~ 
                             log_domestic_tourist + 
                             log_foreign_tourist + 
                             HDI + 
                             log_accommodation + 
                             log_FDI + 
                             log_DDI + 
                             log_pop_density +
                             #log_revenue + 
                             #log_tourism_employment +
                             log_tourist_attraction, model = "within", data = pdata)
summary(model_unemployment)

# Calculate robust standard errors
robust_se2 <- vcovHC(model_unemployment, type = "HC4")

# Use coeftest to get robust standard errors
robust_summary2 <- coeftest(model_unemployment, vcov = robust_se2)

# Print the robust summary
print(robust_summary2)


#model 3

# Tabel regression --------------------------------------------------------


# END ---------------------------------------------------------------------

# Additional Test for Model 1 -----------------------------------------------------

# Multicollinearity Test
# Fit OLS model directly
ols_model1 <- lm(log_GRDP ~ 
                   log_domestic_tourist + 
                   log_foreign_tourist + 
                   HDI + 
                   log_accommodation + 
                   log_FDI + 
                   log_DDI + 
                   log_pop_density +
                   #log_revenue +
                   #log_tourism_employment +
                   #log_expenditure +
                   log_tourist_attraction, data = data_log)
# Calculate VIF
library(car)
vif(ols_model1)


# Classical Assumption Tests ----------------------------------------------


# Normality Test of Residuals
shapiro.test(model_grdp$residuals)
#Result: P>0.05 = reject h0 (Residual is normal distribution (GOOD))

# Homoscedasticity Test
library(lmtest)
bptest(model_grdp, studentize = FALSE, data =pdata)
#Result: P < 0.05 = Reject H0 (This indicates evidence of heteroscedasticity, meaning that the variance of residuals is not constant across observations.)

# Autocorrelation of Residuals
# Wooldridge test for autocorrelation in panel data
wooldridge_test <- pwartest(model_grdp)
print(wooldridge_test)
#Result: P>0.05 = Fail to reject H0 (No significant evidence of serial correlation in the residuals.)



# Additional Test for Model 2 -----------------------------------------------------

# Multicollinearity Test
# Fit OLS model directly
ols_model2 <- lm(log_unemployment ~ 
                   log_domestic_tourist + 
                   log_foreign_tourist + 
                   HDI + 
                   #log_accommodation + 
                   log_FDI + 
                   log_DDI + 
                   log_pop_density +
                   #log_revenue + 
                   #log_tourism_employment +
                   log_tourist_attraction, data = data_log)
# Calculate VIF
library(car)
vif(ols_model2)


# Classical Assumption Tests
# Normality Test of Residuals
shapiro.test(model_unemployment$residuals)
#Result: P>0.05 = reject h0 (Residual is normal distribution (GOOD))

# Homoscedasticity Test
library(lmtest)
bptest(model_unemployment, studentize = FALSE, data =pdata)
#Result: P < 0.05 = Reject H0 (This indicates evidence of heteroscedasticity, meaning that the variance of residuals is not constant across observations.)

# Autocorrelation of Residuals
# Wooldridge test for autocorrelation in panel data
wooldridge_test <- pwartest(model_unemployment)
print(wooldridge_test)
#Result: P>0.05 = Fail to reject H0 (No significant evidence of serial correlation in the residuals.)

# Map ---------------------------------------------------------------------

# Plotting to the MAP -----------------------------------------------------

setwd("/Users/ignatiusharry/Library/CloudStorage/Dropbox/Data/S2/NCCU/2nd Semester/Causal Inference And Data Science/Final Assignment")
library(tmap)
library(sf)
#Step 1
idn <- st_read("gadm41_IDN.gpkg")
st_layers("gadm41_IDN.gpkg")

#level 2 (province level)
idn.lv2 <- st_read("gadm41_IDN.gpkg", layer = "ADM_ADM_1")
#Rename
idn.lv2 <- idn.lv2 %>%
  rename(Province = NAME_1)
print(idn.lv2$Province)
glimpse(idn.lv2)


head(idn.lv2$NAME_1, n = 34)
plot(st_geometry(idn.lv2))


#step 2
data_log <- inner_join(idn.lv2, data_log, by = join_by(Province == province))
str(data_log)



# Mean --------------------------------------------------------------------
data_log <- data_log %>%
  group_by(Province) %>%
  summarise(
    mean_HDI = mean(HDI, na.rm = TRUE),
    mean_unemployment = mean(log_unemployment, na.rm = TRUE),
    mean_FDI = mean(log_FDI, na.rm = TRUE),
    mean_DDI = mean(log_DDI, na.rm = TRUE),
    mean_foreign_tourist = mean(log_foreign_tourist, na.rm = TRUE),
    mean_domestic_tourist = mean(log_domestic_tourist, na.rm = TRUE),
    mean_tourism_employment = mean(log_tourism_employment, na.rm = TRUE),
    mean_GRDP = mean(log_GRDP, na.rm = TRUE),
    mean_accommodation = mean(log_accommodation, na.rm = TRUE),
    mean_pop_density = mean(log_pop_density, na.rm = TRUE),
    mean_revenue = mean(log_revenue, na.rm = TRUE),
    mean_tourist_attraction = mean(log_tourist_attraction, na.rm = TRUE)
  )



# Creating The Map --------------------------------------------------------

idn_ft_mean <- tm_shape(data_log) +
  tm_fill(col = "mean_foreign_tourist", palette = "Reds", n = 5,  
          title = "Foreign Tourist (mean) ",style = "cont") +
  tm_borders(col = "white", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
print(idn_ft_mean)

idn_dt_mean <- tm_shape(data_log) +
  tm_fill(col = "mean_domestic_tourist", palette = "Blues", n = 5,  
          title = "Domestic Tourist (mean)", style = "cont") +
  tm_borders(col = "white", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")
# Print Map
idn_dt_mean


idn_un_mean <- tm_shape(data_log) +
  tm_fill(col = "mean_unemployment", palette = "YlGnBu", n = 5,  
          title = "Unemployment (mean) ", style = "cont") +
  tm_borders(col = "white", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")
# Print Map
idn_un_mean

idn_grdp_mean <- tm_shape(data_log) +
  tm_fill(col = "mean_GRDP", palette = "YlGnBu", n = 5,  
          title = "GRDP (mean)", style = "cont") +
  tm_borders(col = "white", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")
# Print Map
idn_grdp_mean

idn_accomdtn_mean <- tm_shape(data_log) +
  tm_fill(col = "mean_accommodation", palette = "Oranges", n = 5,  
          title = "Accommodation (mean) ",style = "cont") +
  tm_borders(col = "white", lwd = .015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
print(idn_accomdtn_mean)

idn_attraction_mean <- tm_shape(data_log) +
  tm_fill(col = "mean_tourist_attraction", palette = "Greens", n = 5,  
          title = "Tourist Attraction (mean)", style = "cont") +
  tm_borders(col = "white", lwd = 0.015) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.7, position = c("center", "bottom")) +  
  tm_text("Province", size = 0.3, col = "black")

# Print Map
print(idn_attraction_mean)

