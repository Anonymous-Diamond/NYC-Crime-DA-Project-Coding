
#Download the libraries with install.package("library name") if you havent. 

#GRANGER TEST AND LAG SELECTION

library(vars)
library(readxl)
library(knitr)
library(kableExtra)

# Load the data from the specified Excel file
data_path <- "YOUR DATA PATH"  # Update the path accordingly
crime_data <- read_excel(data_path)

# VAR Model and Optimal Lag Selection 
model_selection <- VARselect(crime_data[, c("d2Psy", "d2Total_Crime")], 
                             lag.max = 10, 
                             type = "both")

# Print the selection results to see the optimal number of lags for each criterion
print("Lag selection results:")
print(model_selection$selection)

# Extract the optimal number of lags for each information criterion
optimal_lags_aic <- model_selection$selection["AIC(n)"]
optimal_lags_hq <- model_selection$selection["HQ(n)"]
optimal_lags_sc <- model_selection$selection["SC(n)"]
optimal_lags_fpe <- model_selection$selection["FPE(n)"]

# Print the optimal lags
print(paste("Optimal lags based on AIC:", optimal_lags_aic))
print(paste("Optimal lags based on HQIC:", optimal_lags_hq))
print(paste("Optimal lags based on SC (Schwarz Criterion):", optimal_lags_sc))
print(paste("Optimal lags based on FPE (Final Prediction Error):", optimal_lags_fpe))

# Determine which criterion to use based on a predefined priority or the smallest lag
optimal_lag_final <- max(c(optimal_lags_aic, optimal_lags_hq, optimal_lags_sc, optimal_lags_fpe))
print(paste("Chosen optimal lag length:", optimal_lag_final))

# Fit VAR model with the chosen optimal lag
var_model <- VAR(crime_data[, c("d2Psy", "d2Total_Crime")], p = optimal_lag_final, type = "both")

# Perform Granger causality tests
granger_results <- causality(var_model, cause = "d2Psy")
granger_results_reverse <- causality(var_model, cause = "d2Total_Crime")

# Print the full causality results
print("Granger causality results from d2Psy to d2Total_Crime:")
print(granger_results)

print("Reverse Granger causality results from d2Total_Crime to d2Psy:")
print(granger_results_reverse)

# Extract Granger causality test results
f_test_value_psy_to_crime <- granger_results$Granger$statistic[1]
df1_psy_to_crime <- granger_results$Granger$parameter[1]
df2_psy_to_crime <- granger_results$Granger$parameter[2]
p_value_psy_to_crime <- granger_results$Granger$p.value[1]
instantaneous_p_value_psy_to_crime <- granger_results$Instant$p.value[1]

f_test_value_crime_to_psy <- granger_results_reverse$Granger$statistic[1]
df1_crime_to_psy <- granger_results_reverse$Granger$parameter[1]
df2_crime_to_psy <- granger_results_reverse$Granger$parameter[2]
p_value_crime_to_psy <- granger_results_reverse$Granger$p.value[1]
instantaneous_p_value_crime_to_psy <- granger_results_reverse$Instant$p.value[1]



#BELOW IS MERELY FOR TABLE DISPLAY AND OPTIONAL
# Create results data frame
granger_results_df <- data.frame(
  Test = c("d2Psy to d2Total_Crime", "d2Total_Crime to d2Psy"),
  F_Test_Value = c(f_test_value_psy_to_crime, f_test_value_crime_to_psy),
  df1 = c(df1_psy_to_crime, df1_crime_to_psy),
  df2 = c(df2_psy_to_crime, df2_crime_to_psy),
  P_Value = c(p_value_psy_to_crime, p_value_crime_to_psy),
  Instantaneous_P_Value = c(instantaneous_p_value_psy_to_crime, instantaneous_p_value_crime_to_psy)
)

kable_granger <- kable(granger_results_df, format = "html", caption = "Granger Causality Test Results for Psy and Total Crime Using Optimal Lags") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = T) %>%
  column_spec(5, color = ifelse(granger_results_df$P_Value < 0.05, "green", "red")) %>%
  column_spec(6, color = ifelse(granger_results_df$Instantaneous_P_Value < 0.05, "green", "red"))

# Display the table
print(kable_granger)


# Linear Regression variable selection table
library(readxl)
library(dplyr)
library(broom)

# Load the data
data_path <- "YOUR DATA PATH"  # Update the path accordingly
crime_data_lm <- read_excel(data_path)

# Function to fit models and extract relevant statistics
fit_models <- function(dep_var) {
  results <- data.frame()
  
  for (i in 1:length(independent_vars)) {
    combns <- combn(independent_vars, i, simplify = FALSE)
    for (vars in combns) {
      formula <- as.formula(paste(dep_var, "~", paste(vars, collapse = "+")))
      model <- lm(formula, data = crime_data_lm)
      summary_model <- summary(model)
      
      # Extracting p-values
      p_values <- summary_model$coefficients[, 4]  # Getting p-values of all terms
      max_p_value <- max(p_values[-1])  # Ignore the intercept p-value for the threshold check
      
      if (max_p_value <= 0.1) {
        # Extracting F-statistic value
        f_statistic <- summary_model$fstatistic[1]
        
        # Storing results
        model_info <- data.frame(
          Dependent = dep_var,
          Independents = toString(vars),
          Min_P_Value = min(p_values[-1]),  # Minimum p-value of the terms excluding intercept
          Max_P_Value = max_p_value,  # Maximum p-value of the terms excluding intercept
          Adjusted_R2 = summary_model$adj.r.squared,
          F_Statistic = f_statistic
        )
        
        # Combining with existing results
        results <- rbind(results, model_info)
      }
    }
  }
  
  results
}

# List of dependent and independent variables
dependent_vars <- c("Total_Crime_Rate", "Violent_Crime_Rate", "Property_Crime_Rate",
                    "MURDER", "RAPE", "ROBBERY", "FELONY_ASSAULT", "BURGLARY",
                    "GRAND_LARCENY", "GRAND_LARCENY_OF_MOTOR_VEHICLE")
independent_vars <- c("Bachelor", "Unemployment", "Density", "Poverty", "Psy", "SingleParent")

# Apply the function across all dependent variables
all_results <- lapply(dependent_vars, fit_models)

# Combine all results into one DataFrame
final_results_lm <- do.call(rbind, all_results)
#You can find the table in "final_results_lm" if this is the variable name you choose to pick.


library(glmnet)
library(knitr)
library(kableExtra)
library(readxl)

# Load the data
data_path <- "YOUR DATA PATH"  # Update the path accordingly
crime_data_ridge <- read_excel(data_path)

# Extracting predictor variables and response variable
predictors <- crime_data_ridge[, c("Unemployment", "Density", "Poverty", "Psy", "Bachelor", "SingleParent")]
response <- crime_data_ridge$Total_Crime_Rate

# Standardize the predictors
predictors_scaled <- scale(predictors)

# Convert to matrix format as required by glmnet
x <- as.matrix(predictors_scaled)
y <- response

# Perform ridge regression with cross-validation
set.seed(123)  # for reproducibility
cv_ridge <- cv.glmnet(x, y, alpha = 0, standardize = TRUE)  

# Plot the cross-validation results
plot(cv_ridge)

# Plot the coefficient paths
ridge_model <- cv_ridge$glmnet.fit
plot(ridge_model, xvar = "lambda", label = TRUE, col = rainbow(ncol(x)))
title("Coefficient Paths for Ridge Regression (Total Crime Rate)")
legend("topright", legend = colnames(predictors), col = rainbow(ncol(x)), lty = 1, cex = 0.8)

# Retrieve the lambda that minimizes the cross-validation error
optimal_lambda <- cv_ridge$lambda.min
print(paste("Optimal lambda:", optimal_lambda))

# Fit the final ridge regression model using the optimal lambda
final_model <- glmnet(x, y, alpha = 0, lambda = optimal_lambda)

# Extract coefficients and print them 
coefficients <- coef(final_model)
print("Ridge Regression Coefficients:")
print(coefficients)

# Optionally print a more detailed summary of the model
print("Ridge Regression Model Summary:")
print(summary(final_model))


#BELOW IS FOR TABLE DISPLAY OPTIONAL

# Extract coefficients from the model and convert to matrix
coeff_matrix <- as.matrix(coef(final_model, s = optimal_lambda))

# Create a data frame from the matrix
coefficients_df <- data.frame(
  Variable = rownames(coeff_matrix),  # Extract row names as variable names
  Estimate = coeff_matrix[,1]         # The coefficients are in the first column
)

kable_ridge <- kable(coefficients_df, format = "html", 
                     col.names = c("Variable", "Coefficient Estimate"), 
                     caption = paste("Ridge Regression Results for Total Crime with Optimal Lambda =", optimal_lambda)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F, font_size = 14) %>%
  column_spec(1, bold = T, color = "black") %>%
  scroll_box(width = "100%", height = "500px")

# Display the table
print(kable_ridge)








HEATMAP
#HEAT MAPS IS BY Police Precinct make sure you download the geospatial file from NYC.gov websites. Borough geospatial is needed as well.


library(ggplot2)
library(sf)
library(dplyr)

# Set the file paths for geospatial data
boroughs_path <- "INSERT GEOSPATIAL DATA PATH YOU DOWNLOADED HERE FOR BOROUGHS (ITS IN .SHP FORMAT)"
precincts_path <- "INSERT GEOSPATIAL DATA PATH YOU DOWNLOADED HERE FOR POLICE PRECINCT (ITS IN .SHP FORMAT)"
crime_data_path <- "YOUR CRIME DATA PATH"

# Read the geospatial data for boroughs and precincts
boroughs <- st_read(boroughs_path)
precincts <- st_read(precincts_path)


crime_data <- read.csv(crime_data_path) 

# Rename and prepare data for joining and visualization
boroughs <- boroughs %>%
  mutate(Boro_Name = boro_name)

precincts <- precincts %>%
  rename(Precinct = precinct)

crime_data <- crime_data %>%
  rename(Precinct = Precinct)

crime_data_precinct_sf <- left_join(precincts, crime_data, by = "Precinct")

crime_data_precinct_sf <- crime_data_precinct_sf %>%
  group_by(Precinct) %>%
  summarise(Crime_Count = n(), geometry = first(geometry))

# Calculate centroids for label placement in boroughs
boroughs$centroid <- st_centroid(boroughs$geometry)

# Create the heatmap with custom colors, label scaling of your choice
plot <- ggplot() +
  geom_sf(data = boroughs, fill = NA, color = "black", size = 1) +
  geom_sf(data = crime_data_precinct_sf, aes(fill = Crime_Count), color = "white", size = 0.2) +
  geom_text(data = boroughs, aes(x = st_coordinates(centroid)[,1], y = st_coordinates(centroid)[,2], label = Boro_Name), 
            size = 4, color = "black", check_overlap = TRUE, fontface = "bold") +
  scale_fill_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "red"),
                       values = scales::rescale(c(0, 0.2, 0.4, 0.6, 0.8, 1)),
                       name = "Crime Count") +
  labs(title = "Crime Heatmap by Police Precinct") +
  theme_minimal() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title.align = 0.5,
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  guides(fill = guide_colorbar(title = "Crime Count", title.position = "top", barwidth = 40, barheight = 1.5))

# Display the plot 
print(plot)


library(dplyr)
library(sf)
library(ggplot2)
library(scales)
# Ensure 'crimes_sf' is loaded as an SF object
# Extract longitude and latitude from the geometry
crime_data <- as.data.frame(st_coordinates(crimes_sf))

# Now, join this coordinate data back with the original data to retain the crime count
crime_data <- bind_cols(crime_data, as.data.frame(crimes_sf))

# Rename the extracted coordinate columns appropriately if needed
names(crime_data)[1:2] <- c("Longitude", "Latitude")

# Aggregate crime data by location and count the number of crimes
crime_counts <- crime_data %>%
  group_by(Longitude, Latitude) %>%
  summarise(Crime_Count = n(), .groups = 'drop')
#
boroughs_path <- "Your borough spatial file pathway"# your borough spatial data to label borough boundaries
boroughs <- st_read(boroughs_path)

# Filter for locations with a crime count above a certain threshold, e.g., more than 10 crimes
threshold <- 20
significant_crime_locations <- crime_counts %>%
  filter(Crime_Count > threshold)

# Set a base color for boroughs and define colors for different crime severity levels
base_borough_color <- "thistle"  # A shade of lilac for the boroughs
crime_colors <- c("lightblue", "yellow", "orange", "red")  # Colors for crime severity

# Assuming 'significant_crime_locations' is prepared with 'Crime_Count' and coordinates
# Adjust the dataset for plotting
significant_crime_locations$size <- ifelse(significant_crime_locations$Crime_Count >= 400, 12,
                                           ifelse(significant_crime_locations$Crime_Count >= 300, 9,
                                                  ifelse(significant_crime_locations$Crime_Count >= 200, 6, 3)))
significant_crime_locations$color <- cut(significant_crime_locations$Crime_Count,
                                         breaks = c(0, 100, 200, 300, 400),
                                         labels = crime_colors,
                                         right = FALSE)

#Adjust color and labels, title, and scaling in whichever way desired
# Prepare the plot
plot <- ggplot() +
  geom_sf(data = boroughs, fill = "thistle", color = "black", size = 0.5) +  # Boroughs with base color
  geom_point(data = significant_crime_locations, aes(x = Longitude, y = Latitude, size = Crime_Count, color = Crime_Count), alpha = 0.8) +
  scale_size_area(max_size = 12, guide = "none") +  # No legend for sizes
  scale_color_gradientn(
    colors = c("cyan", "yellow", "orange", "red"),
    values = rescale(c(20, 100, 250, 400)),
    name = "Crime  Frequency",
    breaks = c(20, 100, 250, 400),
    labels = c("20-100", "100-249", "250-399", "400+"),
    limits = c(20, max(significant_crime_locations$Crime_Count))
  ) +
  labs(title = "Crime Frequency Map by Location") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Add labels for boroughs using the correct column name and a larger font size
plot <- plot + geom_text(data = boroughs, aes(label = boro_name, x = st_coordinates(st_centroid(geometry))[, 1], y = st_coordinates(st_centroid(geometry))[, 2]), size = 6, fontface = "bold", color = "darkslategray")

# Display the plot in R
print(plot)




