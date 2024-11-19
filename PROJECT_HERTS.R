# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(corrplot)
library(cluster)
library(factoextra)
library(readr)
library(tidyr)
library(scales)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("readr")
install.packages("treemap")
install.packages("ggalt")
install.packages("scales")



fortune_data <- read.csv("C:\\Users\\kr720\\Downloads\\Largest_Companies.csv", stringsAsFactors = FALSE)

# View the first few rows
head(fortune_data)

# Get a summary of the dataset
summary(fortune_data)

# Check the structure of the dataset
str(fortune_data)

# Check for missing values in each column
colSums(is.na(fortune_data))

# Rename columns manually
colnames(fortune_data) <- c("Rank", "Name", "Industry", "Revenue USD Millions", "Revenue growth", "Employees", "Headquarters")

# Check the new column names
colnames(fortune_data)

# Clean 'Revenue USD Millions' and 'Revenue growth'
fortune_data$`Revenue USD Millions` <- as.numeric(gsub(",", "", fortune_data$`Revenue USD Millions`))
fortune_data$`Revenue growth` <- as.numeric(gsub("%", "", fortune_data$`Revenue growth`))

# Check for any NA values in these columns
sum(is.na(fortune_data$`Revenue USD Millions`))
sum(is.na(fortune_data$`Revenue growth`))

# Verify the cleaned data
head(fortune_data)

# Summary of the dataset to understand basic statistics
summary(fortune_data)

# Check for missing values across the entire dataset
colSums(is.na(fortune_data))


#Sactter_Plot
# Load necessary libraries
library(dplyr)
library(scales)

# Ensure the necessary columns are treated as numeric
fortune_data$Employees <- as.numeric(gsub("[^0-9]", "", fortune_data$Employees))
fortune_data$`Revenue growth` <- as.numeric(gsub("%", "", fortune_data$`Revenue growth`))

# Filter for top 100 companies based on revenue or rank if needed (assuming Rank column is present)
top_100_companies <- fortune_data %>%
  arrange(desc(`Revenue USD Millions`)) %>%  # Arrange by revenue if 'Rank' isn't specified
  head(100)

# Plot the scatter plot
plot(top_100_companies$Employees, top_100_companies$`Revenue growth`,
     main = "Correlation between Employee Count and Revenue Growth among Top 100 Companies",
     xlab = "Employee Count",
     ylab = "Revenue Growth (%)",
     pch = 19, col = "darkblue",
     xaxt = "n", yaxt = "n")

# Custom x-axis and y-axis with comma and percentage formatting
axis(1, at = pretty(top_100_companies$Employees), labels = comma(pretty(top_100_companies$Employees)))
axis(2, at = pretty(top_100_companies$`Revenue growth`), labels = percent(pretty(top_100_companies$`Revenue growth`), scale = 1))

# Add a linear regression line to the plot
abline(lm(`Revenue growth` ~ Employees, data = top_100_companies), col = "red")




#Histogram_Plot
# Load necessary libraries
library(dplyr)
library(scales)

# Ensure the 'Revenue growth' column is numeric (remove the '%' symbol)
fortune_data$`Revenue growth` <- as.numeric(gsub("%", "", fortune_data$`Revenue growth`))

# Filter for the top 100 companies based on revenue
top_100_companies <- fortune_data %>%
  arrange(desc(`Revenue USD Millions`)) %>%
  head(100)

# Extract the 'Revenue growth' column for the histogram
hist_data <- top_100_companies$`Revenue growth`

# Plot histogram of Revenue Growth with a normal distribution curve overlay
histogram <- hist(hist_data, 
                  probability = TRUE,  # Use density on the y-axis
                  breaks = 10,  # Number of bins for the histogram
                  main = "Distribution of Revenue Growth",
                  xlab = "Revenue Growth (%)",
                  ylab = "Density",
                  col = "lightblue",
                  border = "black")

# Add a normal distribution curve over the histogram
curve(dnorm(x, mean = mean(hist_data, na.rm = TRUE), sd = sd(hist_data, na.rm = TRUE)), 
      col = "red", 
      lwd = 2, 
      add = TRUE)

# Add counts for each bar
for (i in 1:length(histogram$counts)) {
  # Get the position for each count label (on top of each bar)
  x_pos <- histogram$mids[i]
  y_pos <- histogram$counts[i] * max(histogram$density) / max(histogram$counts)  # Adjust the height for visibility
  
  # Add the count as a label on top of each bar
  text(x_pos, y_pos, labels = as.character(histogram$counts[i]), cex = 0.8, col = "black", pos = 3)
}

# Add a note explaining what the density values represent
note_text <- "Note: Y-axis represents the density of Revenue Growth values. 
Revenue dsitributes in 10 frequency bins. Here values represent 0.01 = 10 etc..."
mtext(note_text, side = 1, line = 4, adj = 0, cex = 0.8)

# Perform Kendall's Tau correlation test
kendall_test_result <- cor.test(top_100_companies$`Revenue growth`, top_100_companies$Employees, method = "kendall")

# Print the result
print(kendall_test_result)



