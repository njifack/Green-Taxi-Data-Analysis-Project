# Green Taxi Data Analysis Project

This project focuses on analyzing and exploring green taxi trip data from 2018. The analysis includes data loading, basic exploration, visualization, preprocessing, modeling, and evaluation.

###  Data

The green taxi trip data for 2018 is loaded using the `read_csv` function from the `readr` package.

### Basic Data Exploration

- The dimensions of the dataset are explored using the `dim` function.
- The structure of the dataset is examined using `glimpse`.
- Summary statistics, the first few rows, and the last few rows of the dataset are displayed using `summary`, `head`, and `tail` respectively.

### Visualization

- A histogram and a boxplot are created to visualize the distribution and outliers of the tip amount.
- Scatter plots are generated to explore the relationship between the speed of the trip and the tip amount.

### Preprocessing

- Datetime objects are converted to the appropriate format.
- Outliers in the tip amount data are identified and capped at the 99th percentile.
- Missing values in the fare amount and total amount columns are imputed using the median.
- Negative tip amounts are removed, and the tip amount is log-transformed to reduce skewness.
- Continuous variables are normalized using z-score standardization.
- RatecodeID is converted to a factor and dummy variables are created.

### Modeling and Evaluation

- The k-NN regression algorithm is implemented to predict tip amounts.
- The mean squared error (MSE) is used to evaluate model performance.
- The optimal value of k is determined by plotting the MSE against different values of k.

### Visualization of Results

- Hexbin plots are created to visualize the relationship between trip distance, tip amount, and time of day.

### Interpretation of Results

The k-NN regression model with the optimal k value provides insights into the factors influencing tip amounts in green taxi trips. By examining the relationships between features such as trip distance, fare amount, total amount, and tip amount, we gain a deeper understanding of tipping behavior and its drivers.

### Libraries Used

The following R libraries are utilized in this project:

- tidyr
- readr
- dplyr
- lubridate
- ggplot2
- FNN


## How to Use

1. **Requirements**: Ensure you have R installed on your system.
2. **Setup Environment**: Install the required libraries listed in the "Libraries Used" section.
3. **Run the Code**: Execute the R script to load the data, perform analysis, and visualize the results.
