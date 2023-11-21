# QDA
COVID-19 Data Analysis and Visualization:
This repository contains code for analyzing and visualizing COVID-19 data focusing on England's boroughs, covering various themes such as dwelling, health, economics, and age factors.

Dataset and Setup:
The initial steps involve importing the COVID-19 dataset (Covid_data.csv) and exploring its structure using R programming.

Preprocessing and Visualization:
The code performs initial data preprocessing steps, including handling missing values and examining the dataset's structure using tools like Amelia, funModeling, tidyverse, Hmisc, and ggplot2 for visualization.

Data Transformation and Normalization:
To prepare the data for analysis, various transformations are applied, including converting attributes into proportions and scaling methods such as Min-Max Scaling, Z-Score, and SoftMax Scaling. Outlier replacement functions are used to address extreme values.

Statistical Analysis:
The code conducts statistical tests to evaluate normal distribution among variables, using methods like the Kolmogorov-Smirnov (KS) test. Visualization tools like histograms and correlation matrices aid in understanding data relationships.

Correlation Analysis and Regression:
The code explores correlations between dependent and independent variables, employing Pearson and partial correlation tests. It further investigates regression analysis, including Linear Regression and Poisson Regression, to elucidate relationships between COVID-19 deaths and various factors.

Visualization:
The code generates several visualizations, including scatterplots and bar charts, to depict relationships between COVID-19 deaths and factors like age, health, dwelling types, and economic activities across different areas in England.

City-wise Death Distribution:
Visual representations categorize areas based on COVID-19 death counts, providing insights into low, medium, high, and very high death zones within England.

Conclusion:
The analysis and visualizations offer valuable insights into the factors influencing COVID-19 deaths in different regions of England.
For detailed code implementation and analysis, refer to the provided R script.
