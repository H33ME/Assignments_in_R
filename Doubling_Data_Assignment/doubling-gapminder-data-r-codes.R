# SET THE WORKING DIRECTORY -----------------------------------------------
setwd("~/Projects_in_R/Doubling_Data_Assignment/")

# LOAD REQUIRED PACKAGES --------------------------------------------------

library(tidyverse)
library(gapminder)

# LOAD THE DATA -----------------------------------------------------------

gapminder_data <- gapminder %>%
  # factor the character variables
  mutate_if(is.character, as.factor) %>%
  # drop missing values
  drop_na() %>%
  # clean the names
  janitor::clean_names()
gapminder_data %>%
  slice_min(life_exp, n=5)

gapminder_data %>%
  slice_max(life_exp, n=1)


# initial plots created in the previous assignment
# DATA VISUALIZATIONS USING POPULATION AND LIFE EXPCTATION FOR DIFFERENT CONTINENT  ----------------------------------------------

gapminder_data %>%
  ggplot(aes(x=life_exp, y=pop, color=continent))+
  geom_point()+
  labs(
    title='A SCATTERPLOT FOR POPULATION AND LIFE EXPECTATION FOR DIFFERENT CONTINENT',
    x = 'LIFE EXPECTATION',
    y='POPULATION',
    fill='CONTINENTS'
  )

# DATA VISUALIZATION USING THE GDP OF DIFFERENT CONTITNENT FOR 1952 AND 2007 -------------------------------------------------------------------------
gapminder_data %>%
  filter(year %in% c(1952, 2007)) %>%
  ggplot(aes(x= as.factor(year), y = gdp_percap, fill = continent)) +
  geom_boxplot(position = 'dodge') +
  labs(
    title = 'A Boxplot for GDP of different continent for 1952 and 2007',
    x = 'years',
    y = 'GDP',
    fill='continent'
  )

# set seeds for productivity
set.seed(123)
# doubling the data using the funciton below
double_and_shuffle_data <- function(original_data) {
  # Get the number of rows in the original data
  num_rows <- nrow(original_data)

  # Initialize an empty data frame to store the doubled and reshuffled data
  doubled_data <- data.frame(matrix(ncol = ncol(original_data), nrow = num_rows * 2))

  # Assign column names to the new data frame
  colnames(doubled_data) <- colnames(original_data)

  # Use a for loop to shuffle values in each variable
  for (col in colnames(original_data)) {
    # Combine the original variable values with their shuffled versions
    shuffled_values <- sample(c(original_data[[col]], original_data[[col]]))

    # Assign the shuffled values to the new data frame
    doubled_data[[col]] <- shuffled_values
  }

  return(doubled_data)
}

# apply the function to the gapminder_data
doubled_gapminder_data <- double_and_shuffle_data(gapminder_data)

# recreating the plots
doubled_gapminder_data %>%
  ggplot(aes(x=life_exp, y=pop, color=continent)) +
  geom_point() +
  labs(
    title='A SCATTERPLOT FOR POPULATION AND LIFE EXPECTATION FOR DIFFERENT CONTINENT',
    subtitle="This scatterplot was made from the doubled gapminder data",
    x = 'LIFE EXPECTATION',
    y='POPULATION',
    fill='CONTINENTS'
  )

# DATA VISUALIZATION USING THE GDP OF DIFFERENT CONTITNENT FOR 1952 AND 2007 -------------------------------------------------------------------------
doubled_gapminder_data %>%
  filter(year %in% c(1952, 2007)) %>%
  ggplot(aes(x= as.factor(year), y = gdp_percap, fill = continent)) +
  geom_boxplot(position = 'dodge') +
  labs(
    title = 'A Boxplot for GDP of different continent for 1952 and 2007',
    subtitle="This boxplot was made from the doubled gapminder data",
    x = 'years',
    y = 'GDP',
    fill='continent'
  )
