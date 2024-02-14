
# SET THE WORKING DIRECTORY -----------------------------------------------
setwd('~/Projects_in_R/Power_Of_Good_Design/')

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
  ggplot(aes(x= as.factor(year), y= gdp_percap, fill=continent))+
  geom_boxplot(position = 'dodge')+
  labs(
    title = 'A Boxplot for GDP of different continent for 1952 and 2007',
    x = 'years',
    y = 'GDP',
    fill='continent' # nolint: quotes_linter.
  )

