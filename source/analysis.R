library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Data Summary
#----------------------------------------------------------------------------#

trends <- read.csv('/Users/nathanlee/Documents/info201/assignments/a4-nlee777/incarceration_trends.csv')

# gets total number of observations
get_observations <- function() {
  return(nrow(trends))
}


#returns the average percent of white female prisoners in comparison to all female prisoners
get_white_female_prison_ratio <- function() {
  pct <- mean(trends$white_female_prison_pop, na.rm=TRUE) / mean(trends$female_prison_pop, na.rm=TRUE) * 100
  return(round(pct, 2))
}

#returns the average percent of black female prisoners in comparison to all female prisoners
get_black_female_prison_ratio <- function() {
  pct <- mean(trends$black_female_prison_pop, na.rm=TRUE) / mean(trends$female_prison_pop, na.rm=TRUE) * 100
  return(round(pct, 2))
}

#returns the average percent of latinx male prisoners in comparison to all male prisoners
get_latinx_male_prison_ratio <- function() {
  pct <- mean(trends$latinx_male_prison_pop, na.rm=TRUE) / mean(trends$male_prison_pop, na.rm=TRUE) * 100
  return(round(pct, 2))
}



## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# Returns dataframe detailing jail population by year
get_year_jail_pop <- function() {
  df <- trends %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarise(total = sum(total_jail_pop, na.rm=TRUE))
  return(df)   
}

# Plots bar plot of the US prison population by year
plot_jail_pop_for_us <- function()  {
  df <- get_year_jail_pop()
  p <- ggplot(data=df, aes(x=year, y=total)) +
    geom_bar(stat="sum") + 
    theme(legend.position = "none") +
    labs(
      title = "Growth of the U.S Prison Population",
      x = "Year",
      y = "Total Jail Population",
      caption = "Figure 1: U.S. Prison Population Growth"
    )
  library(scales)
  p <- p + scale_y_continuous(labels = label_comma())
  return(p)   
} 






## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# See Canvas
#----------------------------------------------------------------------------#

# filters data given a vector of states, sorts by year, and sums jail populations
get_pop_by_states <- function(states) {
  df <- trends %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarise(state_pop = sum(total_jail_pop, na.rm=TRUE))
  return(df)
}

# this function returns a line plot of each states prison growth, given a vector of states
plot_pop_by_states <- function(states) {
  df <- get_pop_by_states(states)
  p <- ggplot(df) + 
    geom_smooth(aes(x=year, y=state_pop, color=state), se=F) +
    labs(
      title = "Growth of Prison Population by States",
      x = "Year",
      y = "Total Jail Population",
      caption = "Figure 2: U.S. Prison Population Growth by Specific States"
    )
  return(p)
}








## Section 5  ---- 

#filters and returns a dataframe of maximum white and black prisoners in each year
get_male_female_pop <- function() {
  df <- trends %>%
    group_by(year) %>%
    summarise(
      female_pop_sum = max(female_jail_pop, na.rm=TRUE),
      male_pop_sum = max(male_jail_pop, na.rm=TRUE)
    ) %>%
    select(year, male_pop_sum, female_pop_sum)
  return(df)
}



#this function returns a plot of two lines representing male and female prison pop growth. 
plot_male_female_pop <- function() {
  df <- get_male_female_pop()
  p <- ggplot(df) +
    geom_smooth(aes(x=year, y=male_pop_sum, color="male"), se=F) + 
    geom_smooth(aes(x=year, y=female_pop_sum, color="female"), se=F) + 
    labs(
      title = "Male Vs. Female Prison Population Growth",
      x = "Year",
      y = "Highest Jail Population",
      caption = "Figure 3: U.S. Prison Population Growth by Male and Female Genders",
      color = "Gender"
    )
  return(p)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#

#this function returns a filtered df, that subsets the data only to 2016 data
get_2016_us_data <- function() {
  df <- trends %>%
    filter(year == 2016)
  return(df)
}

#this function returns a heatmap plot of the U.S that shows Latinx prison populations in each county
plot_2016_us <- function() {
  library(maps)
  df <- get_2016_us_data()
  state_shape <- map_data("county") %>% 
    unite(polyname, region, subregion, sep = ",") %>%
    left_join(county.fips, by = "polyname")
  county_shape <- state_shape %>%
    left_join(df, by = "fips")
  
  p <- ggplot(county_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = latinx_prison_pop),
      color = "white",
      size = .1        
    ) + 
    labs(
      title = "Map of U.S Latinx Prison Populations in 2016",
      x = "",
      y = "",
      caption = "Figure 4: Latinx Prison Populations Across The Country",
      fill = "Latinx Prison Population"
    )
  return(p)
}

plot_2016_us()
