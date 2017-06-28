# Authors: Van Nguyen, Emma Kearney


# Load needed libraries
require(dplyr)

# Add HUD data. This is the data about fair market rent. Every property
# must be listed for rent at least the amount determined by HUD
hud.data <- data.frame(num.of.bed = c(0.5, 1, 2, 3, 4, 5), hud.rent = c(701, 844, 1057, 1322, 1568, 1568*1.15))

# Define file_path
file.path = './full_data.csv'
# Load the data
df <- read.csv(file.path)


# Define function clean.the.data to create new columns into the data frame.
# Combine two models for houses and apartments into one.
clean.the.data <- function(df){
  
  df <- df %>%
    mutate(bath.per.bed = num.of.bath/num.of.bed)
  df <- left_join(df, hud.data, by = 'num.of.bed')
  
  house <- df %>%
    filter(apt.or.house == 'House') %>%
    select(rent, num.of.bed, num.of.bath, 
           sqft, on.bus.route, utilities, 
           dist.amherst, hud.rent, bath.per.bed)
  house <- na.omit(house)
 
  
  apt <- df %>%
    filter(apt.or.house == 'Apartment') %>%
    select(rent, num.of.bed, num.of.bath, 
           sqft, on.bus.route, utilities, 
           dist.amherst, hud.rent, bath.per.bed)
  apt <- na.omit(apt)
 
  
  fit.apt <- lm(rent ~ hud.rent + 
                      hud.rent:num.of.bed + 
                      sqft + 
                      utilities +
                      num.of.bath +
                      on.bus.route,
                    data = apt, x = T, y = T)
  x.apt <- fit.apt$x
  y.apt <- fit.apt$y

  
  fit.house <- lm(rent ~ hud.rent*num.of.bed + #because hud.rent and num.of.bed interact
                        sqft +
                        dist.amherst, 
                      data = house, x = T, y = T)
  x.house <- fit.house$x
  y.house <- fit.house$y
  
  
  n1 <- dim(x.apt)[1]
  m1 <- dim(x.apt)[2]
  n2 <- dim(x.house)[1]
  m2 <- dim(x.house)[2]
  
  big.X <- matrix(0, nrow = n1 + n2, 
                  ncol = m1 + m2)
  
  big.X[1:n1, 1:m1] <- x.apt
  big.X[(n1 + 1):(n1 + n2), (m1 + 1):(m1 + m2)] <- x.house
  
  big.y <- c(y.apt, y.house)
  
  df <- data.frame(big.y, big.X)
  names(df) <- c("rent", 
                 paste(names(fit.apt$coefficients), '.apt', sep = ''),
                 paste(names(fit.house$coefficients), '.house', sep = ''))
  
  return(df)
}

output <- clean.the.data(df)

# Linear model to predict rent for houses and apartments
linear.model <- lm(rent~.-1, data = output)
