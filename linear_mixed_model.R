# Set randomization seed; Load Libraries --------------------
set.seed(99)
library(dplyr)
library(ggplot2)
library(lme4)

# Load in data --------------------
load('Boston.RData')
colnames(Boston.data) <- c("Address", "Town", "Rent", "Bedrooms", "SqFt",
                           "Apt.or.House", "Utilities.Included", 
                           "Latitude", "Longitude", "Zipcode",
                           "State", "Num.Bedrooms")

# Remove rows with missing data, remove Bedrooms column because of redundancy --------------------
data <- Boston.data[complete.cases(Boston.data),]
data <- data %>% 
  select(-Bedrooms)
data$State <- as.factor(data$State)

# Refactor Town levels --------------------
top5 <- data %>% 
  group_by(Town) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))

top5 <- as.vector(top5$Town[1:5])
data <- data %>% 
  mutate(Town = ifelse(as.vector(Town) %in% top5 , as.vector(Town), "Others"))
data$Town <- as.factor(data$Town)


# Train/Test split --------------------
# Find all addresses that have single listings:
temp <- data %>% 
  group_by(Address) %>%
  summarize (count = n())
idx <- which(temp$count == 1)
single.address <- temp$Address[idx]

# Filter addresses with single listings from full data
# Then, split train/test from the rest of the data
# We still want ratio 70/30, so ntrain = nrow(data)*.07 - nrow(single)

single <- data %>%
  filter(Address %in% single.address)

multiple <- data %>%
  filter(!(Address %in% single.address))

scalar <- (0.7*nrow(data) - nrow(single))/(nrow(data) - nrow(single))

# make random number column
multiple$rand <- runif(nrow(multiple), 0, 1)
# group by address and get 70% of number at address as number to grab (round number down)
multiple <- multiple %>% 
  group_by(Address) %>% 
  mutate(n = floor(n()*scalar)) %>% 
  arrange(Address, rand)
# grab first n from above as training
train <- multiple %>% 
  group_by(Address) %>% 
  filter(row_number() <= n)
# remove n and random colum
train <- train %>% 
  select(-rand, -n)
# add back in addresses only occurring once
train <- bind_rows(single, train)
# grab rest as test
test <- multiple %>% 
  group_by(Address) %>% 
  filter(row_number() > n)
# remove n and random colum
test <- test %>% 
  select(-rand, -n)



# (1) Are errors not normally distributed? Justify LME.

# Baseline model --------------------
baseline.train <- train %>%
  select(-Address, -Zipcode)

baseline.test <- test %>%
  select(-Address, -Zipcode)

null <- lm(Rent ~ 1, data = baseline.train)
full <- lm(Rent ~ ., data = baseline.train)
baseline.model <- step(null, scope=list(lower = null, upper = full),
                       direction = 'both', trace = FALSE)

# Calculate RMSE
predictions <- predict(baseline.model, baseline.test)
baseline.rmse <- sqrt(mean((baseline.test$Rent - predictions)^2))
baseline.rmse



# Analysis of error --------------------
resid <- baseline.model$residuals

# Normal Q-Q Plot
qqnorm(resid, pch = 19)
qqline(resid)

# Histogram of error
resid.dat <- data.frame(resid)
ggplot(resid.dat) + 
  geom_histogram(aes(resid), 
                 fill = 'cornflowerblue', 
                 colour = 'white') + 
  labs(x = 'Residuals', y = 'Count', 
       title ='Baseline Linear Model Residuals\nFrom Model Trained on Train Data')

# Plot of residuals v. address
resid.dat <- cbind(step_data, resid)
resid.dat %>% 
  group_by(Address) %>% 
  mutate(n = n(), min_resid = min(resid), 
    max_resid = max(resid), mean_resid = mean(resid)) %>% 
  filter(n > 5) %>% 
  ungroup() %>% 
  filter(abs(mean_resid) > 1000) %>% 
  ggplot(aes(x = Address, y = resid)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(baseline.model)



# (2) Does a random intercept model improve performance?

# Fit mixed model with random intercept  --------------------
mixed.train <- train %>%
  select(-Zipcode, -Latitude, - Longitude)

mixed.test <- test %>%
  select(-Zipcode, -Latitude, - Longitude)

mixed.model <- lmer(Rent ~ Num.Bedrooms + 
                      Town + 
                      Apt.or.House + 
                      Utilities.Included + 
                      SqFt + 
                      State + 
                      (1 | Address), 
                    data = mixed.train, REML = FALSE)

test.predictions <- predict(mixed.model, newdata = mixed.test)
mixed.rmse <- sqrt(mean((mixed.test$Rent - test.predictions)^2))
