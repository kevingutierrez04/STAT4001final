#APPLICATION PROJECT
#Music Popularity Drivers: Why Do You Like the Music You Like?
#Based on Spotify Data
#GROUP 17
#Kevin Gutierrez Garcia
#Swati Negi
#Maria Garmonina

data <- read.csv("clean data.csv")
data_clean <- data[, c(1:1, 5:5, 7:23, 25:34)] #removes columns with text values
library(dplyr)
install.packages("MASS")
library(MASS)

#convert true/false to 1/0 and make sure that all values are numeric
data_clean1[] <- lapply(data_clean, function(x) {
  if (is.logical(x)) as.numeric(x) else x
})
data_clean2[] <- lapply(data_clean1, function(x) as.numeric(as.character(x)))
data_clean2[is.na(data_clean2)] <- 0
#split the data into the training and the testing datasets
n <- 2000
simulated_outcomes <- sample(1:11450, n, replace = FALSE)
testing0 <- data_clean2[simulated_outcomes, ]
training <- data_clean2[setdiff(seq_len(nrow(data_clean2)), simulated_outcomes), ]

#use Monte Carlo simulation to keep the distributions of the parameters similar and generate more testing values
set.seed(57) #for reproducibility
means <- colMeans(data_clean2, na.rm=TRUE)
stdevs <- apply(data_clean2, 2, sd, na.rm=TRUE)

num_simulations <- 1000
random_inputs <- matrix(
  rnorm(length(means)*num_simulations, mean = rep(means, each = num_simulations), sd = rep(stdevs, each = num_simulations)), ncol=length(means)
)
random_inputs_df <- as.data.frame(random_inputs)
colnames(random_inputs_df) <- colnames(data_clean2)
testing <- rbind(testing0, random_inputs_df)

training[] <- lapply(training, function(x) as.numeric(as.character(x)))
testing[] <- lapply(testing, function(x) as.numeric(as.character(x)))

mod1 <- lm(popularity ~., data = training)
summary(mod1)
#Adjusted R-squared: 0.3226, os our model is noisy and bad
cor(training)

#loudness and energy have a correlation of 0.7, so we will remove energy for mod2 because it has a larger p-value
mod2 <- lm(popularity ~. -energy, data = training)
summary(mod2)
#Adjusted R-squared: 0.3191

#now we remove soundtrack, which was not a significant variable for either mod1 or mod2
mod3 <- lm(popularity ~. -energy -soundtrack, data = training)
summary(mod3)
#Adjusted R-squared: 0.3192

#now we remove soul, which was not significant for any model
mod4 <- lm(popularity ~. -energy -soundtrack -soul, data = training)
summary(mod4)
#Adjusted R-squared: 0.3193

#now we remove tempo, which was not significant for any model
mod5 <- lm(popularity ~. -energy -soundtrack -soul -tempo, data = training)
summary(mod5)
#Adjusted R-squared: 0.3192

#now we remove key, which was not significant for any model
mod6 <- lm(popularity ~. -energy -soundtrack -soul -tempo -key, data = training)
summary(mod6)
#Adjusted R-squared: 0.3192

#now we remove the least significant variable - disco
mod7 <- lm(popularity ~. -energy -soundtrack -soul -tempo -key -disco, data = training)
summary(mod7)
#Adjusted R-squared: 0.3189

#now we remove the least significant variable - instrumentalness
mod8 <- lm(popularity ~. -energy -soundtrack -soul -tempo -key -disco -instrumentalness, data = training)
summary(mod8)
#Adjusted R-squared: 0.3183


#removing some correlations:
mod9 <- lm(popularity ~. -hip.hop -rap -energy, data=training)
summary(mod9)
#Adjusted R-squared: 0.3255

mod10 <- lm(popularity ~. -hip.hop -rap -energy -speechiness, data=training)
summary(mod10)
#Adjusted R-squared: 0.3225 

mod11 <- lm(popularity ~. -explicit -energy -speechiness, data=training)
summary(mod11)
#Adjusted R-squared: 0.3214

mod12 <- lm(popularity ~. -explicit -loudness -acousticness -speechiness, data=training)
summary(mod12)
#Adjusted R-squared: 0.29

mod13 <- lm(popularity ~. -hip.hop -rap -energy -speechiness -acousticness, data=training)
summary(mod13)
#Adjusted R-squared: 0.318

#going from mod9
mod14 <- lm(popularity ~. -hip.hop -rap -energy -tempo, data=training)
summary(mod14)
#Adjusted R-squared: 0.3255

mod15 <- lm(popularity ~. -hip.hop -rap -energy -tempo -soul, data=training)
summary(mod15)
#Adjusted R-squared: 0.3256

mod16 <- lm(popularity ~. -hip.hop -rap -energy -tempo -soul -key, data=training)
summary(mod16)
#Adjusted R-squared: 0.3256

mod17 <- lm(popularity ~. -hip.hop -rap -energy -tempo -soul -key -soundtrack, data=training)
summary(mod17)
#Adjusted R-squared: 0.3255

mod18 <- lm(popularity ~. -hip.hop -rap -energy -tempo -soul -key -soundtrack -disco, data=training)
summary(mod18)
#Adjusted R-squared: 0.3254


#Now let's see if our model would work better for subsets of the training set based on different number of followers of the artist
# Dividing the data based on principal artist followers
# Find the 25th, 50th, and 75th percentiles
percentiles <- quantile(training$principal.artist.followers, c(0.25, 0.50, 0.75), na.rm=TRUE)

# Define the percentiles for subsetting
percentile_25 <- percentiles[1]
percentile_50 <- percentiles[2]
percentile_75 <- percentiles[3]

# This works only if the data is a dataframe and not a list

subset_25 <- training %>% filter(principal.artist.followers <= percentile_25)
subset_50 <- training %>% filter(principal.artist.followers > percentile_25, principal.artist.followers <= percentile_50)
subset_75 <- training %>% filter(principal.artist.followers > percentile_50, principal.artist.followers <= percentile_75)
subset_100 <- training %>% filter(principal.artist.followers > percentile_75)

# Model 1 with the 25th percentile
model_25 <- lm(popularity ~ ., data=subset_25)
summary(model_25)
# Adjusted R-squared:  0.2281

# Model 2 with the 50th percentile
model_50 <- lm(popularity ~ ., data=subset_50)
summary(model_50)
# Adjusted R-squared:  0.2351

# Model 3 with the 75th percentile
model_75 <- lm(popularity ~ ., data=subset_75)
summary(model_75)
# Adjusted R-squared:  0.2728

# Model 4 with data between 75th and 100th percentile
model_100 <- lm(popularity ~ ., data=subset_100)
summary(model_100)
# Adjusted R-squared:  0.3281

# Let's play around with the last dataset and see if we can improve the model
model_100_1 <- lm(popularity ~. -acousticness -tempo -rap -jazz -disco, data=subset_100)
summary(model_100_1)
# Adjusted R-squared:  0.3288

model_100_2 <- lm(popularity ~. -acousticness -tempo -rap -jazz -disco -soundtrack -soul, data=subset_100)
summary(model_100_2)
# Adjusted R-squared:  0.329

model_100_3 <- lm(popularity ~. -acousticness -tempo -rap -jazz -disco -soundtrack -soul -release.month -liveness, data=subset_100)
summary(model_100_3)
# Adjusted R-squared:  0.3292

model_100_4 <- lm(popularity ~. -acousticness -tempo -rap -jazz -disco -soundtrack -soul -release.month -liveness -instrumentalness -key, data=subset_100)
summary(model_100_4)
# Adjusted R-squared:  0.3281

# As we can see the R^2 value decreases if we remove further variables, hence our model3 is the best performing with R^2 of 0.3292


#Let's see if our model works better for a specific genre
rock <- subset(training, training$rock==1)
pop <- subset(training, training$pop==1)
rap <- subset(training, training$rap==1)
hip_hop <- subset(training, training$hip.hop==1)
country <- subset(training, training$country==1)
soundtrack <- subset(training, training$soundtrack==1)
latin <- subset(training, training$latin==1)
jazz <- subset(training, training$jazz==1)
disco <- subset(training, training$disco==1)
soul <- subset(training, training$soul==1)

mod1rock <- lm(popularity ~., data = rock)
summary(mod1rock)
#Adjusted R-squared: 0.2244

mod1pop <- lm(popularity ~., data = pop)
summary(mod1pop)
#Adjusted R-squared: 0.3523

mod1rap <- lm(popularity ~., data = rap)
summary(mod1rap)
#Adjusted R-squared: 0.3245

mod1hiphop <- lm(popularity ~., data = hip_hop)
summary(mod1hiphop)
#Adjusted R-squared: 0.257

mod1country <- lm(popularity ~., data = country)
summary(mod1country)
#Adjusted R-squared: 0.3758

mod1soundtrack <- lm(popularity ~., data = soundtrack)
summary(mod1soundtrack)
#Adjusted R-squared: 0.1472

mod1latin <- lm(popularity ~., data = latin)
summary(mod1latin)
#Adjusted R-squared: 0.4345

mod1jazz <- lm(popularity ~., data = jazz)
summary(mod1jazz)
#Adjusted R-squared: 0.3585

mod1disco <- lm(popularity ~., data = disco)
summary(mod1disco)
#Adjusted R-squared: 0.4219

mod1soul <- lm(popularity ~., data = soul)
summary(mod1soul)
#Adjusted R-squared: 0.4127

#Using all the parameters, we got the highest adjusted R^2 for latin, so now let's play around with this set
#First of all let's remove the genres that give NA in our summary
mod2latin <- lm(popularity ~. -country -soundtrack -latin -disco -soul, data = latin)
summary(mod2latin)
#As expected, our adjusted R^2 value doesn't change

#Now let's remove the least siginificant varible judging by the p-values - release month
mod3latin <- lm(popularity ~. -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod3latin)
#Adjusted R-squared: 0.4356, it increased a little bit

#Let's remove the next least significant variable - instrumentalness
mod4latin <- lm(popularity ~. -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod4latin)
#Adjusted R-squared: 0.4368, it increased a little bit

#Let's remove the next least significant variable - jazz
mod5latin <- lm(popularity ~. -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod5latin)
#Adjusted R-squared: 0.4378, it increased a little bit

#Let's remove the next least significant variable - explicit
mod6latin <- lm(popularity ~. -explicit -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod6latin)
#Adjusted R-squared: 0.4386, it increased a little bit

#Let's remove the next least significant variable - key
mod7latin <- lm(popularity ~. -key -explicit -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod7latin)
#Adjusted R-squared: 0.4392, it increased a little bit

#Let's remove the next least significant variable - liveness
mod8latin <- lm(popularity ~. -liveness -key -explicit -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod8latin)
#Adjusted R-squared: 0.4396, it increased a little bit

#Let's remove the next least significant variable - energy
mod9latin <- lm(popularity ~. -energy -liveness -key -explicit -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod9latin)
#Adjusted R-squared: 0.4397, it increased a little bit

#Let's remove the next least significant variable - valence
mod10latin <- lm(popularity ~. -valence -energy -liveness -key -explicit -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod10latin)
#Adjusted R-squared: 0.4399, it increased a little bit

#Let's remove the next least significant variable - valence
mod11latin <- lm(popularity ~. -pop -valence -energy -liveness -key -explicit -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod11latin)
#Adjusted R-squared: 0.4401, it increased a little bit

#Let's remove the next least significant variable - mode
mod12latin <- lm(popularity ~. -mode -pop -valence -energy -liveness -key -explicit -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod12latin)
#Adjusted R-squared: 0.4399, it decreased a little bit

#Let's keep the mode and remove speechiness instead
mod13latin <- lm(popularity ~. -speechiness -pop -valence -energy -liveness -key -explicit -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary(mod13latin)
#Adjusted R-squared: 0.4393, it decreased a little bit

#A more fair model - without the number of followers, which an artist cannot directly influence, although it is one of the significant variables
mod14latin <- lm(popularity ~. -principal.artist.followers -pop -valence -energy -liveness -key -explicit -jazz -instrumentalness -release.month -country -soundtrack -latin -disco -soul, data = latin)
summary (mod14latin)
#Adjusted R-squared: 0.4218. It is less than in mod11latin, but more useful for young musicians


#Let's play around with disco
mod1disco <- lm(popularity ~. -disco, data = disco)
summary(mod1disco)
#0.4219

mod2disco <- lm(popularity ~. -explicit -rap -hip.hop -country -soundtrack -latin -disco, data = disco)
summary(mod2disco)
#0.4219

mod3disco <- lm(popularity ~. -album.total.tracks -instrumentalness -speechiness -tempo -soul -explicit -rap -hip.hop -country -soundtrack -latin -disco, data = disco)
summary(mod3disco)
#0.522

mod4disco <- lm(popularity ~. -key -track.number -jazz -danceability -duration.min -rock -album.total.tracks -instrumentalness -speechiness -tempo -soul -explicit -rap -hip.hop -country -soundtrack -latin -disco, data = disco)
summary(mod4disco)
#0.5714. this is our best model

mod5disco <- lm(popularity ~. -principal.artist.followers -key -track.number -jazz -danceability -duration.min -rock -album.total.tracks -instrumentalness -speechiness -tempo -soul -explicit -rap -hip.hop -country -soundtrack -latin -disco, data = disco)
summary(mod5disco)
#0.5488. this is a fairer one


#Let's play around with soul
mod1soul <- lm(popularity ~. -soul, data = soul)
summary(mod1soul)


#Now that we have identified our best fits (mod16, mod4disco and mod5disco), we can move to testing our model
pred_mod1 <- predict(mod1, newdata=testing)
mae_mod1 <- mean(abs(pred_mod1 - testing$popularity)) #mean absolute error
mse_mod1 <- mean((pred_mod1 - testing$popularity)^2) #mean squared error
mae_mod1
mse_mod1

pred_mod16 <- predict(mod16, newdata=testing)
mae_mod16 <- mean(abs(pred_mod16 - testing$popularity)) #mean absolute error
mse_mod16 <- mean((pred_mod16 - testing$popularity)^2) #mean squared error
mae_mod16
mse_mod16

discotest <- subset(testing, testing$disco==1)
pred_mod4disco <- predict(mod4disco, newdata=discotest)
mae_mod4disco <- mean(abs(pred_mod4disco - discotest$popularity)) #mean absolute error
mse_mod4disco <- mean((pred_mod4disco - discotest$popularity)^2) #mean squared error
mae_mod4disco
mse_mod4disco

pred_mod5disco <- predict(mod5disco, newdata=discotest)
mae_mod5disco <- mean(abs(pred_mod5disco - discotest$popularity)) #mean absolute error
mse_mod5disco <- mean((pred_mod5disco - discotest$popularity)^2) #mean squared error
mae_mod5disco
mse_mod5disco

foltest <- testing %>% filter(principal.artist.followers > percentile_75)
pred_mod100_3 <- predict(model_100_3, newdata=foltest)
mae_mod100_3 <- mean(abs(pred_mod100_3 - foltest$popularity)) #mean absolute error
mse_mod100_3 <- mean((pred_mod100_3 - foltest$popularity)^2) #mean squared error
mae_mod100_3
mse_mod100_3

# Taylor Swift part
# We want to test if Taylor Swift's songs are as popular as her. We hypothesize that her songs's popularity is less than 90
# H_0: taylor_swift$popularity >= 90.  H_a: taylor_swift$popularity < 90
taylor_swift <- data %>% filter(grepl("Taylor Swift", artists.names, ignore.case = TRUE))
sample_mean <- mean <- mean(taylor_swift$popularity)
population_sd <- sd(data$popularity)
sample_size <- length(taylor_swift$popularity)
null_mean <- 90
z_stat <- (sample_mean - null_mean) / (population_sd / sqrt(sample_size))
# Based on 95% confidence interval, the z value in z-table is 1.65
z_critical <- 1.65 
z_stat < -z_critical
# This is true, hence we can reject the null hypothesis and conclude that Taylor Swift's songs are not more popular than 90