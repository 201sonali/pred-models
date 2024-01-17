# load libraries
library(ggplot2)
library(mapproj)
library(geoR)
library(leaflet)
library(gridExtra)

# Load rain data
setwd("~/MSDS/predictive models")
rain <- read.table("rain.txt",header=T) 

# Convert feet to miles
rain$altitude <- rain$altitude/5280


# PART A

# Checking the distribution of both random variables
ggplot() + 
  geom_histogram(data = rain, aes(x = rainfall), color="white", fill="palegreen4") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Distribution of Rainfall') 

ggplot() + 
  geom_histogram(data = rain, aes(x = altitude), color="white", fill="palegreen4") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Distribution of Altitude') 


# Spatial variation of both random variables
ggplot(rain, aes(x = x, y = y)) +
  geom_point(size=3, aes(color = rainfall)) +
  theme_minimal() +
  scale_color_gradient(low = "cornflowerblue", high = "brown1") +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Spatial Variation of Rainfall') 

# It looks like there is some spatial dependence in the sense that points nearby each each have similar
# rainfall amounts. In other words, areas with higher rainful, specifically the pink points, have higher 
# rainful points/areas close to them. The same logic follows  for areas with lower rainfall. However, 
# there does not seem to be one area where there is more or less rain overall.

ggplot(rain, aes(x = x, y = y)) +
  geom_point(size=3, aes(color = altitude)) +
  theme_minimal() +
  scale_color_gradient(low = "cornflowerblue", high = "brown1") +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Spatial Variation of Altitude')  

# The altitudes seem to have more apparent spatial dependence, in the sense that there are 
# noticably more high altitudes grouped together in the bottom right hand corner of the plot,
# specifically corresponding to high x values and low y values. There also does seem to be
# a correlation between points nearby each other having similar altitudes, just as with the rainfall.


# PART B

rain$rainfallsqrt <- sqrt(rain$rainfall)
model1 <- lm(rainfallsqrt ~ altitude, data = rain, na.action = na.omit)
summary(model1)
4.275**2
# The estimated regression equation is y=14.1273-(8.3681*altitude)
# The estimated error variance is 18.27563.
# The proportion of variation in the square root of rainfall that is explained by altitude is 0.02157, 
# shown by the adjusted R-squared.


# PART C

distmatrix <- dist(rain, method = "euclidean", upper = F, diag = T)
distmatrix

distances <- c()
x1 <- c()
y1 <- c()
x2 <- c()
y2 <- c()
pred_rain1 <- c()
pred_rain2 <- c()
rain1 <- c()
rain2 <- c()
count <- 1
rain$row = 1:100

for (r in 1:99) {
  for (n in r:99){
    distances <- append(distances, distmatrix[count])
    x1 <- append(x1, rain$x[rain$row==(r)])
    y1 <- append(y1, rain$y[rain$row==(r)])
    x2 <- append(x2, rain$x[rain$row==(1+n)])
    y2 <- append(y2, rain$y[rain$row==(1+n)])
    pred_rain1 <- append(pred_rain1, 14.1273-(8.3681*(rain$altitude[rain$row==(r)])))
    pred_rain2 <- append(pred_rain2, 14.1273-(8.3681*(rain$altitude[rain$row==(1+n)])))
    rain1 <- append(rain1, rain$rainfallsqrt[rain$row==(r)])
    rain2 <- append(rain2, rain$rainfallsqrt[rain$row==(1+n)])
    count <- count+1
  }}

ggplot() +      
  geom_histogram(aes(distances), bins=20, boundary=0, fill="thistle3", color="white") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Relative Frequency of Euclidean Distances') 


# PART D

df <- data.frame(x1, y1, x2, y2, distances, pred_rain1, pred_rain2, rain1, rain2)

df$residuals1 <- df$rain1-df$pred_rain1
df$residuals2 <- df$rain2-df$pred_rain2

cor_coeff <- c()
bin_length <- c()

df1 <- df[df$distances<20,]
cor_coeff <- append(cor_coeff, cor(df1$residuals1, df1$residuals2))
bin_length <- append(bin_length, nrow(df1))

df2 <- df[df$distances>=20 & df$distances<40,]
cor_coeff <- append(cor_coeff, cor(df2$residuals1, df2$residuals2))
bin_length <- append(bin_length, nrow(df2))

df3 <- df[df$distances>=40 & df$distances<60,]
cor_coeff <- append(cor_coeff, cor(df3$residuals1, df3$residuals2))
bin_length <- append(bin_length, nrow(df3))

df4 <- df[df$distances>=60 & df$distances<80,]
cor_coeff <- append(cor_coeff, cor(df4$residuals1, df4$residuals2))
bin_length <- append(bin_length, nrow(df4))

df5 <- df[df$distances>=80 & df$distances<100,]
cor_coeff <- append(cor_coeff, cor(df5$residuals1, df5$residuals2))
bin_length <- append(bin_length, nrow(df5))

df6 <- df[df$distances>=100 & df$distances<120,]
cor_coeff <- append(cor_coeff, cor(df6$residuals1, df6$residuals2))
bin_length <- append(bin_length, nrow(df6))

df7 <- df[df$distances>=120 & df$distances<140,]
cor_coeff <- append(cor_coeff, cor(df7$residuals1, df7$residuals2))
bin_length <- append(bin_length, nrow(df7))

df8 <- df[df$distances>=140 & df$distances<160,]
cor_coeff <- append(cor_coeff, cor(df8$residuals1, df8$residuals2))
bin_length <- append(bin_length, nrow(df8))

df9 <- df[df$distances>=160 & df$distances<180,]
cor_coeff <- append(cor_coeff, cor(df9$residuals1, df9$residuals2))
bin_length <- append(bin_length, nrow(df9))

df10 <- df[df$distances>=180 & df$distances<200,]
cor_coeff <- append(cor_coeff, cor(df10$residuals1, df10$residuals2))
bin_length <- append(bin_length, nrow(df10))

df11 <- df[df$distances>=200 & df$distances<220,]
cor_coeff <- append(cor_coeff, cor(df11$residuals1, df11$residuals2))
bin_length <- append(bin_length, nrow(df11))

df12 <- df[df$distances>=220 & df$distances<240,]
cor_coeff <- append(cor_coeff, cor(df12$residuals1, df12$residuals2))
bin_length <- append(bin_length, nrow(df12))

df13 <- df[df$distances>=240 & df$distances<260,]
cor_coeff <- append(cor_coeff, cor(df13$residuals1, df13$residuals2))
bin_length <- append(bin_length, nrow(df13))

df14 <- df[df$distances>=260 & df$distances<280,]
cor_coeff <- append(cor_coeff, cor(df14$residuals1, df14$residuals2))
bin_length <- append(bin_length, nrow(df14))

df15 <- df[df$distances>=280 & df$distances<300,]
cor_coeff <- append(cor_coeff, cor(df15$residuals1, df15$residuals2))
bin_length <- append(bin_length, nrow(df15))

bin_center <- c()

for (n in 1:15) {
  bin_center <- append(bin_center, (n*20)/2)
}

residual_df <- data.frame(cor_coeff, bin_center, bin_length)
residual_df

ggplot(residual_df, aes(x=bin_center, y=cor_coeff)) +
  geom_point(aes(color=bin_length), size=3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Correlation Between Bin Residuals') 




# PART F

rain_geo <- as.geodata(
   cbind(rain$rainfallsqrt, # variable of interest
         rain$x,           # x and y in a meaningful sense aka after projection
         rain$y,
         rain$altitude), 
   covar.col = 4, 
   data.col = 1, 
   coords.col = 2:3)



# estimate the parameters of an exponential covariance function using MLE
model2 <- likfit(
   rain_geo,
   cov.model = "exponential",
   ini.cov.pars = c(5, .01), # starting values, what we're interested in estimating
   fix.nugget=TRUE, # want to fix at 0
   nugget=0,
   trend=~covar1) # starting value


summary(model2)

phi <- model2$cov.pars[2]

cor_coeff_fitted <- c()
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df1$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df2$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df3$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df4$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df5$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df6$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df7$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df8$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df9$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df10$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df11$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df12$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df13$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df14$distances)*-1/phi))
cor_coeff_fitted <- append(cor_coeff_fitted, exp(mean(df15$distances)*-1/phi))

residual_df$cor_coeff_fitted <- cor_coeff_fitted

ggplot(residual_df, aes(x=bin_center)) +
   geom_point(aes(y=cor_coeff), color="thistle3", size=3) +
   geom_point(aes(y=cor_coeff_fitted), color="seagreen4", size=3) +
   theme_minimal() +
   theme(plot.title = element_text(hjust=0.5), legend.position = "left") +
   labs(title = 'Correlation Between Bin Residuals')


residual_df$cor_coeff_abs <- abs(residual_df$cor_coeff)

ggplot(residual_df, aes(x=bin_center)) +
  geom_point(aes(y=cor_coeff_abs), color="thistle3", size=3) +
  geom_point(aes(y=cor_coeff_fitted), color="seagreen4", size=3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5), legend.position = "left") +
  labs(title = 'Correlation Between Bin Residuals')


