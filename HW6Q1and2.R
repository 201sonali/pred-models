rm(list=ls())
# Load libraries
library(ggplot2)
library(gridExtra)
library(sf)
library(sp)
library(spdep)
library(spatialreg)
library(rgdal)

# Load the crime_dat sf object
load("C:/Users/Sonali Singh/Documents/MSDS/predictive models/crimedata.RData")

colors <- c("darkseagreen3", "lightsalmon2", "lightskyblue3", "thistle3")

# PART A

# Map of Census Tract
ggplot(data = crime_dat) +
  geom_sf(fill = "lightsalmon2", color="white") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Map of Census Tracts in Franklin County, OH') 

# Poverty vs Crime
ggplot(data=crime_dat, aes(x=poverty, y=crime)) +
  geom_point(color="lightsalmon2") +
  geom_smooth(method="lm", color="lightskyblue3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Poverty vs Crime') 


# PART B


# PART C

# Distribution of the original variables 
crime_plot <- ggplot(data = crime_dat) +
  geom_histogram(aes(x = crime), bins = 20, color="white", fill="darkseagreen3", binwidth=10) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Original Crime Distribution') 

pov_plot <- ggplot(data = crime_dat) +
  geom_histogram(aes(x = poverty), bins = 20, color="white", fill="darkseagreen3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Original Poverty Distribution') 

# Because skewed, take the log
crime_dat$logcrime <- log(crime_dat$crime)

# Distribution of the log variables
logcrime_plot <- ggplot(data = crime_dat) +
  geom_histogram(aes(x = logcrime), bins = 20, color="white", fill="thistle3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Log Crime Distribution') 

# Plot all
grid.arrange(crime_plot,
             logcrime_plot,
             pov_plot,
             nrow = 2,
             respect = TRUE)

# Check/remove NA values
sum(is.na(crime_dat$crime))
sum(is.na(crime_dat$poverty))

# Map
crime_spatial <- ggplot(data = crime_dat) +  
  geom_sf(aes(fill = logcrime)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  labs(title = 'Spatial Variation in Crime') 

# Map
pov_spatial <- ggplot(data = crime_dat) +  
  geom_sf(aes(fill = poverty)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  labs(title = 'Spatial Variation in Poverty') 

grid.arrange(crime_spatial,
             pov_spatial,
             nrow = 1,
             respect = TRUE)




# Construct neighborhood list
nb <- poly2nb(crime_dat) # default is queen's case, good for irregular shapes

# Plot neighbors via centroids
nb_lines <- nb %>%
  nb2lines(coords = coordinates(as(crime_dat, "Spatial"))) %>%
  as("sf") %>%
  st_set_crs(st_crs(crime_dat))

ggplot(data = crime_dat) +
  geom_sf(fill = "white", color = "lightgrey") + 
  geom_sf(data = nb_lines, col = "lightskyblue3") +
  labs(title = "Adjacent Census Tracts") 


# Part E

# Look for number of regions with no links
nb 

# Extract island number with no links

island = c()

for (i in 1:length(nb)) {
  if (length(nb[i][[1]])==1) {
    if (nb[i][1]==("0")) {
      island <- append(island, i)
    }}}

# Identify island and check you only have 1
island
length(island)

# Extract centroid coordinates of island
x = st_centroid(crime_dat$geometry[island])[[1]][1]
y = st_centroid(crime_dat$geometry[island])[[1]][2]

x
y

# Plot centroid on previous map
ggplot(data = crime_dat) +
  geom_sf(fill = "white", color = "lightgrey") + 
  geom_sf(data = nb_lines, col = "lightskyblue3") +
  labs(title = "Adjacent Census Tracts") +
  geom_point(x=x, y=y, size=2, col="lightsalmon2")


# Set color pallet
colors <- c("darkseagreen3", "lightsalmon2", "lightskyblue3", "thistle3")


# Part F

# Removing islands and crime rates of 0
crime_dat <- crime_dat[-195,]
crime_dat <- crime_dat[crime_dat$crime!=0,]

# Recreating new nb object
nb <- poly2nb(crime_dat) 

# Constructing binary adjacency matrix
W_mat <- nb2listw(nb, 
                  style = 'B', # B for binary matrix
                  zero.policy = T) 

# Spatial Moving Average Model
fit_SMA <- spautolm(logcrime ~ poverty, #instead of ~1, include covariate
                    listw = W_mat,
                    family = "SMA", # assumes simulataneous moving average on error, not the data itself
                    data = crime_dat)   # because you think the error itself has a trend you want to see
summary(fit_SMA)
# so we fit a spatial model because there's spatial dependence
# pval still statistically sig

# Residuals for SMA
crime_dat$resids_SMA <- residuals(fit_SMA) #fitted SMA residuals
ggplot(data = crime_dat) +
  geom_sf(aes(fill = resids_SMA)) +
  labs(title = "SMA Residuals")
# isn't much spatial dependence compared to before, formalize with Moran's I

# Moran's I for SMA
moran.mc(crime_dat$resids_SMA, 
         W_mat, 
         1000, 
         zero.policy = T)
# Good pval!!!! --> SMA address spatial dependence in errors and is giving us better inferences on regression coefficients

AIC(fit_SMA)

# Part G

# Non-spatial linear model
fit_ns <- lm(logcrime ~ poverty, data = crime_dat)
summary(fit_ns)
# poverty is statistically significant bc small p value

# Residuals for Non-spatial
crime_dat$resids_ns <- fit_ns$residuals
ggplot(data = crime_dat) +
  geom_sf(aes(fill = resids_ns)) +
  labs(title = "NS Residuals")
# Checking that assumption of the model hold, specifically that the errors are iid 
# residuals are estimates of these errors
# one way these could be violated in spatial dependence, could formally be checked w/ hypothesis test

# Moran's I for Non-spatial
moran.mc(crime_dat$resids_ns, 
         W_mat, 
         1000, 
         zero.policy = T)
# stat is not that meaningful, but the pval is small so we:
# 1. reject the null 
# 2. say there is spatial autocorrelation
# 3. fit SMA model



# Part H

# Map of Census Tract
ggplot(data = crime_dat) +
  geom_sf(fill = "thistle", color="white") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(title = 'Map of Census Tracts in Franklin County, OH') +
  geom_point(x=-83.019707, y=40.001633)

st_contains(x=-83.019707, y=40.001633)

crime_dat

# Rough estimate of where the tract may be, narrow down
stadium = c()

for (i in 1:nrow(crime_dat)) {
   x = st_centroid(crime_dat$geometry[i])[[1]][1]
     if (x < -83 & x > -83.1) {
        y = st_centroid(crime_dat$geometry[i])[[1]][2]
        if (y < 40.01 & y > 39.98){
            stadium <- append(stadium, i)
        }}}

# Iterate through the stadium list and plot the centroid, figure out it's 21
stadium 

# Can use this to plot on ggplot and double check

x = st_centroid(crime_dat$geometry[21])[[1]][1]
y = st_centroid(crime_dat$geometry[21])[[1]][2]
x
y


# Fill by if the tract contains Ohio Stadium
crime_dat$stadium <- rep("F", times=nrow(crime_dat))
crime_dat$stadium[21] = "T"

# Plot Ohio Stadium 
ggplot(data = crime_dat) +
  geom_sf(aes(fill = stadium), color="white", show.legend=FALSE) +
  geom_point(x=-83.019707, y=40.001633, size=2) +
  geom_label(x=-83.019707, y=40.001633, aes(label="Ohio Stadium, Tract 21"), hjust=-0.05, vjust=-0.1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  labs(title = 'Tract with Ohio Stadium')+
  scale_fill_manual(values = colors)


fit_ns <- lm(logcrime ~ poverty, data = crime_dat)

fit_SMA <- spautolm(logcrime ~ poverty, 
                    listw = W_mat,
                    family = "SMA", 
                    data = crime_dat)

summary(fit_ns)
summary(fit_SMA)

stadium_poly <- crime_dat$geometry[21]

st_intersects(st_point())

pred_CAR <- fit_CAR$fit

fit_SMA$fit$fitted.values[21]
fit_SMA$fitted.values[21]

fit_ns$fitted.values[21]

$fitted.values[21]

y1 = 1.999317 + (0.045886 * crime_dat$poverty[21]) #SMA
y1

y2 = 1.918417 + (0.057356 * crime_dat$poverty[21]) #NONSPATIAL
y2

crime_dat$poverty[21]
crime_dat$logcrime[21]
crime_dat$crime[21]

