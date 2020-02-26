## https://mgimond.github.io/Spatial/point-pattern-analysis-in-r.html

## Point pattern analysis in R ##

# Most point pattern analysis tools are available in the spatstat package. 
  # These tools are designed to work with points stored as ppp objects and not 
  # SpatialPointsDataFrame objects. A ppp may or may not have attribute information 
  # (also referred to as marks). Knowing whether or not a function requires that an 
  # attributes table be present in the ppp object matters if a the operation is to 
  # complete successfully.

# In the following chunk of code, a point feature of Starbucks stores in Massachusetts 
  # is downloaded from the website as a SpatialPointsDataFrame then converted to a ppp 
  # object. Converting a SpatialPointsDataFrame to a ppp requires the use of the maptools 
  # package. Note the intermediate step that strips the point object of a dataframe 
  # before being converted to a mark free ppp object.

library(maptools)

z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/starbucks.rds"))
S1 <- readRDS(z)
SP <- as(S1, "SpatialPoints")

library(spatstat)
p2 <- as(SP, "ppp")

# We will also load a polygon outline of the state of Massachusetts. It will be used to 
  # define the extent of the study area (this is needed for hypothesis testing later in 
  # this tutorial). It too must be converted to a spatstat readable format. 
  # We’ll convert it to an owin object.

z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/ma.rds"))
S2 <- readRDS(z)
W <- as(S2, "owin")

# Some spatstat applications make use of rasters which must be stored as an im object. 
  # The following chunk of code downloads a raster of Massachusetts population density.

library(spatstat)  # Needed for function as.im()

z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/pop_sqmile.rds"))
r <- readRDS(z)
pop <- as.im(r)  # Convert r object to an im object

# The above datasets will be used later in this tutorial.

# Note that im objects created by spatstat can be converted to a raster object using the 
  # raster() function from the raster package. This can be helpful if you wish to use a 
  # mapping application such as tmap to map the rasters instead of the base plotting function.


## Density based analysis ##

# Modeling intensity as a function of a covariate #

# In this example, we make use of the Starbucks and population density datasets 
  # loaded at the beginning of this tutorial.

# First, we’ll map the data. The population density raster is mapped on a log scale.
library(tmap)
tm_shape(r) + 
  tm_raster(style = "quantile", 
            palette = "Greys") +
  tm_shape(S1) + 
  tm_bubbles(col = "red", 
             alpha = 0.5, 
             border.col = "yellow", 
             border.lwd = 0.5) + 
  tm_legend(outside = TRUE, 
            text.size = .8)

# Next, we’ll generate the Poisson point process model and plot the results. Note that we are 
  # using the ppp representation of the Starbucks points (object p2) and not the SpatialPointsDataFrame 
  # representation (object S1).

library(spatstat)
# Create the Poisson point process model
PPM1 <- ppm(p2 ~ pop)

# Plot the relationship
plot(
  effectfun(PPM1, 
            "pop", 
            se.fit = TRUE), 
  main = NULL, 
  cex.axis = 0.6, 
  cex.lab = 0.6, 
  legend = FALSE
)


## Distance based analysis ##

# Next, we’ll explore three different distance based analyses.

# Average nearest neighbor analysis #

# Here, we’ll compute the average nearest neighbor (ANN) distances for the tropical rain forest dataset.
library(spatstat)
P <- bei

# To compute the average first nearest neighbor distance (1st order):
mean(nndist(P, k = 1))

# To compute the average second nearest neighbor distance (2nd order):
mean(nndist(P, k = 2))

# The parameter k can take on any order neighbor (up to n-1 where n is the total number of points).
# The average nearest neighbor function can be expended to generate an ANN vs neighbor order plot. 
# In the following example, we’ll plot ANN as a function of neighbor order for the first 100 closest neighbors:
  
ANN <- apply(nndist(P, k = 1:100), 
             2, 
             FUN = mean)

plot(ANN ~ eval(1:100), 
     type = "b", 
     main = NULL )

# .....................