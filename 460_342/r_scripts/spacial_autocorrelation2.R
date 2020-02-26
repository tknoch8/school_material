# https://mgimond.github.io/Spatial/point-pattern-analysis-in-r.html

library(spdep)

# The spdep functions used in this exercise make use of different spatial object 
  # classes than those used by the spatstat package. Here, we’ll load layers as 
  # SpatialPolygonsDataFrame objects. If we were to work with point data instead, 
  # we would store the point shapefiles as SpatialPointsDataFrame.

# We’ll first load the spatial object used in this exercise from a remote 
  # website–income and education data aggregated at the county level for the 
  # state of Maine. This object will be loaded as a SpatialPolygonsDataFrame 
  # and will therefore not require conversion.

z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/Income_schooling.rds"))
s1 <- readRDS(z)
s1  # spatial object of class SpatialPolygonsDataFrame

# The spatial object has five attributes. The one of interest for this exercise is
  # Income (per capita, in dollars).

# Let’s map the income distribution using a quantile classification scheme.
library(tmap)

tm_shape(s1) + 
  tm_polygons(style = "quantile", 
              col = "Income") +
  tm_legend(outside = TRUE, 
            text.size = .8) 

# Define neighboring polygons

# The first step requires that we define “neighboring” polygons. This could refer 
  # to contiguous polygons, polygons within a certain distance band, or it could 
  # be non-spatial in nature and defined by social, political or cultural “neighbors”.

# Here, we’ll adopt a contiguous neighbor definition where we’ll accept any contiguous 
  # polygon that shares at least one vertex (this is the “queen” case and is defined 
  # by setting the parameter queen=TRUE). If we required that at least one edge be 
  # shared between polygons then we would set queen=FALSE.

nb <- spdep::poly2nb(s1, queen = TRUE)
nb[1]  # neighbors for first polygon in object

# Polygon 1 has 4 neighbors. The first polygon is associated with 
  # the attribute name Aroostook:
s1$NAME[1]

# The four neighboring polygons are associated with the names:
s1$NAME[c(2,3,4,5)]

# Next, we need to assign weights to each neighboring polygon. In our case, 
  # each polygon will be assigned equal weight when computing the neighboring 
  # mean values.
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# To see the weight of the first polygon’s four neighbors type:
lw$weights[1]

# Each neighbor is assigned a quarter of the total weight. 
  # This means that when R computes the average neighboring income 
  # values, each neighbor’s income will be multiplied by 0.25 
  # before being tallied.

# Finally, we’ll compute the average neighbor income value for each polygon. 
  # These values are often referred to as spatially lagged values.
Inc.lag <- lag.listw(lw, s1$Income)

# Compute Morans I:
moran.test(s1$Income, lw)

# Note that the p-value computed from the moran.test function is not computed 
  # from an MC silumation but analytically instead. This may not always prove 
  # to be the most accurate measure of significance. To test for significance 
  # using the MC simulation method instead, use the moran.mc function.
MC <- moran.mc(s1$Income, lw, nsim = 599)

# Plot the distribution (note that this is a density plot instead of a histogram)
plot(MC, main = NULL)  # not sure how to interpret


## Moran’s I as a function of a distance band

# In this section, we will explore spatial autocorrelation as a function of 
  # distance bands.
# Instead of defining neighbors as contiguous polygons, we will define neighbors 
  # based on distances to polygon centers. We therefore need to extract the center 
  # of each polygon
coo <- coordinates(s1)
# The object coo stores all sixteen pairs of coordinate values.

# Next, we will define the search radius to include all neighboring polygon centers 
  # within 50 km (or 50,000 meters)
S.dist <- dnearneigh(coo, 0, 50000)

# The dnearneigh function takes on three parameters: the coordinate values coo, 
  # the radius for the inner radius of the annulus band, and the radius for the 
  # outer annulus band. In our example, the inner annulus radius is 0 which implies 
  # that all polygon centers up to 50km are considered neighbors.

# Note that if we chose to restrict the neighbors to all polygon centers between 
  # 50 km and 100 km, then we would define a search annulus (instead of a circle) 
  # as dnearneigh(coo, 50000, 100000).

# Now that we defined our search circle, we need to identify all neighboring 
  # polygons for each polygon in the dataset.
lw <- nb2listw(S.dist, style = "W", zero.policy = TRUE)

# Run the MC simulation.
MI <- moran.mc(s1$Income, lw, nsim = 599, zero.policy = TRUE) 
plot(MI)
MI

# compare distance band method with contiguous polygon method
tm_shape(s1) + tm_polygons(style = "quantile", col = "Income") +
  tm_legend(outside = TRUE, text.size = .8)