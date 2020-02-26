## https://eurekastatistics.com/using-mahalanobis-distance-to-find-outliers/

## Using Mahalanobis Distance to Find Outliers ##

# R's mahalanobis() function provides a simple means of detecting outliers in 
  # multidimensional data.
# For example, suppose you have a dataframe of heights and weights:
hw <- data.frame(Height.cm = c(164, 167, 168, 169, 169, 170, 170, 170, 171, 172, 
                               172, 173, 173, 175, 176, 178),
                 Weight.kg = c( 54,  57,  58,  60,  61,  60,  61,  62,  62,  64,  
                                62,  62,  64,  56,  66,  70))

# When plotting these data (generated for this example using an interactive plot), 
  # you could mark as outliers those points that are, for instance, more than two 
  # (sample) standard deviations from the mean height or mean weight:
is.height.outlier <- abs(scale(hw$Height.cm)) > 2
is.weight.outlier <- abs(scale(hw$Weight.kg)) > 2
pch <- (is.height.outlier | is.weight.outlier) * 16
plot(hw, pch = pch)

# Note that the point with height equal to 175 cm (in the bottom-right corner of the 
  # graph) has not been marked as an outlier, as it's less than 2 standard deviations 
  # from the mean height and mean weight. And yet that is the point that most clearly 
  # does not follow the linear relationship between height and weight that we see in 
  # this data. It is—arguably—the real outlier here.
# By the way, the choice of scales for the above graph is somewhat misleading. A 
  # clearer picture of the effect of height on weight would have been obtained by at 
  # least letting the y scale start at zero. But I'm using this data merely to 
  # illustrate outlier detection; I hope you'll overlook this bad practice!
  # Now let's give mahalanobis() a spin:
n.outliers   <- 2  # Mark as outliers the 2 most extreme points
m.dist.order <- order(mahalanobis(hw, colMeans(hw), cov(hw)), decreasing=TRUE)
is.outlier   <- rep(FALSE, nrow(hw))
is.outlier[m.dist.order[1:n.outliers]] <- TRUE
pch <- is.outlier * 16
plot(hw, pch = pch)

# The above code marks as outliers the two most extreme points according to their 
  # Mahalanobis distance (also known as the generalised squared distance). This is, 
  # very roughly speaking, the distance of each point (the rows of the dataframe) 
  # from the centre of the data that the dataframe comprises, normalised by the 
  # standard deviation of each of the variables (the columns of the dataframe) and 
  # adjusted for the covariances of those variables. It may be thought of as the 
  # multidimensional analogue of the t-statistic—which is defined as (x-x) / s, where 
  # x is the sample mean and s is the sample standard deviation. (For details, visit 
  # Wikipedia's page on Mahalanobis distance.) As you can see, this time the point in 
  # the bottom-right corner of the graph has been caught:

# And this technique works in higher dimensions too. This code produces a 3-dimensional 
  # spinnable scatterplot:
library(rgl)
hwa <- data.frame(Height.cm = c(164, 167, 168, 168, 169, 169, 169, 170, 172, 
                                173, 175, 176, 178),
                  Weight.kg = c( 55,  57,  58,  56,  57,  61,  61,  61,  64,  
                                 62,  56,  66,  70),
                  Age = c( 13, 12, 14, 17, 15, 14, 16, 16, 13, 15,  
                           16, 14, 16))
m.dist.order <- order(mahalanobis(hwa, colMeans(hwa), cov(hwa)), decreasing = TRUE)
is.outlier <- rep(FALSE, nrow(hwa))
is.outlier[m.dist.order[1:2]] <- TRUE  # Mark as outliers the 2 most extreme points
col <- is.outlier + 1
plot3d(hwa$Height.cm, hwa$Weight.kg, hwa$Age, type = "s", col = col)














