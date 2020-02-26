library(MASS)
library(vegan)
data(varespec)
?varespec  # 44 species cover values across 24 locations

#------------------------------------------------------------------------------
# ordination serves to summarize community data (such as species abundance data) 
# by producing a low-dimensional ordination space in which similar species and 
# samples are plotted close together, and dissimilar species and samples are 
# placed far apart.
#------------------------------------------------------------------------------

# dissimilarity of cover values by location pair
vare.dist <- vegdist(varespec)

# perform nmds on dissimilarity object
vare.nmds0 <- isoMDS(vare.dist)  # defaults to two dimensions
vare.nmds0  # 'stress' is indication of goodness of fit

#------------------look at observed vs. ordination dissimilarities-------------

stressplot(vare.nmds0, vare.dist)  
# the correlation based on stress is R^2 = 1−S^2.
# the “fit-based R2” is the correlation between the fitted values θ(d) and
# ordination distances d_tilda, or between the step line and the points.

ordiplot(vare.nmds0, type = "t")
# shows location scores with respect to the two dimensions from the ordination
# similar interpretaion to PCA???

#----------------------------meta-mds------------------------------------------

# The iterative search is very difficult in nmds, because of nonlinear relationship 
# between ordination and original dissimilarities. The iteration easily gets trapped 
# into local optimum instead of finding the global op- timum. Therefore it is 
# recommended to use several random starts, and select among similar solutions with 
# smallest stresses.

vare.metmds <- metaMDS(varespec, trace = FALSE)
vare.metmds

# defaults to 20 random starting configurations
# finds two converging solutions
# uses 'Wisconsin double standardization'
# output interpretation:
  # The range of data values was so large that the data were square root transformed, 
  # and then submitted to Wisconsin double standardiza- tion, or species divided by 
  # their maxima, and stands standardized to equal totals

plot(vare.metmds, type = "t")

#----------------------------thoughts about nmds-------------------------------

# we could use nmds with geographic variables and compare villages/districts across
# different sets of variables.

# we might not need to/be able to use metaMDS, as I'm not quite sure about Wisconsin double
# standardization for non-ecology data.

