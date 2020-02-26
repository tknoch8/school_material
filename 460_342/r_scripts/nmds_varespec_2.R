# https://ourcodingclub.github.io/2018/05/04/ordination.html

library(tidyverse)
library(vegan)
library(ape)

data(varespec)

View(varespec)

# perform nmds and plot
varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

# In this tutorial, we only focus on unconstrained ordination or indirect 
  # gradient analysis. This ordination goes in two steps. First, we will 
  # perfom an ordination on a species abundance matrix. Then we will use 
  # environmental data (samples by environmental variables) to interpret 
  # the gradients that were uncovered by the ordination.

# Ordination and classification (or clustering) are the two main classes 
  # of multivariate methods that community ecologists employ. To some 
  # degree, these two approaches are complementary. Classification, or 
  # putting samples into (perhaps hierarchical) classes, is often useful 
  # when one wishes to assign names to, or to map, ecological communities. 
  # However, given the continuous nature of communities, ordination can be 
  # considered a more natural approach. Ordination aims at arranging samples 
  # or species continuously along gradients.

# First step is to calculate a distance matrix. 
# Here we use Bray-Curtis distance metric
dist <- vegdist(varespec,  method = "bray")

# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA <- pcoa(dist)

# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10]
        # Can you also calculate the cumulative explained variance of the first 3 axes?
        
        # Some distance measures may result in negative eigenvalues. In that case, add a correction:
        PCOA <- pcoa(dist, correction = "cailliez")
        
        # Plot your results
        biplot.pcoa(PCOA)
        
        # You see what`s missing? 
        # Indeed, there are no species plotted on this biplot. 
        # That's because we used a dissimilarity matrix (sites x sites) 
        # as input for the PCOA function. 
        # Hence, no species scores could be calculated. 
        # However, we could work around this problem like this:
        biplot.pcoa(PCOA, varespec)