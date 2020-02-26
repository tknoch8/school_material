library(here)

source(here::here("r_scripts", "pca_fa_function.R"))

library(gt)
library(tidyr)

data("pizzaplace")

glimpse(pizzaplace)

data("countrypops")

glimpse(countrypops)

data("gtcars")

glimpse(gtcars)

my_analysis <- run_pca_fa(gtcars, label_by = gtcars$mpg_c, color_by = gtcars$mfr)

my_analysis$pc1_pc2_plot






# is_a_number <- function(x) {
#   if ("numeric" %in% class(x) || "double" %in% class(x) || "integer" %in% class(x)) {
#     return(TRUE)
#   } else if ("character" %in% class(x) || "factor" %in% class(x)) {
#     return(FALSE)
#   } else {
#     return(FALSE)
#   }
# }
# 
# 
# #-------------- select numeric columns for calculations ---------------------
# gtcars %>%  view()
# 
# gtcars %>% 
#   # drop_na() %>% 
#   select(-year)
# 
# std_data <- gtcars %>% 
#   select_if(is_a_number) %>% 
#   scale()
# 
# #-------------- perform pca -------------------------------------------------
# pr_comp <- prcomp(na.omit(std_data))
# #-------------- get pca summary ---------------------------------------------
# pr_summary <- pr_comp %>% 
#   summary()
