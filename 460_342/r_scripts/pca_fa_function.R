library(tidyverse)
library(dplyr)
library(ggplot2)
#library(purrr)
library(tibble)
library(tidyr)
library(GGally)
library(ggrepel)
library(here)

euroemp <- read.csv(here("data_raw", "Euroemp.csv"))

run_pca_fa <- function(data, label_by = NULL, color_by = NULL, num_pcs_for_loadings = 4, ...) {

  # label_by <- enquo(label_by)
  # color_by <- enquo(color_by)
  #num_pcs_for_loadings <- enquo(num_pcs_for_loadings)
  
  #-------------- create blank list to fill -----------------------------------
  my_list <- list()
  
  #-------------- select numeric columns for calculations ---------------------
  is_a_number <- function(x) {
    if ("numeric" %in% class(x) || "double" %in% class(x) || "integer" %in% class(x)) {
      return(TRUE)
    } else if ("character" %in% class(x) || "factor" %in% class(x)) {
      return(FALSE)
    }
  }
  
  std_data <- data %>% 
    select_if(is_a_number) %>% 
    scale()
  
  std_data <- na.omit(std_data)
  
  #-------------- perform pca -------------------------------------------------
  pr_comp <- prcomp(na.omit(std_data))
  
  #-------------- get pca summary ---------------------------------------------
  pr_summary <- pr_comp %>% 
    summary()
  
  #-------------- get factor loads --------------------------------------------
  # "importance" of each variable in determining PCs
  fac_loads <- round(cor(std_data, pr_comp$x), 2)
  
  #-------------- add some objects to output list -----------------------------
  my_list$standardized_data <- std_data
  my_list$pricipal_component_decomposition <- pr_comp
  my_list$decomposition_summary <- pr_summary
  my_list$factor_loads <- fac_loads
  
  #-------------- plot when label_by OR color_by argument is NULL -------------
  if (is.null(color_by) || is.null(label_by)) {
    pc_plot <- data.frame(pr_comp$x) %>% 
      ggplot(
        aes(
          x = PC1,
          y = PC2
        )
      ) +
      geom_point(
        color = "red"
      ) +
      # geom_text_repel(
      #   label = ""
      # ) +
      theme(
        legend.title = element_blank(),
        legend.position = "top"
      )
  
  } else {
  #-------------- plot when BOTH arguments are NOT NULL -----------------------
  pc_plot <- data.frame(pr_comp$x) %>% 
    ggplot(
      aes(
        x = PC1,
        y = PC2
      )
    ) +
    geom_point(
      color = "red"
    ) +
    geom_text_repel(
      aes(
        label = label_by,  # not working
        colour = color_by   # not working
      )
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = "top"
    )
  }
  
  #-------------- add plot to output list -------------------------------------
  my_list$pc1_pc2_plot <- pc_plot
  
  
  ### transition to factor analysis ###
  
  #-------------- add more objects to output list -----------------------------
  unrotated_fac_loadings <- round(pr_comp$rotation %*% diag(pr_comp$sdev)[,1:num_pcs_for_loadings], 2)
  
  my_list$unrotated_factor_loadings <- unrotated_fac_loadings

  
  # proportions of variability attributable to common factors, across observations (communalities)
  prop_vari_attr <- round(diag(unrotated_fac_loadings %*% t(unrotated_fac_loadings)), 2)
  
  my_list$communalities <- prop_vari_attr
  
  # "verimax rotation"
  rot <- varimax(unrotated_fac_loadings, eps = 1e-12)$rotmat
  
  rot_fac_loadings <- round(unrotated_fac_loadings %*% rot, 2)
  
  my_list$rotated_factor_loadings <- rot_fac_loadings
  
  f_star <- round(std_data %*% rot_fac_loadings %*% solve(t(rot_fac_loadings) %*% rot_fac_loadings), 2)
  
  
  #-------------- tidy eval problems ??? --------------------------------------
  
  # label_by <- enquo(label_by)
  # color_by <- enquo(color_by)
  
  # vec_1 <- data %>% 
  #   select(!!label_by)
  # 
  # vec_2 <- data %>% 
  #   select(!!color_by)
  # 
  # factor_loads <- data.frame(vec_1, vec_2, f_star)
  # 
  # my_list$factor_loadings <- factor_loads
  
  return(my_list)
  
}

# test <- run_pca_fa(euroemp, label_by = euroemp$Country, color_by = euroemp$Group)
# 
# test$pc1_pc2_plot
# # 
# test <- run_pca_fa(euroemp)
# 
# test$pc1_pc2_plot
