
# Loading packages: -------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(palmerpenguins)
library(ggplot2); theme_set(theme_minimal())
library(ranger)
library(kableExtra)
library(vip)


# Loading the data: -------------------------------------------------------

penguins <- penguins

penguins_raw <- penguins_raw


# Exploring the data: -----------------------------------------------------

# Looking at summary statistics:

summary(penguins)

summary(penguins_raw)

# Looking at the available variables:

names(penguins)

names(penguins_raw)


# Visualising the data: ---------------------------------------------------

penguins %>%
  ggplot(aes(flipper_length_mm, bill_length_mm,
             color = sex,
             size = body_mass_g)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~species)


# Pre-processing the data: ------------------------------------------------

penguins_scaled <- penguins %>%
  drop_na %>% 
  select(where(is.numeric),
         -year) %>% 
  mutate_at(colnames(.),
            ~(scale(.) %>% as.vector))


# K-Means clustering: -----------------------------------------------------

# Setting the random seed for reproducibility:

set.seed(1234)

# Applying the k-means algorithm to the data 
# and joining the results to the original data

k_clust <- penguins_scaled %>%
  kmeans(centers = 3) %>%
  augment(drop_na(penguins))

k_clust


# Visualising results: ----------------------------------------------------

# The clusters as colour:

k_clust %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             colour = .cluster)) + 
  geom_point() +
  scale_colour_manual(values = c("darkorange","purple","cyan4"))

# As above, but with clusters as numbers, species as colour:

k_clust %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             colour = species)) +   #<<
  geom_text(aes(label= .cluster)) + #<<
  scale_colour_manual(values = c("darkorange","purple","cyan4"))


# With the raw data (this is why tidy data is nice):

penguins_raw %>% 
  drop_na(where(is.numeric)) %>% 
  select(where(is.numeric),
         -`Sample Number`,
         -`Date Egg`) %>% 
  mutate_at(colnames(.),
            ~(scale(.) %>% as.vector))%>% 
  kmeans(centers = 3) %>% 
  augment(drop_na(penguins_raw,
                  where(is.numeric)))%>% 
  ggplot(aes(x = `Culmen Length (mm)`, y = `Culmen Depth (mm)`, colour = Species)) +
  geom_text(aes(label= .cluster)) +
  scale_colour_manual(values = c("darkorange","purple","cyan4"))


# Classification: ---------------------------------------------------------


# Pre-processing: ---------------------------------------------------------

penguins_prep <- penguins %>%
  filter(!is.na(sex)) %>%
  select(-year, -island)


# Test/train split: -------------------------------------------------------

# Setting the random seed for reproducibility:

set.seed(123)

# Splitting the data, stratifying by sex:

penguin_split <- initial_split(penguins_prep, strata = sex) 

# Creating new objects for test and training sets:

penguin_train <- training(penguin_split)

penguin_test <- testing(penguin_split)


# Bootstrapping: ----------------------------------------------------------

set.seed(123)

penguin_boot <- bootstraps(penguin_train)


# Specifying models: ------------------------------------------------------

# The logistic regression model:

glm_spec <- logistic_reg() %>%
  set_engine("glm")

# The random forest model:

rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")


# Fitting models: ---------------------------------------------------------

# Creating workflow:

penguin_wf <- workflow() %>%
  add_formula(sex ~ .)

# Fitting logistic regression model:

glm_rs <- penguin_wf %>%
  add_model(glm_spec) %>%
  fit_resamples(resamples = penguin_boot,
                control = control_resamples(save_pred = TRUE))

# Fitting random forest model:

rf_rs <- penguin_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE))


# Getting results: --------------------------------------------------------

# For the logistic regression model:

collect_metrics(glm_rs)

# For the random forest model:

collect_metrics(rf_rs) 


#  A deeper look at the logictic regression model: ------------------------

# Creating a confusion matrix:

glm_rs %>%
  conf_mat_resampled()

# Creating a roc plot:

glm_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

# Cross-validation: -------------------------------------------------------

# Fitting the best model from the training data:

penguin_final <- penguin_wf %>%
  add_model(glm_spec) %>%
  last_fit(penguin_split)

# Getting results:

collect_metrics(penguin_final)

# Another confusion matrix:

collect_predictions(penguin_final) %>%
  conf_mat(sex, .pred_class)

# Final roc plot:

penguin_final %>%
  collect_predictions() %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

# Getting coefficients:

penguin_final$.workflow[[1]] %>%
  tidy(exponentiate = TRUE)

# VIP plot:

penguin_final %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20)
