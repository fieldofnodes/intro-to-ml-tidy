#### packages to load ####
library(tidyverse)
library(tidymodels)
library(tune)
library(here)
#### end ####
#### functions to call ####
fit_data <- function(formula, model, data, ...) {
wf <- workflows::add_model(workflows::add_formula(workflows::workflow(), formula), model)
fit(wf, data, ...)
}

fit_split <- function(formula, model, split, ...) {
  wf <- workflows::add_model(workflows::add_formula(workflows::workflow(), formula, blueprint = hardhat::default_formula_blueprint(indicators = FALSE)), model)
  tune::last_fit(wf, split, ...)
}

get_tree_fit <- function(results) {
  results %>% 
    pluck(".workflow", 1) %>% 
    workflows::pull_workflow_fit() 
}

get_boot_trees <- function(seed = 1, tree_depth = 4) {
  # Make recipe
  so_rec <- 
    recipe(remote ~ ., 
           data = stackoverflow) 
  
  # Make learner
  tmp_tree_lnr <-
    decision_tree(tree_depth = tree_depth) %>%         
    set_engine("rpart", model = TRUE) %>%      
    set_mode("classification")
  
  # Make workflow
  temp_flow <- 
    workflow() %>% 
    add_model(tmp_tree_lnr) %>% 
    add_recipe(so_rec) 
  
  # Begin resampling
  set.seed(seed)
  so_boots <- so_train %>% 
    bootstraps(times = 1) %>% 
    pluck("splits", 1)
  
  boot_fit <- temp_flow %>% 
    fit(data = analysis(so_boots)) %>% 
    pull_workflow_fit() %>% 
    pluck("fit")
  
  boot_fit
}

get_boot_votes <- function(seed = 1, team = 1) {
  tree <- get_boot_trees(seed)
  set.seed(seed * team)
  start <- sample((nrow(so_test)/2 - 4), 1)
  mini_test <- so_test %>% 
    mutate(obs = row_number()) %>%  
    group_by(remote) %>% 
    slice(start:(start + 4))
  preds <- 
    tree %>% 
    predict(mini_test, type = "class") %>% 
    enframe(name = "row_num", value = "estimate") %>% 
    bind_cols(select(mini_test, truth = remote, obs)) %>% 
    select(obs, truth, estimate)
  preds
}


#### end ####
#### Datasets to load ####
stackoverflow <- read_rds(here::here("materials/data/stackoverflow.rds"))
so_split <- initial_split(stackoverflow, strata = remote)
so_train <- training(so_split)
so_test  <- testing(so_split)
#### end ####


vanilla_tree_spec <-
  decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")


set.seed(100)
fit_split(remote ~ .,
          model = vanilla_tree_spec, 
          split = so_split,
          metrics = metric_set(accuracy, roc_auc)) %>% 
  collect_metrics()


big_tree_split <-
  decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification") %>% 
  set_args(
    min_n = 1,
    cost_complexity = 0)

set.seed(100)
fit_split(remote ~ .,
          model = big_tree_split, 
          split = so_split,
          metrics = metric_set(accuracy, roc_auc)) %>% 
  collect_metrics()


get_boot_votes(seed = 0, team = 2020)

rand_forest(mtry = 4, trees = 500, min_n = 1)



rf_spec <-
  rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

set.seed(100)
fit_split(remote ~ ., 
          model = rf_spec, 
          split = so_split) %>% 
  collect_metrics()


rf4_spec <- rf_spec %>% 
  set_args(mtry = 4)

rf8_spec <- rf_spec %>% 
  set_args(mtry = 8)

rf12_spec <- rf_spec %>% 
  set_args(mtry = 12)

rf20_spec <- rf_spec %>% 
  set_args(mtry = 20)

set.seed(100)
fit_split(remote ~ ., 
          model = rf4_spec, 
          split = so_split) %>% 
  collect_metrics()

set.seed(100)
fit_split(remote ~ ., 
          model = rf8_spec, 
          split = so_split) %>% 
  collect_metrics()


set.seed(100)
fit_split(remote ~ ., 
          model = rf12_spec, 
          split = so_split) %>% 
  collect_metrics()

set.seed(100)
fit_split(remote ~ ., 
          model = rf20_spec, 
          split = so_split) %>% 
  collect_metrics()

treebag_spec <-
  rand_forest(mtry = .preds()) %>%
  set_engine("ranger") %>% 
  set_mode("classification")
set.seed(100)
fit_split(remote ~ ., 
          model = treebag_spec,
          split = so_split) %>% 
  collect_metrics()

rf_imp_spec <-
  rand_forest(mtry = 4) %>% 
  set_engine("ranger", importance = 'impurity') %>% 
  set_mode("classification")

imp_fit <- 
  fit_split(remote ~ ., 
            model = rf_imp_spec,
            split = so_split) 

imp_fit

get_tree_fit(imp_fit)

imp_plot <- get_tree_fit(imp_fit)
vip::vip(imp_plot, geom = "point")


treebag_imp_spec <- 
  rand_forest(mtry = .preds()) %>% 
  set_engine("ranger", importance = 'permutation') %>% 
  set_mode("classification")



imp_fit <- 
  fit_split(remote ~ ., 
            model = treebag_imp_spec,
            split = so_split) 

imp_fit

get_tree_fit(imp_fit)

imp_plot <- get_tree_fit(imp_fit)
vip::vip(imp_plot, geom = "point")



rf_spec <- rand_forest(mtry = .preds()) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")
set.seed(100)
rf_fitwf <- fit_split(remote ~ ., 
                      model = rf_spec, 
                      split = so_split) 
rf_fit <- get_tree_fit(rf_fitwf)
vip::vip(rf_fit, geom = "point")








