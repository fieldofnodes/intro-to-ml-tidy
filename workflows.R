#### packages to load ####
library(tidyverse)
library(tidymodels)
library(parsnip)
library(remotes)
library(workflows)
library(tune)
library(kknn)
library(rpart)
library(rpart.plot)
library(rattle)
library(AmesHousing)
library(ranger)
library(partykit)
library(vip)
library(modeldata)
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


fit_data <- function(formula, model, data, ...) {
  wf <- workflows::add_model(workflows::add_formula(workflows::workflow(), formula), model)
  fit(wf, data, ...)
}

fit_split <- function(formula, model, split, ...) {
  wf <- workflows::add_model(workflows::add_formula(workflows::workflow(), 
                                                    formula, 
                                                    blueprint = hardhat::default_formula_blueprint(indicators = FALSE, allow_novel_levels = TRUE)), model)
  tune::last_fit(wf, split, ...)
}

source(here::here("materials/exercises/04-helpers.R"))
fit_split <- function(formula, model, split, ...) {
  wf <- workflows::add_model(workflows::add_formula(workflows::workflow(), formula, blueprint = hardhat::default_formula_blueprint(indicators = FALSE)), model)
  tune::last_fit(wf, split, ...)
}
get_tree_fit <- function(results) {
  results %>% 
    pluck(".workflow", 1) %>% 
    workflows::pull_workflow_fit() 
}



#### end ####
#### Datasets to load ####
stackoverflow <- read_rds(here::here("materials/data/stackoverflow.rds"))
so_split <- initial_split(stackoverflow, strata = remote)
so_train <- training(so_split)
so_test  <- testing(so_split)
ames <- make_ames() %>% 
  dplyr::select(-matches("Qu"))
ames_split <- initial_split(ames, prop = 0.75)
#### end ####


ames_zsplit <- ames %>% 
  mutate(z_price = (Sale_Price - mean(Sale_Price)) / sd(Sale_Price)) %>% select(Sale_Price, z_price)
  initial_split()

lm_spec <- 
  linear_reg() %>% # Pick linear regression
  set_engine(engine = "lm") # set engine

ames_train <- training(ames_split) %>% 
  mutate(z_price = (Sale_Price - mean(Sale_Price)) / sd(Sale_Price))

ames_test <- testing(ames_split) %>% 
  mutate(z_price = (Sale_Price - mean(Sale_Price)) / sd(Sale_Price))

lm_fit <- fit_data(Sale_Price ~ Gr_Liv_Area, 
                   model = lm_spec, 
                   data = ames_train)

price_pred  <- lm_fit %>% 
  predict(new_data = ames_test) %>% 
  mutate(price_truth = ames_test$Sale_Price)


rmse(price_pred, truth = price_truth, estimate = .pred)



lm_spec <- 
  linear_reg() %>% 
  set_engine("lm")

bb_wf <- 
  workflow() %>% 
  add_formula(Sale_Price ~ Bedroom_AbvGr + 
                Full_Bath + Half_Bath) %>% 
  add_model(lm_spec)

fit_split(
  bb_wf,
  split = ames_split
)


library(tidyverse)
library(AmesHousing)
library(tidymodels)
library(tune)
library(workflows)
ames <- make_ames() %>% 
  dplyr::select(-matches("Qu"))
set.seed(100)
ames_split <- initial_split(ames)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
fit_data <- function(object, model, data, ...) {
  if (inherits(object, "formula")) {
    object <- add_model(add_formula(workflow(), object, blueprint = hardhat::default_formula_blueprint(indicators = FALSE, ...)))
  }
  fit(object, data, ...)
}
fit_split <- function(object, model, split, ...) {
  if (inherits(object, "formula")) {
    object <- add_model(add_formula(workflow(), object, blueprint = hardhat::default_formula_blueprint(indicators = FALSE)), model)
  }
  tune::last_fit(object, split, ...)
}



lm_spec <- 
  linear_reg() %>% 
  set_engine("lm")
bb_wf <- 
  workflow() %>% 
  add_formula(Sale_Price ~ Bedroom_AbvGr + 
                Full_Bath + Half_Bath) %>% 
  add_model(lm_spec)

fit_split(
  bb_wf,
  split = ames_split
)


workflow() %>% update_formula(Sale_Price ~ Bedroom_AbvGr)


all_wf <- 
  bb_wf %>% 
  update_formula(Sale_Price ~ .)
fit_split(all_wf, split = ames_split) %>% 
  collect_metrics()

rt_spec <- 
  decision_tree() %>%          
  set_engine(engine = "rpart") %>% 
  set_mode("regression")
rt_wf <- 
  all_wf %>% 
  update_model(rt_spec)
fit_split(rt_wf, split = ames_split) %>% 
  collect_metrics()



all_fitwf <- fit_split(rt_wf, split = ames_split)
all_fitwf %>% 
  collect_predictions()


all_fitwf %>% 
  pluck(".workflow", 1)

all_fitwf %>% 
  pluck(".workflow", 1) %>% 
  pull_workflow_fit()


all_fitwf %>% 
  pluck(".workflow", 1) %>% 
  pull_workflow_spec()