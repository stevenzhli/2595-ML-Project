# 1A
plot_grid <- function(plot) {
  #' custom grid for the 33 variable face_wrap plot to move all xw_ features to
  #' a single row by removing the 2 place holder plots
  require(grid)
  grob <- ggplotGrob(plot)
  idx <- which(grob$layout$name %in% c("panel-2-5","panel-7-5","strip-t-7-4","strip-t-8-4","axis-l-4-7","axis-l-4-8"));
  for (i in idx) grob$grobs[[i]] <- nullGrob();
  grid.newpage()
  grid.draw(grob)
}

# 2A
calc_models_ic <- function(list_models, logistic = F) {
  #' calculate lm() model information criterion metrics (AIC, BIC) for a list of models
  basic <- c("model","AIC","BIC")
  metrics <- if(logistic){ basic } else {c(basic, "adj.r.squared","r.squared")}
  mod_glance <- purrr::map_dfr(list_models, broom::glance, .id = "model") %>%
    select(all_of(metrics))
  mod_glance
}

pull_lm_coefs_signif <- function(a_model, top_n=10) {
  caret::varImp(a_model) %>%
    rownames_to_column(var="coef") %>%
    rename(score = Overall) %>%
    arrange(desc(score)) %>%
    slice_head(n=top_n)
}


# 2B
pull_bayes_posterior_sigma <- function(list_models) {
  #' pull the posterior sample σ from a list of models, construct a dataframe
  pull_sigma <- function(a_model, a_name) {
    a_model %>% as_tibble() %>%
      select(sigma) %>%
      mutate(model = a_name)
  }
  purrr::map2_dfr(
    list_models, names(list_models),
    pull_sigma
  )
}

pull_bayes_coefs <- function(a_model, interval=0.9, signif=F) {
  #' pull the significant coefficients from a Bayesian model
  # if the upper and lower bound do not cross 0
  out <- cbind(mean = coef(a_model), sd = se(a_model)) %>%
    as_tibble(rownames="coef")
  if (signif == T) {
    poster_int <- posterior_interval(a_model, prob=interval) %>%
      as_tibble(rownames="coef") %>% slice_tail(n=-3)
    signif_coefs <- poster_int %>% filter(poster_int[,2]*poster_int[,3] > 0) %>% pull(coef)
    return(out %>% filter(coef %in% signif_coefs))
  }
  return(out)
}

# 2C
make_test_input_list <- function(var_name, all_data, top_num_vars, top_cat_var=NA) {
  #' setup one column of test grid based on a provided variable (var_name)
  #' support 3 levels most for regression: 
  #' - y-axis, response
  #' - x-axis, top_num_vars[1]
  #' - h-grid, top_num_vars[2]
  #' - v-grid, top_cat_var or top_num_vars[3]
  #' @param top_num_vars vector of string, numeric variable names
  #' @param top_cat_var string, categorical variable to expand
  #' @param all_data dataframe containing all data fields
  
  # pull current input variable
  xvar <- all_data %>% select(all_of(var_name)) %>% pull()
  if (var_name == top_num_vars[1]) {
    # primary numeric: use 25 unique values between the min/max values
    xgrid <- seq(min(xvar), max(xvar), length.out = 25)
  } else if (length(top_num_vars) > 1 & var_name == top_num_vars[2]) {
    # secondary numeric: specify 5 quantiles
    xgrid <- quantile(xvar, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
    xgrid <- as.vector(xgrid)
  } else if (length(top_num_vars) > 2 & var_name == top_num_vars[3]) {
    # tertiary numeric: specify 5 quantiles
    xgrid <- quantile(xvar, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
    xgrid <- as.vector(xgrid)
  } else if (!is.na(top_cat_var) & var_name == top_cat_var){
    # categorical variables
    xgrid <- na.omit(unique(xvar))
  } else if (is.numeric(xvar)) {
    # other numerical: set to their median values
    xgrid <- median(xvar, na.rm = TRUE)
  } else {
    # other categorical, set to most abundant one
    xgrid <- names(which.max(table(xvar)))
  }
  return(xgrid)
}

make_test_input_grid <- function(all_input_names, all_data, top_num_vars, top_cat_var=NA) {
  #' wrapper function to construct the test grid based on original data
  #' iteratively go through each column and construct the grid
  #' @param all_input_names vector of string, all input names in the dataframe
  
  test_list <- purrr::map(
    all_input_names,
    make_test_input_list,
    top_num_vars = top_num_vars,
    top_cat_var = top_cat_var,
    all_data = all_data
  )
  expand.grid(
    test_list,
    KEEP.OUT.ATTRS = F,
    stringsAsFactors = T
  ) %>%
    purrr::set_names(all_input_names)
}

make_post_pred <- function(a_model, new_data) {
  #' make posterior linear predictor prediction and posterior response prediction
  #' from a rstanarm `stan_lm` linear model
  require(rstanarm)
  
  # response (prediction interval)
  res_pred <- posterior_predict(a_model, newdata = new_data) %>%
    as_tibble() %>% rowid_to_column("post_id") %>%
    pivot_longer(!c("post_id"), names_to = 'pred_id') %>%
    mutate(across(.cols = 'pred_id', .fns = as.numeric)) %>%
    group_by(pred_id) %>%
    summarise(num_post = n(),
              y_avg = mean(value),
              y_lwr = quantile(value, 0.05),
              y_upr = quantile(value, 0.95)) %>%
    ungroup()
  # linear predictor (confidence interval)
  lin_pred <- posterior_linpred(a_model, newdata = new_data) %>%
    as_tibble() %>% rowid_to_column("post_id") %>%
    pivot_longer(!c("post_id"), names_to = 'pred_id') %>%
    mutate(across(.cols = 'pred_id', .fns = as.numeric)) %>%
    group_by(pred_id) %>%
    summarise(num_post = n(),
              trend_avg = mean(value),
              trend_lwr = quantile(value, 0.05),
              trend_upr = quantile(value, 0.95)) %>%
    ungroup()
    
  left_join(res_pred, lin_pred, by="pred_id") %>%
    left_join(new_data %>% tibble::rowid_to_column("pred_id"), by = "pred_id")
}

make_post_pred_cl <- function(a_model, new_data) {
  #' make posterior binary classification prediction
  #' from a rstanarm `stan_glm` model
  require(rstanarm)
  
  # event prob trend predictor
  lin_pred <- posterior_epred(a_model, newdata = new_data) %>%
    as_tibble() %>% rowid_to_column("post_id") %>%
    pivot_longer(!c("post_id"), names_to = 'pred_id') %>%
    mutate(across(.cols = 'pred_id', .fns = as.numeric)) %>%
    group_by(pred_id) %>%
    summarise(num_post = n(),
              trend_avg = mean(value),
              trend_lwr = quantile(value, 0.05),
              trend_upr = quantile(value, 0.95)) %>%
    ungroup()
  lin_pred %>%
    left_join(new_data %>% tibble::rowid_to_column("pred_id"), by = "pred_id")
}

make_tidy_pred <- function(a_model, new_data, ...) {
  #' make linear model prediction from a saved `tidymodels` workflow
  require(tidymodels)
  
  # response (prediction interval)
  res_pred <- predict(a_model, new_data, type = "pred_int", ...) %>%
    as_tibble() %>% rowid_to_column("post_id") %>%
    pivot_longer(!c("post_id"), names_to = 'pred_id') %>%
    mutate(across(.cols = 'pred_id', .fns = as.numeric)) %>%
    group_by(pred_id) %>%
    summarise(num_post = n(),
              y_avg = mean(value),
              y_lwr = quantile(value, 0.05),
              y_upr = quantile(value, 0.95)) %>%
    ungroup()
  # linear predictor (confidence interval)
  res_pred <- predict(a_model, new_data, type = "pred_int", ...) %>%
    as_tibble() %>% rowid_to_column("post_id") %>%
    pivot_longer(!c("post_id"), names_to = 'pred_id') %>%
    mutate(across(.cols = 'pred_id', .fns = as.numeric)) %>%
    group_by(pred_id) %>%
    summarise(num_post = n(),
              y_avg = mean(value),
              y_lwr = quantile(value, 0.05),
              y_upr = quantile(value, 0.95)) %>%
    ungroup()
  
}

# Shared
save_models <- function(list_models) {
  #' save model files for a list of model objects
  mod_names <- names(list_models)
  len_models <- length(list_models)
  for (i in 1:len_models) {
    model_name <- mod_names[i]
    save_path <- file.path("models", model_name)
    list_models[[i]] %>% readr::write_rds(save_path)
  }
}

# compute sem
se <- function(x){sqrt(var(x)/length(x))}

