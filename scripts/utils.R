plot_grid <- function(plot) {
  #' custom grid for the 33 variable face_wrap plot to move all xw_ features to 
  #' a single row by removing the 2 place holder plots
  require(grid)
  grob = ggplotGrob(plot)
  idx <- which(grob$layout$name %in% c("panel-2-5","panel-7-5","strip-t-7-4","strip-t-8-4","axis-l-4-7","axis-l-4-8"));
  for (i in idx) grob$grobs[[i]] <- nullGrob();
  grid.newpage()
  grid.draw(grob)
}

calc_models_ic <- function(list_models) {
  #' calculate model information criterion metrics (AIC, BIC) for a list of models
  mod_glance <- purrr::map(list_models, broom::glance) %>% 
    bind_rows(.id="model") %>% 
    select(c("model","AIC","BIC","adj.r.squared","r.squared"))
  mod_glance
}

find_imp_coef <- function(list_models, alpha) {
  #' filter the coefficients for a set of `lm` models for significant coefs
  #' based on provided significant level (alpha)
  #' returns only the unique terms associated with the coefs
  mod_tidy <- purrr::map(list_models, broom::tidy) %>% 
    bind_rows() %>% filter(p.value <= alpha)
  mod_tidy %>% distinct(term) %>% pull(term)
}

save_models <- function(list_models) {
  #' save model file for a list of models
  mod_names = names(list_models)
  len_models = length(list_models)
  for (i in 1:len_models) {
    model_name = mod_names[i]
    save_path = file.path("models",model_name)
    list_models[[i]] %>% readr::write_rds(save_path)
  }
}