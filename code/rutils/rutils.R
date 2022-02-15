lm_loop <- function(outcome_variable, explanatory_variable, correct_unaffected = FALSE, correct_affected = FALSE, correct_age = FALSE, df){
  
  df_out <- tibble("outcome" = character(), "predictor" = character(),
                   "predictor_coefficient" = numeric(),
                   "confidence_lower" = numeric(), "confidence_upper" = numeric(),
                   "p_value_outcome" = numeric(),
                   "adjusted_r2" = numeric(), "complete_formula" = character(),
                   "predictor_significance" = character())
  
  counter <- 1
  
  for (o in outcome_variable){
    for (e in explanatory_variable){
      
      
      df_out[counter, 1] <- o # loading the outcome
      
      base_formula <- sprintf("%s ~ %s", o, e)
      
      if (correct_affected == TRUE) {
        if (str_detect(o, 'u$') == TRUE){
          unaffected_name <- paste0(substr(o, 1, nchar(o)-1), 'a') # this changes the last letter of the outcome to "u" for unaffected
          unaffected_variable <- sprintf("+ %s", unaffected_name)}
      }else{unaffected_variable <- ""} 
      
      if (correct_unaffected == TRUE) {
        if (str_detect(o, 'a$') == TRUE){
          unaffected_name <- paste0(substr(o, 1, nchar(o)-1), 'u') # this changes the last letter of the outcome to "u" for unaffected
          unaffected_variable <- sprintf("+ %s", unaffected_name)}
      }else{unaffected_variable <- ""}
      
      if (correct_age == TRUE){age <- "+age"} 
      else if (correct_age == FALSE){age <- ""}
      
      complete_formula <- paste(base_formula, age, unaffected_variable)
      
      model <- lm(formula = complete_formula, data = df)
      confidence_intervals <- confint(model)
      model_summary <- summary(model)
      
      
      # Packing the df
      df_out[counter, 2] <- e #name of the predictor
      df_out[counter, 3] <- round(model_summary[["coefficients"]][2, 1], 2) # Coefficient of the predictor
      df_out[counter, 4] <- round(confidence_intervals[2, 1], 2) # lower confidence interval
      df_out[counter, 5] <- round(confidence_intervals[2, 2], 2) # upper confidence interval
      df_out[counter, 6] <- round(model_summary[["coefficients"]][2, 4], 2) # upper confidence interval
      df_out[counter, 7] <- round(model_summary[["adj.r.squared"]], 2) # upper confidence interval
      df_out[counter, 8] <- complete_formula
      df_out[counter, 9] <- if_else(df_out[counter, 6] < 0.05, "Significant", "NS") 
      
      counter <- counter + 1
    }
  }
  return(df_out)
}


loo_test_for_cor <- function(df, loop_cor_df) {
  loo_list <- as.list(df[, "id"])
  loo_list <- append(loo_list, as.list(loop_cor_df[loop_cor_df$predictor_significance == "Significant", "complete_formula"], "sig_formula")) # Select only the significant models

  .sig_models_vector <- vector()

  for (f in loo_list$complete_formula) {
  
    p_vector <- vector() 

    for (s in loo_list$id) {
    
    dummy_df <- df %>% filter(id != s)
    model_sum <- summary(lm(f, dummy_df))
    p_vector <- append(p_vector, model_sum[["coefficients"]][2, 4])
    
  }
    
    if (all(p_vector <= 0.05)) {
      
      .sig_models_vector <- append(.sig_models_vector, f)    
      
    }
}

  sig_models_after_loo_df <- tibble()
  for (mod in .sig_models_vector) {
    t <-  loop_cor_df %>% filter(complete_formula == mod)
    sig_models_after_loo_df <- bind_rows(sig_models_after_loo_df, t)  
  }
  
  return(sig_models_after_loo_df)
}







.avPlots.invis <- function(MODEL, ...) {
  
  ff <- tempfile()
  png(filename = ff)
  OUT <- car::avPlots(MODEL, ...)
  dev.off()
  unlink(ff)
  OUT }

ggAVPLOTS  <- function(MODEL, XLAB, YLAB = NULL) {
  
  #Extract the information for AV plots
  AVPLOTS <- .avPlots.invis(MODEL)
  K       <- length(XLAB)
  
  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K)
  for (i in 1:K) {
    LAB <- XLAB[[i]]
    DATA         <- data.frame(AVPLOTS[[i]])
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1], 
                                               y = colnames(DATA)[2]), 
                                    data = DATA) +
      geom_point(colour = 'red', size = 1) + 
      geom_smooth(method = 'lm', se = TRUE, 
                  color = 'blue', formula = y ~ x, linetype = 'solid', size = 0.5) +
      xlab(LAB) +
      ylab(paste0('Response Residual <br> (',
                  ifelse(is.null(YLAB), 
                         paste0(names(DATA)[2], ' | others'), YLAB), ')')) +
      theme(axis.title = ggtext::element_markdown(),
            axis.title.x = ggtext::element_markdown(),
            axis.title.y = ggtext::element_markdown(),
            axis.text = element_text(size = 9),
            axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9))}
  
  #Return output object
  GGPLOTS }