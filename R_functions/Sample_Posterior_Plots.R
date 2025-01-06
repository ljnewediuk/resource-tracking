
#################################################
###                                           ###
### Function to plot posterior distributions  ###
###                                           ###
### Samples posterior distributions across    ###
### full range of values for scaled variables ###
### and plots mean ± sd. Returns ggplot.      ###
###                                           ###
#################################################

library(brms)
library(tidybayes)

# Arguments:
#   y: Response variable to be predicted 
#   a: ß variable
#   b: Optional additional ß variable if in model
#   c: Optional additional ß variable if in model
#   factor_var: Optional ß variable as factor
#   m_name: Name of model (from files without extension)
#   n_pred: Number of predictions to draw from the posterior distribution

plot_glm <- function(y, a, b = NULL, c = NULL, factor_var = NULL, m_name, n_pred = 100) {
  
  # Get model
  m <- readRDS(paste0('models/', m_name, '.rds'))$model
  vars <- readRDS(paste0('models/', m_name, '.rds'))$scaled_vars
  
  # Make list of variables, beginning with y var and a 
  var_conds <- c(m_name, y, a)
  # Add second variable (b) if required...
  if(! is.null(b)) var_conds <- c(var_conds, b)
  # Add third variable (c) if required...
  if(! is.null(c)) var_conds <- c(var_conds, c)
  # Add factor variable if required...
  if(! is.null(factor_var)) var_conds <- c(var_conds, factor_var)
  
  # Define high and low values
  # Variable a
  var_a_low <- round(min(vars[, var_conds[3]], digits = 1, na.rm = T))
  var_a_high <- round(max(vars[, var_conds[3]], digits = 1, na.rm = T))
  # Variable b (if used)
  if(! is.null(b)) {
    var_b_low <- round(min(vars[, var_conds[4]], digits = 1, na.rm = T))
    var_b_high <- round(max(vars[, var_conds[4]], digits = 1, na.rm = T))
  }
  # Variable c (if used)
  if(! is.null(c)) {
    var_c_low <- round(min(vars[, var_conds[4]], digits = 1, na.rm = T))
    var_c_high <- round(max(vars[, var_conds[4]], digits = 1, na.rm = T))
  }
  # Factor variable if used
  if(! is.null(factor_var)) {
    fact_low <- round(min(vars[, var_conds[5]], digits = 1, na.rm = T))
    fact_high <- round(max(vars[, var_conds[5]], digits = 1, na.rm = T))
  }
  
  # Sample posterior distribution x n across range of values for two vars,
  # plus high and low values of factor
  pred <- data.frame(
    var_a = rep(seq(from = var_a_low, to = var_a_high, length.out = n_pred), 4)
  )
  if(! is.null(b)) pred <- pred %>%
    mutate(var_b = rep(seq(from = var_b_low, to = var_b_high, length.out = n_pred), 4))
  if(! is.null(c)) pred <- pred %>%
    mutate(var_c = rep(seq(from = var_c_low, to = var_c_high, length.out = n_pred), 4))
  if(! is.null(factor_var)) pred <- pred %>% 
    mutate(fact = rep(c(rep(fact_low, n_pred), rep(fact_high, n_pred)), 2))
  
  # Rename columns to match variables
  colnames(pred) <- var_conds[-c(1:2)]
  
  # Add to df
  predict_DT <- add_predicted_draws(pred, m) %>%
    group_by(.row) %>%
    # Summarize 89% credible intervals of posterior
    mutate(mean = mean(.prediction),
           CI.low = quantile(.prediction, probs = 0.055),
           CI.high = quantile(.prediction, probs = 0.945)) %>%
    select(! .chain:.prediction) %>%
    unique()
  
  # Get data points for plotting
  std_dat <- m[["data"]]
  
  # Factor data points ~ depending on high/low variability
  if(! is.null(factor_var)) {
    std_dat[[var_conds[5]]] <- ifelse(std_dat[[var_conds[5]]] < 0, fact_low, fact_high)
  }
  
  # Plot posterior samples & data
  p <- ggplot(predict_DT) + 
    geom_ribbon(data = predict_DT, 
                aes(x = get(var_conds[3]), ymin = CI.low, ymax = CI.high),
                alpha = 0.5) +
    geom_line(data = predict_DT, aes(x = get(var_conds[3]), y = mean), size = 1) +
    geom_point(data = std_dat, aes(x = get(var_conds[3]), y = get(var_conds[2])),
               size = 2) +
    scale_colour_manual(values = c('#0d0c8f', '#00b0ff', '#a9ecff', 'grey')) +
    theme(panel.background = element_rect(fill = 'white'),
          axis.title.x = element_text(size = 20, colour = 'black',  vjust = -4),
          axis.title.y = element_text(size = 20, colour = 'black', vjust = 5),
          axis.text = element_text(size = 18, colour = 'black'),
          axis.line.x = element_line(colour = 'black', size = .75),
          axis.line.y = element_line(colour = 'black', size = .75),
          plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
          legend.position = 'none') +
    ylab(var_conds[2]) +
    xlab(var_conds[3])
  
  if(! is.null(factor_var)) {
    p <- p +
      facet_wrap(~factor(get(var_conds[5])))
  }
  
  # Return plot
  return(p)
  
}


