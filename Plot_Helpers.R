library(tidyverse)
library(ggpmisc)
library(stringr)

#################################### PLOT LABELING #######################################


get_plot_labels <- function(plot_kind, plot_type_info, extra_info=NULL){
  
  if(plot_kind == "residual" && plot_type_info == "externally_studentized"){
    plot_title <- "Externally Studentized Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Externally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  } 
  else if(plot_kind == "residual" && plot_type_info == "internally_studentized"){
    plot_title <- "Internally Studentized Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Internally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual" && plot_type_info == "regular") {
    plot_title <- "Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel) 
  }
  else if(plot_kind == "residual" && plot_type_info == "deleted"){
    plot_title <- "Deleted (Prediction) Residuals vs Fitted Values"
    plot_xlabel <- "Fitted Values"
    plot_ylabel <- "Deleted (aka Prediction) Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "externally_studentized"){
    plot_title <- "Distribution of Externally Studentized Residuals"
    plot_xlabel <- "Externally Studentized Residual Values"
    plot_ylabel <- "Count of Externally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "internally_studentized"){
    plot_title <- "Distribution of Internally Studentized Residuals"
    plot_xlabel <- "Internally Studentized Residual Values"
    plot_ylabel <- "Count of Internally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "regular"){
    plot_title <- "Distribution of Residuals"
    plot_xlabel <- "Residual Values"
    plot_ylabel <- "Count of Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_histogram" && plot_type_info == "deleted"){
    plot_title <- "Distribution of Deleted Residuals"
    plot_xlabel <- "Deleted Residual Values"
    plot_ylabel <- "Count of Deleted Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "externally_studentized"){
    plot_title <- "QQ Plot of Externally Studentized Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Externally Studenzied Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "internally_studentized"){
    plot_title <- "QQ Plot of Internally Studentized Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Internally Studenzied Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "deleted"){
    plot_title <- "QQ Plot of Deleted (Prediction) Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Deleted (Prediction) Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_qq" && plot_type_info == "regular"){
    plot_title <- "QQ Plot of Residuals"
    plot_xlabel <- "Theoretical Quantiles"
    plot_ylabel <- "Residuals Quantiles"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "externally_studentized"){
    plot_title <- "Externally Studentized Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Externally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "internally_studentized"){
    plot_title <- "Internally Studentized Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Internally Studentized Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "deleted"){
    plot_title <- "Deleted Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Deleted Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "residual_vs_leverage" && plot_type_info == "regular"){
    plot_title <- "Residuals vs Leverage"
    plot_xlabel <- "Leverage"
    plot_ylabel <- "Residuals"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  else if(plot_kind == "case_stat_vs_obs"){
    full_name <- case_stat_name_map(case_stat=plot_type_info, get_full_name=TRUE)
    plot_title <- paste0(full_name_to_title(full_name), " vs Observation Number")
    plot_xlabel <- "Observation Number"
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=full_name)
  }else if(plot_kind =="partial_residual"){
    variable_name <- full_name_to_title(plot_type_info)
    plot_title <- paste0("Partial Residual Plot for Explanatory Variable ", variable_name)
    plot_xlabel <- variable_name
    plot_ylabel <- paste0("Partial Residual of ", variable_name)
    
    if(extra_info){
      plot_title <- paste0("Augmented ", plot_title)
      plot_ylabel <- paste0("Augmented ", plot_ylabel)
    }
    
    return_list <- list(title=plot_title, xlabel=plot_xlabel, ylabel=plot_ylabel)
  }
  
  return(return_list)
  
}


##################################################################################################

# Calculate all the case statistics to assess model fit.
create_case_df <- function(fit){
  
  case_stats <- case(fit)
  case_df <- as.data.frame(case_stats)
  
  # Add deleted residuals to the dataframe
  case_df['deleted_resids'] <- case_df[,"e"] / (1 - case_df[, "h"])
  
  # Add fitted values to the dataframe
  case_df[,"fitted_values"] <- fit$fitted.values
  
  # Add a column to track observation number
  case_df[,"obs_number"] <- seq(1, as.numeric(nrow(case_df)))
  
  return(case_df)
  
}

##################################################################################################
#################################### RESIDUAL PLOT SECTION #######################################
##################################################################################################

# 1. Calculates all case statistics and stores in a dataframe
# 2. Copies the data for the residual type being plotted to a column named "Resid_Plot_Column"
# 3. Filters the dataframe based on the "remove_less"than" and "remove_greater_than" filters.
get_residual_plot_data <- function(fit, residual_type, remove_less_than, remove_greater_than){
  
  
  # Create the case statistics dataframe
  case_df <- create_case_df(fit)
  
  # Add the appropriate Resid_Plot_Column for this residual_type
  case_df <- set_resid_plot_column(case_df, residual_type=residual_type)
  
  # Filter according to the remove_less_than and remove_greater_than parameters, which filter out values
  # "less than" or "greater than" the specified value.
  case_df <- filter_by_absolute_value(df=case_df, 
                                      filter_column="Resid_Plot_Column", 
                                      remove_less_than=remove_less_than, 
                                      remove_greater_than=remove_greater_than)
  
  return(case_df)
  
}


# Sets the column to be plotted (residual type) based on user input.
set_resid_plot_column <- function(case_df, residual_type){
  
  if(residual_type == "externally_studentized"){
    case_df[,"Resid_Plot_Column"] <- case_df[,"stu.res"]
    
  }
  else if(residual_type == "internally_studentized"){
    case_df[,"Resid_Plot_Column"] <- case_df[,"sta.res"]  
  }
  else if(residual_type == "regular"){
    case_df[,"Resid_Plot_Column"] <- case_df[,"e"]
    
  }
  else if(residual_type == "deleted") {
    case_df[,"Resid_Plot_Column"] <- case_df[,"deleted_resids"]
  }
  else{
    case_df[,"Resid_Plot_Column"] <- case_df[, residual_type]
  }
  
  return(case_df)
}


# Function to filter a dataset based on a given column
# Removes values less than remove_less_than and greater than remove_greater_than
filter_by_absolute_value <- function(df, filter_column, remove_less_than=NULL, remove_greater_than=NULL){
  
  if(is.null(remove_less_than) == FALSE){
    df <- df[abs(df[,filter_column]) >= remove_less_than,]
  }
  
  if(is.null(remove_greater_than) == FALSE){
    df <- df[abs(df[,filter_column]) <= remove_greater_than,]
  }
  
  return(df)
  
}

##################################################################################################
################################## END RESIDUAL PLOT SECTION #####################################
##################################################################################################




##################################################################################################
####################### PARTIAL (COMPONENT PLUS) RESIDUAL PLOT SECTION ###########################
##################################################################################################

check_datatypes <- function(df, analysis_var){
  
  if(!is.numeric(df[,analysis_var])){
    print(paste0(analysis_var, " is  of non-numeric data type."))
    print("Multiple Regression Coefficients Exist For this categorical variable")
    print("Please specify which level you wish to plot the component plus residuals for")
    check_failed = TRUE
  } else{
    check_failed = FALSE
  }
  return(check_failed)
}


add_obs_number_column <- function(df){
  df[,"obs_number"] <- seq(1, as.numeric(nrow(df)))
  return(df)
}

get_augmented_data <- function(df, analysis_var, explanatory_vars) {
  
  # Create the new squared variable name
  squared_var <- paste0(analysis_var, "_squared")
  
  # Add the term to the dataframe
  df[, squared_var] <- df[,analysis_var]**2
  
  # Update the explanatory variables
  explanatory_vars <- append(explanatory_vars, squared_var)
  
  # Update analysis_var, because now there is two.
  analysis_var <- append(analysis_var, squared_var)
  
  augmented_data <- list(data_frame=df, 
                         explanatory_variables=explanatory_vars, 
                         analysis_variables=analysis_var)
  
  return(augmented_data)
  
}

# Fits a linear model based on string values specifying the response and explanatory variables
build_lm_from_strings <- function(df, response_var, explanatory_vars){
  
  # Set up the formula for the linear model fit
  lm_formula <- as.formula(paste0(response_var, "~",str_c(explanatory_vars, collapse=" + ")))
  
  # Fit the model
  fit <- lm(lm_formula, data=df)
  
  return(fit)
  
}

get_analysis_coefs <- function(fit, analysis_var){
  
  # Holds the coefficients for the analysis variables
  analysis_var_coefs <- c()
  
  # For each analysis variable (one variable for standard partial resids, two if augmented)
  for(analysis_index in 1:length(analysis_var)){
    
    # Grab the name of this analysis variable
    analysis_var_name <- analysis_var[analysis_index]
    
    # Loop through the coefficients
    for(coef_index in 1:length(fit$coefficients)){
      
      # Grab the name of the current coef
      coef_name <- names(fit$coefficients[coef_index])
      
      # Check if the name for this coef matches the analysis variable we are currently finding a coef for.
      if(analysis_var_name == coef_name){
        
        # Add the coef to the list
        analysis_var_coefs <- append(analysis_var_coefs, fit$coefficients[[coef_index]])
      }
    }
  }
  
  return(analysis_var_coefs)
}

set_analysis_var_column <- function(df, analysis_var){
  df[,"analysis_variable"] <- df[, analysis_var[1]]
  return(df)
}

compute_partial_residuals <- function(df, fit, analysis_var){
  
  df[,"residuals"] <- fit$residuals
  
  # Setting initial value of the component residuals for the analysis variables to zero.
  df[,"component_resid"] <- 0
  
  # Get the coefficient values for these analysis variable(s)
  analysis_var_coefs <- get_analysis_coefs(fit=fit, analysis_var=analysis_var)
  
  for(index in 1:length(analysis_var_coefs)){
    
    # For each analysis variable, multiple its X values by its coefficient and add it to the component residual column
    df[,"component_resid"] <- df[,"component_resid"] + (df[,analysis_var[index]] * analysis_var_coefs[index])
    
  }
  
  df[,"partial_resid"] <- df[,"residuals"] + df[,"component_resid"]
  
  df <- set_analysis_var_column(df=df, analysis_var=analysis_var)
  
  return(df)
  
}

assign_x_y <- function(df, x, y){
  
  df[, "x_variable"] <- df[,x]
  df[, "y_variable"] <- df[,y]
  return(df)
}

add_least_squares_line <- function(p, df, x_var, y_var, linecolor, linetype, 
                                   show_legend, add_least_squares){
  
  if(!add_least_squares){
    return(p)
  }
  
  df <- assign_x_y(df=df, x=x_var, y=y_var)
  
  fit <- lm(y_variable~x_variable, data=df)
  
  # Get the least squares intercept and slope
  ls_coefs <- fit$coeff
  intercept <- ls_coefs[[1]]
  slope <- ls_coefs[[2]]
  
  p <- p + 
    geom_abline(slope=slope, 
                intercept=intercept, 
                color=linecolor,
                linetype=linetype, 
                show.legend=show_legend)
  
  return(p)
  
}

add_point_removal_comparison_line <- function(p, df, x_var, y_var, linecolor, linetype, 
                                              show_legend, add_pt_removal_line, id_removal_compare_pts,
                                              obs_txt_size, obs_txt_hjust, obs_txt_vjust, obs_txt_color) {
  
  if(typeof(add_pt_removal_line) == "logical" && !add_pt_removal_line){
    return(p)
  }
  
  # Data frame that only contains the points we are removing, saving this so we can
  # color these points differently.
  removed_pts_df <- df[df[,"obs_number"] %in% add_pt_removal_line,]
  
  # Color the points we are removing differently
  p <- p + 
    geom_point(data=removed_pts_df, 
               mapping=aes(x=analysis_variable, y=partial_resid), 
               color=linecolor)
  
  # Remove the list of desired observation numbers before fitting 
  # the new least squares line
  df <- df[!(df[,"obs_number"] %in% add_pt_removal_line),]
  
  p <- add_least_squares_line(p=p, 
                              df=df, 
                              x_var=x_var, 
                              y_var=y_var, 
                              linecolor=linecolor, 
                              linetype=linetype,
                              show_legend=show_legend, 
                              add_least_squares=TRUE)
  
  # Identify the removed points
  if(id_removal_compare_pts){
    p <- p + geom_text(data=removed_pts_df, mapping=aes(x=analysis_variable, y=partial_resid, label=obs_number), 
                       hjust=obs_txt_hjust, vjust=obs_txt_vjust, color=obs_txt_color, size=obs_txt_size)    
  }

  
  return(p)
  
}

add_obs_numbers <- function(p, df, obs_txt_size, obs_txt_color,
                            obs_txt_vjust, obs_txt_hjust, identify_obs) {
  
  if(typeof(identify_obs) == "logical" && !identify_obs){
    return(p)
  }
  
  # If we don't want to identify all observatins, filter based on the vector passed
  # to identify_obs
  if(typeof(identify_obs) != "logical"){
    df <- df[df[,"obs_number"] %in% identify_obs,]
  }
  
  p <- p + geom_text(data=df, mapping=aes(x=analysis_variable, y=partial_resid, label=obs_number), 
                     hjust=obs_txt_hjust, vjust=obs_txt_vjust, color=obs_txt_color, size=obs_txt_size)
  
  return(p)
  
}

add_shading_variable <- function(fit, df, shade_by_case){
  
  if(is.null(shade_by_case)){
    return(df)
  }
  
  # Get the short name for the case statistic we want to shade by
  shading_variable <- case_stat_name_map(case_stat=shade_by_case, get_short_name=TRUE)
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    residual_type=shading_variable, 
                                    remove_less_than=NULL, 
                                    remove_greater_than=NULL)
  
  df[,"shade_by_case"] <- case_df[,shading_variable]
  
  return(df)
  
}

##################################################################################################
##################### END PARTIAL (COMPONENT PLUS) RESIDUAL PLOT SECTION #########################
##################################################################################################





##################################################################################################
############################## CASE STAT VS OBSERVATION SECTION ##################################
##################################################################################################



# Convert a full name to a title by removing underscores and adding capitolization
full_name_to_title <- function(case_stat){
  full_name_split <- str_replace_all(case_stat, pattern="_", replacement=" ")
  title <- str_to_title(full_name_split)
  return(title)
}

case_stat_name_map <- function(case_stat, get_full_name=NULL, get_short_name=NULL){
  
  full_to_short <- list(cooks_d="cook", leverage="h", DFFITS="dffit", 
                        externally_studentized_residuals="stu.res", deleted_standard_deviation="si")
  
  short_to_full <- list(cook="cooks_d", h="leverage", dffit="DFFITS", stu.res="externally_studentized_residuals", 
                        si="deleted_standard_deviation")
  
  if(!is.null(get_full_name)){
    
    # If its already the full name
    if(case_stat %in% short_to_full){
      return(case_stat)
    }
    else{
      return(short_to_full[[case_stat]])
    }
    
  }
  else if(!is.null(get_short_name)){
    
    if(case_stat %in% full_to_short){
      return(case_stat)
    }
    else{
      return(full_to_short[[case_stat]])
    }
  }
}

plot_rule_of_thumb <- function(fit, case_df, case_stat, cook_rot, dffit_rotm, 
                              resid_rot, std_rotm, leverage_rotm, p, ref_linecolor, 
                              ref_linetype, annot_reflines, plot_rot_reflines, 
                              flag_extreme, max_flagged, extreme_value_shape, extreme_value_color, 
                              obs_txt_hjust, obs_txt_size, obs_txt_vjust) {
  
  if(!plot_rot_reflines){
    return(p)
  }
  
  
  if(case_stat == "h"){
    p <- plot_leverage_rot(fit=fit, 
                           p=p, 
                           case_df=case_df, 
                           leverage_rotm=leverage_rotm, 
                           ref_linecolor=ref_linecolor, 
                           ref_linetype=ref_linetype, 
                           annot_reflines=annot_reflines, 
                           flag_extreme=flag_extreme,
                           max_flagged=max_flagged, 
                           extreme_value_shape=extreme_value_shape, 
                           extreme_value_color=extreme_value_color, 
                           obs_txt_hjust=obs_txt_hjust, 
                           obs_txt_size=obs_txt_size, 
                           obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "cook"){
    p <- plot_cook_rot(p=p, 
                       ref_linecolor=ref_linecolor, 
                       ref_linetype=ref_linetype, 
                       cook_rot=cook_rot, 
                       case_df=case_df, 
                       flag_extreme=flag_extreme,
                       max_flagged=max_flagged,                            
                       extreme_value_shape=extreme_value_shape, 
                       extreme_value_color=extreme_value_color, 
                       obs_txt_hjust=obs_txt_hjust, 
                       obs_txt_size=obs_txt_size, 
                       obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "dffit"){
    p <- plot_dffit_rot(fit=fit, 
                        p=p, 
                        dffit_rotm=dffit_rotm, 
                        ref_linetype=ref_linetype, 
                        ref_linecolor=ref_linecolor, 
                        annot_reflines=annot_reflines, 
                        case_df=case_df, 
                        flag_extreme=flag_extreme,
                        max_flagged=max_flagged,                            
                        extreme_value_shape=extreme_value_shape, 
                        extreme_value_color=extreme_value_color, 
                        obs_txt_hjust=obs_txt_hjust, 
                        obs_txt_size=obs_txt_size, 
                        obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "stu.res"){ # Externally Studentized Residuals
    p <- plot_ext_std_resid_rot(p=p, 
                                ref_linetype=ref_linetype, 
                                ref_linecolor=ref_linecolor, 
                                resid_rot=resid_rot, 
                                case_df=case_df,
                                flag_extreme=flag_extreme,
                                max_flagged=max_flagged,                            
                                extreme_value_shape=extreme_value_shape, 
                                extreme_value_color=extreme_value_color, 
                                obs_txt_hjust=obs_txt_hjust, 
                                obs_txt_size=obs_txt_size, 
                                obs_txt_vjust=obs_txt_vjust)
    
  }
  else if(case_stat == "si"){  # deleted standard deviation
    p <- plot_deleted_std_rot(fit=fit, 
                              p=p, 
                              ref_linetype=ref_linetype, 
                              ref_linecolor=ref_linecolor, 
                              std_rotm=std_rotm,
                              annot_reflines=annot_reflines, 
                              case_df=case_df,
                              flag_extreme=flag_extreme,
                              max_flagged=max_flagged,                            
                              extreme_value_shape=extreme_value_shape, 
                              extreme_value_color=extreme_value_color, 
                              obs_txt_hjust=obs_txt_hjust, 
                              obs_txt_size=obs_txt_size, 
                              obs_txt_vjust=obs_txt_vjust)
  }
  
  return(p)
  
}

plot_deleted_std_rot <- function(fit, p, ref_linetype, case_df,
                                 ref_linecolor, std_rotm, annot_reflines,
                                 flag_extreme, max_flagged, extreme_value_shape, 
                                 extreme_value_color, obs_txt_size, obs_txt_vjust, obs_txt_hjust) {
  
  rmse <- summary(fit)$sigma
  
  upper_multiplier <- (1 + std_rotm)
  lower_multiplier <- (1 - std_rotm)
  
  upper_threshold <- rmse * upper_multiplier
  lower_threshold <- rmse * lower_multiplier
  
  p <- p + 
    geom_hline(yintercept=rmse, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=upper_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=lower_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype)
  
  if(flag_extreme){
    
    # Filter to find values above the upper threshold or below
    # the lower threshold
    above_filter <- (case_df[,"si"] >= upper_threshold)
    below_filter <- (case_df[,"si"] <= lower_threshold)
    
    # Combine the above filters, and subset to only include values that exceeded the threshold.
    exceeded_threshold <- above_filter | below_filter
    case_df <- case_df[exceeded_threshold,]
    
    case_df[,"si_difference"] <- rmse - case_df[,"si"]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"si_difference"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  if(!annot_reflines){
    return(p)
  }
  
  return(p)
  
}

plot_ext_std_resid_rot <- function(p, ref_linetype, ref_linecolor,resid_rot, case_df, 
                                   flag_extreme,max_flagged, extreme_value_shape, 
                                   extreme_value_color, obs_txt_size, obs_txt_vjust, 
                                   obs_txt_hjust){
  
  upper_threshold <- resid_rot
  lower_threshold <- resid_rot * -1
  
  p <- p + 
    geom_hline(yintercept=upper_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=lower_threshold, 
               color=ref_linecolor,
               linetype=ref_linetype)
  
  
  if(flag_extreme){
    
    above_filter <- (case_df[,"stu.res"] >= upper_threshold)
    below_filter <- (case_df[,"stu.res"] <= lower_threshold)
    
    # Combine the above filters, and subset to only include values that exceeded the threshold.
    exceeded_threshold <- above_filter | below_filter
    case_df <- case_df[exceeded_threshold,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"stu.res"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  return(p)
  
}

plot_dffit_rot <- function(fit, p, dffit_rotm, case_df, ref_linetype, 
                           ref_linecolor, annot_reflines,flag_extreme, 
                           max_flagged, extreme_value_shape, extreme_value_color,
                           obs_txt_size, obs_txt_vjust, obs_txt_hjust) {
  
  num_parameters <- length(fit$coefficients)
  num_observations <- as.numeric(nrow(case_df))
  
  upper_threshold <- dffit_rotm * sqrt((num_parameters - 1)/num_observations)
  lower_threshold <- upper_threshold * -1
  
  p <- p + 
    geom_hline(yintercept=upper_threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) + 
    geom_hline(yintercept=lower_threshold, 
               color=ref_linecolor,
               linetype=ref_linetype)
  
  
  if(flag_extreme){
    
    above_filter <- (case_df[,"dffit"] >= upper_threshold)
    below_filter <- (case_df[,"dffit"] <= lower_threshold)
    
    # Combine the above filters, and subset to only include values that exceeded the threshold.
    exceeded_threshold <- above_filter | below_filter
    case_df <- case_df[exceeded_threshold,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"dffit"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  
  if(!annot_reflines){
    return(p)
  }
  
  return(p)
  
}

get_average_leverage <- function(fit, case_df){
  
  num_parameters <- length(fit$coefficients)
  num_observations <- as.numeric(nrow(case_df))
  
  # Calculate the average leverage
  avg_leverage = num_parameters / num_observations
  
  return(avg_leverage)
  
}

plot_leverage_rot <- function(fit, p, case_df, leverage_rotm, ref_linecolor, 
                              ref_linetype, annot_reflines, flag_extreme, 
                              max_flagged, extreme_value_shape,extreme_value_color,
                              obs_txt_size, obs_txt_vjust, obs_txt_hjust){
  
  threshold <- get_leverage_threshold(fit=fit, 
                                      leverage_line_multiplier=leverage_rotm, 
                                      case_df=case_df)
  
  average_leverage <- get_average_leverage(fit=fit, case_df=case_df)
  
  p <- p + 
    geom_hline(yintercept=threshold, 
               color=ref_linecolor, 
               linetype=ref_linetype) +
    geom_hline(yintercept=average_leverage, 
               color=ref_linecolor, 
               linetype=ref_linetype)
  
  if(flag_extreme){
    
    case_df <- case_df[case_df[,'h'] >= threshold,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"h"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  if(!annot_reflines){
    return(p)
  }
  
  return(p)
}

plot_cook_rot <- function(p, cook_rot, ref_linecolor, ref_linetype, case_df,
                          flag_extreme, max_flagged, extreme_value_shape, 
                          extreme_value_color,obs_txt_size, obs_txt_vjust, obs_txt_hjust){
  
  p <- p + 
    geom_hline(yintercept=cook_rot, 
               color=ref_linecolor, 
               linetype=ref_linetype)
  
  if(flag_extreme){
    
    threshold_filter <- (case_df[,"cook"] >= cook_rot)
    case_df <- case_df[threshold_filter,]
    
    # Grab the top  max_flagged values that exceeded the threshold, or all of them if less than max_flagged.
    num_extreme_observations <- as.numeric(nrow(case_df))
    num_obs_to_flag <- as.numeric(min(num_extreme_observations, max_flagged))
    case_df <- case_df[order(abs(case_df[,"dffit"]), decreasing=TRUE),][1:num_obs_to_flag,]
    
    p <- p + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color) + 
      geom_point(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column), color=extreme_value_color, 
                 shape=extreme_value_shape) + 
      geom_text(data=case_df, mapping=aes(x=obs_number, y=Resid_Plot_Column, label=obs_number), hjust=obs_txt_hjust,
                vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  return(p)
  
}

##################################################################################################
############################ END CASE STAT VS OBSERVATION SECTION ################################
##################################################################################################




##################################################################################################
################################ RESIDUAL VS LEVERAGE SECTION ####################################
##################################################################################################

get_leverage_threshold <- function(fit, leverage_line_multiplier, case_df){
  
  # Calculate average leverage
  avg_leverage <- get_average_leverage(fit=fit, case_df=case_df)
  
  # Calculate the leverage level at which we want to inspect observations closer
  leverage_threshold <- leverage_line_multiplier * avg_leverage
  
  return(leverage_threshold)
  
}

flag_extreme_observations <- function(fit, residual_type, case_df, flag_extreme_obs, 
                                      max_points_flagged, show_extreme_obs_numbers, resid_line_threshold, 
                                      extreme_value_color, p, leverage_line_multiplier, obs_txt_size, obs_txt_vjust,
                                      obs_txt_hjust){
  
  # Current implementation, flagging only makes sense for externally studentized residuals
  if(!flag_extreme_obs || residual_type != "externally_studentized"){
    return(p)
  }
  
  leverage_threshold <- get_leverage_threshold(fit, leverage_line_multiplier, case_df)
  
  # Filter dataframe to only contain observation that exceed the acceptable thresholds
  # for both leverage and (externally studentized) residual size
  case_df <- case_df[case_df[,"h"] > leverage_threshold,]
  case_df <- case_df[abs(case_df[,"Resid_Plot_Column"]) > resid_line_threshold,]
  
  num_values_to_flag <- min(as.numeric(nrow(case_df)), max_points_flagged)
  
  # Dataframe containing up to max_point_flagged observations with the largest Cooks D
  case_df <- case_df[order(abs(case_df[,"cook"]), decreasing=TRUE),][1:num_values_to_flag,]
  
  # Plot extreme values in red
  p <- p + geom_point(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column), shape=8, color=extreme_value_color) +
    geom_point(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column), color=extreme_value_color)
  
  # If we don't need to show the extreme observation numbers, we can stop the function here.
  if(!show_extreme_obs_numbers){
    return(p)
  }
  
  # Annotate the observation numbers for the extreme points
  p <- p +
    geom_text(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column, label=obs_number),
              hjust=obs_txt_hjust, vjust=obs_txt_vjust, size=obs_txt_size, 
              color=extreme_value_color)
  
  return(p)
}


add_reference_lines <- function(fit, residual_type, case_df, add_reference_lines,
                                leverage_line_multiplier, p, resid_line_threshold, 
                                reference_linetype, reference_linecolor, annotate_thresholds) {
  
  if(!add_reference_lines){
    return(p)
  }
  
  leverage_threshold <- get_leverage_threshold(fit, leverage_line_multiplier, case_df)
  
  p <- p + geom_vline(xintercept=leverage_threshold, 
                      color=reference_linecolor, 
                      linetype=reference_linetype)
  
  # It only makes sense to plot the residual reference lines to externally studentized
  # residuals, so if its any other type, end the function here.
  if(residual_type != "externally_studentized"){
    return(p)
  }
  
  positive_resid_thresh=resid_line_threshold
  negative_resid_thresh=resid_line_threshold*-1
  
  p <- p + 
    geom_hline(yintercept=positive_resid_thresh, color=reference_linecolor, 
               linetype=reference_linetype) + 
    geom_hline(yintercept=negative_resid_thresh, color=reference_linecolor,
               linetype=reference_linetype)
  
  # If we don't want to annotate the threshold values, function can end here
  if(!annotate_thresholds){
    return(p)  
  }
  
  # Annotate the leverage threshold
  leverage_annot <- paste0(leverage_line_multiplier, "x avg leverage")
  leverage_df <- data.frame(txt=leverage_annot, x_txt=leverage_threshold, y_txt=-Inf)
  
  p <- p +
    geom_text(data=leverage_df, mapping=aes(x=x_txt, y=y_txt, label=txt, hjust=-0.05, vjust=-0.5), 
              color=reference_linecolor)
  
  return(p)
  
}

##################################################################################################
############################### END RESIDUAL VS LEVERAGE SECTION #################################
##################################################################################################






##################################################################################################
################################## RESIDUAL HISTOGRAM SECTION ####################################
##################################################################################################

# The deleted and regular residual plots can not have their binwidth set directly in a very intuitive manner
# Therefore it is easier to set the number of bins instead. To overlay the normal curve however, we need to know
# binwidth. This function calculates the correct bin width based on the x-axis values and the number of bins.
get_binwith <- function(binwidth, num_bins, current_plot){
  
  if(is.null(binwidth) == TRUE){
    
    build <- ggplot_build(current_plot)
    x_min <- build$layout$panel_params[[1]]$x.range[1]
    x_max <- build$layout$panel_params[[1]]$x.range[2]
    x_length <- x_max - x_min
    binwidth <- x_length / num_bins
    
  } 
  
  return(binwidth)
  
}

##################################################################################################
################################ END RESIDUAL HISTOGRAM SECTION ##################################
##################################################################################################


##################################################################################################
######################################## QQ PLOT SECTION #########################################
##################################################################################################

# Function to calculate the intercept and slope of the line that goes through the first and third
# quartiles of the (theoretical_quartile, data_quartile) x,y pairs. 
get_quartile_line_params <- function(qq){
  
  # Get the first and third quartiles for the theoretical distribution
  x_quartiles <- quantile(qq$x)
  first_quartile_x <- x_quartiles[[2]]
  third_quartile_x <- x_quartiles[[4]]
  
  # Get the first and third quartiles for the observed distribution (the data).
  y_quartiles <- quantile(qq$y)
  first_quartile_y <- y_quartiles[[2]]
  third_quartile_y <- y_quartiles[[4]]
  
  # Fit a line between the first and third quartiles, to get the intercept and slope for plotting
  line_df <- data.frame(x_quartiles= c(first_quartile_x, third_quartile_x), 
                        y_quartiles=c(first_quartile_y, third_quartile_y))
  
  quartile_line_fit <- lm(y_quartiles~x_quartiles, data=line_df)
  
  
  # Extract line parameters from the fit
  quartile_line_coefs <- quartile_line_fit$coeff
  quartile_line_intercept <- quartile_line_coefs[[1]]
  quartile_line_slope <- quartile_line_coefs[[2]]
  
  line_params <- list(slope=quartile_line_slope, intercept=quartile_line_intercept)
  
  return(line_params)
}

flag_n_largest <- function(qq_df, case_df, p, flag_largest_resid, flag_nlargest, flag_color_resid, 
                           flag_marker_shape, flag_txt_hjust,flag_txt_vjust, flag_txt_size){
  
  
  # Combined the case statistics (residual values) with the qqplot data
  # combined_df <- cbind(qq_df, case_df)
  combined_df <- merge(x=qq_df, y=case_df, by.x="obs_number", by.y="obs_number")
  
  #return(list(c_df=combined_df, q_df=qq_df, case_df=case_df))
    
  # Sort by absolute value of residual, so the largest residuals are at the top of the
  # dataframe, then grab the nlargest of those.
  nlargest_resids <- combined_df[order(abs(combined_df[,"Resid_Plot_Column"]), 
                                       decreasing=TRUE),][1:flag_nlargest,]
    
  p <- p + 
    geom_point(data=nlargest_resids, mapping=aes(x=x_values, y=y_values), color=flag_color_resid) + 
    geom_point(data=nlargest_resids, mapping=aes(x=x_values, y=y_values), color=flag_color_resid, 
               shape=flag_marker_shape) + 
    geom_text(data=nlargest_resids, mapping=aes(x=x_values,y=y_values, label=obs_number), 
              hjust=flag_txt_hjust, vjust=flag_txt_vjust, color=flag_color_resid, size=flag_txt_size)    

  return(p)
  
}


##################################################################################################
###################################### END QQ PLOT SECTION #######################################
##################################################################################################


##################################################################################################
##################################### SCATTER PLOT SECTION #######################################
##################################################################################################

filter_scatter_data <- function(df, filter_column, keep_values, remove_less_than, 
                                remove_greater_than) {
  
  # If there is nothing we want to filter
  if(is.null(filter_column)){
    return(df)
  }
  
  # Categorical filtering based on a vector of categories to keep
  if(!is.null(keep_values)){
    df <- df[df[,filter_column] %in% keep_values,]
    return(df)
  }
  
  # Numeric filtering, remove everything less than
  if(!is.null(remove_less_than)){
    df <- df[df[,filter_column] >= remove_less_than,]
  }
  
  # Numeric filtering, remove everything greater than
  if(!is.null(remove_greater_than)){
    df <- df[df[,filter_column] <= remove_greater_than,]
  }
 
  return(df)
  
}

get_plotting_data <- function(df, x_var, y_var, shade_var, shape_var, size_var, keep_values, 
                              remove_less_than, remove_greater_than, filter_column){
  
  df[,"Response"] <- df[,y_var]
  df[,"Explanatory"] <- df[,x_var]
  
  df <- filter_scatter_data(df=df, 
                            filter_column=filter_column, 
                            keep_values=keep_values, 
                            remove_less_than=remove_less_than, 
                            remove_greater_than=remove_greater_than)
  
  # If we want to shade the points based on a variable
  if(!is.null(shade_var)){
    df[,"shading_variable"] <- as.factor(df[,shade_var])
    shading_variable <- df[,"shading_variable"]
  }
  else{
    shading_variable <- NULL
  }
  
  # If we want to change the points shapes based on a variable
  if(!is.null(shape_var)){
    df[,"shape_variable"] <- as.factor(df[,shape_var])
    shape_variable <- df[,"shape_variable"]
  }
  else{
    shape_variable <- NULL
  }
  
  # If we want to change the points sizes based on a variable
  if(!is.null(size_var)){
    df[,"size_variable"] <- df[,size_var]
    size_variable <- df[,"size_variable"]
  }
  else{
    size_variable <- NULL
  }
  
  
  return_list <- list(data_frame=df, 
                      shading_var=shading_variable, 
                      shape_var=shape_variable, 
                      size_var=size_variable)
  
  return(return_list)
  
}

add_legend_data <- function(p, shade_var, shape_var){
  
  if(is.null(shade_var) == FALSE){
    p <- p + labs(color=shade_var)
  }
  else{
    p <- p + labs(color=NULL)
  }
  
  if(is.null(shade_var) == FALSE){
    p <- p + labs(shape=shape_var)
  }
  else{
    p <- p + labs(shape=NULL)
  }
  
  return(p)
  
}

add_scatterplot_title <- function(p, show_regression, x_var, y_var, fit=NULL, round_digits=4){
  
  if(!show_regression){
    
    plot_title <- paste0("Scatter Plot of ", y_var, " vs ", x_var)
    p <- p + ggtitle(plot_title) + 
      xlab(x_var) + 
      ylab(y_var)
    
    return(p)
  }
  
  # Save summary statistics to add to title
  rmse <- round(summary(fit)$sigma, round_digits)
  r_square <- round(summary(fit)$r.squared, round_digits)
  adj_r_square <- round(summary(fit)$adj.r.squared, round_digits)
  
  # Create plot title.
  plot_title <- paste0("Regression of ", y_var, " on ", x_var, "\n", 
                  "RMSE=", rmse, "  R Square=", r_square, "  Adjusted R Square=", adj_r_square)
  
  p <- p + ggtitle(plot_title) + 
    xlab(x_var) + 
    ylab(y_var)
  
  return(p)
  
}

get_reg_table_location <- function(table_loc){
  
  if(table_loc == "upper_right"){
    x_coord <- Inf
    y_coord <- Inf
  }
  else if(table_loc == "lower_right"){
    x_coord <- Inf
    y_coord <- -Inf
    
  }
  else if(table_loc == "upper_left"){
    x_coord <- -Inf
    y_coord <- Inf
    
  }
  else if(table_loc == "lower_left"){
    x_coord <- -Inf
    y_coord <- -Inf
  }
  
  coords <- list(x_coordinate=x_coord, 
                 y_coordinate=y_coord)
  
  return(coords)
  
}


add_regression_table <- function(reg_table, table_loc, p, x_var, fit=NULL, round_digits=4){
  
  if(!reg_table){
    return(p)
  }
  
  table_coordinates <- get_reg_table_location(table_loc=table_loc)
  
  # Information for creating the table
  annot_data <- data.frame(summary(fit)$coefficients)
  
  # Round values so table isn't way too big
  annot_data <- round(annot_data, digits=round_digits)
  
  # Adjust the tables row names based on the dataset.
  row.names(annot_data)[row.names(annot_data) == "Explanatory"] <- paste0(x_var, "(Slope)")
  
  # Adjust the tables column names
  names(annot_data) <- c("Estimate", "Std_Error", "T_value", "P_value")
  
  p <- p + 
    annotate(geom="table", 
             x=table_coordinates[["x_coordinate"]], 
             y=table_coordinates[["y_coordinate"]], 
             label=list(annot_data), 
             table.rownames=TRUE)
  
  return(p)
  
}

add_regression_and_title <- function(df, show_regression, p, conf_level, pred_band, 
                                     conf_band, reg_linecolor, conf_linecolor, pred_linecolor,
                                     conf_linetype, pred_linetype, x_var, y_var, round_digits, 
                                     reg_table, table_loc){
  
  if(!show_regression){
    p <- add_scatterplot_title(p=p, show_regression=show_regression, x_var=x_var, y_var=y_var)
    return(p)
  }
  
  fit <- lm(Response~Explanatory, data=df)
  
  # Data for prediction band
  pred_data <- as.data.frame(predict(fit, interval="prediction", level=conf_level))
  pred_data <- rename(pred_data, pred_lower=lwr, pred_upper=upr)
  
  # Data for confidence band
  conf_data <- as.data.frame(predict(fit, interval="confidence", level=conf_level))
  conf_data <- rename(conf_data, conf_lower=lwr, conf_upper=upr)
  
  # combine original data, confidence band data, and prediction band data.
  df <- cbind(df, conf_data, pred_data[,c("pred_lower", "pred_upper")])

  
  # Add the regression line
  p <- p + geom_smooth(method="lm", color=reg_linecolor, level=conf_level)
  
  # Add the confidence band
  if(conf_band){
    p <- p + geom_line(data=df, mapping=aes(x=Explanatory, y=conf_lower), 
                       color=conf_linecolor, linetype=conf_linetype) +
      geom_line(data=df, mapping=aes(x=Explanatory, y=conf_upper), 
                color=conf_linecolor, linetype=conf_linetype)
  }
  
  # Add the prediction band
  if(pred_band){
    p <- p + geom_line(data=df, mapping=aes(x=Explanatory, y=pred_lower), color=pred_linecolor, 
                       linetype=pred_linetype) + 
      geom_line(data=df, mapping=aes(x=Explanatory, y=pred_upper), color=pred_linecolor, 
                linetype=pred_linetype)

  }
  
  p <- add_scatterplot_title(p=p, show_regression=show_regression, x_var=x_var, y_var=y_var, 
                             fit=fit, round_digits=round_digits)
  
  p <- add_regression_table(reg_table=reg_table, table_loc=table_loc, p=p, x_var=x_var, 
                            fit=fit, round_digits=round_digits)
  
  return(p)
  
}

##################################################################################################
################################### END SCATTER PLOT SECTION #####################################
##################################################################################################

