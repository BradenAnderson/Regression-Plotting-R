library(tidyverse)
library(car)
library(utilities)
library(HH)
library(patchwork)
library(stringr)
library(EnvStats)
source("./Plot_Helpers.R")

############################# Initial Setup For Test ####################################

#house_df <- read.csv("./train.csv")

#house_df[,"Total_SF"] <- house_df[,"X1stFlrSF"] + 
#  house_df[,"X2ndFlrSF"] + house_df[,"TotalBsmtSF"]


#df <- read.csv("./Practice_House_Data.csv")
#head(df)
#names(df)

############################################################################################


#################################### RESIDUAL PLOT SECTION #######################################

# FUNCTION TO PLOT RESIDUALS VS FITTED VALUES
# valid residual_types: "externally_studentized", "internally_studentized", "regular", "deleted" (PRESS)
#                 
plot_residuals <- function(fit, residual_type="externally_studentized", plot_zero_hline=TRUE,
                           zero_hline_linetype="solid", zero_hline_color="red", remove_less_than=NULL,
                           remove_greater_than=NULL, flag_extreme_values=TRUE, extreme_thresh_std=3.5,
                           extreme_thresh_regular=3, id_extreme_values=FALSE, extreme_value_color="red",
                           obs_txt_size=3, obs_txt_hjust=0, obs_txt_vjust=-0.4){
  

  
  case_df <- get_residual_plot_data(fit=fit, 
                                    residual_type=residual_type, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  
  plot_labels <- get_plot_labels(plot_kind="residual", plot_type_info=residual_type)
  
  p <- ggplot(data=case_df, mapping=aes(x=fitted_values, y=Resid_Plot_Column)) + 
    geom_point() +
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title)
  
  if(plot_zero_hline){
    p <- p + geom_hline(yintercept=0, linetype=zero_hline_linetype, color=zero_hline_color)
    
  }
  
  if(flag_extreme_values){
    
    extreme_rule_of_thumb <- list("externally_studentized"=case_df[abs(case_df[,"stu.res"]) >= extreme_thresh_std,], 
                                  "internally_studentized"=case_df[abs(case_df[,"sta.res"]) >= extreme_thresh_std,], 
                                  "regular"=case_df[order(abs(case_df[,"e"]), decreasing=TRUE),][1:extreme_thresh_regular,],
                                  "deleted"=case_df[order(abs(case_df[,"deleted_resids"]), decreasing=TRUE),][1:extreme_thresh_regular,])
    
    extreme_df <- as.data.frame(extreme_rule_of_thumb[[residual_type]])
    
    p <- p + geom_point(data=extreme_df, mapping=aes(x=fitted_values, y=Resid_Plot_Column), shape=8, color=extreme_value_color) +
      geom_point(data=extreme_df, mapping=aes(x=fitted_values, y=Resid_Plot_Column), color=extreme_value_color)
    
  }
  
  if(id_extreme_values){
    p <- p +       
      geom_text(data=extreme_df, mapping=aes(x=fitted_values, y=Resid_Plot_Column, label=obs_number), 
                hjust=obs_txt_hjust, vjust=obs_txt_vjust, color=extreme_value_color, size=obs_txt_size)
    
  }
  
  
  return(p)
}

################################## END RESIDUAL PLOT SECTION #####################################


#################################### RESIDUAL HISTOGRAM SECTION #######################################

plot_residual_histogram <- function(fit, residual_type="externally_studentized", binwidth=NULL, num_bins=NULL,
                                    remove_less_than=NULL, remove_greater_than=NULL, fill_color="Pink", 
                                    outline_color="Navy", overlay_normal=TRUE, normal_linetype="solid", 
                                    normal_linecolor="black", normal_linesize=0.5){
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    residual_type=residual_type, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  plot_labels <- get_plot_labels(plot_kind="residual_histogram", plot_type_info=residual_type)
  
  num_obs <- sum(!is.na(case_df$Resid_Plot_Column))
  
  p <- ggplot(data=case_df, mapping=aes(x=Resid_Plot_Column)) +
    geom_histogram(binwidth=binwidth, bins=num_bins, fill=fill_color, color=outline_color) + 
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title) 
  
  if(overlay_normal){
    
    binwidth <- get_binwith(binwidth=binwidth, 
                            num_bins=num_bins, 
                            current_plot=p)
    
    p <- p +stat_function(fun=function(x)
      dnorm(x, mean=mean(case_df$Resid_Plot_Column), sd=sd(case_df$Resid_Plot_Column))*binwidth*num_obs, 
      linetype=normal_linetype, 
      color=normal_linecolor, 
      size=normal_linesize)
    
  }
  
  return(p)
    
}

################################## END RESIDUAL HISTOGRAM SECTION #####################################





############################## PARTIAL (COMPONENT PLUS) RESIDUAL PLOT SECTION ###############################

# Before using this function, make sure the reference levels for any categorical variables are set as desired
# by performing: df <- within(df, relevel(categorical_variable, ref=desired_level)) before passing df into this function.
#
# identify_obs --> TRUE to identify all, vector to specify which observation numbers to identify
# add_pt_removal_line --> either FALSE or a vector of OBSERVATION NUMBERS for the points to remove and then 
#                         replot a regression line for.
#
#
plot_partial_residuals <- function(df, analysis_var, response_var="SalePrice", explanatory_vars=c("GrLivArea", "OverallQual"), 
                                   augmented=FALSE, identify_obs=FALSE, show_regression=TRUE, remove_less_than=NULL, 
                                   remove_greater_than=NULL, desired_factor_level=NULL, alpha=0.5, add_least_squares=TRUE,
                                   add_smooth_fit=FALSE, smooth_method="loess", ls_linecolor="red", ls_linetype="solid", 
                                   smooth_linecolor="#B026FF", smooth_linetype="dashed", ls_showlegend=TRUE, obs_txt_color="red", 
                                   obs_txt_vjust=-0.4,obs_txt_hjust=0, obs_txt_size=3, add_pt_removal_line=FALSE, 
                                   removed_pt_color="#30D5C8", removed_pt_showlegend=TRUE, removed_pt_linetype="solid", 
                                   id_removal_compare_pts=TRUE, shade_by_case=NULL){
  
  
  if(check_datatypes(df=df, analysis_var=analysis_var) && is.null(desired_factor_level)){
    return("Invalid datatypes error")
  }
  
  # Add observation number column
  df <- add_obs_number_column(df)
  
  # If the variable we want to plot partial resids for is a categorical variable, each level will have its own coef in the
  # regression model. This section is used to update the analysis_vars name to the one that will be given to its coef in the
  # lm function, which will be the simple concatenation of variable_namelevel_name
  if(!is.null(desired_factor_level)){
    analysis_var <- str_c(analysis_var, desired_factor_level)
  }
  
  # Get plot title and axis labels
  plot_labels <- get_plot_labels(plot_kind="partial_residual", plot_type_info=analysis_var, extra_info=augmented)
  
  if(augmented){
    augmented_data <- get_augmented_data(df=df, analysis_var=analysis_var, explanatory_vars=explanatory_vars)
    df <- augmented_data[["data_frame"]]
    analysis_var <- augmented_data[["analysis_variables"]]
    explanatory_vars <- augmented_data[["explanatory_variables"]]
    
  }
  
  
  fit <- build_lm_from_strings(df=df,response_var=response_var, explanatory_vars=explanatory_vars)
  
  df <- compute_partial_residuals(df=df, fit=fit, analysis_var=analysis_var)
  
  df <- add_shading_variable(fit=fit, df=df, shade_by_case=shade_by_case)
  
  #return(df)
  
  p <- ggplot() +
    geom_point(data=df, mapping=aes(x=analysis_variable, y=partial_resid,  color=shade_by_case), alpha=alpha) +
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title) 
  
  p <- add_legend_data(p, shade_var=shade_by_case, shape_var=NULL)
  
  # If we want to plot a least squares line through the data
  p <- add_least_squares_line(p=p, df=df, x_var="analysis_variable", y_var="partial_resid", linecolor=ls_linecolor, 
                              linetype=ls_linetype,show_legend=ls_showlegend, add_least_squares=add_least_squares)
  
  # If we want points to point their observation number, for investigative purposes
  p <- add_obs_numbers(p=p, df=df, obs_txt_color=obs_txt_color, obs_txt_size=obs_txt_size, obs_txt_vjust=obs_txt_vjust, 
                       obs_txt_hjust=obs_txt_hjust, identify_obs=identify_obs)
  
  p <- add_point_removal_comparison_line(p=p, df=df, x_var="analysis_variable", y_var="partial_resid", 
                                         linecolor=removed_pt_color, linetype=removed_pt_linetype, 
                                         show_legend=removed_pt_showlegend,add_pt_removal_line=add_pt_removal_line,
                                         id_removal_compare_pts=id_removal_compare_pts,obs_txt_size=obs_txt_size, 
                                         obs_txt_hjust=obs_txt_hjust, obs_txt_vjust=obs_txt_vjust, obs_txt_color=removed_pt_color)
  
  if(add_smooth_fit){
    p <- p + 
      geom_smooth(data=df, mapping=aes(x=analysis_variable, y=partial_resid),
                  method=smooth_method, color=smooth_linecolor)
    
  }
  
  return(p)
  
}


############################ END PARTIAL (COMPONENT PLUS) RESIDUAL PLOT SECTION #############################






#################################### CASE STAT VS OBSERVATION SECTION #######################################

# This function is intended to be used with the following case statistics:
# 1) DFFITS, 2) Cooks D, 3) Leverage, 4) Externally Studentized Residual, 5) Deleted Standard Deviation
#
# NOTE: rot abbreviates "rule of thumb" (indicates the value is the rule of thumb)
#       rotm abbreviates "rule of thumb multiplier" indicates the value is a multiplier in an equation
#       that creates the rule of thumb.
#
plot_case_stat_vs_obs <- function(fit, case_stat="cook", remove_less_than=NULL, remove_greater_than=NULL, 
                                  cook_rot=1, dffit_rotm=2, leverage_rotm=3, resid_rot=3, std_rotm=0.05,
                                  ref_linecolor="red", ref_linetype="dashed", annot_reflines=TRUE, 
                                  plot_rot_reflines=TRUE, alpha=0.3, flag_extreme=TRUE, max_flagged=5, 
                                  extreme_value_shape=8, extreme_value_color="red", obs_txt_size=3, 
                                  obs_txt_vjust=-0.4, obs_txt_hjust=0){
  
  case_stat_short_name <- case_stat_name_map(case_stat, get_short_name=TRUE)
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    residual_type=case_stat_short_name, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  
  plot_labels <- get_plot_labels(plot_kind="case_stat_vs_obs", plot_type_info=case_stat)
  
  p <- ggplot(data=case_df) + 
    geom_point(mapping=aes(x=obs_number, y=Resid_Plot_Column), alpha=alpha) +
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title) 
    
  
  p <- plot_rule_of_thumb(fit=fit, case_df=case_df, case_stat=case_stat_short_name, cook_rot=cook_rot, 
                          dffit_rotm=dffit_rotm, resid_rot=resid_rot, std_rotm=std_rotm, leverage_rotm=leverage_rotm,
                          p=p, ref_linecolor=ref_linecolor, ref_linetype=ref_linetype, annot_reflines=annot_reflines, 
                          plot_rot_reflines=plot_rot_reflines, flag_extreme=flag_extreme, max_flagged=max_flagged, 
                          extreme_value_shape=extreme_value_shape, extreme_value_color=extreme_value_color, obs_txt_size=obs_txt_size, 
                          obs_txt_vjust=obs_txt_vjust, obs_txt_hjust=obs_txt_hjust)
  
  return(p)
  
}

#################################### END CASE STAT VS OBSERVATION SECTION #######################################




#################################### RESIDUAL VS LEVERAGE SECTION #######################################

plot_residual_vs_leverage <- function(fit, residual_type="externally_studentized", remove_less_than=NULL, 
                                      remove_greater_than=NULL, add_reference_lines=TRUE, 
                                      leverage_line_multiplier=3, resid_line_threshold=2, reference_linetype="dashed",
                                      reference_linecolor="red", annotate_thresholds=TRUE, flag_extreme_obs=TRUE,
                                      max_points_flagged=4, show_all_obs_numbers=FALSE, extreme_value_color="red", 
                                      show_extreme_obs_numbers=TRUE, obs_txt_size=3, obs_txt_vjust=-0.4, obs_txt_hjust=0) {
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    residual_type=residual_type, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  plot_labels <- get_plot_labels(plot_kind="residual_vs_leverage", plot_type_info=residual_type)
  
  p <- ggplot(data=case_df, mapping=aes(x=h, y=Resid_Plot_Column)) +
    geom_point() + 
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title) 
  
  p <- add_reference_lines(fit=fit, residual_type=residual_type, case_df=case_df, add_reference_lines=add_reference_lines,
                           leverage_line_multiplier=leverage_line_multiplier, p=p, resid_line_threshold=resid_line_threshold,
                           reference_linetype=reference_linetype, reference_linecolor=reference_linecolor, 
                           annotate_thresholds=annotate_thresholds)
  
  p <- flag_extreme_observations(residual_type=residual_type, extreme_value_color=extreme_value_color, fit=fit, p=p, 
                                 show_extreme_obs_numbers=show_extreme_obs_numbers, max_points_flagged=max_points_flagged, 
                                 resid_line_threshold=resid_line_threshold, case_df=case_df, obs_txt_size=obs_txt_size,
                                 obs_txt_vjust=obs_txt_vjust, obs_txt_hjust=obs_txt_hjust, flag_extreme_obs=flag_extreme_obs, 
                                 leverage_line_multiplier=leverage_line_multiplier)
  
  return(p)
  
}

#################################### END RESIDUAL VS LEVERAGE SECTION #######################################




#################################### RESIDUAL QQ PLOT SECTION #######################################

# there are three valid qq_linetypes: 1) "robust" (quartiles), 
#                                     2) "0-1" (intercept zero, slope 1), 3) "least squares"
#
#
plot_residual_qq <- function(fit, residual_type="externally_studentized", distribution="norm", param_list=list(mean=0, sd=1), 
                             estimate_params=FALSE, plot_type="Q-Q", add_line=TRUE, qq_linetype="robust", 
                             duplicate_points_method="standard", points_color="#003f5c", line_color="#ffa600", 
                             linetype="solid", round_digits=5, flag_largest_resid=TRUE, flag_nlargest=3, remove_less_than=NULL, 
                             remove_greater_than=NULL, flag_color_resid="red", flag_marker_shape=8, alpha=1, flag_txt_hjust=0, 
                             flag_txt_vjust=-0.3, flag_txt_size=4){
  
  
  case_df <- get_residual_plot_data(fit=fit, 
                                    residual_type=residual_type, 
                                    remove_less_than=remove_less_than, 
                                    remove_greater_than=remove_greater_than)
  
  
    
  plot_labels <- get_plot_labels(plot_kind="residual_qq", plot_type_info=residual_type)
  
  
  resid_column <- setNames(case_df[,"Resid_Plot_Column"], case_df[,"obs_number"])
  
  # Get the QQPlot data
  qq <- EnvStats::qqPlot(x=resid_column, plot.type=plot_type, qq.line.type=qq_linetype, 
                         add.line=add_line, param.list=param_list, duplicate.points.method=duplicate_points_method, 
                         estimate.params=estimate_params, distribution=distribution)
  
  
  # Get intercept and slope of line through 1st and third quartiles.
  line_params <- get_quartile_line_params(qq)
  
  # (theoretical, observed) pairs
  plot_df = data.frame(x_values=qq$x, y_values=qq$y, obs_number=as.numeric(names(qq$y)))
  
  p <- ggplot(data=case_df) + 
    geom_point(data=plot_df,mapping=aes(x=x_values, y=y_values), color=points_color, alpha=alpha) + 
    geom_abline(slope=line_params$slope, intercept=line_params$intercept, color=line_color,linetype=linetype) + 
    xlab(plot_labels$xlabel) + 
    ylab(plot_labels$ylabel) + 
    ggtitle(plot_labels$title)
  
  p <- flag_n_largest(qq_df=plot_df, case_df=case_df, p=p, flag_largest_resid=flag_largest_resid, flag_nlargest=flag_nlargest, 
                      flag_color_resid=flag_color_resid, flag_marker_shape=flag_marker_shape, flag_txt_hjust=flag_txt_hjust,
                      flag_txt_vjust=flag_txt_vjust,flag_txt_size=flag_txt_size)
  
  
  return(p)
  
}

#################################### END RESIDUAL QQ PLOT SECTION #######################################



#################################### SCATTER PLOT SECTION #######################################

# Note to user: It can be misleading to map variables to both "shape" and "size"
#               at the same time, because you cannot easily compare sizes for different
#               shapes. Also, some shapes are by definition different sizes, so the comparisons
#               made when mapping to both aesthetics will be misleading.
#
plot_scatter <- function(data, x_var, y_var, shade_var=NULL, shape_var=NULL, 
                         size_var=NULL, max_size=8, size_guide=FALSE, alpha=0.8,
                         show_regression=FALSE, conf_level=0.95, pred_band=TRUE, conf_band=TRUE, 
                         reg_linecolor="#ffa600", conf_linecolor="#bc5090", pred_linecolor="#003f5c",
                         conf_linetype="solid", pred_linetype="dashed", round_digits=5, reg_table=TRUE,
                         table_loc="upper_left", filter_column=NULL, keep_values=NULL, 
                         remove_less_than=NULL, remove_greater_than=NULL){
  
  df <- data
  
  data_list <- get_plotting_data(df=df, 
                                 x_var=x_var, y_var=y_var, 
                                 shade_var=shade_var, shape_var=shape_var,
                                 size_var=size_var, filter_column=filter_column, 
                                 keep_values=keep_values, remove_less_than=remove_less_than, 
                                 remove_greater_than=remove_greater_than)
  
  df <- data_list[["data_frame"]]
  shading_variable <- data_list[["shading_var"]]
  shape_variable <- data_list[["shape_var"]]
  size_variable <- data_list[["size_var"]]
  
  p <- ggplot(data=df, mapping=aes(x=Explanatory, y=Response)) + 
    geom_point(aes(color=shading_variable, shape=shape_variable, size=size_variable), alpha=alpha) +
    scale_shape_manual(values=1:nlevels(shape_variable)) + 
    scale_size_area(max_size=max_size, guide=size_guide)
  
  p <- add_legend_data(p, shade_var, shape_var)
  
  p <- add_regression_and_title(df=df, show_regression=show_regression, p=p, conf_level=conf_level, 
                                pred_band=pred_band, conf_band=conf_band, reg_linecolor=reg_linecolor, 
                                conf_linecolor=conf_linecolor, pred_linecolor=pred_linecolor, 
                                conf_linetype=conf_linetype, pred_linetype=pred_linetype, x_var=x_var, 
                                y_var=y_var, round_digits=round_digits, reg_table=reg_table, 
                                table_loc=table_loc)
  
  return(p)
  
}



#################################### END SCATTER PLOT SECTION #######################################
