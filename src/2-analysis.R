# Data Analysis 
# 
# Run CLMM models, produce figs and tables in paper


# Setup ----
pacman::p_load(dplyr, data.table, ordinal, RVAideMemoire, kableExtra, ggplot2)
source(here::here("src/0-source.R"))

df = read.csv(file.path(path, "clean/clean.csv"), stringsAsFactors = TRUE)
baseline = read.csv(file.path(path, "clean/baseline.csv"), stringsAsFactors = TRUE)

SKIP_CLMM_ESTIMATION = TRUE # if FALSE, will rerun CLMM models


# Data prep ---- 

#' return value by IV
min_value = function(IV, data) {
  fmla = as.formula(sprintf("response ~ %s", IV))
  as.character(arrange(aggregate(fmla, data, mean), response)[1, 1])
}

#' set relative level of factors to the group with the lowest DV score
#' after filtering to the provided variable group
set_level = function(data, DV) {
  data = filter(data, variable_group == DV)
  data$Domain = relevel(data$Domain, min_value("Domain", data))
  data$Intervention =  relevel(data$Intervention,  min_value("Intervention", data))
  data$Rationale = relevel(data$Rationale, min_value("Rationale", data))
  data
}

df_acc =  set_level(df, "Acceptability")
df_auto = set_level(df, "Autonomy")
df_succ = set_level(df, "Success")

#' make dataframe with DVs as factors for CLMM models
set_vars_as_factor = function(data, cols) {
  data |> 
    mutate_at(cols, factor) |> 
    select(all_of(cols), BatchDate, variable_group, CB, ResponseId) 
}

cols = c("Domain", "Intervention", "Rationale", "CB", "response")
df_acc_fct =  set_vars_as_factor(df_acc,  cols)
df_auto_fct = set_vars_as_factor(df_auto, cols)
df_succ_fct = set_vars_as_factor(df_succ, cols)


# Run CLMM models ----

fmla = as.formula("response ~ Domain + Intervention + Rationale + BatchDate + CB + (1 | ResponseId)")
fmla_int = as.formula("response ~ Domain + Intervention + Rationale + Domain*Intervention + Domain*Rationale + Rationale*Intervention + Domain*Intervention*Rationale + BatchDate + CB + (1 | ResponseId)")

if(SKIP_CLMM_ESTIMATION) { # skip estimation (~10 hours to run on PC with i7 16GB RAM) and read in results from Dropbox
  
  # manually read files in from dropbox 
  fit_acc_c = read_result("fit_acc_c")
  fit_auto_c = read_result("fit_auto_c")
  fit_suc_c = read_result("fit_suc_c")
  fit_acc_int_c = read_result("fit_acc_int_c")
  fit_auto_int_c = read_result("fit_auto_int_c")
  fit_suc_int_c = read_result("fit_suc_int_c")  
  aov_fit_acc_c = read_result("aov_fit_acc_c")
  aov_fit_auto_c = read_result("aov_fit_auto_c")
  aov_fit_suc_c = read_result("aov_fit_suc_c")
  aov_fit_acc_int_c = read_result("aov_fit_acc_int_c")
  aov_fit_auto_int_c = read_result("aov_fit_auto_int_c")
  aov_fit_suc_int_c = read_result("aov_fit_suc_int_c")  
  
  # read in first batch files
  fit_acc_c = read_result("fit_acc_c_first_batch")
  fit_auto_c = read_result("fit_auto_c_first_batch")
  fit_suc_c = read_result("fit_suc_c_first_batch")
  fit_acc_int_c = read_result("fit_acc_int_c_first_batch")
  fit_auto_int_c = read_result("fit_auto_int_c_first_batch")
  fit_suc_int_c = read_result("fit_suc_int_c_first_batch")  
  aov_fit_acc_c = read_result("aov_fit_acc_c_first_batch")
  aov_fit_auto_c = read_result("aov_fit_auto_c_first_batch")
  aov_fit_suc_c = read_result("aov_fit_suc_c_first_batch")
  aov_fit_acc_int_c = read_result("aov_fit_acc_int_c_first_batch")
  aov_fit_auto_int_c = read_result("aov_fit_auto_int_c_first_batch")
  aov_fit_suc_int_c = read_result("aov_fit_suc_int_c_first_batch")
} else {
  
  tik = Sys.time()
  
  fit_acc_c  = clmm(formula = fmla, data = df_acc_fct)
  fit_auto_c = clmm(formula = fmla, data = df_auto_fct)
  fit_suc_c  = clmm(formula = fmla, data = df_succ_fct)
  fit_acc_int_c  = clmm(formula = fmla_int, data = df_acc_fct)
  fit_auto_int_c = clmm(formula = fmla_int, data = df_auto_fct)
  fit_suc_int_c  = clmm(formula = fmla_int, data = df_succ_fct)

  aov_fit_acc_c = Anova.clmm(fit_acc_c)
  aov_fit_auto_c = Anova.clmm(fit_auto_c)
  aov_fit_suc_c = Anova.clmm(fit_suc_c)
  aov_fit_acc_int_c = Anova.clmm(fit_acc_int_c)
  aov_fit_auto_int_c = Anova.clmm(fit_auto_int_c)
  aov_fit_suc_int_c = Anova.clmm(fit_suc_int_c)

  save_results(fit_acc_c)
  save_results(fit_auto_c)
  save_results(fit_suc_c)
  save_results(fit_acc_int_c)
  save_results(fit_auto_int_c)
  save_results(fit_suc_int_c)
  save_results(aov_fit_acc_c)
  save_results(aov_fit_auto_c)
  save_results(aov_fit_suc_c)
  save_results(aov_fit_acc_int_c)
  save_results(aov_fit_auto_int_c)
  save_results(aov_fit_suc_int_c)
  
  # repeat only using the participants collected in the first batch
  fmla = as.formula("response ~ Domain + Intervention + Rationale  + CB + (1 | ResponseId)")
  fmla_int = as.formula("response ~ Domain + Intervention + Rationale + Domain*Intervention + Domain*Rationale + Rationale*Intervention + Domain*Intervention*Rationale + CB + (1 | ResponseId)")
  
  fit_acc_c_first_batch  = clmm(formula = fmla, data = subset(df_acc_fct, BatchDate == "2023-01-11"))
  fit_auto_c_first_batch = clmm(formula = fmla, data = subset(df_auto_fct, BatchDate == "2023-01-11"))
  fit_suc_c_first_batch  = clmm(formula = fmla, data = subset(df_succ_fct, BatchDate == "2023-01-11"))
  fit_acc_int_c_first_batch  = clmm(formula = fmla_int, data = subset(df_acc_fct, BatchDate == "2023-01-11"))
  fit_auto_int_c_first_batch = clmm(formula = fmla_int, data = subset(df_auto_fct, BatchDate == "2023-01-11"))
  fit_suc_int_c_first_batch  = clmm(formula = fmla_int, data = subset(df_succ_fct, BatchDate == "2023-01-11"))
  
  aov_fit_acc_c_first_batch = Anova.clmm(fit_acc_c_first_batch)
  aov_fit_auto_c_first_batch = Anova.clmm(fit_auto_c_first_batch)
  aov_fit_suc_c_first_batch = Anova.clmm(fit_suc_c_first_batch)
  aov_fit_acc_int_c_first_batch = Anova.clmm(fit_acc_int_c_first_batch)
  aov_fit_auto_int_c_first_batch = Anova.clmm(fit_auto_int_c_first_batch)
  aov_fit_suc_int_c_first_batch = Anova.clmm(fit_suc_int_c_first_batch)
  
  save_results(fit_acc_c_first_batch)
  save_results(fit_auto_c_first_batch)
  save_results(fit_suc_c_first_batch)
  save_results(fit_acc_int_c_first_batch)
  save_results(fit_auto_int_c_first_batch)
  save_results(fit_suc_int_c_first_batch)  
  save_results(aov_fit_acc_c_first_batch)
  save_results(aov_fit_auto_c_first_batch)
  save_results(aov_fit_suc_c_first_batch)
  save_results(aov_fit_acc_int_c_first_batch)
  save_results(aov_fit_auto_int_c_first_batch)
  save_results(aov_fit_suc_int_c_first_batch)
  
  tok = Sys.time()
  log = sprintf("CLMM models took %s %s\nStart: %s\nEnd: %s", round(tok-tik, 2), units(tok-tik), tik, tok)
  write(log, file = file.path(result_path, "models", "log", sprintf("CMER-results-first-batch-%s.txt", gsub(":", "", Sys.time()))))
}


# Main table of ANODE results ---- 

#' Clean up ANOVA results for table
format_table = function(fit) {
  
  data.frame(fit) |>
    mutate(var = row.names(fit), 
           p = ifelse(Pr..Chisq. < 0.001, "<0.001", round(Pr..Chisq., 3)), 
           effect = case_when(Pr..Chisq. < 0.01 ~ "***",
                              Pr..Chisq. < 0.05 ~ "**",
                              Pr..Chisq. < 0.1 ~ "*",
                              TRUE ~ ""), 
           var = ifelse(var == "CB", "Counter Balance", var), 
           var = gsub("Nudge", "Intervention", var), 
           var = gsub(":", "*", var)) |> 
    select(-Pr..Chisq.)
}


tbl = merge(format_table(aov_fit_acc_int_c), 
            format_table(aov_fit_auto_int_c),
            by = "var", 
            all.x = TRUE) |> 
  merge(format_table(aov_fit_suc_int_c),
        by = "var", 
        all.x = TRUE)

tbl = tbl[c(3, 7, 9, 4, 6, 8, 5, 1, 2), ] # reorder rows manually
col_names = c("ChiSq", "Df", "P-value", "") # rename cols

tbl |> 
  kbl(digits = 3, 
      align = "lrrrlrrrlrrrl",
      col.names = c("", rep(col_names, 3)), 
      row.names = FALSE, 
      caption = "Analysis of Deviance Across Interaction Models") |> 
  kable_classic_2() |> 
  add_header_above(c(" ", "Acceptability" = 4, "Autonomy" = 4, "Success" = 4)) |> 
  save_kable(file = file.path(result_path, "tables", "F_stat_all_dv_tbl.html"))

#' Clean up ANOVA results for table - repeat for first batch

tbl_first_batch = merge(format_table(aov_fit_acc_int_c_first_batch), 
            format_table(aov_fit_auto_int_c_first_batch),
            by = "var", 
            all.x = TRUE) |> 
  merge(format_table(aov_fit_suc_int_c_first_batch),
        by = "var", 
        all.x = TRUE)

tbl_first_batch = tbl_first_batch[c(2, 6, 8, 3, 5, 7, 4, 1), ] # reorder rows manually
col_names = c("ChiSq", "Df", "P-value", "") # rename cols

tbl_first_batch |> 
  kbl(digits = 3, 
      align = "lrrrlrrrlrrrl",
      col.names = c("", rep(col_names, 3)), 
      row.names = FALSE, 
      caption = "Analysis of Deviance Across Interaction Models") |> 
  kable_classic_2() |> 
  add_header_above(c(" ", "Acceptability" = 4, "Autonomy" = 4, "Success" = 4)) |> 
  save_kable(file = file.path(result_path, "tables", "F_stat_all_dv_tbl_first_batch.html"))


# Bar plots ---- 

common_barplot = function(DV, IV, first_batch=FALSE) {
  
  # subset data to DV and IV
  if(first_batch) {
    tmp = subset(df, variable_group == DV & BatchDate == "2023-01-11")
  } else {
    tmp = subset(df, variable_group == DV)
  }
  
  fmla = as.formula(sprintf("response ~ %s * ResponseId", IV))
  dat = aggregate(fmla, tmp, mean)
  plot_dat = Rmisc::summarySE(dat, measurevar = "response", groupvars=c(IV))
  names(plot_dat) = c("x", "N", "response", "sd", "se", "ci")
  
  # make plot 
  ggplot(plot_dat,
         aes(x = x, y = response, ymin = response - ci, ymax = response + ci)) +
    # geom_bar(stat = "identity") +
    geom_segment(aes(y = 1, yend = response, x = x, xend = x), size = 20, color = "grey30") + # work around to start bars at 1, not 0
    geom_errorbar(width = 0.2) +
    labs(x = IV, y = DV) + 
    coord_cartesian(ylim = c(1, 7)) +
    scale_y_continuous(n.breaks = 7) + 
    theme(
      panel.background = element_rect(fill = "white", colour = "black"),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 9),
      axis.title.y = element_text(face = "bold", size = 12),
      axis.text.y = element_text(size = 9))
  
}

#' used to give adhoc overlapping of common barplots
overlapping_ci_plot = function(DV, IV, first_batch=FALSE) {
  
  # subset data to DV and IV
  if(first_batch) {
    tmp = subset(df, variable_group == DV & BatchDate == "2023-01-11")
  } else {
    tmp = subset(df, variable_group == DV)
  }
  
  fmla = as.formula(sprintf("response ~ %s * ResponseId", IV))
  dat = aggregate(fmla, tmp, mean)
  plot_dat = Rmisc::summarySE(dat, measurevar = "response", groupvars=c(IV))
  names(plot_dat) = c("x", "N", "response", "sd", "se", "ci")
  
  # determine which pairs are different 
  ggplot(plot_dat,
         aes(x = x, y = response, ymin = response - ci, ymax = response + ci)) +
    # geom_bar(stat = "identity") +
    # geom_segment(aes(y = response - ci, yend = response + ci, x = x, xend = x), size = 20, color = "grey30") + # work around to start bars at 1, not 0
    geom_point() + 
    geom_errorbar(width = 4) +
    labs(x = IV, y = DV) + 
    coord_cartesian(ylim = c(1, 7)) +
    scale_y_continuous(n.breaks = 7) + 
    coord_flip() + 
    theme(
      panel.background = element_rect(fill = "white", colour = "black"),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 9),
      axis.title.y = element_text(face = "bold", size = 12),
      axis.text.y = element_text(size = 9))
}

# loop through making all permutations of bar plots
for (DV in c("Acceptability", "Autonomy", "Success")) {
  for (IV in c("Intervention", "Rationale", "Domain")) {
    
    plt = common_barplot(DV, IV)
    ggsave(filename = file.path(result_path, "plots",  sprintf("barplot-%s-%s.png", DV, IV)), 
           plot = plt, 
           height = 3.5, # we can change dimensions of the plot output here
           width = 5.5)
    
    plt = overlapping_ci_plot(DV, IV)
    ggsave(filename = file.path(result_path, "plots",  sprintf("overlap-barplot-%s-%s.png", DV, IV)), 
           plot = plt, 
           height = 3.5, # we can change dimensions of the plot output here
           width = 5.5)    
    
    plt = common_barplot(DV, IV, first_batch=TRUE)
    ggsave(filename = file.path(result_path, "plots",  sprintf("barplot-%s-%s-first_batch.png", DV, IV)), 
           plot = plt, 
           height = 3.5, # we can change dimensions of the plot output here
           width = 5.5)
    
    plt = overlapping_ci_plot(DV, IV, first_batch=TRUE)
    ggsave(filename = file.path(result_path, "plots",  sprintf("overlap-barplot-%s-%s-first_batch.png", DV, IV)), 
           plot = plt, 
           height = 3.5, # we can change dimensions of the plot output here
           width = 5.5)
  }
}
  

# join two bar graphs into one for paper 
title = ggpubr::text_grob("Figure 1: average ratings of acceptability and perceived threat to autonomy for each level of Intervention", size = 10, face = "italic", x=0, hjust=0)
notes = ggpubr::text_grob("Error bars represent +/- 1 SE.", size = 9, face = "italic", x=0, hjust=0)

plt = gridExtra::grid.arrange(common_barplot("Acceptability", "Intervention"), 
                              common_barplot("Autonomy", "Intervention"), 
                              nrow = 1, 
                              top = title, 
                              bottom = notes)
ggsave(filename = file.path(result_path, "plots", "intervention-barplot-acceptability-autonomy.png"), 
       plot = plt, 
       height = 4, # we can change dimensions of the plot output here
       width = 10)


plt_first_batch = gridExtra::grid.arrange(common_barplot("Acceptability", "Intervention", first_batch = TRUE), 
                                          common_barplot("Autonomy", "Intervention", first_batch = TRUE), 
                                          nrow = 1, 
                                          top = title, 
                                          bottom = notes)
ggsave(filename = file.path(result_path, "plots", "intervention-barplot-acceptability-autonomy-first-batch.png"), 
       plot = plt_first_batch, 
       height = 4, # we can change dimensions of the plot output here
       width = 10)


# Appendix Analysis ----

## Checking each 5x5x5 ANOVA - ANOVA results holding the other two candidates fixed
# anova(y ~ x1, data = subset(data, x2==a & x3==b))
# we drop the CB here, as we are filtering into each CB

interventions = unique(df$Intervention)
domains = unique(df$Domain)
rationales = unique(df$Rationale)
dvs = unique(df$variable_group)

# using an ANOVA here to compare mean responses
intervention_domain = data.frame()
for (DV in dvs) {
  for (i in interventions) { # notice we have three loops varying what we loop over in these terms
    for (j in domains) {
      
      data = subset(df, Domain == j & Intervention == i & variable_group == DV)
      fit = lm(formula = response ~ 0 + Rationale, data = data)
      coef_tbl = summary(fit)$coefficients |> data.frame()
      coef_tbl$intervention = as.character(i)
      coef_tbl$domain = as.character(j)
      coef_tbl$var = row.names(coef_tbl)
      coef_tbl$dv = DV
      intervention_domain = rbind(intervention_domain, coef_tbl)
    }
  }
}

intervention_rationale = data.frame()
for (DV in dvs) {
  for (i in interventions) {
    for (j in rationales) {
      
      data = subset(df, Rationale == j & Intervention == i & variable_group == DV)
      fit = lm(formula = response ~ 0 + Domain, data = data)
      coef_tbl = summary(fit)$coefficients |> data.frame()
      coef_tbl$intervention = as.character(i)
      coef_tbl$rationale = as.character(j)
      coef_tbl$var = row.names(coef_tbl)
      coef_tbl$dv = DV
      intervention_rationale = rbind(intervention_rationale, coef_tbl)
    }
  }
}

domain_rationale = data.frame()
for (DV in dvs) {
  for (i in domains) {
    for (j in rationales) {
      
      data = subset(df, Rationale == j & Domain == i & variable_group == DV)
      fit = lm(formula = response ~ 0 + Intervention, data = data)
      coef_tbl = summary(fit)$coefficients |> data.frame()
      coef_tbl$domain = as.character(i)
      coef_tbl$rationale = as.character(j)
      coef_tbl$var = row.names(coef_tbl)
      coef_tbl$dv = DV
      domain_rationale = rbind(domain_rationale, coef_tbl)
    }
  }
}

# plot 5 x 5 facet plot of the effects for each of the groups
# bonferroni correction on CIs -> 95% CI w correction 
#      with 5 CIs, there are 10 pairwise comparisons
#   => alpha = 1 - 0.05 / m; 
#      m = number of pairwise comparisons (10)
plot_five_five = function(data, groups, bonferroni_correction=FALSE) {

  # testing
  # data = intervention_domain
  # group = "ID"
  #
  if(bonferroni_correction) {
    alpha = 0.05 / 125
    caption = "Error bars show 95% CI with Bonferroni corrections for a total of 125 comparisons."
  } else {
    alpha = 0.05
    caption = "Error bars show 95% CI without Bonferroni corrections for multiple hypothesis testing."
  }
  
  crit_value = qnorm(1 - alpha / 2) 
  
  plt = data |> 
    filter(!var %in% c("(Intercept)", "CB", "EndDate") ) |> 
    mutate(factor = case_when(grepl("Domain", var) ~ "Domain", # cleaning up text for plot
                              grepl("Intervention", var) ~ "Intervention", 
                              grepl("Rationale", var) ~ "Rationale", 
                              TRUE ~ "OTHER"), 
           var = gsub("Domain|Intervention|Rationale", "", var)) |> 
    ggplot(aes(x = dv, y = Estimate, color = var, 
               ymin = Estimate - crit_value * Std..Error, ymax = Estimate + crit_value * Std..Error )) +
    geom_point(position = position_dodge(0.7)) + 
    geom_errorbar(position = position_dodge(0.7), width = 0.2) +
    scale_color_viridis_d() + 
    scale_y_continuous(limits = c(1.7, 7)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
          legend.position = "bottom", 
          panel.spacing.x=unit(0.1, "lines"), 
          panel.spacing.y=unit(0.1, "lines"),
          plot.subtitle = element_blank(), 
          strip.text = element_text(size = 8)) + 
    labs(caption = caption, 
         color = "Measure")
  
  # control facet wrap by pairwise group
  if (groups == "ID") {
    plt + facet_grid(domain~intervention, labeller = label_wrap_gen(multi_line = TRUE, width = 10)) + labs(x = "Intervention", y = "Domain", title = "2.1 Exploring differences between levels of Rationale")
  } else if (groups == "DR") {
    plt + facet_grid(rationale~domain, labeller = label_wrap_gen(multi_line = TRUE, width = 10)) + labs(x = "Domain", y = "Rationale", title = "2.2 Exploring differences between levels of Intervention")
  } else if (groups == "IR") {
    plt + facet_grid(rationale~intervention, labeller = label_wrap_gen(multi_line = TRUE, width = 10)) + labs(x = "Intervention", y = "Rationale", title = "2.3 Exploring differences between levels of Domain")
  } else {
    cat("Error")
  }
}

height = 6
width = 8

plot_five_five(intervention_domain, "ID")
ggsave(filename = file.path(result_path, "plots",  sprintf("intervention-domain.png")), height = height, width = width)
plot_five_five(domain_rationale, "DR")
ggsave(filename = file.path(result_path, "plots",  sprintf("domain-rationale.png")), height = height, width = width)
plot_five_five(intervention_rationale, "IR")
ggsave(filename = file.path(result_path, "plots",  sprintf("intervention-rationale.png")), height = height, width = width)

# repeat analysis with Bonferroni corrections. 
plot_five_five(intervention_domain, "ID", bonferroni_correction=TRUE)
ggsave(filename = file.path(result_path, "plots",  sprintf("intervention-domain-bon.png")), height = height, width = width)
plot_five_five(domain_rationale, "DR", bonferroni_correction=TRUE)
ggsave(filename = file.path(result_path, "plots",  sprintf("domain-rationale-bon.png")), height = height, width = width)
plot_five_five(intervention_rationale, "IR", bonferroni_correction=TRUE)
ggsave(filename = file.path(result_path, "plots",  sprintf("intervention-rationale-bon.png")), height = height, width = width)