# Pilot Data Cleaning
# 
# Read in the Qualtrics survey & prepare data for analysis 

# setup ---- 
pacman::p_load(tidyr, lmerTest, dplyr, forcats, ordinal, RVAideMemoire, ggplot2)


# Functions ----

# run LMER and CLMM models 
model = function(data, LHS) {
  
  # prev results changed factors for success
  if(LHS == "success") {
    data = data |> 
      mutate(Domain = fct_relevel(Domain, "Electric Vehicle"), 
             Nudge = fct_relevel(Nudge, "Social Proof"), 
             Treatment = fct_relevel(Treatment, "Control"))
  }
  
  fmla = as.formula(sprintf("%s ~ Domain + Nudge + Treatment + (1|ResponseId) + CounterBalance", LHS))
  
  # lmer model
  fit_lmer = lmer(fmla, data, REML=FALSE)
  anova_lmer = anova(fit_lmer)
  summary_lmer = summary(fit_lmer)
  
  # clmm model
  clmm_data = data |> 
    mutate(acceptability = as.factor(acceptability),
           autonomy = as.factor(autonomy),
           success = as.factor(success))
  
  fit_clmm = clmm(fmla, clmm_data)
  anova_clmm = Anova.clmm(fit_clmm)
  summary_clmm = summary(fit_clmm)
  
  # return model results
  list(fit_lmer = fit_lmer, 
       anova_lmer = anova_lmer, 
       summary_lmer = summary_lmer, 
       eta_sq_lmer = eta_sq(fit_lmer),
       fit_clmm = fit_clmm, 
       anova_clmm = anova_clmm, 
       summary_clmm = summary_clmm, 
       eta_sq_clmm = eta_sq(fit_clmm))
}

# Join data across models and format 
table = function(model) {
  
  lmer_cols = model$summary_lmer$coefficients |>
    tibble::as.tibble() |> 
    mutate(term = rownames(model$summary_lmer$coefficients))
  
  clmm_cols = model$summary_clmm$coefficients |> 
    tibble::as.tibble() |> 
    mutate(term = rownames(model$summary_clmm$coefficients))
  
  tbl = full_join(lmer_cols, clmm_cols, by = "term", suffix = c("_lmer", "_clmm")) |> 
    janitor::clean_names()
  
  # format and reorder cols 
  tbl |> 
    mutate(factor = case_when(grepl("Domain", term) ~ "Domain", 
                              grepl("Nudge", term) ~ "Nudge", 
                              grepl("Treatment", term) ~ "Treatment", 
                              grepl("CounterBalance", term) ~ "Group", 
                              TRUE ~ "Group"), 
           term = gsub("Domain|Nudge|Treatment|CounterBalance", "", term), 
           or = exp(estimate_clmm), 
           ci_lmer = sprintf("%.2f - %.2f", estimate_lmer - 1.96 * (std_error_lmer), estimate_lmer + 1.96 * (std_error_lmer)),
           ci_clmm = sprintf("%.2f - %.2f", exp(estimate_clmm - 1.96 * (std_error_clmm)), exp(estimate_clmm + 1.96 * (std_error_clmm)))) |> 
    select(factor, term, estimate_lmer, ci_lmer, df, t_value, pr_t, or, ci_clmm, z_value, pr_z) |> 
    filter(!grepl("(Intercept)|\\|", term))
  
}

make_plot = function(x, y) {
  
  y_lab = stringr::str_to_title(substitute(y)) # convert argument as an element to a string
  
  plot_data |> 
    group_by({{x}}) |> # double curly brackets allow passing variable as an argument to the function
    summarise(mean = mean({{y}}, na.rm=T), 
              se = sd({{y}}, na.rm=T) / sqrt(n())) |> 
    ggplot(aes(x = {{x}}, y = mean, ymin = mean - se, ymax = mean + se)) + 
    geom_bar(stat="identity") + 
    geom_errorbar(width=0.2) +
    theme_classic() + 
    labs(y = y_lab) + 
    theme(panel.background = element_rect(fill="white",colour="black"),
          axis.title.x = element_text(face="bold",size=12),
          axis.text.x = element_text(size=9),
          axis.title.y = element_text(face="bold",size=12),
          axis.text.y = element_text(size=9))
}


# Clean data ----
raw = read.csv('data/Ethics_pilot_all_long.csv')
df = raw |> 
  pivot_wider(names_from = measure, values_from = value) |> 
  mutate(Domain = fct_relevel(Domain, "Retirement Savings"), 
         Nudge = fct_relevel(Nudge, "Default"), 
         Treatment = fct_relevel(Treatment, "Control"), 
         CounterBalance = factor(CounterBalance), 
         CounterBalance = fct_relevel(CounterBalance, "1"))


# Analysis ---- 
tictoc::tic("Run models")
results = lapply(c("acceptability", "autonomy", "success"), \(x) model(df, x)) # run on acceptability for testing 
tictoc::toc() # 60s

tbl_acceptability = table(results[[1]]) 
tbl_autonomy = table(results[[2]])
tbl_success = table(results[[3]])


# view tables
require(kableExtra)
tbl_acceptability |> kbl(digits = 2) |> kable_paper("striped")
tbl_autonomy |> kbl(digits = 2) |> kable_paper("striped")
tbl_success |> kbl(digits = 2) |> kable_paper("striped")


# Boxplots 
cols = c("Domain", "Nudge", "Treatment", "CounterBalance")
plot_data = df |> 
  mutate_at(cols, as.character)

make_plot(x = Domain, y = acceptability)
make_plot(x = Nudge, y = acceptability)
make_plot(x = Treatment, y = acceptability)

make_plot(x = Treatment, y = autonomy)
make_plot(x = CounterBalance, y = autonomy)

make_plot(x = Domain, y = success)
make_plot(x = Nudge, y = success)
make_plot(x = Treatment, y = success)

