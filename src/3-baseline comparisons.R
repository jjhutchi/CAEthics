## Data Analysis - Baseline Comparisons
#
# Compare response values in baseline to experiment values


# Setup ----
pacman::p_load(dplyr, data.table, ordinal, RVAideMemoire, kableExtra, ggplot2, lme4)
source(here::here("src/0-source.R"))

df = read.csv(file.path(path, "clean", "clean.csv"), stringsAsFactors = TRUE)
baseline = read.csv(file.path(path, "clean", "baseline.csv"), stringsAsFactors = TRUE)


#' function to conduct data for t-testing experiment responses against baseline responses
#' as well as include the stars associated with p-value
experiment_baseline_t_test = function(y1, y2, cb, label) {
  
  mu1 = mean(y1)
  mu2 = mean(y2)
  s1 = sd(y1)
  s2 = sd(y2)
  n1 = length(y1)
  n2 = length(y2)
  
  t = ( mu1 - mu2 ) / ( sqrt (s1^2/n1 + s2^2/n2 ) )
  dof = (s1^2 / n1 + s2^2 / n2)^2 / ( (s1^2/n1)^2 / (n1 - 1) +  (s2^2/n2)^2 / (n2 - 1))
  p = 1 - pt(t, df=dof)
  
  
  if( p > 0.1 ) {
    lab = ""
  } else if( p > 0.05) {
    lab = "*"
  } else if(p > 0.01) {
    lab = "**"
  } else {
    lab = "***"
  }
  
  data.frame(label = lab, 
             pvalue = p, 
             tstat = t, 
             CB = cb,
             questions = label)
  
}


# Baseline figures ----
# get the average response per CB group, and compare with the average of these baseline scores

# two-way t-test for paired samples
# n1 > n2, we have 5 group of responses versus one set of responses for the baseline
# t = mu_1 - mu_2 / sqrt(s_1^2/n_1 + s_2^2/n_2)
# df = (s_1^2 / n_1 + s_2^2 / n_2)^2 / ( (s_1^2/n_1)^2 / (n_1 - 1) +  (s_2^2/n_2)^2 / (n_2 - 1))
# source: http://daniellakens.blogspot.com/2015/01/always-use-welchs-t-test-instead-of.html
baseline_comparison = left_join(df |> select(ResponseId, response, CB, question), 
                                baseline |> select(ResponseId, response, column), 
                                by = c("ResponseId", "question"="column"),
                                suffix = c("", "_baseline"), 
                                all.x = TRUE) 
baseline = left_join(baseline, 
                     df |> select(ResponseId, CB) |> unique(), 
                     by = "ResponseId") 

avg_response = mean(df[ , ]$response)
avg_baseline = mean(baseline[!is.na(baseline$CB), ]$response)

q_list = c("Q1|Q2|Q3", "Q4|Q5|Q6", "Q7")
tstats = data.frame()

for (i in 1:25) {
  for(j in q_list) {
    
    y1 = df[df$CB == i & grepl(j, df$question), ]$response
    y2 = baseline[baseline$CB == i & grepl(j, baseline$column) & !is.na(baseline$CB), ]$response
    
    tstat = experiment_baseline_t_test(y1, y2, i, j)
    tstats = rbind(tstats, tstat)
  }
}

caption = "Notes:
Mean responses in the baseline and experiment conditions were compared with
two-way paired t-test across all counter balance groups. This plot does not 
correct for multiple tests, and should be viewed as descriptive evidence that 
participants responded differently in the main experiment than in the baseline scenario."

plt = tstats |> 
  mutate(facet_name = case_when(
    questions == "Q1|Q2|Q3" ~ "Acceptability", 
    questions == "Q4|Q5|Q6" ~ "Autonomy", 
    TRUE ~ "Success"
  )) |> 
  ggplot(aes(x = tstat, y = CB, group = facet_name)) + 
  geom_point() + 
  facet_wrap(.~facet_name) + 
  geom_vline(xintercept = 1.96) + 
  geom_vline(xintercept = -1.96) + 
  geom_vline(xintercept = 0, linetype = "dashed") +  
  # scale_x_continuous(limits = c(-7,7)) + 
  theme(plot.subtitle = element_blank()) + 
  labs(title = "Baseline Condition Responses", 
       x = "T Stat", 
       y = "Counter Balance Group", 
       caption = caption)
ggsave(filename = file.path(result_path, "plots",  "baseline-test.png"), 
       plot = plt, 
       height = 5, # we can change dimensions of the plot output here
       width = 7)



# plot mean response in baseline v. experiment across all Qs
baseline_comparison |> 
  group_by(CB) |> 
  summarise(mean = mean(response), 
            baseline = mean(response_baseline)) |> 
  # left_join(stars, by = "CB") |> 
  ggplot() +
  geom_segment(aes(x = mean, xend = baseline, y = CB, yend = CB)) + 
  geom_point(aes(x = mean, y = CB), color = "darkcyan") + 
  geom_point(aes(x = baseline, y = CB), color = "grey30") + 
  # geom_text(aes(x = (mean + baseline) / 2, y = CB + 0.1, label = labels)) + 
  geom_vline(xintercept = avg_response, color = "darkcyan") + 
  geom_vline(xintercept = avg_baseline, color = "grey30") + 
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  # scale_y_continuous(breaks = 1:25) + 
  theme_bw() + 
  labs(caption = "Stars show paired t-test significance.\nTesting average response in experiment\ndiffers from average baseline response.\n *** < 0.01 ** < 0.05 * < 0.1", 
       x = "Average Response", 
       y = "Counter Balance Group")

# sample size by group
tbl = df |> 
  select(ResponseId, CB, EndDate) |> 
  group_by(CB) |> 
  unique() |>
  tally() 

cbind(tbl[1:13, ], rbind(tbl[14:25, ], c("Total", 434))) |> 
  kbl(col.names = rep(c("Counter Balance", "N"), 2), align = "lclc") |> 
  kable_paper(full_width=FALSE)

# table of people per cb group
df |> 
  select(ResponseId, CB, EndDate) |> 
  unique() |> 
  mutate(EndDate = as.Date(EndDate)) |> 
  group_by(CB, EndDate) |> 
  tally() |> 
  ggplot(aes(x = CB, y = n, fill = EndDate)) + 
  geom_bar(stat="identity") + 
  scale_y_continuous(n.breaks = 15, expand = c(0, 1)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(title = "Number of participants per counter balance group", 
       subtitle = "Light Blue: Collected March 7, Blue: Collected Feb 7, Dark Blue: Collected Jan 11", 
       x = "Counter Balance Group", 
       y = "Count")
ggsave(filename = file.path(result_path, "plots",  "participants-per-cb.png"), 
       height = 5, # we can change dimensions of the plot output here
       width = 7)