# Data Cleaning
# 
# Read in the Qualtrics survey & prepare data for 2-analysis.R

pacman::p_load(dplyr, tidyr, readxl, kableExtra)
source(here::here("src/0-source.R"))

# read in data
raw = data.table::fread(file.path(path, "raw", "Ethics Randomizer - Sci Reports Study_March 8, 2023_15.53.csv"))
cb_table = readxl::read_excel(file.path(path, "Fully Crossed List - FINAL.xlsx"))


data = raw[-c(1:2), ] # remove irrelevant rows from Qualtrics
data = data[as.POSIXct(data$EndDate, tz = "") > as.POSIXct("2023-01-10", tz = "")] # remove data captured before first batch date Jan 11 - testing Qualtrics survey
data = data[Status == 0 & Progress == "100" & Finished == 1, ]


cols = names(data)[grepl(pattern = "CB|Response", names(data))] # cols to keep
attn_cols = names(data)[grepl(pattern = "Att", names(data))]
baseline_cols = names(data)[grepl(pattern = "Baseline", names(data))]


# Prep main data ----
# express in tall format, each row shows id, CB, domain, question, and response 
tmp = data |>
  select(all_of(c(cols, attn_cols, "EndDate"))) |> 
  tidyr::pivot_longer(-c(ResponseId, attn_cols, EndDate),                 # reshaping to tall format so we can pull the CB, group, and question numbers from the column names
                      names_to = "column", 
                      values_to = "response") |> 
  tidyr::extract(column,                          # the column we are extracting data from
                 c("CB", "group", "question"),    # what we want to name each column of the data we extract
                 c("CB(.*) - (.*)_(.*)"),         # the structure of the data to extract from. We use REGEX to pull the data in the ()'s
                 convert = TRUE,                  # change the data to its likely type, i.e. numbers from strings to integers
                 remove = TRUE) |>                # switch to FALSE to see original column names for debugging
  mutate(question = sprintf("Q%s", question), 
         response = as.numeric(substr(response, 1, 1))) |> # drop the text in responses by keeping only the first character 
  filter(response != "", 
         !grepl("Page|Click", question)) # these extra columns sneaked in from the Qualtrics survey structure, we can drop them now

# reshape to wide format
df = tmp |> 
  select(ResponseId, CB, response, question, group, attn_cols, EndDate) |> 
  tidyr::pivot_wider(id_cols = c(ResponseId, CB, group, attn_cols, EndDate), 
                     names_from = question, 
                     values_from = response)

# filter attention checks
attn_ans = names(cb_table)[grepl("_ans", names(cb_table))]
pre_filter = df |> 
  left_join(cb_table, by = c("CB" = "Counter Balance", "group" = "Domain Short")) |> 
  group_by(ResponseId) |> 
  mutate(attn_correct = sum(AttCheckFI == FI_ans, 
                            AttCheckEV == EV_ans,  
                            AttCheckOD == OD_ans, 
                            AttCheckFS == FS_ans,  
                            AttCheckRS == RS_ans) / n()) 

# save results of how many people per day are kept and dropped 
pre_filter |> 
  mutate(pass_check = attn_correct > 2, 
         batch_date = as.Date(EndDate)) |>
  select(ResponseId, pass_check, batch_date) |> 
  unique() |> 
  group_by(batch_date, pass_check) |> 
  summarise(n = n()) |> 
  tidyr::pivot_wider(names_from = pass_check, values_from = n) |> 
  kbl(col.names = c("Batch Date", "Fail", "Pass"), format = "html") |>  
  kable_classic_2(full_width=F) |> 
  save_kable(file = file.path(result_path, "tables", "Attention_check_batch_date.html"))


df = pre_filter |> # we divide by n() since we observe the same data point multiple times.  
  filter(attn_correct > 2) |> 
  select(-all_of(c(attn_cols, attn_ans)),
         -"Att Check Values - FI, OD, FS, RS, EV") |>
  ungroup()

# reshape to long 
df = df |> 
  tidyr::pivot_longer(cols = Q1:Q7, 
                      names_to = "question", 
                      values_to = "response") |> 
  mutate(variable_group = case_when(
    question %in% c("Q1", "Q2", "Q3") ~ "Acceptability", 
    question %in% c("Q4", "Q5", "Q6") ~ "Autonomy", 
    question %in% c("Q7") ~ "Success", 
    TRUE ~ "ERROR"
  ))

# final tidying: drop unnecessary cols, rename batchdate, rename Nudge to Intervention, reverse code questions 
clean = df |> 
  select(-c(attn_correct)) |> 
  mutate(BatchDate = as.Date(EndDate)) |> 
  rename(Intervention = Nudge) |> 
  mutate(response = case_when(
    question %in% c("Q5", "Q6") ~ abs(response - 7) + 1, # flip the 7 point scale
    TRUE ~ response))

# save result 
write.csv(clean, file.path(path, "clean", "clean.csv"), row.names = FALSE)


# prep and save baseline data ----
baseline = data |> 
  select(all_of(baseline_cols), "ResponseId") |> 
  tidyr::pivot_longer(-ResponseId,
                      names_to = "question", 
                      values_to = "response") |> 
  mutate(column = gsub("Baseline_", "Q", question), 
         response = as.numeric(substr(response, 1, 1))) |> 
  filter(response != "", 
         !grepl("Page|Click", column)) |> 
  mutate(variable_group = "baseline", 
         response = case_when(
           question %in% c("Q5", "Q6") ~ abs(response - 7) + 1, # flip the 7 point scale
           TRUE ~ response))

write.csv(baseline, file.path(path, "clean", "baseline.csv"), row.names = FALSE)
