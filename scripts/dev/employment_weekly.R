## Weekly filled jobs (34 days) & Weekly median earnings (34 days)

library(readr)

filter_paid_jobs <- "Number of paid jobs - 34 days"
filter_median_earnings <- "Weekly earnings - median - 34 days"


config <- read_config_file()
path <- paste0(config$data_directory, "Embargoed datasets")
files <- file.info(list.files(path, pattern = "*.indicators-weekly-paid-jobs-34-days.*", full.names = T))
update <- read.csv(rownames(files)[which.max(files$mtime)])

## Number of paid jobs
if (!is.null(filter_paid_jobs)) {
paid_jobs <- update %>% filter(Indicator == filter_paid_jobs) %>% select(Parameter = Week_end,everything()) %>%
  mutate(Parameter = as.Date(ymd(Parameter))) %>%
  spread(High_industry, Value) %>%
  select(Parameter, Total, everything(), -Indicator)

  colnames(data) <- c("Parameter", paste0("col_", 2:ncol(data)))
}else{
  "please enter a filter for visa type or employment"
}

## Median earnings
if (!is.null(filter_median_earnings)) {
  median_earnings <- update %>% filter(Indicator == filter_median_earnings) %>% select(Parameter = Week_end,everything()) %>%
    mutate(Parameter = as.Date(ymd(Parameter))) %>%
    spread(High_industry, Value) %>%
    select(Parameter, Total, everything(), -Indicator)

  colnames(data) <- c("Parameter", paste0("col_", 2:ncol(data)))
}else{
  "please enter a filter for visa type or employment"
}
file.rename(from = paste0(directory, "/COVID - 19 Weekly_paid_jobs_34_days.csv"), to = paste0(directory, "/Previous/COVID - 19 Weekly_paid_jobs_34_days.csv"))
file.rename(from = paste0(directory, "/COVID - 19 Weekly_median_earnings_34_days.csv"), to = paste0(directory, "/Previous/COVID - 19 Weekly_median_earnings_34_days.csv"))

write_csv(paid_jobs,file = paste0(path,"/COVID - 19 Weekly_paid_jobs_34_days.csv"))
write_csv(median_earnings,file = paste0(path,"/COVID - 19 Weekly_median_earnings_34_days.csv"))

