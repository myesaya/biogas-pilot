
library(tidyverse)
library(gtsummary)

# -------------------------------------------------------------------------
kapeni_age<-read_csv(here::here("data/raw/kapeni_ages.csv"))

#I want to put the ages into categories and create a new variable age_cat
kapeni_age<-kapeni_age%>%
  mutate(
    age_cat=case_when(
    age<=6~"3-6",
    age>=7 & age<=11~"7-11",
    age>=12 & age<=16~"12-16",
    age>=17~"17+",

  ))
#pivot wider the ages

df<-kapeni_age |>
pivot_wider(names_from = age_cat, values_from = number)

#change gender variable to factor
df$gender<-as.factor(df$gender)
#change factor levels in gender variable

df$gender<-factor(df$gender,
                  levels =c("Male","Female"))


# tbl_summary -------------------------------------------------------------

df |>
  select(gender, `3-6`, `7-11`, `12-16`, `17+`) |>
  tbl_summary(by=gender,
              type = list(`3-6` ~ "continuous",
                          `7-11` ~ "continuous",
                          `12-16` ~ "continuous",
                          `17+` ~ "continuous"),
              statistic = list(all_continuous() ~ "{sum}")) |>
  remove_row_type(variables = c('3-6', '7-11','12-16','17+'), type = "missing") |>
  modify_header(label = "**Age category**",
                stat_1 ~ "**Males**",
                stat_2 ~ "**Females**") |>
  modify_footnote(c(all_stat_cols()) ~ NA)






