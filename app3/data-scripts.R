
library(tidyverse)
library(WDI)

codes <- readxl::read_excel("data/P_Millennium Development Goals.xlsx", sheet = 2) 

labels <- janitor::make_clean_names(colnames(codes)) |> set_names(colnames(codes))

codes <- codes |> 
  janitor::clean_names() |> 
  select(code, indicator_name, long_definition, statistical_concept_and_methodology, topic) |> 
  mutate(topic = str_extract(topic, ".+?(?=:)"))

d <- WDI(country = "all", indicator = codes$code, start = 1990)
d2 <- WDI(country = "all", indicator = c("SH.H2O.SMDW.ZS", "SH.STA.SMSS.ZS", "IT.NET.USER.ZS"), start = 1990)

out <- bind_rows(d, d2)

write_rds(out, "out.rds")
