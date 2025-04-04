library(tidyverse)
library(readxl)

vars_to_keep <- read_excel("misc/selected_variables.xlsx")

ez_2021_raw <- read_excel("data/data-raw/IRS-SOI-extract/21eoextractez.xlsx")
regular_2021_raw <- read_excel("data/data-raw/IRS-SOI-extract/21eoextract990.xlsx")

ez_2021_clean <- ez_2021_raw |> 
  filter(subseccd == "03") |> 
  mutate(EIN = as.character(EIN)) |> 
  select(one_of(vars_to_keep |> filter(form == "990ez") |> pull(vname))) |> 
  mutate(ez = TRUE) |> 
  # Some of these values are suspiciously low, like assets of $1 or $100
  # filter(totassetsend > 1000, totliabend > 1000, totrevnue > 1000) |> 
  mutate(
    debt_to_asset_ratio = totliabend / totassetsend,
    debt_to_equity_ratio = totliabend / (totassetsend - totliabend),
    profit_margin_ratio = (totrevnue - totexpns) / totrevnue,
    closed = contractioncd == "Y"
  )

regular_2021_clean <- regular_2021_raw |> 
  filter(subseccd == 03) |> 
  mutate(EIN = as.character(EIN)) |> 
  mutate(subseccd = as.character(subseccd)) |> 
  select(one_of(vars_to_keep |> filter(form == "990") |> pull(vname))) |> 
  mutate(ez = FALSE) |> 
  rename(totrevnue = totrevenue, totexpns = totfuncexpns) |> 
  # Some of these values are suspiciously low, like assets of $1 or $100
  # filter(totassetsend > 1000, totliabend > 1000, totrevnue > 1000) |> 
  mutate(
    debt_to_asset_ratio = totliabend / totassetsend,
    debt_to_equity_ratio = totliabend / (totassetsend - totliabend),
    profit_margin_ratio = (totrevnue - totexpns) / totrevnue,
    closed = ceaseoperationscd == "Y" | sellorexchcd == "Y"
  )

# Running variables
# gross receipts < 200000 and total assets < 500000

full <- bind_rows(ez_2021_clean, regular_2021_clean)

full_smaller <- full |>
  filter(totliabend > 0) |> 
  select(EIN, tax_pd, ez, totliabend, totrevnue, totassetsend, contains("ratio"), closed) |>
  filter(
    (totrevnue > 0 & totrevnue < 1000000) &
      (totassetsend > 0 & totassetsend < 1000000)
  ) |> 
  mutate(form = factor(ez, levels = c(TRUE, FALSE), labels = c("990EZ", "990"))) |> 
  rename(revenue = totrevnue, assets = totassetsend)

saveRDS(full_smaller, here::here("data/clean_2021.rds"))
