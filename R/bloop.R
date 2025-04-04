library(tidyverse)
library(readxl)
library(scales)

# Look at 5 specific points? 
# - Low revenue, high assets
# - Medium revenue, high assets
# - High revenue, high assets
# - High revenue, medium assets
# - High revenue, low assets

# Revenue: 200000, 100000, 50000
# Assets: 500000, 250000, 100000

focus_areas <- tribble(
  ~totrevnue, ~totassetsend, ~label,
  50000, 500000, "A",
  100000, 500000, "B",
  200000, 500000, "C",
  200000, 250000, "D",
  200000, 100000, "E" 
)

vars_to_keep <- read_excel("misc/selected_variables.xlsx")

ez_2021_raw <- read_excel("data/data-raw/IRS-SOI-extract/21eoextractez.xlsx")
regular_2021_raw <- read_excel("data/data-raw/IRS-SOI-extract/21eoextract990.xlsx")

ez_2021_clean <- ez_2021_raw |> 
  mutate(EIN = as.character(EIN)) |> 
  select(one_of(vars_to_keep |> filter(form == "990ez") |> pull(vname))) |> 
  mutate(ez = TRUE) |> 
  mutate(
    debt_to_asset_ratio = totliabend / totassetsend,
    debt_to_equity_ratio = totliabend / (totassetsend - totliabend),
    profit_margin_ratio = (totrevnue - totexpns) / totrevnue
  )

regular_2021_clean <- regular_2021_raw |> 
  mutate(EIN = as.character(EIN)) |> 
  mutate(subseccd = as.character(subseccd)) |> 
  select(one_of(vars_to_keep |> filter(form == "990") |> pull(vname))) |> 
  mutate(ez = FALSE) |> 
  rename(totrevnue = totrevenue, totexpns = totfuncexpns) |> 
  mutate(
    debt_to_asset_ratio = totliabend / totassetsend,
    debt_to_equity_ratio = totliabend / (totassetsend - totliabend),
    profit_margin_ratio = (totrevnue - totexpns) / totrevnue
  )

# Running variables
# gross receipts < 200000 and total assets < 500000

full <- bind_rows(ez_2021_clean, regular_2021_clean)

full_smaller <- full |>
  filter(totliabend > 0) |> 
  select(EIN, tax_pd, ez, totliabend, totrevnue, totassetsend, contains("ratio")) |>
  filter(
    (totrevnue > 0 & totrevnue < 1000000) &
      (totassetsend > 0 & totassetsend < 1000000)
  ) |> 
  mutate(form = factor(ez, levels = c(TRUE, FALSE), labels = c("990EZ", "990")))

full_smaller |>
  arrange(ez) |> # plot the false points first so they don't cover up the trues
  ggplot(aes(x = totrevnue, y = totassetsend)) +
  geom_point(aes(color = form), alpha = 0.15, size = 0.005) +
  annotate(geom = "segment", x = 0, xend = 200000, y = 500000, color = "#E17C05", linewidth = 7, alpha = 0.75) +
  annotate(geom = "segment", x = 0, xend = 200000, y = 500000, color = "white", linewidth = 0.5, alpha = 1) +
  annotate(geom = "segment", x = 200000, y = 0, yend = 500000, color = "#E17C05", linewidth = 7, alpha = 0.75) +
  annotate(geom = "segment", x = 200000, y = 0, yend = 500000, color = "white", linewidth = 0.5, alpha = 1) +
  geom_label(
    data = focus_areas, 
    aes(label = label), 
    label.r = unit(0.6, "lines"),
    family = "Inter",
    fontface = "bold"
  ) +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_color_manual(values = c("#94346E", "#38A6A5"), guide = guide_legend(override.aes = list(size = 2.5, alpha = 1))) +
  labs(
    x = "Total revenue",
    y = "Total assets",
    color = "IRS form"
  ) +
  coord_cartesian(xlim = c(0, 300000), ylim = c(0, 750000)) +
  theme_minimal(base_family = "Inter") +
  theme(
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 1),
    legend.position = "bottom",
    legend.justification = "left",
    legend.title.position = "left",
    legend.margin = margin(l = 0, t = 0)
  )


# TODO:
# - Are there any organizations that are in both the regular 990 and the EZ?
# - Why are multiple EINs included? How to handle those?
# - Why is there absolutely no treatment effect? Lots of 0s?
# - Is there a need for 5 points? Would 3 work? - Visualize things with the simulated data to make sure stuff is working and makes sense
# - Normalized running variable thing?


thing <- read_csv("https://github.com/rdpackages/rdmulti/raw/refs/heads/master/R/simdata_multis.csv")


Y <- thing$y
X1 <- thing$x1
X2 <- thing$x2
zvar <- thing$t
cvec <- c(thing$c1[1], thing$c1[2], thing$c1[3])
cvec2 <- c(thing$c2[1], thing$c2[2], thing$c2[3])

aux <- rdms(Y = Y, X = X1, C = cvec, X2 = X2, C2 = cvec2, zvar = zvar)


thing |>
  # arrange(ez) |> # plot the false points first so they don't cover up the trues
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = zvar))
