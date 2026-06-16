# Load packages used by the app
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)

# helpers.R

# Read data from a CSV file and perform data preprocessing
expansions <- read_csv("data/expansions.csv") |>
  mutate(
    evaluation = factor(evaluation, levels = c("None", "A", "B")),
    propensity = factor(propensity, levels = c("Good", "Average", "Poor"))
  )

# Compute expansion rates by trial and group
expansion_groups <- expansions |>
  group_by(industry, propensity, contract, evaluation) |>
  summarize(
    success_rate = round(mean(outcome == "Won") * 100),
    avg_amount = round(mean(amount)),
    avg_days = round(mean(days)),
    n = n()
  ) |>
  ungroup()

# Compute expansion rates by trial
overall_rates <- expansions |>
  group_by(evaluation) |>
  summarise(rate = round(mean(outcome == "Won"), 2))

# Restructure expansion rates by trial as a vector
rates <- structure(overall_rates$rate, names = overall_rates$evaluation)

# Define lists for propensity, contract and industry choices
propensities <- c("Good", "Average", "Poor")
contracts <- c("Monthly", "Annual")
industries <- c(
  "Academia",
  "Energy",
  "Finance",
  "Government",
  "Healthcare",
  "Insurance",
  "Manufacturing",
  "Non-Profit",
  "Pharmaceuticals",
  "Technology"
)

# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()


# Define the Shiny UI layout
ui <- page_sidebar(
  # Add github link (removed for connect cloud)
  # ribbon_css("https://github.com/rstudio/demo-co/tree/main/evals-analysis-app"),

  # Add title
  title = "Effectiveness of DemoCo App Free Trial by Customer Segment",
)

# Define the Shiny server function
server <- function(input, output) {}

# Create the Shiny app
shinyApp(ui = ui, server = server)
