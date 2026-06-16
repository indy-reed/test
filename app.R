# Load packages used by the app
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)

# helpers.R

# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()

#
# General financial functions for models:
#
# Mortgage payment - payment assumes an annuity immediate rather than an annuity
# due - meaning the first payment is made at time = 1 rather than at time = 0.
# Assumes annual as opposed to monthly payment of all mortgage, property tax,
# and insurance. Assumes home loan compounds annually at fixed interest rate.

a_nbar_i_pv <- function(i, n) {
  # where i is the interest rate and n is the number of payments (or the length
  # of the loan)
  (1 - (1 + i)^-n) / i
}

# Inflation rate is assumed to act geometrically and compounds like interests.
# The real interest rate or rate of return is the ratio of the nominal interest
# rate to the inflation rate.

r_real <- function(r_nominal, r_inflation) {
  (1 + r_nominal) / (1 + r_inflation) - 1
}

# Functions for adjusting the time value of money based on a fixed rate.

# Future value
fv <- function(pv, i, n) {
  # where i is the rate of return (it may be nominal or real - adjusted for
  # inflation), n is the time period and pv is the present value of the amount.
  pv * (1 + i)^n
}

# Present value
pv <- function(fv, i, n) {
  # where i is the rate of return (it may be nominal or real - adjusted for
  # inflation) and n is the time period.
  fv * (1 + i)^-n
}

create_payment_schedule <- function(p, r, i, a, ptr, hir, n) {
  # where r is the interest rate, n is the number of payments (or the length
  # of the loan), p is the mortgage repayment amount, a is the appreciation
  # rate.
  #   PV = (Payment) [Annuity Immediate (interest rate, term) ] = Home price
  payment <- p / a_nbar_i_pv(i = r, n = n)

  # Construct mortgage table as reference for visualization

  # Number of years from start of the mortgage
  time <- seq(n)

  # Level mortgage payment, level property tax payment, level insurance premium
  # payment, mortgage interest and principal
  mortgage_payment <- rep(payment, n)
  interest_payment <- rep(NA_real_, n)
  principal_payment <- rep(NA_real_, n)

  # Property tax and homeowners insurances will be based on the value of the
  # house.
  home_value <- rep(p, n)
  property_tax_rate <- rep(ptr * p, n)
  home_insurance_rate <- rep(hir * p, n)

  # variable used to track how much of the home loan has been paid.
  # The value will be used to determine how much interest is paid for
  # a particular payment.
  remaining_principal <- p

  # For the moment, it is easier to iterate through each year and decrease
  # the principal from the loan amount.
  for (j in 1:n) {
    interest_payment[j] <- remaining_principal * r
    principal_payment[j] <- payment - interest_payment[j]
    remaining_principal <- remaining_principal - principal_payment[j]
  }

  # appreciated home value. Assumes constant appreciation rate. Parameter
  # supplied in function input. Nominal rate - not adjusted to inflation.
  nominal_value <- fv(
    pv = home_value,
    i = a,
    n = time
  )

  # appreciated value of property tax amount
  nominal_property_tax <- fv(
    pv = property_tax_rate,
    i = a,
    n = time
  )

  # appreciated value of homeowners insurance amount
  nominal_home_insurance <- fv(
    pv = home_insurance_rate,
    i = a,
    n = time
  )

  # inflation adjustments. Assumes inflation compounds with time at a constant
  # rate (i) which is supplied as a parameter to the function.
  # Each nominal mortgage payment is adjusted for inflation.
  real_mortgage_payment <- pv(
    fv = mortgage_payment,
    i = i,
    n = time
  )

  # Each nominal portion of principal is adjusted for inflation.
  real_principal_payment <- pv(
    fv = principal_payment,
    i = i,
    n = time
  )

  # Each interest portion of principal is adjusted for inflation.
  real_interest_payment <- pv(
    fv = interest_payment,
    i = i,
    n = time
  )

  # Real appreciation rate: nominal appreciation rate (a) adjusted for
  # inflation (i).
  real_appreciation_rate <- r_real(a, i)

  # Real future value of house over time.
  real_value <- fv(
    pv = p,
    i = real_appreciation_rate,
    n = time
  )

  real_property_tax <- fv(
    pv = property_tax_rate,
    i = real_appreciation_rate,
    n = time
  )

  real_home_insurance <- fv(
    pv = home_insurance_rate,
    i = real_appreciation_rate,
    n = time
  )

  # Combine information into a data frame for visualization and other analysis.
  payment_schedule <- tibble(
    time = time,
    nominal_mortgage = mortgage_payment,
    nominal_principal = principal_payment,
    nominal_interest = interest_payment,
    nominal_value = nominal_value,
    nominal_property_tax = nominal_property_tax,
    nominal_home_insurance = nominal_home_insurance,
    real_mortgage = real_mortgage_payment,
    real_principal = real_principal_payment,
    real_interest = real_interest_payment,
    real_value = real_value,
    real_property_tax = real_property_tax,
    real_home_insurance = real_home_insurance
  )

  return(payment_schedule)
}

term <- 30

# Define the Shiny UI layout
ui <- fluidPage(
  # Application title
  titlePanel("To own or not to own")
)

# Define the Shiny server function
server <- function(input, output) {}

# Create the Shiny app
shinyApp(ui = ui, server = server)
