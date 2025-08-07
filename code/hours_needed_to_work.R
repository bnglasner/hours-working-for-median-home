# Taking the median wage, median sales price of a new home, average 30-year fixed mortgage rate, and assuming a 20% down payment, how many hours need to be worked in a given year to afford a home in that year over time? 

# Ben Glasner | 8/7/2025

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(plotly)


###########################
###   Set Paths         ###
###########################
project_directories <- list(
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/hours-working-for-median-home",
  "bngla"             = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/hours-working-for-median-home",
  "Research"          = "C:/Users/Research/EIG Dropbox/Benjamin Glasner/GitHub/hours-working-for-median-home"
)

current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}
path_project <- project_directories[[current_user]]
path_data    <- file.path(path_project, "data")
path_output  <- file.path(path_project, "output")

###########################
###   Load Data         ###
###########################

# Median Wage (monthly by decile)
wages <- read_excel(file.path(path_data, "Decile Monthly Wage.xlsx")) %>%
  mutate(date = ymd(observation_date)) %>%
  select(date, wage_50th)

# Home prices
home_prices <- read_excel(file.path(path_data, "MSPUS.xlsx"), sheet = 2) %>%
  mutate(date = ymd(observation_date)) %>%
  rename(median_home_price = MSPUS)

# Mortgage rates
mortgage_rates <- read_excel(file.path(path_data, "MORTGAGE30US.xlsx"), sheet = 2) %>%
  mutate(date = ymd(observation_date)) %>%
  rename(mortgage_rate = MORTGAGE30US)

# BEA Deflator (PCE)
pce_deflator <- read_excel(file.path(path_data, "PCEPILFE.xlsx"), sheet = 2) %>%
  mutate(date = ymd(observation_date),
         year = year(date)) %>%
  rename(pce_index_2017 = `PCEPILFE`) %>%
  mutate(pce_index_2017 = if_else(date == "2025-01-01", 125.2348, pce_index_2017))

# 2025-01-01	124.407
# 2025-02-01	124.999
# 2025-03-01	125.118
# 2025-04-01	125.343
# 2025-05-01	125.610
# 2025-06-01	125.932

# (124.407 + 124.999 + 125.118 + 125.343 + 125.610 + 125.932)/6

############################
###   Prepare Dataset    ###
############################

# Create annual median wage
annual_data <- wages %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(median_wage_avg_real = mean(wage_50th, na.rm = TRUE)) %>%
  ungroup()

# Merge in mortgage, price, and PCE
annual_data <- annual_data %>%
  left_join(home_prices %>% mutate(year = year(date)) %>%
              group_by(year) %>%
              summarise(median_home_price_nominal = mean(median_home_price, na.rm = TRUE)),
            by = "year") %>%
  left_join(mortgage_rates %>% mutate(year = year(date)) %>%
              group_by(year) %>%
              summarise(mortgage_rate = mean(mortgage_rate, na.rm = TRUE)),
            by = "year") %>%
  left_join(pce_deflator, by = "year")

# Normalize to 2017 dollars
annual_data <- annual_data %>%
  mutate(deflator = 100/pce_index_2017 ) %>%
  mutate(
    median_home_price_real  = median_home_price_nominal*deflator
  )

#########################################
###   Calculate Mortgage & Hours     ###
#########################################

# Mortgage calculator function: annual payment (P&I only)
calc_annual_payment <- function(principal, rate, term_years = 30) {
  r <- rate / 100 / 12
  n <- term_years * 12
  payment_monthly <- (r * principal) / (1 - (1 + r)^(-n))
  return(payment_monthly)
}

annual_data <- annual_data %>%
  mutate(
    down_payment = 0.20 * median_home_price_real,
    loan_amount = median_home_price_real - down_payment,
    monthly_mortgage_payment = map2_dbl(loan_amount, mortgage_rate, calc_annual_payment),
    hours_needed = monthly_mortgage_payment / median_wage_avg_real # denominator = real hourly wage
  )

##########################
###   Visualization    ###
##########################

annual_data %>%
  ggplot(aes(x = year, y = hours_needed)) +
  geom_line(size = 1.2, color = "#1a654d") +
  labs(
    title = "Monthly Hours Needed to Afford a Median-Priced Home's Monthly Mortgage",
    subtitle = "Assumes 20% down payment and 30-year fixed mortgage (P&I only)",
    x = NULL,
    y = "Hours of Work per Month (real median wages & prices)",
    caption = "Source: FRED, BEA, IPUMS, author's calculations"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(breaks = seq(min(annual_data$year), max(annual_data$year), by = 5))


# Compute real hourly wage and total income at hours_needed
annual_data <- annual_data %>%
  mutate(
    monthly_income_from_hours_needed = median_wage_avg_real * hours_needed,
    hours_needed_downpayment = down_payment / median_wage_avg_real
  )


annual_data <- annual_data %>%
  mutate(
    hover_text = paste0(
      "<b>Year:</b> ", year, "<br>",
      "<b>Monthly Hours Needed:</b> ", round(hours_needed, 1), "<br>",
      "<b>Hours Needed for Down Payment:</b> ", round(hours_needed_downpayment, 1), "<br>",
      "<b>Hourly Wage:</b> $", formatC(median_wage_avg_real, format = "f", digits = 2), "<br>",
      "<b>Mortgage Rate:</b> ", round(mortgage_rate,2), "%<br>",
      "<b>Monthly Income at Required:</b> $", formatC(monthly_mortgage_payment, format = "f", digits = 0), "<br>"
    )
  )


setwd(path_output)
write.xlsx(annual_data, file = "Annual Data.xlsx")

# First plot: Monthly mortgage hours
p1 <- plot_ly(
  data = annual_data,
  x = ~year,
  y = ~hours_needed,
  type = 'scatter',
  mode = 'lines+markers',
  text = ~hover_text,
  hoverinfo = 'text',
  name = "Monthly Mortgage",
  line = list(color = '#1a654d', width = 2)
) %>%
  layout(
    yaxis = list(
      title = "Monthly Hours Needed (Mortgage)",
      # rangemode = "tozero",
      range = c(35,95)
    ),
    showlegend = FALSE
  )

# Second plot: One-time down payment hours
p2 <- plot_ly(
  data = annual_data,
  x = ~year,
  y = ~hours_needed_downpayment,
  type = 'scatter',
  mode = 'lines+markers',
  name = "Down Payment",
  text = ~paste0(
    "<b>Year:</b> ", year, "<br>",
    "<b>Hours Needed for Down Payment:</b> ", round(hours_needed_downpayment, 1), "<br>",
    "<b>Home Price:</b> $", formatC(median_home_price_real, format = "f", digits = 0), "<br>"
  ),
  hoverinfo = 'text',
  line = list(color = '#e1ad28', width = 2, dash = "dash")
) %>%
  layout(
    yaxis = list(
      title = "One-Time Hours Needed (Down Payment)",
      # rangemode = "tozero",
      range = c(1500,3000)
    ),
    showlegend = FALSE
  )

# Combine with subplot
subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(
    title = list(
      text = paste0(
        "Workload Required to Afford a Median-Priced Home",
        "<br><sub>Includes monthly mortgage payment and one-time down payment</sub>"
      ),
      x = 0
    ),
    xaxis = list(title = "Year", tickmode = "linear", dtick = 5),
    annotations = list(
      list(
        x = 0,
        y = -0.15,
        text = "Source: FRED, BEA, IPUMS, author's calculations",
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "bottom",
        font = list(size = 11, color = "gray")
      )
    ),
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
    margin = list(l = 60, r = 40, t = 60, b = 80)
  )
