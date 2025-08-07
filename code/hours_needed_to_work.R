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
library(writexl)

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
setwd(path_data)

# Median Wage (monthly by decile and already adjusted using monthly PCE at 2025-01-01 levels)
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
  rename(pce_index_2025 = `PCEPILFE_NBD20250101`) %>%
  group_by(year) %>%
  summarise(pce_index_2025 = mean(pce_index_2025, na.rm = TRUE)) %>%
  ungroup()


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
  mutate(deflator = 100/pce_index_2025 ) %>%
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
write_xlsx(annual_data, path = "Annual Data.xlsx")


# Define recession periods
recessions <- data.frame(
  start = c(1980, 1981.75, 1990.5, 2001, 2008, 2020),
  end   = c(1980.75, 1982, 1991, 2001.75, 2009, 2020.5)
)

# Recession bars
recession_shapes <- lapply(1:nrow(recessions), function(i) {
  list(
    type = "rect",
    x0 = recessions$start[i],
    x1 = recessions$end[i],
    xref = "x",
    yref = "paper",
    y0 = 0,
    y1 = 1,
    fillcolor = "lightgray",
    opacity = 0.3,
    line = list(width = 0)
  )
})

# Horizontal reference lines
horizontal_lines <- list(
  list(type = "line", x0 = min(annual_data$year), x1 = max(annual_data$year),
       y0 = 80, y1 = 80, yref = "y1", xref = "x",
       line = list(dash = "dot", color = "gray")),
  list(type = "line", x0 = min(annual_data$year), x1 = max(annual_data$year),
       y0 = 3200, y1 = 3200, yref = "y2", xref = "x",
       line = list(dash = "dot", color = "gray"))
)

# Plot 1: Mortgage hours
p1 <- plot_ly(
  data = annual_data,
  x = ~year,
  y = ~hours_needed,
  type = 'scatter',
  mode = 'lines+markers',
  text = ~hover_text,
  hoverinfo = 'text',
  name = "Monthly Mortgage",
  line = list(color = '#227c63', width = 2)
) %>%
  layout(
    yaxis = list(
      title = "Monthly Work Hours Needed",
      range = c(35, 120)
    ),
    showlegend = FALSE
  )

# Plot 2: Down payment hours
p2 <- plot_ly(
  data = annual_data,
  x = ~year,
  y = ~hours_needed_downpayment,
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste0(
    "<b>Year:</b> ", year, "<br>",
    "<b>Hours Needed for Down Payment:</b> ", round(hours_needed_downpayment, 1), "<br>",
    "<b>Home Price:</b> $", formatC(median_home_price_real, format = "f", digits = 0), "<br>"
  ),
  hoverinfo = 'text',
  name = "Down Payment",
  line = list(color = '#2f5c9e', width = 2)
) %>%
  layout(
    yaxis = list(
      title = "Total Work Hours Needed (Down Payment)",
      range = c(1800, 3600)
    ),
    showlegend = FALSE
  )

# Combine the plots into a subplot
subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(
    title = list(
      text = paste0(
        "Workload Required to Afford a Median-Priced Home Over Time",
        "<br><sub>Measured in hours of work per month (mortgage) and total hours (down payment), based on inflation-adjusted median wages</sub>"
      ),
      x = 0
    ),
    xaxis = list(title = "Year", tickmode = "linear", dtick = 5),
    shapes = c(recession_shapes, horizontal_lines),
    annotations = list(
      # list(
      #   x = 1980, y = 82,
      #   text = "Two weeks of<br>full-time work",
      #   xref = "x", yref = "y1",
      #   showarrow = TRUE, arrowhead = 2,
      #   ax = 0, ay = -30,
      #   font = list(size = 11)
      # ),
      # list(
      #   x = 2024, y = 3300,
      #   text = "Entry costs<br>are historically high",
      #   xref = "x", yref = "y2",
      #   showarrow = TRUE, arrowhead = 2,
      #   ax = 0, ay = -40,
      #   font = list(size = 11)
      # ),
      list(
        x = 0, y = -0.18,
        text = paste0(
          "<b>Note:</b> Assumes a 20% down payment and a 30-year fixed-rate mortgage. ",
          "Monthly burden reflects principal & interest only—property taxes, insurance, and maintenance are excluded. ",
          "Real wages are based on median hourly earnings from the CPS Outgoing Rotation Group, adjusted using the PCE index (2017=100)."
        ),
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "bottom",
        font = list(size = 11, color = "gray")
      ),
      list(
        x = 0, y = -0.25,
        text = "Source: FRED (MSPUS, MORTGAGE30US, PCEPILFE); IPUMS CPS ORG via author's calculations",
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "bottom",
        font = list(size = 11, color = "gray")
      )
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
    margin = list(l = 60, r = 40, t = 70, b = 110)
  )


################################

library(magick)
library(webshot2)  # orca is more complex, webshot is easier here

# Ensure output folder exists
dir.create("frames", showWarnings = FALSE)

years <- sort(unique(annual_data$year))

for (i in seq_along(years)) {
  year_max <- years[i]
  data_subset <- annual_data %>% filter(year <= year_max)
  
  # Panel 1
  p1 <- plot_ly(data_subset, x = ~year, y = ~hours_needed,
                type = "scatter", mode = "lines+markers",
                line = list(color = '#227c63')) %>%
    layout(
      yaxis = list(title = "Monthly Work Hours Needed", range = c(35, 120)),
      xaxis = list(title = "Year", range = c(min(years), max(years))),
      showlegend = FALSE
    )
  
  # Panel 2
  p2 <- plot_ly(data_subset, x = ~year, y = ~hours_needed_downpayment,
                type = "scatter", mode = "lines+markers",
                line = list(color = '#2f5c9e')) %>%
    layout(
      yaxis = list(title = "Total Work Hours Needed (Down Payment)", range = c(1800, 3600)),
      xaxis = list(title = "Year", range = c(min(years), max(years))),
      showlegend = FALSE
    )
  
  # Combine + full annotation
  gif_frame <- subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
    layout(
      title = list(
        text = paste0(
          "Workload Required to Afford a Median-Priced Home Over Time",
          "<br><sub>Measured in hours of work per month (mortgage) and total hours (down payment), based on inflation-adjusted median wages</sub>"
        ),
        x = 0
      ),
      annotations = list(
        list(
          x = 0, y = -0.18,
          text = paste0(
            "<b>Note:</b> Assumes a 20% down payment and a 30-year fixed-rate mortgage. ",
            "Monthly burden reflects principal & interest only—property taxes, insurance, and maintenance are excluded. ",
            "Real wages are based on median hourly earnings from the CPS Outgoing Rotation Group, adjusted using the PCE index (2017=100)."
          ),
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = 11, color = "gray")
        ),
        list(
          x = 0, y = -0.25,
          text = "Source: FRED (MSPUS, MORTGAGE30US, PCEPILFE); IPUMS CPS ORG via author's calculations",
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = 11, color = "gray")
        )
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 60, r = 40, t = 70, b = 110)
    )
  
  htmlwidgets::saveWidget(gif_frame, "temp_plot.html", selfcontained = TRUE)
  webshot("temp_plot.html", file = sprintf("frames/frame_%03d.png", i), vwidth = 900, vheight = 700)
}


# Read in the frames
img_list <- list.files("frames", full.names = TRUE, pattern = "png")
frames <- image_read(img_list)

n_frames <- length(frames)

# Try to get a centisecond delay that evenly divides into 100
valid_delays <- c(1, 2, 4, 5, 10, 20, 25, 50, 100)  # Factors of 100
ideal_delay <- 2000 / n_frames  # 2000 cs = 20 seconds
frame_delay_cs <- valid_delays[which.min(abs(valid_delays - ideal_delay))]

# Pause time (in seconds) → number of extra frames
pause_seconds <- 10
pause_frames <- round((pause_seconds * 100) / frame_delay_cs)

fps_final <- 100 / frame_delay_cs  # must be integer

final_frame <- rep(frames[length(frames)], times = pause_frames)
all_frames <- c(frames, final_frame)

animated <- image_animate(image_join(all_frames), fps = fps_final)
image_write(animated, "housing_burden_animation.gif")
