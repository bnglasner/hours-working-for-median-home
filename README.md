🏡 Hours Needed to Afford a Median-Priced Home (1971–2025)
Author: Ben Glasner
Last updated: August 7, 2025

This repository calculates and visualizes how many hours a typical American worker needs to work each month to afford a median-priced home across time, using real hourly wages, home prices, and mortgage rates. It also calculates the one-time burden of saving for a 20% down payment.

🔍 What This Project Does
Given the:

Median hourly wage from CPS Outgoing Rotation Group data

Median sales price of a new home (FRED series MSPUS)

30-year fixed mortgage rate (FRED series MORTGAGE30US)

Inflation adjustment using the PCE price index (FRED series PCEPILFE)

This project calculates:

Monthly hours needed to afford the monthly mortgage payment

Total hours needed to save for a 20% down payment

It visualizes both in an interactive plotly chart split across two stacked panels.

📊 Data Sources
🧱 Median Sales Price of New Homes
FRED Series: MSPUS

Description: Median sales price of houses sold in the United States (not seasonally adjusted)

Last updated: 2025-07-24

📉 30-Year Fixed Mortgage Rate
FRED Series: MORTGAGE30US

Description: Average 30-year fixed mortgage interest rate in the U.S.

Last updated: 2025-07-31

🧾 Personal Consumption Expenditures Price Index (Core PCE)
FRED Series: PCEPILFE

Description: Personal consumption expenditures excluding food and energy (core inflation index), used to normalize all dollar values to 2017 levels

Last updated: 2025-07-31

💵 Real Hourly Wages
Source: Current Population Survey (CPS) Outgoing Rotation Group (ORG)

Metric: Median hourly wage, adjusted for inflation using PCE and averaged across calendar years

📈 Visualization
The output is a two-panel plotly visualization:

Top panel: Monthly hours needed to afford the monthly mortgage

Bottom panel: One-time hours needed to afford a 20% down payment

Hover text displays:

Hourly wage

Mortgage rate

Home price

Monthly cost and labor burden

