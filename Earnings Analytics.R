# Libraries ----
if (!require("tidyquant")) install.packages("tidyquant"); library(tidyquant)
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("rvest")) install.packages("rvest"); library(rvest)
if (!require("scales")) install.packages("scales"); library(scales)
if (!require("birk")) install.packages("birk"); library(birk)
if (!require("janitor")) install.packages("janitor"); library(janitor)
if (!require("jsonlite")) install.packages("jsonlite"); library(jsonlite)
if (!require("htmlTable")) install.packages("htmlTable"); library(htmlTable)

# Block Warnings
options(warn=-1)

# Local Variables
ticker               <- "AAPL"
earnings_date        <- "2024-03-28" %>% as.Date()
Days_Before_Earnings <- 2
Days_After_Earnings  <- 5

getYahooEarnings <- function(ticker){
  
  # Getting the Earnings
  db_Earnings <- str_glue("https://finance.yahoo.com/calendar/earnings?failsafe=1&ynet=0&_device=desktop&device=desktop&symbol={ticker}") %>%
    rvest::read_html() %>%
    rvest::html_nodes("table") %>%
    html_table(fill = TRUE) %>%
    pluck(1) %>%
    dplyr::mutate(Month = substr(`Earnings Date`, 1, 3),
                  Day   = substr(`Earnings Date`, 5, 6),
                  Year  = substr(`Earnings Date`, 8, 12),
                  PM_AH = substr(`Earnings Date`, 18, 19))
  
  # Fixing some formats
  db_Earnings <- db_Earnings %>%
    dplyr::mutate(Month = case_when(Month == "Jan" ~ "01",
                                    Month == "Feb" ~ "02",
                                    Month == "Mar" ~ "03",
                                    Month == "Apr" ~ "04",
                                    Month == "May" ~ "05",
                                    Month == "Jun" ~ "06",
                                    Month == "Jul" ~ "07",
                                    Month == "Aug" ~ "08",
                                    Month == "Sep" ~ "09",
                                    Month == "Oct" ~ "10",
                                    Month == "Nov" ~ "11",
                                    Month == "Dec" ~ "12",
                                    TRUE ~ Month),
                  `Earnings Date` = str_glue("{Day}-{Month}-{Year}") %>% as.Date(format = "%d-%m-%Y")) %>%
    dplyr::select(Symbol, `Earnings Date`, `EPS Estimate`, `Reported EPS`, PM_AH) %>%
    janitor::clean_names() %>%
    dplyr::mutate(eps_estimate = eps_estimate %>% as.numeric(),
                  reported_eps = reported_eps %>% as.numeric(),
                  Surprise     = case_when(reported_eps > eps_estimate ~ "Above Expectations",
                                           reported_eps == eps_estimate ~ "Met Expectations",
                                           TRUE ~ "Below Expectations"))

  # Returning the data
  return(db_Earnings)
}

# Getting the data and removing future earnings dates
db_Yahoo_Earnings <- getYahooEarnings(ticker)

# Getting expected move
db_option <- str_glue("https://cdn.cboe.com/api/global/delayed_quotes/options/{ticker}.json") %>%
  read_json(simplifyVector = TRUE) 

db_option_cboe <- db_option[["data"]][["options"]] %>% 
  as.data.frame() %>%
  dplyr::mutate(Spot_Price   = db_option[["data"]][["current_price"]],
                expiry       = option %>% str_sub(-15, -10) %>% as.Date(format = "%y%m%d"),
                opt_type     = option %>% str_sub(-9, -9),
                strike       = ((option %>% str_sub(-8, length(option))) %>% as.numeric())/1000,
                days2Exp     = as.Date(expiry) - Sys.Date(),
                Mid_price    = round((bid + ask)/2, 2),
                Time_Process = Sys.time(),
                Flag_Expiry  = case_when(as.Date(expiry) == as.Date(earnings_date) ~ "Here",
                                         TRUE ~ "No"),
                Flag_Strike  = case_when(strike < db_option[["data"]][["current_price"]] ~ "OTM",
                                         TRUE ~ "ITM")) %>%
  dplyr::filter(Flag_Expiry == "Here" & Flag_Strike == "ITM") %>%
  dplyr::slice(1:2)

# Extracting the Prices for Call and Puts
bid_price_ATM_Calls <- db_option_cboe %>%
  dplyr::filter(opt_type == "C") %>%
  dplyr::select(bid) %>%
  pull(1)

bid_price_ATM_Puts <- db_option_cboe %>%
  dplyr::filter(opt_type == "P") %>%
  dplyr::select(bid) %>%
  pull(1)

# Expected Move (In Dollars)
Expected_Move_dll <- (bid_price_ATM_Calls + bid_price_ATM_Puts)*0.84

# Expected Move (In %)
Expected_Move_pct <- (bid_price_ATM_Calls + bid_price_ATM_Puts)*0.84/db_option[["data"]][["current_price"]]

# Analyzing the Suprise frequency
db_Yahoo_Earnings %>%
  dplyr::group_by(Surprise) %>%
  tally() %>%
  dplyr::mutate(pct      = (n/sum(n)),
                Surprise = factor(Surprise, levels = c("Above Expectations", "Met Expectations", "Below Expectations"))) %>%
  dplyr::select(-n) %>%
  ggplot(aes(x = Surprise, y = pct, fill = Surprise)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = pct %>% percent(accuracy = 0.01)), position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = str_glue("Estimated vs Reported EPS of {ticker} - From Yahoo Finance"),
       subtitle = str_glue("Earning Release data since {db_Yahoo_Earnings$earnings_date %>% min()}"),
       caption  = "By: Carlos Jimenez",
       x = "",
       y = "Percentage of time") + 
  theme(legend.position = "none",
        axis.ticks.y    = element_blank(),
        axis.text.y     = element_blank())

db_Yahoo_Earnings %>%
  dplyr::group_by(pm_ah, Surprise) %>%
  tally() %>%
  dplyr::mutate(pct      = (n/sum(n)),
                Surprise = factor(Surprise, levels = c("Above Expectations", "Met Expectations", "Below Expectations"))) %>%
  dplyr::select(-n) %>%
  ggplot(aes(x = Surprise, y = pct, fill = Surprise)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = pct %>% percent(accuracy = 0.01)), limits = c(0, 1.5), position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = str_glue("Estimated vs Reported EPS of {ticker} - From Yahoo Finance (Reported After-Hours (AH) or Pre-Market (PM)"),
       subtitle = str_glue("Earning Release data since {db_Yahoo_Earnings$earnings_date %>% min()}"),
       caption  = "By: Carlos Jimenez",
       x = "",
       y = "Percentage of time") + 
  theme(legend.position = "none",
        axis.ticks.y    = element_blank(),
        axis.text.y     = element_blank()) +
  facet_free(pm_ah ~ ., scales="free")

# Building a DB with the performance of the Price X days before and after the Earnings Date
DB_Earnings_Performance <- NULL
for(Earning_date in db_Yahoo_Earnings$earnings_date %>% unique()){ # Earning_date <- "2022-02-10"
  
  # Sometimes Yahoo Finance allows you to extract Future Earning Dates (we will not use those)
  if(Earning_date %>% as.Date() > (Sys.Date() - Days_Before_Earnings - Days_After_Earnings - 1)){
    # Dont do anything
  }else{
    
    # Extract if it was a morning or afternoon earnings release
    release <- db_Yahoo_Earnings %>%
      dplyr::filter(earnings_date == Earning_date) %>%
      dplyr::select(pm_ah) %>%
      pull(1)

    # Extracting and calculating the performance of the stock
    db_Hist_Performance <- tq_get(ticker %>% toupper(),
                                  from = (Earning_date %>% as.Date()) - Days_Before_Earnings - 1,
                                  to   = (Earning_date %>% as.Date()) + Days_After_Earnings,
                                  get  = "stock.prices",
                                  complete_cases = TRUE) %>%
      dplyr::select(date, close)
    
    # Extracting the first price
    First_Price <- db_Hist_Performance %>%
      dplyr::select(close) %>%
      pull() %>%
      first()
    
    # Calculating the Performance of the price X days before and after the Earnings Release
    db_Hist_Performance <- db_Hist_Performance %>%
      dplyr::transmute(date        = date,
                       close       = close,
                       diff_pct    = (close - First_Price)/close,
                       diff_dollar = (close - First_Price),
                       Days2Earn   = (date - (Earning_date %>% as.Date())) %>% as.numeric(),
                       ID_Earning  = Earning_date %>% as.Date(),
                       release     = release) %>%
      janitor::clean_names()

    # Adding to a DB
    if(is.null(DB_Earnings_Performance)){
      DB_Earnings_Performance <- db_Hist_Performance
    }else{
      DB_Earnings_Performance <- DB_Earnings_Performance %>%
        bind_rows(db_Hist_Performance)
    } 
  }
}

# Plot the performance of the stock X days before and after the earnings date
DB_Earnings_Performance %>%
  dplyr::mutate(release = case_when(release == "AM" ~ "Pre-Market",
                                    TRUE ~ "After Hours") %>% as.factor()) %>%
  dplyr::group_by(id_earning) %>%
  ggplot(aes(x = days2earn, y = diff_pct, group = id_earning, colour = release)) + 
  geom_line(alpha = 0.5) +
  scale_colour_manual(values = c(`Pre-Market` = "steelblue", `After Hours` = "red")) +
  geom_vline(xintercept = 0, 
             linetype   = "dotted", 
             color      = "black", 
             size       = 0.5) +
  geom_hline(yintercept = 0, 
             linetype   = "dotted", 
             color      = "black", 
             size       = 0.5) +
  geom_hline(yintercept = -1*Expected_Move_pct, 
             linetype   = "dotted", 
             color      = "purple", 
             size       = 1) +
  geom_hline(yintercept = +1*Expected_Move_pct, 
             linetype   = "dotted", 
             color      = "purple", 
             size       = 1) +
  geom_hline(yintercept = -1*Expected_Move_pct*1.5, 
             linetype   = "dotted", 
             color      = "orange", 
             size       = 1) +
  geom_hline(yintercept = +1*Expected_Move_pct*1.5, 
             linetype   = "dotted", 
             color      = "orange", 
             size       = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = str_glue("Behavior of the share price {Days_Before_Earnings} days before and {Days_After_Earnings} days after the earnings release."),
       subtitle = str_glue("Historical analysis done on {ticker}. Data since {db_Yahoo_Earnings$earnings_date %>% min()}."),
       caption  = "By: Carlos Jimenez",
       x = "Days (Before and After Earnings Release)",
       y = "Accumulated Return") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title    = element_blank())
