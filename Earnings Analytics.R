library(tidyquant)
library(tidyverse)
library(rvest)
library(scales)
library(birk)

# Block Warnings
options(warn=-1)

# Local Variables
ticker               <- "AAPL"
earnings_date        <- "2023-01-26" 
Days_Before_Earnings <- 2
Days_After_Earnings  <- 5

# External Function
html_table_fix <- function(x, header = NA, trim = TRUE, fill = FALSE, dec = ".") {
  
  stopifnot(html_name(x) == "table")
  
  # Throw error if any rowspan/colspan present
  rows <- html_nodes(x, "tr")
  n <- length(rows)
  cells <- lapply(rows, "html_nodes", xpath = ".//td|.//th")
  
  ncols <- lapply(cells, html_attr, "colspan", default = "1")
  # Replace empty values of colspan with "1"
  ncols <- lapply(ncols, function(x) {x[x==""] <- "1"; x})
  ncols <- lapply(ncols, as.integer)
  nrows <- lapply(cells, html_attr, "rowspan", default = "1")
  nrows <- lapply(nrows, as.integer)
  
  p <- unique(vapply(ncols, sum, integer(1)))
  maxp <- max(p)
  
  if (length(p) > 1 & maxp * n != sum(unlist(nrows)) &
      maxp * n != sum(unlist(ncols))) {
    # then malformed table is not parsable by smart filling solution
    if (!fill) { # fill must then be specified to allow filling with NAs
      stop("Table has inconsistent number of columns. ",
           "Do you want fill = TRUE?", call. = FALSE)
    }
  }
  
  values <- lapply(cells, html_text, trim = trim)
  out <- matrix(NA_character_, nrow = n, ncol = maxp)
  
  # fill colspans right with repetition
  for (i in seq_len(n)) {
    row <- values[[i]]
    ncol <- ncols[[i]]
    col <- 1
    for (j in seq_len(length(ncol))) {
      out[i, col:(col+ncol[j]-1)] <- row[[j]]
      col <- col + ncol[j]
    }
  }
  
  # fill rowspans down with repetition
  for (i in seq_len(maxp)) {
    for (j in seq_len(n)) {
      rowspan <- nrows[[j]][i]; colspan <- ncols[[j]][i]
      if (!is.na(rowspan) & (rowspan > 1)) {
        if (!is.na(colspan) & (colspan > 1)) {
          # special case of colspan and rowspan in same cell
          nrows[[j]] <- c(utils::head(nrows[[j]], i),
                          rep(rowspan, colspan-1),
                          utils::tail(nrows[[j]], length(rowspan)-(i+1)))
          rowspan <- nrows[[j]][i]
        }
        for (k in seq_len(rowspan - 1)) {
          l <- utils::head(out[j+k, ], i-1)
          r <- utils::tail(out[j+k, ], maxp-i+1)
          out[j + k, ] <- utils::head(c(l, out[j, i], r), maxp)
        }
      }
    }
  }
  
  if (is.na(header)) {
    header <- all(html_name(cells[[1]]) == "th")
  }
  if (header) {
    col_names <- out[1, , drop = FALSE]
    out <- out[-1, , drop = FALSE]
  } else {
    col_names <- paste0("X", seq_len(ncol(out)))
  }
  
  # Convert matrix to list to data frame
  df <- lapply(seq_len(maxp), function(i) {
    utils::type.convert(out[, i], as.is = TRUE, dec = dec)
  })
  names(df) <- col_names
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(df[[1]]))
  
  if (length(unique(col_names)) < length(col_names)) {
    warning('At least two columns have the same name')
  }
  
  df
} 

getYahooEarnings <- function(ticker){
  
  # Getting the Earnings
  db_Earnings <- str_glue("https://finance.yahoo.com/calendar/earnings?failsafe=1&ynet=0&_device=desktop&device=desktop&symbol={ticker}") %>%
    rvest::read_html() %>%
    rvest::html_nodes("table") %>%
    html_table_fix() %>%
    dplyr::mutate(Month           = substr(`Earnings Date`, 1, 3),
                  Day             = substr(`Earnings Date`, 5, 6),
                  Year            = substr(`Earnings Date`, 8, 12),
                  PM_AH           = substr(`Earnings Date`, 18, 19))
  
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
db_Yahoo <- getYahooEarnings(ticker)

# Getting expected move
db_option <- getOptionChain(ticker, src = "yahoo", Exp = str_glue("{lubridate::year(Sys.Date())}/{lubridate::year(Sys.Date()) + 1}")) %>%
  unlist(recursive = FALSE) %>%
  enframe() %>%
  unnest(cols = c(value))

  # Extracting the type of Option
  opt_type <- c()
  for(i in 1:nrow(db_option)){ # i <- 1
    
    # Selecting the name column
    name_tag <- db_option$name[i]
    
    # Getting the length of that name
    name_length <- nchar(name_tag)
    
    # Getting the option type
    opt_type <- c(opt_type, substr(name_tag, 14, name_length))
  }
  
  # Adding the Option Type
  db_option_enhanced <- db_option %>%
    dplyr::mutate(type = opt_type)
  
# Getting the closet Expiration date (option) from the Earnings release
date_index <- which.closest(db_option_enhanced$Expiration %>% as.Date() %>% unique(), earnings_date %>% as.Date())

closet_option_expiration_date <- db_option_enhanced$Expiration %>% 
  unique() %>%
  pluck(date_index)
  
# Calculating the Expected Move
  # Calls
  bid_price_ATM_Calls <- db_option_enhanced %>%
    dplyr::filter(Expiration == closet_option_expiration_date & type == "calls") %>%
    dplyr::mutate(ATM = ifelse(Strike < getQuote(ticker)$Last, "ITM", "OTM")) %>%
    dplyr::filter(ATM == "OTM") %>%
    head(n = 1) %>%
    dplyr::select(Bid) %>%
    pull(1)
    
  # Puts
  bid_price_ATM_Puts <- db_option_enhanced %>%
    dplyr::filter(Expiration == closet_option_expiration_date & type == "puts") %>%
    dplyr::mutate(ATM = ifelse(Strike < getQuote(ticker)$Last, "ITM", "OTM")) %>%
    dplyr::filter(ATM == "OTM") %>%
    head(n = 1) %>%
    dplyr::select(Bid) %>%
    pull(1)

  # Expected Move (In Dollars)
  Expected_Move_dll <- (bid_price_ATM_Calls + bid_price_ATM_Puts)*0.84
  
  # Expected Move (In %)
  Expected_Move_pct <- (bid_price_ATM_Calls + bid_price_ATM_Puts)*0.84/getQuote(ticker)$Last

# Analyzing the Suprise frequency
db_Yahoo %>%
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
       subtitle = str_glue("Earning Release data since {db_Yahoo$earnings_date %>% min()}"),
       caption  = "By: Carlos Jimenez",
       x = "",
       y = "Percentage of time") + 
  theme(legend.position = "none",
        axis.ticks.y    = element_blank(),
        axis.text.y     = element_blank())

db_Yahoo %>%
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
       subtitle = str_glue("Earning Release data since {db_Yahoo$earnings_date %>% min()}"),
       caption  = "By: Carlos Jimenez",
       x = "",
       y = "Percentage of time") + 
  theme(legend.position = "none",
        axis.ticks.y    = element_blank(),
        axis.text.y     = element_blank()) +
  facet_free(pm_ah ~ .)

# Building a DB with the performance of the Price X days before and after the Earnings Date
DB_Earnings_Performance <- NULL
for(Earning_date in db_Yahoo$earnings_date %>% unique()){ # Earning_date <- "2022-02-10"
  
  # Sometimes Yahoo Finance allows you to extract Future Earning Dates (we will not use those)
  if(Earning_date %>% as.Date() > (Sys.Date() - Days_Before_Earnings - Days_After_Earnings - 1)){
    # Dont do anything
  }else{
    
    # Extract if it was a morning or afternoon earnings release
    release <- db_Yahoo %>%
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
       subtitle = str_glue("Historical analysis done on {ticker}. Data since {db_Yahoo$earnings_date %>% min()}."),
       caption  = "By: Carlos Jimenez",
       x = "Days (Before and After Earnings Release)",
       y = "Accumulated Return") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title    = element_blank())
