library(DBI)
library(odbc)
library(knitr)
library(formattable)
library(kableExtra)
library(modelr)
library(tidytext)
library(lubridate)
library(tidyverse)
library(keyring)

# include user functions
# source(file="user-functions.R")

fix_date <- function(text) {
  answer <- ifelse(str_length(text) == 8,
                   str_c(
                     str_sub(text, 1, 4), '-',
                     str_sub(text, 5, 6), '-',
                     str_sub(text, 7, 8)
                   ),
                   NA
  )
  answer <- date(answer)
}


fix_oracle_table <- function(df) {
  df.names <- colnames(df)
  
  # num.df <- df %>%
  #   select_if(is.numeric)
  
  txt.df <- df %>%
    select_if(is.character) %>%
    map(str_trim) %>%
    as_tibble
  
  txt.names <- colnames(txt.df)
  
  other.df <- df %>%
    select(-txt.names)
  
  dates.df <- txt.df %>%
    select(ends_with('_date')) %>%
    map(ymd) %>%
    as_tibble
  
  df <- txt.df %>%
    select(-ends_with('_date')) %>%
    bind_cols(., other.df, dates.df) %>%
    select(df.names)
  
  return(df)
  
}

query_db <- function(db_name, query_string)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          db_name, 
                          UID = keyring::key_get("nu_warehouse_id"),
                          PWD = keyring::key_get("nu_warehouse_secret"))
  
  response <- dbSendQuery(connection, query_string)
  tbl <- dbFetch(response)
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  
  names(tbl) <- tolower(names(tbl))
  
  return(as_tibble(tbl))
}

query_db_byparam <- function(db_name, query_string, myparam)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          db_name, 
                          UID = keyring::key_get("nu_warehouse_id"),
                          PWD = keyring::key_get("nu_warehouse_secret"))
  
  response <- dbSendQuery(connection, query_string)
  dbBind(response, myparam)
  tbl <- dbFetch(response)
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  rm(connection)
  
  names(tbl) <- tolower(names(tbl))
  
  return(as_tibble(tbl))
}

read_table <- function(db_name, tab)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          db_name, 
                          UID = keyring::key_get("nu_warehouse_id"),
                          PWD = keyring::key_get("nu_warehouse_secret"))
  
  tbl <- dbReadTable(connection, tab)
  
  # disconnect from the database
  dbDisconnect(connection)
  
  return(as_tibble(tbl))
}

