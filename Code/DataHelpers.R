# --------------------------------------------------------
# Load files to dataframe---------------------------------
create_master_dataframe <- function(Files, Columns) {
  parent_path <- substr(getwd(), 1, regexpr("\\/[^\\/]*$", getwd()))
  path <- paste(parent_path, 'Data/', sep = "")
  dataframes = vector("list", length(Files))
  for (i in 1 : length(Files)) {
    file <- Files[i]
    prefix <- str_replace(file, 'csv', '')
    df <- read_csv(paste(path, file, sep = ""))
    df <- df %>%
      mutate(Date=as.numeric(format(strptime(Date,"%Y-%m-%d"),"%Y%m%d"))) %>%
      discard(~all(is.na(.x))) %>%
      select(which(colSums(.) != 0)) %>%
      mutate(Date= as.Date(as.character(Date), "%Y%m%d")) %>%
      select(one_of(c('Date', str_replace(Columns, prefix, ''))))
    
    names(df)[-1] <- paste(prefix, colnames(df)[-1], sep = "")
    
    if (i == 1) {
      master <- df
    }
    else {
      master <- merge(master, df, by="Date")
    }
  }  
  return(master)
}

# -------------------------------------------------------------------
# At the moment of trading some data won't be available and some will
offset_columns <- function(Data, Columns, Lags = NULL, Leads = NULL) {
  res <- Data %>% select(one_of(c('Date', Columns)))
  
  if (!is.null(Leads)) { #leading somme target columns
    for (i in 1:length(Columns)) {
      col_name <- Columns[i]
      lead_int <- Leads[i]
      new_col_name <- col_name
      if (lead_int > 0) new_col_name <- paste(col_name, '.LEAD', toString(lead_int), sep = "" )
      res <- res %>%
        mutate_at(vars(col_name), ~ lead(., lead_int)) %>%
        rename(!!new_col_name := !!col_name)
    }
  }
  else {
    for (i in 1:length(Columns)) {
      col_name <- Columns[i]
      lag_int <- Lags[i]
      new_col_name <- col_name
      if (lag_int > 0) new_col_name <- paste(col_name, '.LAG', toString(lag_int), sep = "" )
      res <-  res %>%
        mutate_at(vars(col_name), ~ lag(., lag_int)) %>%
        rename(!!new_col_name := !!col_name)
    }
  }
  
  res %>%  drop_na() %>% return()
}

# --------------------------------------------------------
# Create dateranges for datasets--------------------------
create_dateranges <- function(Features, Labels, Analysis_Mode, Backtest_Days, Model_Training_Days, Ensemble_Training_Days) {
  dates = Features %>%
    merge(Labels, by = "Date", all = FALSE) %>%
    select(Date)
  dataframes = list()
  steps = c(1)
  n = length(dates$Date)
  if (Analysis_Mode == 0) steps <- c(Backtest_Days:1)
  index = 1
  for (i in steps) {
    latest_date = sort(dates$Date,partial=n-i)[n-i]
    filtered = dates %>%
      filter(Date <= latest_date) %>%
      arrange(desc(Date)) %>%
      top_n(Model_Training_Days + Ensemble_Training_Days)
    dataframes[i] = list(filtered)
    index = index + 1
  }
  
  return(dataframes)
}

# --------------------------------------------------------
# Create base datasets using dateranges-------------------
extract_training_data <- function(Dates, Features, Labels, Mode = 'MODEL', Training_Days) {
  labels <-  Labels %>%
    merge(Dates, by = "Date", all = FALSE) %>%
    mutate_at(vars(-Date), ~ . - lag(., 1)) %>%
    drop_na()
  
  names(labels)[-1] <- paste(colnames(labels)[-1], 'DIFF1', sep = ".")
  
  extract_amount <- Training_Days
  if (Mode == 'ENSEMBLE') extract_amount <- -Training_Days
  
  features <- Features %>% merge(Dates, by = "Date", all = FALSE) %>%
    top_n(extract_amount, Date)
  
  labels <- labels %>% top_n(extract_amount, Date)
  
  result <- list(features,
                 labels)
  names(result) <- c('Features',
                     'Labels')
  return(result)
}