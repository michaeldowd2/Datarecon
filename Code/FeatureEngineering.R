# --------------------------------------------------------
# MA Crossover Features ----------------------------------
ma_crossover_features <- function(Data) {
  count = 1
  for (i in 1 : length(SHORT_MAS)) {
    short_MA = SHORT_MAS[i]
    for (j in 1 : length(LONG_MAS)) {
      long_MA = LONG_MAS[j]
      if (long_MA > short_MA) {
        df = single_MA_crossover(Data, short_MA, long_MA)
        if (count == 1) {
          master = df
        } 
        else {
          master = merge(master, df, by = "Date", all = FALSE)
        }
        count = count + 1
      }
    }
  }
  return(master)
}

single_MA_crossover <- function(Data, short_MA, long_MA) {
  df = Data %>%
    arrange(Date) %>%
    mutate_at(vars(-Date), ~ (roll_mean(., short_MA, align = "right", fill = NA)) -
                (roll_mean(., long_MA, align = "right", fill = NA)))  %>%
    filter(!is.na(.[[2]]))
  prefix = paste('MA', toString(short_MA), '_', 'MA', toString(long_MA), sep = '')
  
  names(df)[-1] <- paste(colnames(df)[-1], prefix, sep = ".")
  
  return(df)
}

# --------------------------------------------------------
# Dates Features -----------------------------------------
dates_features <- function(Data) {
  Data %>%
    mutate(Part_Of_Year = yday(Date)/365) %>%
    mutate(Part_Of_Month = day(Date)/31) %>%
    mutate(Part_Of_Week = case_when(weekdays(Date) == 'Monday' ~ 0,
                                    weekdays(Date) == 'Tuesday' ~ 0.25,
                                    weekdays(Date) == 'Wednesday' ~ 0.5,
                                    weekdays(Date) == 'Thursday' ~ 0.75,
                                    weekdays(Date) == 'Friday' ~ 1.0, TRUE ~ 0.0)) %>%
    select(Date, Part_Of_Year, Part_Of_Month, Part_Of_Week) %>%
    return()
}