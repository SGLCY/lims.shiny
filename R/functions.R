

# Functions ---------------------------------------------------------------

# To replace with NA's consecuative test in the template. 
# For the excel workbook creation
# reworked from here 
# https://stackoverflow.com/questions/47013698/replace-consecutive-repeated-values-with-na-in-list
to_na <- function(var) {
  
  # this function will apply to the foodex categorries which are factors
  var= as.character(var)
  
  # The furst value needs to remain unchanged
  c(var[1], if_else(var == lag(var), NA_character_, var)[2:length(var)])
}


#  Aggregate Occurence data -----------------------------------------------


aggregate_occurrence <- function(data, t_uom, .aggr_level){
  
  # table of aggregate values
  tbl_average <- 
    data %>% 
    # if μg/Kg
    {if(t_uom =="μg/Kg") {
      mutate(., across(ends_with("_res"), ~ . / 1000))
    } else .
    } %>% 
    group_by(across(all_of(.aggr_level))) %>% 
    summarise(
      N = n(),
      across(ends_with("_res"), occurrence_summary, .names = "{col}_{fn}"),
      #across(detect_lim, ~median(.), na.rm = TRUE, .names = "lod")
      #across(detect_lim, ~n_distinct(.), na.rm = TRUE, .names = "n_lod")
    ) %>% 
    relocate(
      ends_with("_min"), ends_with("_mean"), ends_with("_median"), ends_with("_p95"), .after  = N
    ) %>% 
    ungroup() 
  
  #tbl_final 
  tbl_foodex_desc %>% 
    distinct(across(all_of(.aggr_level))) %>% 
    left_join(
      tbl_average
    ) %>% 
    # remove the '_res'
    rename_with(str_remove, matches("res_"), "res_") %>% 
    #replace_na(list(N = 0)) %>% 
    {.}
  
}


# UI generation for the filters -------------------------------------------


make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    label = get_col_label(var)
    sliderInput(var, label, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x) ) {
    levs <- levels(x)
    label = get_col_label(var)
    shinyWidgets::pickerInput(var, label, 
                              choices = levs, 
                              selected = levs, 
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `live-Search`  = TRUE,
                                             liveSearchStyle = "contains"
                              )
                              )
  } else {
    # Not supported
    NULL
  }
}


filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x) ) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}



# Error Alert -------------------------------------------------------------

error_alert <- function(.session, .title, .text, .type = "error", .html =  FALSE){
  
  shinyWidgets::sendSweetAlert(session = .session,
                               title = .title,
                               text = .text,
                               type = .type,
                               html= .html
                               )
}



# Capture dataset info ----------------------------------------------------

capture_data_info <- function(data){
  
  
}


get_col_label <- function(var){
  
  label <- dictionary_label_vector[[var]]
  
  if(is.na(label)){
    return(var)
  }  else 
    return(label)
  
}

