
# This is a Shiny web application for aggregating occurrence data
# extracted for the LIMS system of SGL

library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(forcats)
library(tidyr)
library(purrr)
#library(reactable) #if active, then usethis::use_package()
#library(shinyDataFilter)
#library(tidyverse)

options(shiny.maxRequestSize = 10 * 1024^2)

ggplot2::theme_set(ggplot2::theme_light(16))


options(shiny.reactlog=TRUE)


timeoutSeconds <- 1500
timeoutMinutes <- timeoutSeconds/60

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)



# Customisation Values ----------------------------------------------------

font.size <- "10pt"

side_width   <- 3
main_width   <- 12 - side_width
#table_digits <- 2

substance_category   <- c("Additive", "Pesticide", "Veterinary Drug Residue", "Contaminant", "Genotoxic-Carcinogen")
reference_value_type <- c("Acceptable Intake", "Tolerable Intake", "Provisional Maximum Tolerable Intake", "Benchmark Dose Level (BMDL)")
exposure_type        <- c("DAILY", "WEEKLY")


data_dictionary <- readxl::read_xlsx("Data/data_dictionary.xlsx") 
dictionary_label_vector <- 
  data_dictionary %>% 
  select(var_id, var_label) %>% 
  tibble::deframe() %>% 
  # Some labels are not filled 
  imap_chr(~ {
    if(is.na(.x)){
      .x = .y
    } else {
      .x
    }
  })

foodex1 <- read_xlsx("Data/Foodex.1.xlsx") %>% 
  select(
    -ends_with("_HCODE"),
    -ends_with("_ID")
  ) %>% 
  mutate(across(everything(), as.factor))

# a list of statistics for occurrence data
occurrence_summary <- list(
  # N      = ~n(),
  min    = ~min(., na.rm = TRUE),
  mean   = ~mean(., na.rm = TRUE),
  #sd     = ~sd(., na.rm = TRUE),
  median = ~median(., na.rm = TRUE),
  #max    = ~max(., na.rm = TRUE),
  p95    = ~quantile(., 0.95)
  
  # Statistic on the LOD
)

errorMsg_uom <- paste0("Cannot aggregate occurrence data!", "\n", 
                       "There are more than one unit-of-measure in the dataset (t_uom)", "\n",
                       "Please check your dataset or filter it down to the substance using", "\n",
                       "the filters on the left")
# Foodex1 ----

tbl_foodex_desc <- 
  foodex1 %>% 
  distinct(FOODEX_L1_DESC, FOODEX_L2_DESC, FOODEX_L3_DESC, FOODEX_L4_DESC) %>% 
  relocate(FOODEX_L1_DESC, FOODEX_L2_DESC, FOODEX_L3_DESC, FOODEX_L4_DESC)


fdx1_group_level2 <- c("FOODEX_L1_DESC", "FOODEX_L2_DESC")
fdx1_group_level3 <- c("FOODEX_L1_DESC" ,"FOODEX_L2_DESC", "FOODEX_L3_DESC")

tbl_unique_level3 <- 
  foodex1 %>% 
  distinct(FOODEX_L3_DESC, FOODEX_L2_DESC, FOODEX_L1_DESC)


# UI ----------------------------------------------------------------------


ui <- fluidPage( theme = shinythemes::shinytheme("yeti"),
                 #shinythemes::themeSelector(),
                 # Application title
                 titlePanel("Aggregate LIMS data"),
                 #img(src = "C:/Users/User/OneDrive/IMPROVAST/SGL/lims.shiny/logo.jpg"),
                 
                 #waiter::use_waiter(),
                 waiter::waiter_on_busy(),
                 shinyFeedback::useShinyFeedback(),
                 
                 # Thick horizontal line
                 tags$head(
                   tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                 ),
                 tags$script(inactivity),  
                 
                 
                 sidebarLayout(
                   sidebarPanel(
                     hr(),
                     h3("Upload your occurrence raw data (or use the demo file")
                     , radioButtons("data_input", "Input Data", choices = c("Demo Data" = 0, "Upload Data" = 1), selected = 0)
                     #, fileInput("file", "An excel (.xls or .xlsx) file please", buttonLabel = "Upload...", accept = ".xlsx")
                     #, p("Or use the demo file")
                     , uiOutput("upload_data_ui")
                     , uiOutput("substance_info_ui")
                     , hr()
                     , uiOutput("select_columns_ui")
                     #, uiOutput("filter")
                     , width = side_width
                   ),
                   
                   
                   mainPanel(
                     
                     tabsetPanel(id = "data_tabset",
                                 
                                 tabPanel(title = "Aggregated data", 
                                          uiOutput("template_ui"),
                                          DT::DTOutput("tbl_aggregate")
                                 ),
                                 tabPanel(title = "Raw data", 
                                          DT::DTOutput("tbl_occurrence")
                                          #reactable::reactableOutput("tbl_occurrence")
                                 ),
                                 tabPanel(title = "FoodEx1",
                                          h3("The FoodEx1 food classification system"),
                                          DT::dataTableOutput("foodex1")
                                 ),
                                 tabPanel(title = "Data Description",
                                          h3("A description of the columns in the dataset"),
                                          br(),
                                          p("The table shows what each column in the data represents"),
                                          DT::dataTableOutput("data_dictionary")
                                 )
                     )
                     
                   )
                 )
)


# VAriable names I need in the dataset
vars_nessescary <- 
  c(
    "year_ch", "date_samp", "date_anal", 
    paste0("sample_", 1:3), 
    paste0("sample_", 1:3, "e") , 
    "foodex", "quality", "res_num", "t_eng",
    "detect_lim", "determ_lim",
    "progsampst"  #  smpling method
    , "t_uom", "purp_anal"
  )


file_input_error <- 
  c(      "Your data does not have the correct column names!\nPlease check the following:\n
           a) Your data are in a sheet called `Sheet1`,\n
           b) The column names are the ones shown in the Data Description tab")


# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {
  
  w <- waiter::Waiter$new(
    id = "tbl_aggregate",
    html = waiter::spin_4(), 
    color = waiter::transparent(.5)
  )
  
  
  rv <- reactiveValues(
    substance =  "",
    n_obs = NULL,
    t_uom = "μg/Kg"
  )
  
  
  dataset_exists <- reactiveVal()
  dataset_info <- reactiveValues(
    name = ""
  )
  
  
  observeEvent(data_merged(), {
    
    rv$substance <- unique(data_merged()$t_eng)
    #rv$t_uom <- unique(data_merged()$t_uom)
  })
  
  output$substance <- renderText({
    
    substance =  paste(rv$substance,collapse = ",")
    
    print(paste0("Substance:",substance))
    
  })
  
  output$n_obs <- renderText(paste0("Number of raw occurrence observations used:", rv$n_obs))
  
  output$file_name <- renderText({
    
    paste0("Dataset  :",  "'", dataset_info$name, "'")
    
  })
  
  # check_data_input <-  function(){
  #     
  #     
  #     if(input$btn_use_demo>0 | isTruthy(input$file)){
  #         TRUE
  #     }
  #     
  #     else {
  #         validate("Upload a dataset or use the Demo Data")
  #     }
  #     
  # }
  
  
  # Read Data (or use Demo) -------------------------------------------------
  
  dataset <- reactive({
    
    is_ok <- FALSE
    
    if(input$data_input == 1 ){
      
      # Upload data
      #req(input$file)
      req(dataset_exists())
      
      ext <- tools::file_ext(input$file$name)
      
      # file.rename(input$file$datapath,
      #             paste(input$file$datapath, ext, sep="."))
      
      print(input$file$datapath)
      
      # capture the file details
      dataset_info$name <- input$file$name
      dataset_info$path <- input$file$datapath
      dataset_info$size <- input$file$size
      dataset_info$type <- input$file$type
      
      # if the readxl has some warinings it all gets stuck!!!!
      out <- tryCatch(
        
        { suppressWarnings(
          
          #out <- readxl::read_excel(paste(input$file$datapath, ext, sep=".")) 
          out <- readxl::read_excel(input$file$datapath) 
        )
          
        },
        
        error=function(cond){return(cond)}
      )
      
      if(!"tbl_df" %in% class(out) ){
        
        message <- paste0("<p>",paste(out, collapse = "<br>"), "</p>")
        
        error_alert(
          .session = session,
          .title = "Unable to read the file",
          .text = message,
          .type = "info",
          .html = TRUE
        )
      } else {
        
        # Succesfull read. Nw check for the variables in the dataset
        is_ok <- all(vars_nessescary %in% names(out))
        
        if(!is_ok){
          error_alert(
            .session = session,
            .title = "Wrong data format",
            .text = "You dont have the columns in the dataset. 
              Please check the 'Data Description' tab for the required columns"
          )
        }
      }
      
      dataset_exists <- TRUE
      
    }
    
    if(input$data_input == 0){
      
      # use demo data
      path <- "Data/demo_data_CD 2011-2013.xlsx"
      
      out <- suppressWarnings(readxl::read_xlsx(path))
      is_ok <- TRUE
      # initialise existance of upload
      dataset_exists(FALSE)
      
      # data info
      dataset_info$name <- str_remove(path, "Data/")
      dataset_info$path <- path
      dataset_info$size <- file.size(path)
      dataset_info$type <- tools::file_ext(path)
      
    } 
    
    
    req(is_ok)
    
    out
    
  })
  
  # to unblock the reading the of the file 
  observeEvent(input$file, {
    dataset_exists(TRUE)
  })
  
  output$upload_data_ui <- renderUI({
    
    req(input$data_input == 1)
    
    fileInput("file", "An excel (.xls or .xlsx) file please", buttonLabel = "Upload...", accept = ".xlsx")
    
    
  })
  
  data_to_use <- reactive({
    
    req(dataset())
    
    dataset() %>% 
      select(all_of(vars_nessescary)) %>% 
      mutate(across(where(is.character), ~ fct_explicit_na(.))
      )  
  })
  
  # Filtering ---------------------------------------------------------------
  
  # filtered_data <-  callModule(
  #     
  #     shiny_data_filter,
  #     "data_filter",
  #     data = data_to_use(),
  #     verbose = FALSE
  #   )
  #   
  
  data_columns <- reactive({
    
    #  named list
    # Name = label, value = actual column name
    #  To use in the pickerInput
    dictionary_label_vector %>% 
      tibble::enframe() %>%
      select(value, name) %>% 
      tibble::deframe() %>% 
      as.list()
    
    #names(data_to_use())
    
    })
  
  output$select_columns_ui <- renderUI({
    
    req(data_columns())
    tagList(
      h3("Filter your data"),
      shinyWidgets::pickerInput("selected_columns", "Choose columns to filter", 
                                choices = data_columns(),
                                multiple = TRUE,
                                selected = "",
                                options = list(`actions-box` = TRUE,
                                               `live-Search`  = TRUE,
                                               liveSearchStyle = "contains"
                                )
      ),
      uiOutput("filter_data_ui")
      
    )
    
  })
  
  output$filter_data_ui <- renderUI({
    
    req(input$selected_columns)
    
    tagList(
      
      h5("Filters:"),
      purrr::map(input$selected_columns, ~ make_ui(data_to_use()[[.x]], .x))
    )
    
    
  })
  
  selected_rows <- reactive({
    
    req(data_to_use())
    #req(input$selected_columns)
    print("in selected rows")
    # Is a logical vector to be used for the filtering
    if(!isTruthy(input$selected_columns)){
      
      rep(TRUE, nrow(data_to_use()))
      
    } else {
      
      
      each_var <- purrr::map(input$selected_columns, ~ filter_var(data_to_use()[[.x]], input[[.x]]))
      
      reduce(each_var, `&`)
      
    }
    
  })
  
  
  # filtered_data <- reactive({
  #   
  #   data_to_use()[selected_rows(), ]
  #   
  # })
  
  data_merged <- reactive({
    
    req(selected_rows())
    print("nowdata_merged")
    
    #dt <- isolate(data_to_use())
    
    data_to_use()[selected_rows(), ] %>% 
      
      rename(FOODEX_L4_CODE = foodex) %>% 
      # Clean dates
      mutate(
        date_samp = lubridate::dmy(date_samp),
        date_anal = lubridate::dmy(date_anal)
      ) %>% 
      # Substitution of 0 values
      mutate(
        censored = if_else(res_num == 0 , TRUE, FALSE)
      ) %>% 
      mutate(
        LB_res = res_num,
        MB_res = if_else(censored, detect_lim/2, res_num),
        UB_res = if_else(censored, detect_lim, res_num),
      ) %>% 
      # MERGE with FoodEx1
      left_join(
        foodex1, by = ("FOODEX_L4_CODE")
      ) %>% 
      {.}
    
  })
  
  
  
  fdx_group_level <- reactive({
    
    req(input$aggr_level)
    
    switch(
      input$aggr_level,
      #"Level 1" = c("FOODEX_L1_DESC"),
      "Level 2" = c("FOODEX_L1_DESC","FOODEX_L2_DESC"),
      "Level 3" = c("FOODEX_L1_DESC","FOODEX_L2_DESC", "FOODEX_L3_DESC")
      
    )
  })
  
  observeEvent(data_merged(),{
    
    # uom <- unique(data_merged()$t_uom)
    # 
    # if(length(uom)>1){
    #   
    # }
    
    rv$t_uom  = unique(data_merged()$t_uom)
    
  })
  
  tbl_aggregate <- reactive({
    w$show()
    
    #req(data_merged())
    print("in aggregation")
    dt <- data_merged()
    rv$n_obs <- nrow(dt)
    # FoodEx1 aggregation
    
    if(length(rv$t_uom)>1){
      NULL
    } else{
      
      list(
        level2 = aggregate_occurrence(dt, rv$t_uom, .aggr_level = fdx1_group_level2),
        
        level3 = aggregate_occurrence(dt, rv$t_uom, .aggr_level = fdx1_group_level3) %>% 
          filter(!is.na(.data[["N (N_censored)"]]))
      )
    }
    #  FoodEx 2 aggregation
    
    # perhaps use a switch to distinguish fdx1 and fdx2
    
  })
  
  output$tbl_aggregate <- DT::renderDataTable({
    
    #check_data_input()
    
    req(input$aggr_level)
    req(fdx_group_level())
    
    if(is.null(tbl_aggregate())){
      
      show_toaster("Problem with the unit of measure",
                   "Check the message")
      
      validate(errorMsg_uom )
    }
    
    table_to_view <- 
      switch(
        input$aggr_level,
        #"Level 1" = c("FOODEX_L1_DESC"),
        "Level 2" = tbl_aggregate()$level2,
        "Level 3" = tbl_aggregate()$level3
        
      )
    
    columns <- setdiff(names(table_to_view),fdx_group_level())
    
    #digits <- if(isTruthy(input$tbl_digits)) input$tbl_digits else 3
    digits <- input$tbl_digits
    
    table_to_view %>% 
      DT::datatable(
        filter = "top",
        options = list(
          # initComplete = DT::JS(
          #   "function(settings, json) {",
          #   paste0("$(this.api().table().body()).css({'font-size': '", font.size, "'});"),
          #   "}"),
          #dom =  't',
          searching= TRUE,
          paging = FALSE,
          deferRender = TRUE,
          scrollX = TRUE,
          scrollY = "600px",
          scroller = TRUE
        )
      ) %>% 
      DT::formatRound(columns = columns[columns != 'N (N_censored)'], digits = digits ) %>% 
      #DT::formatRound(columns = c('N (N_censored)'), digits = 0) %>% 
      {.}
  })
  
  
  # Outputs -----------------------------------------------------------------
  
  output$foodex1 <- DT::renderDataTable({
    foodex1
  }, filter = "top")
  
  output$tbl_occurrence <- DT::renderDataTable({
  #output$tbl_occurrence <- reactable::renderReactable({  
    #check_data_input()
    
    data_merged() %>%
      DT::datatable(
        filter = "top"
        , options = list(
          # initComplete = DT::JS(
          #   "function(settings, json) {",
          #   paste0("$(this.api().table().body()).css({'font-size': '", font.size, "'});"),
          #   "}"),
          paging = FALSE,
          deferRender = TRUE,
          scrollX = TRUE,
          scrollY = "600px",
          scroller = TRUE

        )

      )
    # data_merged() %>% 
    #   reactable(
    #     pagination = TRUE, 
    #     #minRows = 30,
    #     resizable = TRUE,
    #     #showPageSizeOptions = TRUE, pageSizeOptions = c(50, 100, 200),
    #     highlight = TRUE, 
    #     #height = 850, 
    #     filterable = TRUE
    #   )
    
    
  })
  
  output$template_ui <- renderUI({
    
    req(tbl_aggregate())
    print("rendering UI")
    
    tagList(
      
      p("")
      
      , fluidRow(
        column(width = 4, 
               shinyWidgets::radioGroupButtons(
                 inputId = "aggr_level",
                 label = "FoodEx1 aggregation level", 
                 choices = c("Level 2", "Level 3"),
                 selected = "Level 2",
                 status = "primary"
               ),
               hr(),
               textOutput("file_name"),
               textOutput("n_obs"),
               # tags$head(tags$style("#n_obs{color: red;
               #              font-size: 14px;
               #              font-style: italic;
               #              }"
               # )
               # )
               hr()
        ),
        column(width = 4,
               numericInput("tbl_digits", "Decimals?", value = 3, 
                            min = 1, max = 10, step = 1, width = "60px"
               )
        )
        
      )
      ,
      downloadButton("down_excel", "Download Excel Template"),
      br(),
      hr()
      
    )
  }) 
  
  output$data_dictionary <- DT::renderDataTable({
    
    DT::datatable(data_dictionary %>% 
                    mutate(
                      Required = factor(if_else(var_id %in% vars_nessescary, "Yes", "No"))
                    ),
                  filter = "top",
                  options = list(
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      paste0("$(this.api().table().body()).css({'font-size': '", font.size, "'});"),
                      "}"),
                    searching= TRUE,
                    paging = FALSE,
                    deferRender = TRUE,
                    scrollX = TRUE,
                    scrollY = "600px",
                    scroller = TRUE
                  )
    )
    
  })
  
  # Other UIs ---------------------------------------------------------------
  
  
  output$substance_info_ui <- renderUI({
    
    #req(data_merged())
    
    font_size <- "12px"
    
    tagList(
      
      tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
      # https://community.rstudio.com/t/how-to-put-the-inline-textinput-label-at-the-left-of-the-column-and-the-textinput-box-at-the-right-of-the-column/24519
      
      hr(style="color: black"),
      
      
      h3("Edit the substance information accordingly"),
      
      textOutput("substance"),
      
      tags$table(
        tags$tr(width = "100%",
                tags$td(width = "60%", div(style = glue::glue("font-size:{font_size};"), "Substance Category")),
                tags$td(width = "40%", selectInput("substance_category", label = NULL, choices = substance_category ))
        ),
        tags$tr(width = "100%",
                tags$td(width = "60%", tags$div(style = glue::glue("font-size:{font_size};"),"Reference Value (μg/ Kg b.w.")),
                tags$td(width = "40%", numericInput("reference_value", value = 0, min = 0, label = NULL))
        ),
        tags$tr(width = "100%",
                tags$td(width = "60%", tags$div(style = glue::glue("font-size:{font_size};"),"Type of Reference Value")),
                tags$td(width = "40%", selectInput("reference_value_type", label = NULL, choices = reference_value_type ))
        ),
        tags$tr(width = "100%",
                tags$td(width = "60%", tags$div(style = glue::glue("font-size:{font_size};"), "Exposure type")),
                tags$td(width = "40%", selectInput("exposure_type", label = NULL, choices = exposure_type ))
        )
      )
      
      #shiny_data_filter_ui("data_filter")
    )
    
  })
  
  output$down_excel <- downloadHandler(
    
    filename = function(){
      paste0("Occurrence - ", c(as.character(rv$substance)), ".xlsx")
    },
    content = function(file){
      
      digits <- isolate(input$tbl_digits)
      
      dt_level2 <- 
        tbl_aggregate()$level2 %>% 
        mutate(across(-all_of(c(fdx1_group_level2, 'N (N_censored)')), ~round(., digits = digits))
        ) %>% 
        # Remove consequative values for the template
        mutate(across(all_of(fdx1_group_level2), ~ to_na(.)))
      
      dt_level3 <- 
        tbl_aggregate()$level3 %>% 
        mutate(across(-all_of(c(fdx1_group_level3, 'N (N_censored)')), ~round(., digits = digits))
        )
        # remove empty rows
        #filter(!is.na(.data[["N (N_censored)"]]))
      
      print(dt_level3)
      substance_info <- tbl_substance_info()
      
      
      template <- create_template_fdx1(dt_level2, dt_level3, substance_info)
      
      #writexl::write_xlsx(template, file)
      openxlsx::saveWorkbook(template, file = file, overwrite = TRUE)
      
    }
  )  
  
  # Collect Substance info
  tbl_substance_info <- reactive({
    
    # Arrgghh..
    reference_value <- as.character(input$reference_value)
    
    tibble::tribble(
      ~a, ~b, ~c,
      "Chemical Substance", rv$substance, "Type in the name of the chemical",
      "Substance Category", input$substance_category , "Click the cell and Select from the drop down list",
      "Reference value (μg/Kg b.w.)", reference_value, "Type in the reference value",
      "Type of Reference value", input$reference_value_type, "Click the cell and Select from the drop down list",
      "Type", input$exposure_type, "Click the cell and Select"
    )
    
  })
  
  
  
  # https://stackoverflow.com/questions/33839543/shiny-server-session-time-out-doesnt-work
  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste0("Session timeout due to ", 
             #input$timeOut, 
             timeoutMinutes, " minutes"
             ," inactivity -", Sys.time(),
             "\nRefresh your browser to reload the application"),
      footer = NULL
    ))
    session$close()
  })
  
}



# Run the application 
myApp <- function(...){
  
  shinyApp(ui = ui, server = server, ...)
}

