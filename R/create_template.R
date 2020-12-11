
create_template_fdx1 <- function(dt_level2, dt_level3, substance_info){
  
  tbl_occurence_headers <- 
    tibble::tribble(
      ~...1,           ~...2,           ~...3,   ~...4, ~...5, ~...6,           ~...7,           ~...8,           ~...9,   ~...10, ~...11, ~...12, ~...13, ~...14, ~...15,
      NA,              NA, "No of Samples", "mg/kg",    NA,    NA,              NA,              NA,              NA,       NA,     NA,     NA,     NA,     NA,     NA,
      NA,              NA,              NA,   "min",    NA,    NA,          "mean",              NA,              NA, "median",     NA,     NA,  "P95",     NA,     NA,
      "FoodExL1_name", "FoodExL2_name",              NA,    "LB",  "MB",  "UB", "Occur_Mean_LB", "Occur_Mean_MB", "Occur_Mean_UB",     "LB",   "MB",   "UB",   "LB",   "MB",   "UB"
    )
  
  # how i read in the values
  # path <- "Data/occurrence_template.xlsm"
  # tbl_occurence_headers <- 
  #   readxl::read_xlsx(path = path , sheet = "Level2", range = "B6:P8", col_names = FALSE )
  
  
  tbl_level3_headers <- 
    tibble::tribble(
      ~c,      ~d,   ~e,   ~f,              ~g,              ~h,              ~i,       ~j,   ~k,   ~l,    ~m,   ~n,   ~o,
      "No of Samples", "mg/kg",  "-",  "-",             "-",             "-",             "-",      "-",  "-",  "-",   "-",  "-",  "-",
      "-",   "min",  "-",  "-",          "mean",             "-",             "-", "median",  "-",  "-", "P95",  "-",  "-",
      "-",    "LB", "MB", "UB", "Occur_Mean_LB", "Occur_Mean_MB", "Occur_Mean_UB",     "LB", "MB", "UB",  "LB", "MB", "UB"
    )
  
  tbl_level3_levels <- 
    tibble::tribble(
      ~...1,     ~...2,    ~...3,
      "Level 1", "Level 2", "Level 3"
      
    )
  
  
  named_ranges <- 
    
    tibble::tribble(
      ~exposure_type,               ~type_reference_value,       ~substance_category,
      "DAILY",                    "Acceptable Intake",                "Additive",
      "WEEKLY",                     "Tolerable Intake",               "Pesticide",
      NA, "Provisional Maximum Tolerable Intake", "Veterinary Drug Residue",
      NA,          "Benchmark Dose Level (BMDL)",             "Contaminant",
      NA,                                     NA,    "Genotoxic-Carcinogen"
    )
  
  # styles
  
  headerStyle <- openxlsx::createStyle(
    fontSize = 12, 
    fontColour = NULL, #"#FFFFFF", 
    halign = "center",
    fgFill = "grey80", border = "TopBottom", borderColour = "#141010",
    wrapText = TRUE
  )
  
  info_style <- openxlsx::createStyle(
    fontSize = 13, 
    fontColour = NULL, #"#FFFFFF", 
    #halign = "center",
    fgFill = "#7E99C2", border = "TopBottomLeftRight ", borderColour = "#141010",
    wrapText = FALSE
  )
  
  
  wb = openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, sheetName = "UniqueLevel3")
  openxlsx::addWorksheet(wb, sheetName = "Level2")
  openxlsx::addWorksheet(wb, sheetName = "Level3")
  
  # UniqueLevel3 Worksheet --------------------------------------------------
  
  openxlsx::writeDataTable(wb, sheet = "UniqueLevel3", x = tbl_unique_level3)
  
  # hide it
  # first position of UniqueLevel3 sheet
  ul3_pos <- which("UniqueLevel3" %in% wb$sheet_names)
  openxlsx::sheetVisibility(wb)[ul3_pos] <- "hidden"
  
  
  # Level 3 Worksheet -------------------------------------------------------
  
  openxlsx::writeData(wb, sheet = "Level3", x = substance_info[c("a", "b")], colNames = FALSE, startCol = 2, startRow = 1)
  
  openxlsx::writeData(wb, sheet = "Level3", x = tbl_level3_headers, colNames = FALSE, startCol = 5, startRow = 4)
  openxlsx::writeData(wb, sheet = "Level3", x = tbl_level3_levels, colNames = FALSE, startCol = 2, startRow = 6)
  openxlsx::writeData(wb, sheet = "Level3", x = dt_level3, colNames = FALSE, startCol = 2, startRow = 7)
  
  
  # -- Merge cells to look better --#
  openxlsx::mergeCells(wb, sheet = "Level3", cols = 6:17, rows = 4)
  openxlsx::mergeCells(wb, sheet = "Level3", cols = 6:8, rows = 5)
  openxlsx::mergeCells(wb, sheet = "Level3", cols = 9:11, rows = 5)
  openxlsx::mergeCells(wb, sheet = "Level3", cols = 12:14, rows = 5)
  openxlsx::mergeCells(wb, sheet = "Level3", cols = 15:17, rows = 5)
  
  openxlsx::mergeCells(wb, sheet = "Level3", cols = 5, rows = 4:6)
  
  
  
  # -- Format areas of the worksheet --#
  
  # Header
  openxlsx::addStyle(wb, sheet = "Level3", headerStyle, rows = 4:6, cols = 5:17,  gridExpand = TRUE)
  
  
  new_style <- headerStyle
  new_style$fontSize$val <- 14
  
  openxlsx::addStyle(wb, sheet = "Level3", new_style, rows = 6, cols = 2:4,  gridExpand = TRUE)
  
  
  # Substance Info
  openxlsx::addStyle(wb, sheet = "Level3", style = info_style, rows = 1:5, cols = 2:3,  gridExpand = TRUE)
  
  # Set the reference value as numeric
  openxlsx::addStyle(wb, sheet = "Level3", rows = 3, cols = 3, style = openxlsx::createStyle(numFmt = "NUMBER"))
  
  # Width of columns and hide some columns
  openxlsx::setColWidths(wb, "Level3", cols = c(2,3,4), widths = c(30,30, 30))
  
  
  # Level2 Worksheet ------------------------------------------------------------------
  
  
  # Info takes 5 rows
  # Headers start at row 6 and take 3 rows 
  # Occurence data start at 9
  # set colNames = FALSE. we dont; want the col names in either table
  
  openxlsx::writeData(wb, sheet = "Level2", x = substance_info, colNames = FALSE, startCol = 2, startRow = 1)
  openxlsx::writeData(wb, sheet = "Level2", x = tbl_occurence_headers, colNames = FALSE, startCol = 2, startRow = 6)
  openxlsx::writeData(wb, sheet = "Level2", x = dt_level2, colNames = FALSE, startCol = 2, startRow = 9)
  openxlsx::writeData(wb, sheet = "Level2", x = named_ranges,  startCol = 25, startRow = 4)
  
  
  # Merge cells to look better
  openxlsx::mergeCells(wb, sheet = "Level2", cols = 5:16, rows = 6)
  openxlsx::mergeCells(wb, sheet = "Level2", cols = 5:7, rows = 7)
  openxlsx::mergeCells(wb, sheet = "Level2", cols = 8:10, rows = 7)
  openxlsx::mergeCells(wb, sheet = "Level2", cols = 11:13, rows = 7)
  openxlsx::mergeCells(wb, sheet = "Level2", cols = 14:16, rows = 7)
  
  openxlsx::mergeCells(wb, sheet = "Level2", cols = 4, rows = 6:8)
  
  
  # -- Format areas of the worksheet --#
  
  # Header
  openxlsx::addStyle(wb, sheet = "Level2", headerStyle, rows = 6:8, cols = 2:16,  gridExpand = TRUE)
  #openxlsx::addStyle(wb, sheet = "Level2", headerStyle, rows = 6:8, cols = 2:3,  gridExpand = TRUE)
  
  
  # Substance Info
  openxlsx::addStyle(wb, sheet = "Level2", style = info_style, rows = 1:5, cols = 2:3,  gridExpand = TRUE)
  
  # Hide the named ranges in the sheet
  openxlsx::addStyle(wb, sheet = "Level2", style = openxlsx::createStyle(fontColour = "white"), rows = 4:9, cols = 25:27,  gridExpand = TRUE)
  
  # Set the reference value as numeric
  openxlsx::addStyle(wb, sheet = "Level2", rows = 3, cols = 3, style = openxlsx::createStyle(numFmt = "NUMBER"))
  
  # --  Data Validation in the Substance Info area -- #
  
  # Substance Category
  openxlsx::dataValidation(wb, sheet = "Level2", row = 2, cols = 3, type = "list", value = "'Level2'!$AA$5:$AA$9")
  
  # Type of reference value
  openxlsx::dataValidation(wb, sheet = "Level2", row = 4, cols = 3, type = "list", value = "'Level2'!$Z$5:$Z$8")
  
  # Exposure type - weekly daily
  openxlsx::dataValidation(wb, sheet = "Level2", row = 5, cols = 3, type = "list", value = "'Level2'!$Y$5:$Y$6")
  
  
  # Width of columns and hide some columns
  openxlsx::setColWidths(wb, "Level2", cols = c(2,3), widths = c(50,50))
  openxlsx::setColWidths(wb, "Level2", cols = 25:27, 
                         #  issue in shinyapps. 'object 'cols' not found
                         #hidden = rep(TRUE, length(cols))
                         hidden = TRUE
                         )
  
  return(wb)
}


