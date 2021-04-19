setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(nmecr)
first <- readxl::read_xlsx("803638.xlsx")
second <- readxl::read_xlsx("757738.xlsx")
start<- "01/01/2019 00:00"
end<-"12/31/2020 23:45"
raw_data <-
  create_dataframe(
    eload_data = first,
    temp_data = second,
    operating_mode_data = NULL,
    additional_independent_variables = NULL,
    additional_variable_aggregation = c(sum, median),
    start_date = start,
    end_date = end,
    convert_to_data_interval = "15-min",
    temp_balancepoint = 65
  )

openxlsx::write.xlsx(raw_data, 'results.xlsx')
