## code to prepare `CL_FREQ` dataset goes here


temp_excel_file <- file.path(tempdir(), "CLFREQ.xlsx")

download.file(url = 'https://registry.sdmx.org/ws/public/sdmxapi/rest/codelist/SDMX/CL_FREQ/2.1?format=xlsx&detail=full&references=none&saveAS=CLFREQ.xlsx',
              destfile = temp_excel_file, method = 'curl')

tmp <- readxl::read_excel(temp_excel_file, sheet = 1, skip = 11)

CL_FREQ <- readxl::read_excel(temp_excel_file, sheet = 1, skip = 11)
names(CL_FREQ) <- snakecase::to_snake_case(names(CL_FREQ))
CL_FREQ$description <- stringr::str_replace(gsub("\\s+", " ", stringr::str_trim(CL_FREQ$description)), "B", "b")

CL_FREQ <- CL_FREQ %>% dplyr::select(.data$id, .data$name, .data$description, .data$name_locale, .data$description_locale)
CL_FREQ <- as.data.frame(CL_FREQ)
usethis::use_data(CL_FREQ, overwrite = TRUE)
