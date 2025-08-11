### script loads data



path_tst <- getwd() # want to end up here: "C:/Users/THILL03/OneDrive - Environmental Protection Agency (EPA)/R8 All LSASD - Region 8 Lab - DWLabCert/Work_Instruction/test-data/"
# find which element has 'OneDrive - Environmental Protection Agency (EPA)'
# which('OneDrive - Environmental Protection Agency' %in% strsplit(x = path_tst, split = '/')[[1]])
# sapply(X =  as.list(strsplit(x = path_tst, split = '/')[[1]]), FUN = function(x) {gregexpr(text = x, pattern = 'OneDrive - Environmental Protection Agency')})
### the DW LabCert sharepoint folder should be added to the user's OneDrive
### and accessible via file browser.
### loading an R package makes getwd return something in AppData instead of 
### the file directory chain observed during testing.
path_tst <- gsub(x = path_tst, pattern = 'AppData', replacement = 'OneDrive - Environmental Protection Agency (EPA)/')

path_components <- c(as.list(strsplit(x = path_tst, split = '/')[[1]][1:4]),
                     'R8 All LSASD - Region 8 Lab - DWLabCert',
                     'Work_Instruction',
                     'test-data')
path_begin <- do.call(file.path, as.list(path_components))
file_lst   <- list.files(path_begin, recursive = TRUE, full.names = TRUE)

# locations_path <- file.path(getwd(), 'inst', 'extdata', 'data_lab_list_20250709.csv')
locations_path <- sort(grep(x = file_lst, pattern = 'data_lab_list', value = TRUE), decreasing = TRUE)[1]
locations      <- read.csv(locations_path)
locations      <- locations[grepl(x = locations$Region, pattern = '8'), ]

# method_data_path <- file.path(getwd(), 'inst', 'extdata', 'data_NPDWS_methods_20250709.csv')
method_data_path <- sort(grep(x = file_lst, pattern = 'data_NPDWS_methods_', value = TRUE), decreasing = TRUE)[1]
method_data      <- read.csv(method_data_path)



set.seed(42)
lab_data_rows <- expand.grid(paste0(locations$laboratory_name, '_', locations$laboratory_location), method = method_data$common_name)
lab_data_tmp <- data.frame(laboratory_name = sapply(X = strsplit(x = as.character(lab_data_rows$Var1), split = '_'), FUN = '[[', 1),
                       laboratory_location = sapply(X = strsplit(x = as.character(lab_data_rows$Var1), split = '_'), FUN = '[[', 2),
                       method = lab_data_rows$method,
                       PT_result    = sample(c('Pass', 'Fail'), size = nrow(lab_data_rows), replace = TRUE, prob = c(0.9, 0.1)),
                       PT_test_date = Sys.Date() - sample(0:600, nrow(lab_data_rows), replace = TRUE))
### reduce PT dataset to only those methods applicable to a lab.
key1 <- locations$EPA_ID[match(lab_data_rows$Var1, paste0(locations$laboratory_name, '_', locations$laboratory_location))]
for (i in 1:nrow(lab_data_tmp)) {
  lab1    <- key1[i]
  method1 <- lab_data_tmp$method[i]
  methods_to_keep <- method_data$common_name[which(!is.na(method_data[, lab1]) & (nchar(method_data[, lab1]) > 0))]
  if (lab_data_tmp$method[i] %in% methods_to_keep) {
    if(!exists('lab_data')) {
      lab_data <- lab_data_tmp[i, ]
    } else {
      lab_data <- rbind(lab_data, lab_data_tmp[i, ])
    }
  }
}
lab_data$category <- method_data$category[  match(lab_data$method, method_data$common_name)]

# write.csv(lab_data, file = file.path(getwd(), 'inst', 'extdata', 'data_proficiency_test_20250809.csv'), row.names = FALSE) # using seed 142

# lab_data_path   <- file.path(getwd(), 'inst', 'extdata', 'data_proficiency_tests_20250709.csv')
# lab_data        <- read.csv(lab_data_path)
# lab_data$PT_test_date <- as.Date(lab_data$PT_test_date, format = "%m/%d/%Y")
