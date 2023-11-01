# This script should be run on a nightly basis
# It identifies all errors first via regex patterns and then via ML techniques.
rm(list=ls())
source("credentials.R")

pkgs <- c("data.table", 
          "dplyr",
          "stringi", 
          "httr", 
          "tools", 
          "magrittr", 
          "lubridate", 
          "jsonlite",
          "crul",
          "parallel")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))  {
    cat("######################################################################\n")
    cat("Thees are the missing packages: ", paste0(new.pkg, collapse=", "), "\n")
    cat("######################################################################\n")
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.us.r-project.org')
  }
  sapply(pkg, require, character.only = TRUE)
}

ipak(pkgs)

options(scipen = 999)


#### 1: Pull all execution ids for the day via resource usage with the status "Failed" or "Error"
resource_usage_url <- paste0("https://", domino_url, "/admin/generateUsageReport")
date_range <- c(Sys.Date()-lubridate::days(30), Sys.Date())

payload <- paste0("start-date=", format(date_range[1], '%m/%d/%Y'), "&end-date=", format(date_range[2], '%m/%d/%Y'))
#payload <- "start-date=09/15/2023&end-date=09/22/2023"

headers <- c(
  'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
  'Content-Type'= 'application/x-www-form-urlencoded',
  'X-Domino-Api-Key'= domino_user_api_key
)
#cat("Is the error here?\n")
#cat(resource_usage_url, "\n")
response <- httr::POST(url = resource_usage_url, body=payload, add_headers(headers))
#cat("Or post getting the response?\n")
plain_text <- base::rawToChar(response$content)
report_df <- read.csv(textConnection(plain_text))

error_ids <- report_df$Run.id[which(report_df$Status %in% c("Failed", "Error"))]


#### 2: Attempt to Download All Support Bundles ####
##### a. Pull the support bundles for all that are already downloaded #####
existing_directories <- list.files(paste0(data_directory, "support-bundles"), recursive=FALSE)
existing_ids <- existing_directories[!grepl("\\.zip", existing_directories)]

##### b. Unzip those that aren't yet downloaded #####
download_zip <- setdiff(error_ids, existing_ids)
#dest_dir <<- paste0(data_directory, 'support-bundles')

urls <- paste0("https://", domino_url, "/v4/admin/supportbundle/", error_ids)
headers <- c("X-Domino-Api-Key" = domino_user_api_key,
             "accept" = "application/json")


# Maximum number of connections is 128-3 (stdin/out/console) with 5 for wiggle room
x <- 120
indices <- (seq_along(urls) - 1) %/% x + 1
# Split the URLs based on the group numbers
url_list <- split(urls, indices)

# Loop through each batch of urls, using crul to ansychronously make api calls
for (idx in seq_along(url_list)) {
  target_url <- url_list[[idx]]
  cat("Progress: ", base::round(idx/length(url_list)*100, 0), "%\n")
  cc <- crul::Async$new(
    urls = target_url,
    headers = headers
  )
  
  execution_ids <- sapply(target_url, function(singles) stringi::stri_extract(singles, regex="(?<=supportbundle\\/).*")) %>% as.vector()
  
  support_bundle_dir <- paste0(data_directory, 'support-bundles/')
  if(!dir.exists(support_bundle_dir)) {
    dir.create(support_bundle_dir)
  }
  
  
  zip_paths <- paste0(support_bundle_dir, execution_ids,".zip")
  res <- cc$get(disk=zip_paths)
}

all_file_paths <- paste0(data_directory, 'support-bundles/', error_ids, '.zip')
all_file_paths <- all_file_paths[file.exists(all_file_paths)]



unzip_single <- function(target_file) {
  base_name <- tools::file_path_sans_ext(basename(target_file))
  final_dest_dir <- file.path(dest_dir, base_name)
  
  # Ensure the directory exists
  if (!dir.exists(final_dest_dir)) {
    dir.create(final_dest_dir, recursive = TRUE)
    unzip(target_file, exdir = final_dest_dir)
  }
}

# Get the number of cores available
no_cores <- parallel::detectCores() - 1  # using one less to leave a core free

# Use parLapply to unzip in parallel
cl <- parallel::makeCluster(no_cores)
dest_dir <<- paste0(data_directory, 'support-bundles')

parallel::clusterExport(cl, "dest_dir")

# parallel::clusterEvalQ(cl, {
#   library(magrittr)
#   library(httr)
#   library(jsonlite)
# })
#browser()
parallel::parLapply(cl, all_file_paths, unzip_single)
parallel::stopCluster(cl)

#### 3: Run ML Analysis on everything and dump the summaries into an "ml" data summary bucket ####
# found_errors <- lapply(output_df$File_Path, function(target) {
#   target <- stringi::stri_split(target, regex="/") %>% unlist()
#   out <- target[length(target)-1]
#   return(out)
# })

# ml_errors <- setdiff(download_list, found_errors)
# ml_errors <- paste0(ml_errors, collapse="|")
ml_support_bundles <- stringi::stri_extract(all_file_paths, regex=".*(?=\\.zip)")

# Now, identify any non-error "Error/Failed" files and push them through the model api
# Machine learning API
all_files <- list.files(ml_support_bundles, full.names=TRUE)
all_files <- all_files[grep("executor|logjam|run|events|execution", all_files)]
a <- Sys.time()
model_file_errors <- lapply(all_files, function(target_file_name) {
  cat("TARGET FILE: ", target_file_name, "\n")
  target_file <- readLines(target_file_name)
  url <- model_rest_url # located in credentials.R (same with model_api_key)
  response <- httr::POST(
    url,
    authenticate(model_api_key, model_api_key, type = "basic"),
    body=toJSON(list(data=list(text = target_file)), auto_unbox = TRUE),
    content_type("application/json")
  )
  
  predictions <- content(response)
  predictions <- unname(unlist(predictions$result))
  
  num_errors <- length(predictions[which(predictions != "none")])
  # Identify any potential errors which are not none. If they exist, turn it into a dataframe. If not, just cbind that stuff
  if(num_errors > 0) {
    error_lines <- which(predictions != "none")
    error_type <- predictions[error_lines]
    errors <- target_file[error_lines]
    error_description <- target_file[error_lines]
    data <- data.frame('Error'=errors, 'Line_Number'=error_lines, 'Context'=error_description, 'Error_Type'=error_type)
    data$File_Path <- target_file_name
    datetime_str <- stringi::stri_extract(data$Context, regex="\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+Z")
    data$Date_Time <- lubridate::ymd_hms(datetime_str, tz = "UTC")
    data$execution_id <- sapply(data$File_Path, function(file_path) {
      file_path <- stringi::stri_split(file_path, regex="/") %>% unlist()
      execution_id <- file_path[length(file_path)-1]
    })
    
    associated_node <- target_file[grep("assignedNodeName", target_file)]
    associated_node <- stringi::stri_extract(associated_node, regex="ip-[0-9]+-[0-9]+-[0-9]+-[0-9]+\\..*\\.compute.internal")
    if(length(associated_node) > 0) {
      data$Node <- associated_node
    } else {
      data$Node <- NA
    }
    
    out <- data
  } else {
    out <- data.frame(
      Error = character(0),
      Line_Number = integer(0),
      Context = character(0),
      Error_Type = character(0),
      File_Path = character(0),
      Node = character(0)
    )
  }
}) %>% do.call(rbind, .)
b <- Sys.time()
b-a

ml_summary_path <- paste0(data_directory, "support-bundle-summary-ml")

if(!dir.exists(ml_summary_path)) {
  dir.create(ml_summary_path)
}

execution_ids_to_save <- unique(model_file_errors$execution_id) %>% unlist()
lapply(execution_ids_to_save, function(target_id) {
  file_path <- paste0(ml_summary_path, "/", target_id, '-summary.csv')
  target_errors <- model_file_errors[which(model_file_errors$execution_id == target_id),]
  write.csv(target_errors, file_path)
})


missing_errors <- setdiff(error_ids, execution_ids_to_save)
base::saveRDS(missing_errors, "/mnt/data/allstate_log_github/missing_execution_ids_to_investigate_10_31_2023.rds")
