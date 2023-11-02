domino_project_name <- "support_bundle_fork_test"
domino_url <- "prod-field.cs.domino.tech"
data_directory <- paste0("/mnt/data/", domino_project_name, "/")

# Optional! This is for the log error classifier if enabled. These are found via the API call for a deployed model
# model_api_key <- <model-api-key>
# model_rest_url <- <model-rest-url>

domino_user_api_key <- system("echo $DOMINO_USER_API_KEY", intern=TRUE)
