my_token <- "2:GVZZWbVNGDB8fbk_ykQqvo4eyptBbiIIvLovqZ5eb4XIiRNyRWmc"



proj_id <- "14410340138"

exp_url <- "https://api.optimizely.com/v2/experiments"

content_df <- data.frame(
  "allocation_policy" = as.character(),                           "audience_conditions" = as.character(),                         "campaign_id" = as.numeric(),
  "changes" = NULL,                                     "created" = NULL,                                     "description" = NULL,
  "earliest" = NULL,                                    "holdback" = NULL,                                    "id" = NULL,
  "is_classic" = NULL,                                  "key" = NULL,                                         "last_modified" = NULL,
  "latest" = NULL,                                      "metrics" = NULL,                                     "name" = NULL,
  "page_ids" = NULL,                                    "project_id" = NULL,                                  "status" = NULL,
  "traffic_allocation" = NULL,                          "type" = NULL,                                        "variations" = NULL,
  "feature_id" = NULL,                                  "feature_key" = NULL,                                 "feature_name" = NULL,
  "environments.production.environment_id" = NULL,      "environments.production.environment_name" = NULL,    "environments.production.status" = NULL,
  "environments.production.percentage_included" = NULL, "environments.staging.environment_id" = NULL,         "environments.staging.environment_name" = NULL,
  "environments.staging.status" = NULL,                 "environments.staging.percentage_included" = NULL
)
max_pg_num <- 10

for (pg_num in 1:max_pg_num) {

  queryString <- list(
    per_page = "100",
    page = pg_num,
    project_id = proj_id,
    include_classic = "false"
  )

  response <- httr::VERB("GET",
                         url = exp_url,
                         query = queryString,
                         httr::add_headers("Authorization" = paste0("Bearer ", my_token)),
                         httr::content_type("application/octet-stream"),
                         httr::accept("application/json"))

  content <- httr::content(response, as = "text")

  if(content == "[]")
  content_df <- content_df %>%
    dplyr::bind_rows(jsonlite::fromJSON(content, flatten = TRUE))
}



# results
url_results_ts <- "https://api.optimizely.com/v2/export/experiments/24068690610/results/csv"

response_ts <- httr::VERB("GET", url_results_ts,
                 httr::add_headers("Authorization" = paste0("Bearer ", my_token)),
                 httr::content_type("application/octet-stream"),
                 httr::accept("application/json"))

content_ts <- httr::content(response_ts, "text")

readr::read_csv(response_ts$content)
