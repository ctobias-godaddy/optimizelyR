#' clean the raw optimizely results
#'
#' @param source_dir string indicating the path to the raw csv extract
#'
#' @return tbl/df
#'
clean_optimizely_results <- function(source_dir, filename){

  reqd_cols <- c('Start Time (UTC)', 'End Time (UTC)',
                 'Variation Name', 'Metric Event Name', 'Metric Numerator Type', 'Metric Denominator Type', 'Numerator Value',	'Denominator Value',
                 'Metric Value', 'Improvement Value from Baseline',	'Statistical Significance', 'Confidence Interval - Low',	'Confidence Interval - High')

  raw_optimizely <- readr::read_csv(source_dir)

  exp_name <- stringr::str_remove_all(filename, 'experiment-results-')
  exp_name <- stringr::str_remove_all(exp_name, '.csv')
  exp_name <- stringr::str_remove(exp_name, "-[^-]+$")
  exp_name <- stringr::str_remove(exp_name, "-[^-]+$")

  clean_optimizely <- raw_optimizely %>%
    dplyr::select(dplyr::all_of(reqd_cols)) %>%
    dplyr::rename(start = 1,
                  end = 2,
                  variation_name = 3,
                  metric = 4,
                  metric_numerator_type = 5,
                  metric_denominator_type = 6,
                  numerator_value = 7,
                  denominator_value = 8,
                  value = 9,
                  change_vs_baseline = 10,
                  statsig = 11,
                  ci_low = 12,
                  ci_high = 13) %>%
    dplyr::mutate(variation_name = stringr::str_remove_all(variation_name, '"'),
                  metric = stringr::str_remove_all(metric, '"'),
                  metric = stringr::str_remove_all(metric, '"')) %>%
    dplyr::mutate(change_vs_baseline = as.numeric(change_vs_baseline) * 100,
                  statsig = as.numeric(statsig) * 100,
                  ci_low = as.numeric(ci_low) * 100,
                  ci_high = as.numeric(ci_high) * 100
    ) %>%
    dplyr::mutate(experiment_name = exp_name)

  clean_optimizely
}

get_optimizely_results <- function(clean_optimizely_tbl, reqd_metrics, reqd_variants) {

  results_tbl <- clean_optimizely_tbl %>%
    dplyr::filter(variation_name %in% dplyr::all_of(reqd_variants)) %>%
    tidyr::unite("metric_id", metric, metric_numerator_type, sep = "-", remove = FALSE) %>%
    dplyr::filter(metric_id %in% dplyr::all_of(reqd_metrics)) %>%
    dplyr::mutate(numerator_value = format(numerator_value, big.mark = ',')) %>%
    dplyr::mutate(value = ifelse(metric_numerator_type == 'unique conversions', paste0(round(value * 100, 2), "%"), round(value, 2))) %>%
    dplyr::mutate(statsig = ifelse(statsig == 100, ">99%", paste0(statsig, "%"))) %>%
    dplyr::mutate(metric_id = stringr::str_replace_all(metric_id, "-total conversions", " (Avg)"),
                  metric_id = stringr::str_replace_all(metric_id, "-unique conversions", " (%)")
    ) %>%
    dplyr::mutate(conf_int = paste0("[", round(ci_low, 2),"%;", round(ci_high, 2),"%]"),
                  change_vs_baseline = paste0(round(change_vs_baseline, 2), "%")
    ) %>%
    dplyr::select(Variation = variation_name,
                  Metric = metric_id,
                  Conversions = numerator_value,
                  'Conversion Rate' = value,
                  Improvement = change_vs_baseline,
                  'Stat-sig' = statsig,
                  'Confidence Interval' = conf_int)

  results_tbl
}
