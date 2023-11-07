server.optimizelyR <- function(input, output, session) {

  ## don't render anything initially
  output$results <- DT::renderDT(NULL)
  output$select_metrics <- renderUI(NULL)
  output$select_variants <- renderUI(NULL)

  ## get results from file

  clean_results <- reactive({
    file <- input$optimizelyResults
    req(file)
    clean_optimizely_results(source_dir = file$datapath, filename = file$name)
  })

  ## render ui outputs in sidebar

  # metrics
  output$select_metrics <- renderUI({

    req(clean_results())

    avl_metrics <- clean_results() %>%
      dplyr::select(metric, metric_numerator_type) %>%
      tidyr::unite("metric_id", metric, metric_numerator_type, sep = "-") %>%
      dplyr::distinct(metric_id) %>%
      dplyr::pull(metric_id)

    shinyWidgets::pickerInput(inputId = "metricSelect",
                              label = "Select Metrics",
                              choices = avl_metrics,
                              selected = avl_metrics,
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE)
    })

  # variants
  output$select_variants <- renderUI({

    req(clean_results())

    avl_variants <- clean_results() %>%
      dplyr::distinct(variation_name) %>%
      dplyr::pull(variation_name)

    shinyWidgets::pickerInput(inputId = "variantSelect",
                              label = "Select Variants",
                              choices = avl_variants,
                              selected = avl_variants,
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE)
  })

  ## prepare results for publishing

  pub_results <- reactive({

    req(clean_results())

    get_optimizely_results(clean_optimizely_tbl = clean_results(),
                           reqd_metrics = input$metricSelect,
                           reqd_variants = input$variantSelect)
  })

  ## render results
  output$results <- DT::renderDT({

    req(clean_results(), pub_results())

    exp_name <- clean_results() %>%
      dplyr::mutate(experiment_name = stringr::str_replace_all(experiment_name, '-', ' '),
             experiment_name = stringr::str_to_title(experiment_name)
      ) %>%
      dplyr::distinct(experiment_name) %>%
      dplyr::pull(experiment_name)

    dt_str <- clean_results() %>%
      tidyr::unite('exp_date', start, end, sep = " - ") %>%
      dplyr::distinct(exp_date) %>%
      dplyr::pull(exp_date)

    dt_con <- htmltools::withTags(table(
      DT::tableHeader(pub_results()),
      DT::tableFooter(paste0("Experiment Runtime: ", dt_str))
    ))

      DT::datatable(pub_results(),
                    container = dt_con,
                    extensions = c('RowReorder', 'Buttons', 'FixedHeader'),
                    escape = FALSE,
                    rownames = FALSE,
                    callback = DT::JS("// pass on data to R
                                      table.on('row-reorder', function(e, details, changes) {
                                      Shiny.onInputChange('table_row_reorder', JSON.stringify(details));
                                      });"
                    ),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: left; color:black;  font-size:200%;',
                      exp_name
                    ),
                    options = list(dom = 'Brtip',
                                   fixedHeader = TRUE,
                                   rowReorder = TRUE,
                                   buttons =
                                     list(
                                          list(extend = 'collection',
                                               buttons = list(list(extend = 'copy'),
                                                              list(extend = 'excel',
                                                                   filename = paste0(exp_name, "_", format(Sys.Date(), "%Y%m%d")),
                                                                   title = ""),
                                                              list(extend = 'pdf',
                                                                   filename = paste0(exp_name, "_", format(Sys.Date(), "%Y%m%d")))),
                                               text = 'Export'
                                          )),
                                   scrollX = TRUE
                                   )
      )
    })

  # store the order of the table as a reactive value that we
  # can set and also listen for changes
  table_order <- reactiveVal(value=seq(1,10))

  # observe row reordering event - sent from javascript function
  observeEvent(input$table_row_reorder, {
    info <- input$table_row_reorder
    # error checking
    if(is.null(info) | class(info) != 'character') { return() }

    # I'm using the "yaml" package to read JSON, but you can use "jsonlite"
    # if you want. just be aware that jsonlite also uses the "validate"
    # function which conflicts with the same "validate" function in shiny.
    info <- yaml::read_yaml(text=info)
    # info will be empty if a reorder event fired but no row orders changed
    if(length(info) == 0) { return() }

    # load our order vectors
    .order <- table_order()
    .new_order <- .order

    # for each updated row in the info object, update the order vector
    for(i in 1:length(info)) {
      j <- info[[i]]
      .new_order[(j$newPosition + 1)] <- .order[(j$oldPosition + 1)]
    }

    # update our order vector's reactive value
    table_order(.new_order)
  })

}

