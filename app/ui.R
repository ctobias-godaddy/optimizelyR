
# define sidebar
sidebar <- shinydashboard::dashboardSidebar(

  shinyjs::useShinyjs(),
  width = 450,

  shinydashboard::sidebarMenu(

    style = "position: fixed; width: 450px; overflow: auto; height: calc(100vh - 50px) !important;",

    fileInput(inputId = 'optimizelyResults',
              label = "Import Optimizely Results csv",
              multiple = FALSE,
              accept = ".csv",
              width = NULL,
              buttonLabel = "Browse...",
              placeholder = "Import Results csv",
              capture = NULL),

    hr(),

    uiOutput("select_metrics"),

    br(),

    uiOutput("select_variants")
    )
)

# define body
body <- shinydashboard::dashboardBody(

  DT::DTOutput(outputId = "results") %>% withSpinner()

)

# Put them together into a dashboardPage
ui.optimizelyR <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "Optimizely Results Publisher",
    titleWidth = 300),
  sidebar,
  body
)
