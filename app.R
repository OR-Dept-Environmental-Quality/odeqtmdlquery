library(shiny)
library(shinydashboard)
library(dplyr)
library(openxlsx)
library(htmltools)
library(reactable)

options(dplyr.summarise.inform = FALSE)

# odeqtmdl package version that app tables are based on.
odeqtmdl_version <- "0.8.0"

# Load data --------------------------------------------------------------------

load(file = file.path("data", "tmdl_actions_app.rda"))
load(file = file.path("data", "tmdl_targets_app.rda"))
load(file = file.path("data", "tmdl_parameters_app.rda"))
load(file = file.path("data", "tmdl_au_app.rda"))
load(file = file.path("data", "tmdl_au_gnis_app.rda"))
tmdl_reaches_app <- readRDS(file = file.path("data", "tmdl_reaches_app.RDS"))

tmdl_names <- c(sort(unique(tmdl_au_app$TMDL_name)))
tmdl_statuses <- c("Active", "Not Active", "In Development")
tmdl_scopes <- c("TMDL", "Allocation only", "Advisory allocation")
tmdl_huc6 <- c(sort(unique(tmdl_reaches_app$HUC6_full)))
tmdl_huc8 <- c(sort(unique(tmdl_reaches_app$HUC8_full)))
tmdl_parameters <- c(sort(unique(tmdl_reaches_app$TMDL_wq_limited_parameter)))
tmdl_pollutants <- c(sort(unique(tmdl_reaches_app$TMDL_pollutant)))
tmdl_au_ids <- c(sort(unique(tmdl_au_app$AU_ID)))
tmdl_au_gnis_names <- c(sort(unique(tmdl_reaches_app$AU_GNIS_Name)))

# Help Text : coming soon
# str1 <- "<b>Welcome!</b>"
# str2 <- "<p> ...</p>"
# str3 <- "<p> ...</p>"
# str4 <- "<p> ...</p>"
# str5 <- "<b>How to Use</b>"
# str6 <- "<p>....</p>"

# txt_help <- htmltools::HTML(paste(str1,str2,str3,str4,str5,str6, sep = '<tb/>'))

txt_actions <- "The following TMDLs match your query."

txt_targets <- "The following pollutant targets are included in the TMDLs matching your query. Not all TMDL targets or requirments are listed. See the TMDL document for more information"

txt_i_status <- paste0("TMDL status for an individual parameter or pollutant.","\n\n",
                       "Active: TMDL is complete, approved by EPA, and active.","\n\n",
                       "Not Active: TMDL has been withdrawn, disapproved by EPA, or replaced with a newer TMDL.","\n\n",
                       "In Development: TMDL is being developed.")

txt_i_scope <- paste0("Provides information about how the TMDL applies.","\n\n",
                      "TMDL: Identifies reaches where the TMDL was developed to address a category 5 303(d) listing or future 303(d) listing.","\n\n",
                      "Allocation only: Identifies reaches where a TMDL allocation applies but the TMDL does not address a category 5 303(d) listing or future listing in that reach. Typically this situation is applicable for tributaries or canals that are upstream of the reach where the TMDL applies. The pollutant reduction in the upstream reach is needed to achieve the TMDL loading capacity of the downstream reach.","\n\n",

                      "Advisory allocation: Identifies reaches where a TMDL allocation may apply based on assessment of source loads and if pollutant reduction is needed to achieve a TMDL allocation or loading capacity downstream. See TMDL document for details and requirements. The TMDL does not address a 303(d) listing or future listing in this reach.")


# Shiny UI ---------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Oregon TMDL Query Tool",
                                  tags$li(
                                    shiny::a(
                                      shiny::img(

                                        src = 'DEQ-logo-horizontal-white370x74.png',
                                        height = "50px"
                                      ),
                                      href = 'https://www.oregon.gov/deq/Pages/index.aspx',
                                      target = '_blank',
                                      style = "padding-top:10px;"
                                    ),
                                    class = "dropdown"
                                  )),
  shinydashboard::dashboardSidebar(disable = TRUE),

  # Body
  shinydashboard::dashboardBody(
    shiny::includeCSS("www/DEQ_web_style.css"),
    # shiny::fluidRow(align = 'left',
    #          (shinydashboard::box(width = 12,
    #               shiny::htmlOutput("help"),
    #               tags$style(
    #                 "b{font-size: 20px; font-family: Arial;}.col-sm-12{padding-top:15px;}"
    #               )))),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(width = 5,
                    shiny::selectizeInput(inputId = "select_tmdl_names",
                                          label = tags$span("TMDL Name",
                                                            tags$p(
                                                              class = "glyphicon glyphicon-info-sign",
                                                              style = "color:#0072B2;",
                                                              title = "Name of the TMDL document")),
                                          choices = tmdl_names,
                                          selected = character(0),
                                          multiple = TRUE,
                                          width = "100%",
                                          options = list(plugins = list("remove_button")))),
      shiny::column(width = 2,
                    shiny::selectizeInput(inputId = "select_tmdl_status",
                                          label = tags$span("TMDL status",
                                                            tags$p(
                                                              class = "glyphicon glyphicon-info-sign",
                                                              style = "color:#0072B2;",
                                                              title = txt_i_status)),
                                          choices = tmdl_statuses,
                                          selected = "Active",
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button")),
                                          width = "100%")),
      shiny::column(width = 2,
                    shiny::selectizeInput(inputId = "select_tmdl_scope",
                                          label = tags$span("TMDL scope",
                                                            tags$p(
                                                              class = "glyphicon glyphicon-info-sign",
                                                              style = "color:#0072B2;",
                                                              title = txt_i_scope)),
                                          choices = tmdl_scopes,
                                          selected = character(0),
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button")),
                                          width = "100%"))
    ),
    shiny::fluidRow(
      shiny::column(width = 3,
                    shiny::selectizeInput(inputId = "select_wql_param",
                                          label = tags$span("303(d) parameter addressed",
                                                            tags$p(
                                                              class = "glyphicon glyphicon-info-sign",
                                                              style = "color:#0072B2;",
                                                              title = "Water quality limited 303(d) parameter that the TMDL addresses")),
                                          choices = tmdl_parameters,
                                          selected = character(0),
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button")),
                                          width = "100%")),
      shiny::column(width = 3
                    ,
                    shiny::selectizeInput(inputId = "select_tmdl_polluntant",
                                          label = tags$span("TMDL pollutant",
                                                            tags$p(
                                                              class = "glyphicon glyphicon-info-sign",
                                                              style = "color:#0072B2;",
                                                              title = "Pollutant causing the water quality impairment.")),
                                          choices = tmdl_pollutants,
                                          selected = character(0),
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button")),
                                          width = "100%"))),
    shiny::fluidRow(
      shiny::column(width = 3,
                    shiny::selectizeInput(inputId = "select_huc6",
                                          label = tags$span("Basin",
                                                            tags$p(
                                                              class = "glyphicon glyphicon-info-sign",
                                                              style = "color:#0072B2;",
                                                              title = "Six digit USGS hydrological unit code")),
                                          choices = tmdl_huc6,
                                          selected = character(0),
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button")),
                                          width = "100%")),
      shiny::column(width = 3,
                    shiny::selectizeInput(inputId = "select_huc8",
                                          label = tags$span("Subbasin",
                                                            tags$p(
                                                              class = "glyphicon glyphicon-info-sign",
                                                              style = "color:#0072B2;",
                                                              title = "Eight digit USGS hydrological unit code")),
                                          choices = tmdl_huc8,
                                          selected = character(0),
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button")),
                                          width = "100%")),
      shiny::column(width = 3,
                    shiny::selectizeInput(inputId = "select_au",
                                          label = tags$span("DEQ Assessment Unit",
                                                            tags$p(
                                                              class = "glyphicon glyphicon-info-sign",
                                                              style = "color:#0072B2;",
                                                              title = "DEQ Assessment Unit ID")),
                                          choices = NULL,
                                          selected = character(0),
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button"),
                                                         maxOptions = 7000),
                                          width = "100%")),
      shiny::column(width = 3,
                    shiny::selectizeInput(inputId = "select_au_gnis_name",
                                          label = tags$span("Assessment Unit GNIS Name",
                                                            tags$p(
                                                              class = "glyphicon glyphicon-info-sign",
                                                              style = "color:#0072B2;",
                                                              title = "DEQ Assessment Unit stream GNIS name")),
                                          choices = NULL,
                                          selected = character(0),
                                          multiple = TRUE,
                                          options = list(plugins = list("remove_button"),
                                                         maxOptions = 15000),
                                          width = "100%"))),
    shiny::fluidRow(
      shiny::column(width = 2,
                    shiny::actionButton("filter_button", "Select",  icon("filter"))),
      shiny::column(width = 2,
                    shiny::actionButton("reset_button", "Reset all")),
      shiny::column(width = 2,
                    shiny::uiOutput(outputId = "download_button"))),
    shiny::fluidRow(style = "padding-top:20px"),
    shiny::tabsetPanel(
      shiny::tabPanel(title = "TMDL Actions",
                      value = "tmdl_actions_tab",
                      br(),
                      shiny::textOutput(outputId = "text_actions"),
                      reactable::reactableOutput(outputId = "tmdl_actions_result",
                                                 width = "100%")),
      shiny::tabPanel(title = "TMDL Pollutant Targets",
                      value = "tmdl_targets_tab",
                      br(),
                      shiny::textOutput(outputId = "text_targets"),
                      reactable::reactableOutput(outputId = "tmdl_target_result",
                                                 width = "100%")),
      shiny::tabPanel(title = "Assessment Units",
                      value = "tmdl_au_tab",
                      br(),
                      reactable::reactableOutput(outputId = "tmdl_au_result",
                                                 width = "100%")),
      shiny::tabPanel(title = "GNIS Assessment Units",
                      value = "tmdl_au_gnis_tab",
                      br(),
                      reactable::reactableOutput(outputId = "tmdl_au_gnis_result",
                                                 width = "100%"))
    )
  )
)

# Shiny Server -----------------------------------------------------------------

server <- function(input, output, session) {


  shiny::updateSelectizeInput(inputId = "select_au", choices = tmdl_au_ids, selected = character(0), server = TRUE)
  shiny::updateSelectizeInput(inputId = "select_au_gnis_name", choices = tmdl_au_gnis_names, selected = character(0), server = TRUE)

  # Help Text
  # output$help <- shiny::renderUI({txt_help})

  #- Reset Button ----
  shiny::observeEvent(input$reset_button, {

    shiny::updateSelectInput(inputId = "select_tmdl_names", selected = character(0))
    shiny::updateSelectInput(inputId = "select_tmdl_status", selected = "Active")
    shiny::updateSelectInput(inputId = "select_tmdl_scope", selected = character(0))
    shiny::updateSelectInput(inputId = "select_wql_param", selected = character(0))
    shiny::updateSelectInput(inputId = "select_tmdl_polluntant", selected = character(0))
    shiny::updateSelectInput(inputId = "select_huc6", selected = character(0))
    shiny::updateSelectInput(inputId = "select_huc8", selected = character(0))

    output$text_actions <- shiny::renderText({character(0)})
    output$text_targets <- shiny::renderText({character(0)})

    session$reload()

  })

  #- Filter Button ----
  shiny::observeEvent(input$filter_button, {

    # Filter tmdl_reaches based on inputs = fr
    {
      fr <- tmdl_reaches_app

      if (!is.null(input$select_tmdl_status)) {

        fr <- fr %>%
          dplyr::filter(TMDL_status %in% input$select_tmdl_status)
      }

      if (!is.null(input$select_tmdl_names)) {
        fr <- fr %>%
          dplyr::filter(TMDL_name %in% input$select_tmdl_names)
      }

      if (!is.null(input$select_tmdl_scope)) {
        fr <- fr %>%
          dplyr::filter(TMDL_scope %in% input$select_tmdl_scope)
      }

      if (!is.null(input$select_wql_param)) {
        fr <- fr %>%
          dplyr::filter(TMDL_wq_limited_parameter %in% input$select_wql_param)
      }

      if (!is.null(input$select_tmdl_polluntant )) {
        fr <- fr %>%
          dplyr::filter(TMDL_pollutant %in% input$select_tmdl_polluntant)
      }

      if (!is.null(input$select_au_gnis_name)) {
        fr <- fr %>%
          dplyr::filter(AU_GNIS_Name %in% input$select_au_gnis_name)
      }

      if (!is.null(input$select_au)) {
        fr <- fr %>%
          dplyr::filter(AU_ID %in% input$select_au)
      }

      if (!is.null(input$select_huc6)) {
        fr <- fr %>%
          dplyr::filter(HUC6_full %in% input$select_huc6)
      }

      if (!is.null(input$select_huc8)) {
        fr <- fr %>%
          dplyr::filter(HUC8_full %in% input$select_huc8)
      }

      if (!is.null(input$select_au_gnis_name)) {
        f_gnis_au_ids <- fr %>%
          dplyr::pull(AU_ID) %>%
          unique()
      }

      fr <- fr %>%
        dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")"))

    }

    # Filter tmdl_aus based on inputs = fau
    {
      fau <- tmdl_au_app

      if (!is.null(input$select_tmdl_status)) {
        fau <- fau %>%
          dplyr::filter(TMDL_status %in% input$select_tmdl_status)
      }

      if (!is.null(input$select_tmdl_names)) {
        fau <- fau %>%
          dplyr::filter(TMDL_name %in% input$select_tmdl_names)
      }

      if (!is.null(input$select_tmdl_scope)) {
        fau <- fau %>%
          dplyr::filter(TMDL_scope %in% input$select_tmdl_scope)
      }

      if (!is.null(input$select_wql_param)) {
        fau <- fau %>%
          dplyr::filter(TMDL_wq_limited_parameter %in% input$select_wql_param)
      }

      if (!is.null(input$select_tmdl_polluntant)) {
        fau <- fau %>%
          dplyr::filter(TMDL_pollutant %in% input$select_tmdl_polluntant)
      }

      if (!is.null(input$select_au)) {
        fau <- fau %>%
          dplyr::filter(AU_ID %in% input$select_au)
      }

      if (!is.null(input$select_au_gnis_name)) {
        fau <- fau %>%
          dplyr::filter(AU_ID %in% f_gnis_au_ids)
      }

      if (!is.null(input$select_huc6)) {
        fau <- fau %>%
          dplyr::filter(HUC6_full %in% input$select_huc6)
      }

      if (!is.null(input$select_huc8)) {
        fau <- fau %>%
          dplyr::filter(HUC8_full %in% input$select_huc8)
      }

      fau <- fau %>%
        dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")"))
    }

    # Filter tmdl_au_gnis based on inputs = fau_gnis
    {
      fau_gnis <- tmdl_au_gnis_app

      if (!is.null(input$select_tmdl_status)) {
        fau_gnis <- fau_gnis %>%
          dplyr::filter(TMDL_status %in% input$select_tmdl_status)
      }

      if (!is.null(input$select_tmdl_names)) {
        fau_gnis <- fau_gnis %>%
          dplyr::filter(TMDL_name %in% input$select_tmdl_names)
      }

      if (!is.null(input$select_tmdl_scope)) {
        fau_gnis <- fau_gnis %>%
          dplyr::filter(TMDL_scope %in% input$select_tmdl_scope)
      }

      if (!is.null(input$select_wql_param)) {
        fau_gnis <- fau_gnis %>%
          dplyr::filter(TMDL_wq_limited_parameter %in% input$select_wql_param)
      }

      if (!is.null(input$select_tmdl_polluntant)) {
        fau_gnis <- fau_gnis %>%
          dplyr::filter(TMDL_pollutant %in% input$select_tmdl_polluntant)
      }

      if (!is.null(input$select_au)) {
        fau_gnis <- fau_gnis %>%
          dplyr::filter(AU_ID %in% input$select_au)
      }

      if (!is.null(input$select_au_gnis_name)) {
        fau_gnis <- fau_gnis %>%
          dplyr::filter(AU_GNIS_Name %in% input$select_au_gnis_name)
      }

      if (!is.null(input$select_huc6)) {
        fau_gnis <- fau_gnis %>%
          dplyr::filter(HUC6_full %in% input$select_huc6)
      }

      if (!is.null(input$select_huc8)) {
        fau_gnis <- fau_gnis %>%
          dplyr::filter(HUC8_full %in% input$select_huc8)
      }

      fau_gnis <- fau_gnis %>%
        dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")"))

    }

    # Text updates
    output$text_actions <- shiny::renderText({txt_actions})
    output$text_targets <- shiny::renderText({txt_targets})

    # Query table reactive -----------------------------------------------------
    query_table <- shiny::reactive({

      data.frame(Query_Date = Sys.time(),
                 Session = session$token,
                 R_package_version = odeqtmdl_version,
                 TMDL_Name = paste(collapse =  "; ", input$select_tmdl_names),
                 TMDL_Status = paste(collapse = "; ", input$select_tmdl_status),
                 TMDL_Scope = paste(collapse = "; ", input$select_tmdl_scope),
                 Parameter_303d = paste(collapse =  "; ", input$select_wql_param),
                 TMDL_Pollutant = paste(collapse =  "; ", input$select_tmdl_polluntant),
                 AU_GNIS_Name = paste(collapse =  "; ", input$select_au_gnis_name),
                 AU = paste(collapse =  "; ", input$select_au),
                 Basin = paste(collapse =  "; ", input$select_huc6),
                 Subbasin = paste(collapse =  "; ", input$select_huc8))
    })

    #- Action ID query Reactive ------------------------------------------------
    action_ids <- shiny::reactive({

      fau %>%
        dplyr::select(action_id) %>%
        dplyr::distinct() %>%
        dplyr::pull(action_id)

    })

    #- Action AU parameter count Reactive --------------------------------------
    action_param_count_data <- shiny::reactive({

      fau %>%
        dplyr::select(action_id, TMDL_wq_limited_parameter, AU_ID) %>%
        dplyr::distinct() %>%
        dplyr::group_by(action_id, TMDL_wq_limited_parameter) %>%
        dplyr::summarise(AU_count = dplyr::n())

    })

    #- Action Parameter table Reactive --------------------------------------
    action_param_data <- shiny::reactive({

      df <- fau %>%
        dplyr::select(TMDL_name, TMDL_issue_date, EPA_action_date,
                      action_id, TMDL_wq_limited_parameter) %>%
        dplyr::distinct() %>%
        dplyr::left_join(action_param_count_data(),
                         by = c("action_id", "TMDL_wq_limited_parameter")) %>%
        dplyr::arrange(TMDL_name, TMDL_wq_limited_parameter)

      tmdl_parameters_app %>%
        dplyr::left_join(df, by = c("action_id", "TMDL_wq_limited_parameter")) %>%
        dplyr::arrange(TMDL_name, TMDL_wq_limited_parameter) %>%
        dplyr::select(TMDL = TMDL_name,
                      "TMDL Completion Date" = TMDL_issue_date,
                      "EPA Approval Date" = EPA_action_date,
                      "EPA Action ID" = action_id,
                      "303(d) Parameters Addressed" = TMDL_wq_limited_parameter,
                      "TMDL Pollutants" = TMDL_pollutant,
                      "Count of Assessment Units Based on Query" = AU_count,
                      "Total Count of Assessment Units Addressed by TMDL" = AU_count_total)

    })

    #- Action AU count Reactive ---------------------------------------------------
    action_count_data <- shiny::reactive({

      action_param_count_data() %>%
        dplyr::group_by(action_id) %>%
        dplyr::summarise(AU_count = sum(AU_count, na.rm = TRUE))
    })

    #- Action table Reactive ---------------------------------------------------

    action_data <- shiny::reactive({

      tmdl_actions_app %>%
        dplyr::filter(action_id %in% action_ids()) %>%
        dplyr::left_join(action_count_data(), by = "action_id") %>%
        dplyr::arrange(TMDL_issue_date) %>%
        dplyr::select("TMDL" = TMDL_name,
                      "TMDL Completion Date" = TMDL_issue_date,
                      "EPA Approval Date" = EPA_action_date,
                      "EPA Action ID" = action_id,
                      "TMDL Status" = TMDL_status,
                      "TMDL Status Comment" = TMDL_status_comment,
                      "303(d) Parameters Addressed" = TMDL_wq_limited_parameter,
                      "TMDL Pollutants" = TMDL_pollutant,
                      "Count of Assessment Units Based on Query" = AU_count,
                      "Total Count of Assessment Units Addressed by TMDL" = AU_count_total,
                      URL)

    })

    #- Action table Render -----------------------------------------------------
    output$tmdl_actions_result <- reactable::renderReactable({

      reactable::reactable(action_data(),
                           columns = list(
                             "TMDL" = reactable::colDef(minWidth = 325,
                                                        maxWidth = 650,
                                                        headerVAlign = "center",
                                                        cell = function(value, index) {
                                                          if (is.na(action_data()[index, "URL"])) {
                                                            value
                                                          } else {
                                                            # Render as a link
                                                            htmltools::tags$a(href = action_data()[index, "URL"], target = "_blank", as.character(value))
                                                          }
                                                        }),
                             "TMDL Completion Date" = reactable::colDef(maxWidth = 110,
                                                                        align = "center", headerVAlign = "center",
                                                                        format = reactable::colFormat(date = TRUE)),
                             "EPA Approval Date" = reactable::colDef(maxWidth = 110,
                                                                     align = "center", headerVAlign = "center",
                                                                     format = reactable::colFormat(date = TRUE)),
                             "EPA Action ID" = reactable::colDef(minWidth = 160, maxWidth = 170, headerVAlign = "center",
                                                                 align = "right"),
                             "TMDL Status" = reactable::colDef(maxWidth = 110,
                                                               align = "center", headerVAlign = "center"),
                             "TMDL Status Comment" = reactable::colDef(minWidth = 250, maxWidth = 550, headerVAlign = "center"),
                             "303(d) Parameters Addressed" = reactable::colDef(minWidth = 250, maxWidth = 550, headerVAlign = "center"),
                             "TMDL Pollutants" = reactable::colDef(minWidth = 250, maxWidth = 550, headerVAlign = "center"),
                             "Count of Assessment Units Based on Query" = reactable::colDef(minWidth = 100, maxWidth = 175, headerVAlign = "center"),
                             "Total Count of Assessment Units Addressed by TMDL" = reactable::colDef(minWidth = 100, maxWidth = 175, headerVAlign = "center"),
                             "URL" = reactable::colDef(show = FALSE)),
                           sortable = TRUE,
                           showSortIcon = TRUE,
                           searchable = TRUE,
                           compact = TRUE,
                           bordered = TRUE,
                           details = function(index) {
                             dt1 <- action_param_data()[action_param_data()$'EPA Action ID' == action_data()$'EPA Action ID'[index],
                                                        c("303(d) Parameters Addressed",
                                                          "TMDL Pollutants",
                                                          "Count of Assessment Units Based on Query",
                                                          "Total Count of Assessment Units Addressed by TMDL")]

                             htmltools::div(style = "padding: 1rem",
                                            align = "right",
                                            reactable::reactable(dt1,
                                                                 columns = list(
                                                                   "303(d) Parameters Addressed" = reactable::colDef(minWidth = 250,
                                                                                                                     maxWidth = 550,
                                                                                                                     headerVAlign = "center"),
                                                                   "TMDL Pollutants" = reactable::colDef(minWidth = 250,
                                                                                                         maxWidth = 550,
                                                                                                         headerVAlign = "center"),
                                                                   "Count of Assessment Units Based on Query" = reactable::colDef(minWidth = 100,
                                                                                                                                  maxWidth = 175,
                                                                                                                                  headerVAlign = "center"),
                                                                   "Total Count of Assessment Units Addressed by TMDL" = reactable::colDef(minWidth = 100,
                                                                                                                                           maxWidth = 175,
                                                                                                                                           headerVAlign = "center")),
                                                                 outlined = TRUE,
                                                                 bordered = TRUE,
                                                                 fullWidth = FALSE))},
                           onClick = "expand",
                           rowStyle = list(cursor = "pointer")
      )
    })

    #- Target table Reactive ---------------------------------------------------

    target_data <- shiny::reactive({

      fr %>%
        dplyr::select(action_id, TMDL_pollutant, geo_id, TMDL_name) %>%
        dplyr::select(action_id, TMDL_pollutant, geo_id, TMDL_name) %>%
        dplyr::distinct() %>%
        dplyr::group_by(TMDL_pollutant) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!(is.na(geo_id) & n > 1)) %>%
        dplyr::left_join(tmdl_targets_app, by = c("action_id", "TMDL_pollutant", "geo_id")) %>%
        dplyr::mutate(Location = dplyr::case_when(is.na(geo_description) ~ "See TMDL",
                                                  TRUE ~ geo_description),
                      field_parameter = dplyr::if_else(is.na(field_parameter),
                                                       TMDL_pollutant,
                                                       field_parameter),
                      target = dplyr::case_when(is.na(target_value) ~ "See TMDL",
                                                target_type == "percent reduction" ~ paste(target_value, target_units, "reduction"),
                                                is.na(target_units) ~ target_value,
                                                TRUE ~ paste(target_value, target_units)),
                      tmdl_period = dplyr::case_when(is.na(season_start) | is.na(season_end) ~ "See TMDL",
                                                     TRUE ~ paste(season_start,"-", season_end))) %>%
        dplyr::select("Field Parameter" = field_parameter,
                      Location,
                      "Location Geo ID" = geo_id,
                      "TMDL Target" = target,
                      "Target Value" = target_value,
                      "Target Units" = target_units,
                      "Target Type" = target_type,
                      "Statistical Base" = stat_base,
                      "Conditionals" = target_conditionals,
                      "Target Period" = tmdl_period,
                      "TMDL Element" = TMDL_element,
                      "TMDL Reference" = target_reference,
                      "TMDL" = TMDL_name) %>%
        dplyr::distinct() %>%
        dplyr::arrange("Field Parameter", Location)

    })

    #- Target table Render -----------------------------------------------------
    output$tmdl_target_result <- reactable::renderReactable({

      reactable::reactable(data = shiny::isolate(target_data()),
                           columns = list(
                             "Field Parameter" = reactable::colDef(headerVAlign = "center"),
                             "Location" = reactable::colDef(headerVAlign = "center"),
                             "Location Geo ID" = reactable::colDef(headerVAlign = "center"),
                             "TMDL Target" = reactable::colDef(headerVAlign = "center"),
                             "Target Value" = reactable::colDef(show = FALSE),
                             "Target Units" = reactable::colDef(show = FALSE),
                             "Target Type" = reactable::colDef(headerVAlign = "center"),
                             "Statistical Base" = reactable::colDef(headerVAlign = "center"),
                             "Conditionals" = reactable::colDef(headerVAlign = "center"),
                             "Target Period" = reactable::colDef(headerVAlign = "center"),
                             "TMDL Element" = reactable::colDef(headerVAlign = "center"),
                             "TMDL Reference" = reactable::colDef(headerVAlign = "center"),
                             "TMDL" = reactable::colDef(headerVAlign = "center")),
                           sortable = TRUE,
                           showSortIcon = TRUE,
                           searchable = TRUE,
                           compact = TRUE,
                           bordered = TRUE)
    })

    #- AU GNIS table Reactive --------------------------------------------------

    au_gnis_data <- shiny::reactive({

      fau_gnis %>%
        dplyr::select("Assessment Unit GNIS ID" = AU_GNIS,
                      "Assessment Unit GNIS Name" = AU_GNIS_Name,
                      "303(d) Parameter Addressed" = TMDL_wq_limited_parameter,
                      "TMDL Pollutant" = TMDL_pollutant,
                      "TMDL Scope" = TMDL_scope,
                      "Fish Use Period" = Period,
                      "Percent GNIS Assessment Unit Addressed by TMDL" = TMDL_AU_GNIS_Percent,
                      "Percent GNIS Assessment Unit Addressed by Allocation Only" = Allocation_AU_GNIS_Percent,
                      "TMDL" = TMDL_name,
                      "EPA Action ID" = action_id)
    })

    #- AU GNIS table Render ----------------------------------------------------
    output$tmdl_au_gnis_result <- reactable::renderReactable({

      reactable::reactable(data = shiny::isolate(au_gnis_data()),
                           columns = list(
                             "Assessment Unit GNIS ID" = reactable::colDef(width = 400, headerVAlign = "center"),
                             "Assessment Unit GNIS Name"  = reactable::colDef(width = 275, headerVAlign = "center"),
                             "303(d) Parameter Addressed" = reactable::colDef(minWidth = 160, maxWidth = 170, headerVAlign = "center"),
                             "TMDL Pollutant" = reactable::colDef(minWidth = 160, maxWidth = 250, headerVAlign = "center"),
                             "TMDL Scope" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                             "Fish Use Period" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                             "Percent GNIS Assessment Unit Addressed by TMDL" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                             "Percent GNIS Assessment Unit Addressed by Allocation Only" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                             "TMDL" = reactable::colDef(minWidth = 325, maxWidth = 325, headerVAlign = "center"),
                             "EPA Action ID" = reactable::colDef(minWidth = 125, maxWidth = 170,
                                                                 align = "right", headerVAlign = "center")),
                           sortable = TRUE,
                           showSortIcon = TRUE,
                           searchable = TRUE,
                           compact = TRUE,
                           bordered = TRUE)

    })

    #- AU table Reactive -------------------------------------------------------
    au_data <- shiny::reactive({

      fau %>%
        dplyr::arrange(AU_ID, TMDL_wq_limited_parameter, TMDL_pollutant) %>%
        dplyr::select("Assessment Unit ID" = AU_ID,
                      "Assessment Unit Name" = AU_Name,
                      "Assessment Unit Description" = AU_Description,
                      "303(d) Parameter Addressed" = TMDL_wq_limited_parameter,
                      "TMDL Pollutant" = TMDL_pollutant,
                      "TMDL Scope" = TMDL_scope,
                      "Fish Use Period" = Period,
                      "Percent Assessment Unit Addressed by TMDL" = TMDL_AU_Percent,
                      "Percent Assessment Unit Addressed by Allocation Only" = Allocation_AU_Percent,
                      "TMDL" = TMDL_name,
                      "EPA Action ID" = action_id)

    })

    #- AU table Render ---------------------------------------------------------
    output$tmdl_au_result <- reactable::renderReactable({

      reactable::reactable(data = shiny::isolate(au_data()),
                           columns = list(
                             "Assessment Unit ID" = reactable::colDef(width = 250, headerVAlign = "center"),
                             "Assessment Unit Name"  = reactable::colDef(width = 200, headerVAlign = "center"),
                             "Assessment Unit Description" = reactable::colDef(width = 225, headerVAlign = "center"),
                             "303(d) Parameter Addressed" = reactable::colDef(minWidth = 160, maxWidth = 170, headerVAlign = "center"),
                             "TMDL Pollutant" = reactable::colDef(minWidth = 160, maxWidth = 170, headerVAlign = "center"),
                             "TMDL Scope" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                             "Fish Use Period" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                             "Percent Assessment Unit Addressed by TMDL" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                             "Percent Assessment Unit Addressed by Allocation Only" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                             "TMDL" = reactable::colDef(minWidth = 325, maxWidth = 325, headerVAlign = "center"),
                             "EPA Action ID" = reactable::colDef(minWidth = 125, maxWidth = 170,
                                                                 align = "right", headerVAlign = "center")),
                           sortable = TRUE,
                           showSortIcon = TRUE,
                           searchable = TRUE,
                           compact = TRUE,
                           bordered = TRUE)
    })

    #- Download ---------------------------------------------------
    output$download_query_results <- shiny::downloadHandler(
      filename = paste0("TMDL_query_result_", Sys.Date(),".xlsx"),
      content = function(file) {

        options(openxlsx.dateFormat = "mm/dd/yyyy")
        openxlsx::write.xlsx(list(TMDL_actions = shiny::isolate(action_data()),
                                  TMDL_Pollutant_Targets = shiny::isolate(target_data()),
                                  Assessment_Units = shiny::isolate(au_data()),
                                  GNIS_Assessment_Units = shiny::isolate(au_gnis_data()),
                                  Query = shiny::isolate(query_table())),
                             file = file,
                             colWidths = "auto",
                             firstActiveRow = c(2,2,2,2,2),
                             firstRow = c(TRUE,TRUE,TRUE,TRUE,TRUE),
                             rowNames = c(FALSE, FALSE, FALSE, FALSE,FALSE),
                             borders = "rows",
                             startCol = c(1,1,1,1), startRow = c(1,1,1,1,1),
                             headerStyle = openxlsx::createStyle(fgFill = "#000000",
                                                                 halign = "LEFT",
                                                                 textDecoration = "Bold",
                                                                 wrapText = TRUE,
                                                                 border = "Bottom",
                                                                 fontColour = "white",
                                                                 fontName = "Arial",
                                                                 fontSize = 10))
      }
    )

    #- Download Button Render---------------------------------------------------
    output$download_button <- shiny::renderUI({

      tags$span(shiny::downloadButton(outputId = "download_query_results",
                                      label = "Download"), tags$p("Download query results as xlsx"))
    })

  })

}

# Run the application
shiny::shinyApp(ui = ui, server = server)
