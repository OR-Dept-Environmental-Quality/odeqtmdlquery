
library(dplyr)
library(odeqtmdl)
library(reactable)
library(htmltools)

getOption("openxlsx.dateFormat", "mm/dd/yyyy")
options(dplyr.summarise.inform = FALSE)

odeqtmdl_version <- "1.1.2"

# Load data --------------------------------------------------------------------

load(file = file.path("data", "col_desc_app.rda"))
load(file = file.path("data", "tmdl_actions_app.rda"))
load(file = file.path("data", "tmdl_targets_app.rda"))
load(file = file.path("data", "tmdl_parameters_app.rda"))
load(file = file.path("data", "tmdl_au_app.rda"))
load(file = file.path("data", "tmdl_au_gnis_app.rda"))
load(file = file.path("data", "tmdl_au_gnis_LU.rda"))
load(file = file.path("data", "tmdl_geo_id_app.rda"))
load(file = file.path("data", "tmdl_wla_app.rda"))

tmdl_names <- c(sort(unique(tmdl_au_app$TMDL_name)))
tmdl_statuses <- c("Active", "Not Active", "In Development")
tmdl_years <- seq(lubridate::year(min(tmdl_actions_app$EPA_action_date, na.rm = TRUE)),
                  lubridate::year(max(tmdl_actions_app$EPA_action_date, na.rm = TRUE)), by = 1)
tmdl_scopes <- c("TMDL", "Allocation only")
tmdl_huc6 <- c(sort(unique(tmdl_au_app$HUC6_full)))
tmdl_huc8 <- c(sort(unique(tmdl_au_app$HUC8_full)))
tmdl_parameters <- c(sort(unique(tmdl_au_app$TMDL_parameter)))
tmdl_pollutants <- c(sort(unique(tmdl_au_app$TMDL_pollutant)))
tmdl_au_ids <- c(sort(unique(tmdl_au_app$AU_ID)))

tmdl_au_names <- unique(tmdl_au_app$AU_Name)
tmdl_au_gnis_names <- unique(tmdl_au_gnis_app$AU_GNIS_Name)

tmdl_au_names_all <- sort(unique(c(tmdl_au_names,
                                   tmdl_au_gnis_names)))

#- Query -----------------------------------------------------------------------

select_tmdl_status <- "Active"
select_tmdl_scope <- "TMDL"
select_fromyear <- 1988
select_toyear <- 2025
select_tmdl_names <- c("Snake River - Hells Canyon Total Maximum Daily Load (TMDL) (IDEQ and DEQ, 2003)",
                       "Snake River - Hells Canyon Total Maximum Daily Load (TMDL) (IDEQ and DEQ, 2004)")
select_wql_param <- NULL
select_tmdl_polluntant <- NULL
select_au_name <- NULL
select_au <- NULL
select_huc6 <- NULL
select_huc8 <- NULL

input <- data.frame(select_tmdl_status = I(list(select_tmdl_status)),
                    select_tmdl_scope = I(list(select_tmdl_scope)),
                    select_tmdl_names = I(list(select_tmdl_names)),
                    select_fromyear = I(list(select_fromyear)),
                    select_toyear = I(list(select_toyear)),
                    select_wql_param = I(list(select_wql_param)),
                    select_tmdl_polluntant = I(list(select_tmdl_polluntant)),
                    select_au_name = I(list(select_au_name)),
                    select_au = I(list(select_au)),
                    select_huc6 = I(list(select_huc6)),
                    select_huc8 = I(list(select_huc8)))

#- App ------------------------------------------------------------------------

# Get names from the right field
select_au_name_filter <- tmdl_au_names[tmdl_au_names %in% input$select_au_name[[1]]]
select_au_gnis_name_filter <- tmdl_au_gnis_names[tmdl_au_gnis_names %in% input$select_au_name[[1]]]


if (nchar(input$select_fromyear[[1]]) == 0) {
  date_from <- lubridate:::mdy(paste0("01/01/", min(tmdl_years)))
} else {
  date_from <- lubridate:::mdy(paste0("01/01/", input$select_fromyear[[1]]))
}

if (nchar(input$select_toyear[[1]]) == 0) {
  date_to <- lubridate:::mdy(paste0("12/31/", max(tmdl_years)))
} else {
  date_to <- lubridate:::mdy(paste0("12/31/", input$select_toyear[[1]]))
}

if (length(select_au_gnis_name_filter) > 0) {

  select_au_name_filter2 <- tmdl_au_gnis_LU %>%
    dplyr::filter(AU_GNIS_Name %in% select_au_gnis_name_filter) %>%
    dplyr::pull(AU_Name) %>%
    unique()

  select_au_name_filter <- unique(c(select_au_name_filter, select_au_name_filter2))

}

# Filter tmdl_aus based on inputs = fau
{
  fau <- tmdl_au_app

  if (!is.null(input$select_tmdl_status[[1]])) {
    fau <- fau %>%
      dplyr::filter(TMDL_status %in% input$select_tmdl_status[[1]])
  }

  fau <- fau %>%
    dplyr::filter((EPA_action_date >= date_from & EPA_action_date <= date_to)
                  | is.na(EPA_action_date))

  if (!is.null(input$select_tmdl_names[[1]])) {
    fau <- fau %>%
      dplyr::filter(TMDL_name %in% input$select_tmdl_names[[1]])
  }

  if (!is.null(input$select_tmdl_scope[[1]])) {
    fau <- fau %>%
      dplyr::filter(TMDL_scope %in% input$select_tmdl_scope[[1]])
  }

  if (!is.null(input$select_wql_param[[1]])) {
    fau <- fau %>%
      dplyr::filter(TMDL_parameter %in% input$select_wql_param[[1]])
  }

  if (!is.null(input$select_tmdl_polluntant[[1]])) {
    fau <- fau %>%
      dplyr::filter(TMDL_pollutant %in% input$select_tmdl_polluntant[[1]])
  }

  if (!is.null(input$select_au[[1]])) {
    fau <- fau %>%
      dplyr::filter(AU_ID %in% input$select_au[[1]])
  }

  if (length(select_au_name_filter) > 0) {
    fau <- fau %>%
      dplyr::filter(AU_Name %in% select_au_name_filter)
  }


  if (!is.null(input$select_huc6[[1]])) {
    fau <- fau %>%
      dplyr::filter(HUC6_full %in% input$select_huc6[[1]])
  }

  if (!is.null(input$select_huc8[[1]])) {
    fau <- fau %>%
      dplyr::filter(HUC8_full %in% input$select_huc8[[1]])
  }

  f_au_ids <- fau %>%
    dplyr::pull(AU_ID) %>%
    unique()

  f_pollutants <- fau %>%
    dplyr::pull(TMDL_pollutant) %>%
    unique()

  f_action_ids <- fau %>%
    dplyr::pull(action_id) %>%
    unique()

}

# Filter tmdl_au_gnis based on inputs = fau_gnis
{
  fau_gnis <- tmdl_au_gnis_app %>%
    dplyr::filter(AU_ID %in% f_au_ids)

  if (!is.null(input$select_tmdl_status[[1]])) {
    fau_gnis <- fau_gnis %>%
      dplyr::filter(TMDL_status %in% input$select_tmdl_status[[1]])
  }

  fau_gnis <- fau_gnis %>%
    dplyr::filter((EPA_action_date >= date_from & EPA_action_date <= date_to)
                  | is.na(EPA_action_date))

  if (!is.null(input$select_tmdl_names[[1]])) {
    fau_gnis <- fau_gnis %>%
      dplyr::filter(TMDL_name %in% input$select_tmdl_names[[1]])
  }

  if (!is.null(input$select_tmdl_scope[[1]])) {
    fau_gnis <- fau_gnis %>%
      dplyr::filter(TMDL_scope %in% input$select_tmdl_scope[[1]])
  }

  if (!is.null(input$select_wql_param)) {
    fau_gnis <- fau_gnis %>%
      dplyr::filter(TMDL_parameter %in% input$select_wql_param)
  }

  if (!is.null(input$select_tmdl_polluntant[[1]])) {
    fau_gnis <- fau_gnis %>%
      dplyr::filter(TMDL_pollutant %in% input$select_tmdl_polluntant[[1]])
  }

  if (length(select_au_gnis_name_filter) > 0) {
    fau_gnis <- fau_gnis %>%
      dplyr::filter(AU_GNIS_Name %in% select_au_gnis_name_filter)

  }

  f_au_gnis_name <- fau_gnis %>%
    dplyr::pull(AU_GNIS_Name) %>%
    unique()

}

# Filter tmdl_wla by AU and pollutant
{
  fwla <- tmdl_wla_app %>%
    dplyr::filter(AU_ID %in% c(f_au_ids, NA_character_))

  fwla <- fwla %>%
    dplyr::filter(action_id %in% f_action_ids)

  fwla <- fwla %>%
    dplyr::filter(TMDL_pollutant %in% f_pollutants)

  if (!is.null(input$select_tmdl_status[[1]])) {
    fwla <- fwla %>%
      dplyr::filter(TMDL_status %in% input$select_tmdl_status[[1]])
  }

  if (!is.null(input$select_tmdl_names)) {
    fwla <- fwla %>%
      dplyr::filter(TMDL_name %in% input$select_tmdl_names[[1]])
  }

}

# get geo_ids by action ID, AU ID, and pollutant
{
  fgeoid <- tmdl_geo_id_app %>%
    dplyr::filter(action_id %in% f_action_ids)

  fgeoid <- fgeoid%>%
    dplyr::filter(AU_ID %in% f_au_ids)

  fgeoid <- fgeoid %>%
    dplyr::filter(TMDL_pollutant %in% f_pollutants)

  if (length(f_au_gnis_name) > 0) {
    fgeoid <- fgeoid %>%
      dplyr::filter(AU_GNIS_Name %in% f_au_gnis_name)
  }
}

# Query table reactive -----------------------------------------------------
query_table <- function() {

  data.frame(Query_Date = Sys.time(),
             Session = session$token,
             'R package version' = odeqtmdl_version,
             'TMDL Name' = paste(collapse =  "; ", input$select_tmdl_names),
             'TMDL Status' = paste(collapse = "; ", input$select_tmdl_status),
             'TMDL Scope' = paste(collapse = "; ", input$select_tmdl_scope),
             'Date Range' = paste0(date_from, " to ", date_to),
             'Parameter 303d' = paste(collapse =  "; ", input$select_wql_param),
             'TMDL Pollutant' = paste(collapse =  "; ", input$select_tmdl_polluntant),
             'AU Name' = paste(collapse =  "; ", input$select_au_name),
             AU = paste(collapse =  "; ", input$select_au),
             Basin = paste(collapse =  "; ", input$select_huc6),
             Subbasin = paste(collapse =  "; ", input$select_huc8),
             check.names = FALSE,
             stringsAsFactors = FALSE)
}

#- Action ID query Reactive ------------------------------------------------
action_ids <- function(){

  fau %>%
    dplyr::select(action_id) %>%
    dplyr::distinct() %>%
    dplyr::pull(action_id)

}

#- Action AU parameter count Reactive --------------------------------------
action_param_count_data <- function(){

  fau %>%
    dplyr::select(action_id, TMDL_parameter, AU_ID) %>%
    dplyr::distinct() %>%
    dplyr::group_by(action_id, TMDL_parameter) %>%
    dplyr::summarise(AU_count = dplyr::n())

}

#- Action Parameter table Reactive --------------------------------------
action_param_data <- function(){

  df <- fau %>%
    dplyr::select(TMDL_name, TMDL_issue_date, EPA_action_date,
                  action_id, TMDL_parameter) %>%
    dplyr::distinct() %>%
    dplyr::left_join(action_param_count_data(),
                     by = c("action_id", "TMDL_parameter")) %>%
    dplyr::arrange(TMDL_name, TMDL_parameter)

  tmdl_parameters_app %>%
    dplyr::left_join(df, by = c("action_id", "TMDL_parameter")) %>%
    dplyr::arrange(TMDL_name, TMDL_parameter) %>%
    dplyr::select(TMDL = TMDL_name,
                  "TMDL Completion Date" = TMDL_issue_date,
                  "EPA Approval Date" = EPA_action_date,
                  "EPA Action ID" = action_id,
                  "303(d) Parameters Addressed" = TMDL_parameter,
                  "TMDL Pollutants" = TMDL_pollutant,
                  "Count of Assessment Units Based on Query" = AU_count,
                  "Total Count of Assessment Units Addressed by TMDL Action" = AU_count_total)

}

#- Action AU count Reactive ---------------------------------------------------
action_count_data <- function(){

  action_param_count_data() %>%
    dplyr::group_by(action_id) %>%
    dplyr::summarise(AU_count = sum(AU_count, na.rm = TRUE))
}

#- Action table Reactive ---------------------------------------------------

action_data <- function(){

  tmdl_actions_app %>%
    dplyr::filter(action_id %in% action_ids()) %>%
    dplyr::left_join(action_count_data(), by = "action_id") %>%
    dplyr::arrange(dplyr::desc(EPA_action_date)) %>%
    dplyr::select("TMDL" = TMDL_name,
                  "TMDL Completion Date" = TMDL_issue_date,
                  "EPA Approval Date" = EPA_action_date,
                  "EPA Action ID" = action_id,
                  "TMDL Status" = TMDL_status,
                  "TMDL Status Comment" = TMDL_status_comment,
                  "303(d) Parameters Addressed" = TMDL_parameter,
                  "TMDL Pollutants" = TMDL_pollutant,
                  "Count of Assessment Units Based on Query" = AU_count,
                  "Total Count of Assessment Units Addressed by TMDL Action" = AU_count_total,
                  URL)

}

#- Action table Render -----------------------------------------------------

  reactable::reactable(action_data(),
                       columns = list(
                         "TMDL" = reactable::colDef(minWidth = 325,
                                                    maxWidth = 325,
                                                    headerVAlign = "center",
                                                    cell = function(value, index) {
                                                      if (is.na(action_data()[index, "URL"])) {
                                                        value
                                                      } else {
                                                        # Render as a link
                                                        htmltools::tags$a(href = action_data()[index, "URL"], target = "_blank", as.character(value))
                                                      }
                                                    }),
                         "TMDL Completion Date" = reactable::colDef(maxWidth = 85,
                                                                    align = "center", headerVAlign = "center",
                                                                    format = reactable::colFormat(date = TRUE)),
                         "EPA Approval Date" = reactable::colDef(maxWidth = 85,
                                                                 align = "center", headerVAlign = "center",
                                                                 format = reactable::colFormat(date = TRUE)),
                         "EPA Action ID" = reactable::colDef(minWidth = 125, maxWidth = 170, headerVAlign = "center",
                                                             align = "right"),
                         "TMDL Status" = reactable::colDef(maxWidth = 110,
                                                           align = "center", headerVAlign = "center"),
                         "TMDL Status Comment" = reactable::colDef(minWidth = 150, maxWidth = 350, headerVAlign = "center"),
                         "303(d) Parameters Addressed" = reactable::colDef(minWidth = 150, maxWidth = 450, headerVAlign = "center"),
                         "TMDL Pollutants" = reactable::colDef(minWidth = 150, maxWidth = 450, headerVAlign = "center"),
                         "Count of Assessment Units Based on Query" = reactable::colDef(maxWidth = 100, headerVAlign = "center"),
                         "Total Count of Assessment Units Addressed by TMDL Action" = reactable::colDef(maxWidth = 100, headerVAlign = "center"),
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
                                                      "Total Count of Assessment Units Addressed by TMDL Action")]

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
                                                               "Total Count of Assessment Units Addressed by TMDL Action" = reactable::colDef(minWidth = 100,
                                                                                                                                              maxWidth = 175,
                                                                                                                                              headerVAlign = "center")),
                                                             outlined = TRUE,
                                                             bordered = TRUE,
                                                             fullWidth = FALSE))},
                       onClick = "expand",
                       rowStyle = list(cursor = "pointer")
  )

#- Target table Reactive ---------------------------------------------------

target_data <- function(){

  pollus_geo_ids <- fgeoid %>%
    dplyr::select(action_id, TMDL_pollutant, geo_id, TMDL_name) %>%
    dplyr::distinct()

  pollus_no_geo_ids <- fau %>%
    dplyr::mutate(geo_id = NA_character_) %>%
    dplyr::select(action_id, TMDL_pollutant, geo_id, TMDL_name) %>%
    dplyr::distinct() %>%
    dplyr::anti_join(pollus_geo_ids, by = c("action_id", "TMDL_pollutant", "TMDL_name"))

  fgeoid %>%
    dplyr::select(action_id, TMDL_pollutant, geo_id, TMDL_name) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(pollus_no_geo_ids) %>%
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
                  "TMDL" = TMDL_name,
                  "EPA Action ID" = action_id,
                  URL) %>%
    dplyr::distinct() %>%
    dplyr::arrange("Field Parameter", Location)

}

#- Target table Render -----------------------------------------------------

  z <- reactable::reactable(data = target_data(),
                       columns = list(
                         "Field Parameter" = reactable::colDef(headerVAlign = "center"),
                         "Location" = reactable::colDef(headerVAlign = "center"),
                         "Location Geo ID" = reactable::colDef(headerVAlign = "center",
                                                               cell = function(value, index) {
                                                                 if(!is.na(value)) {
                                                                 htmltools::tags$a(href = target_data()[index, "URL"], target = "_blank", as.character(value))
                                                                   }
                                                               }
                         ),
                         "TMDL Target" = reactable::colDef(headerVAlign = "center"),
                         "Target Value" = reactable::colDef(show = FALSE),
                         "Target Units" = reactable::colDef(show = FALSE),
                         "Target Type" = reactable::colDef(headerVAlign = "center"),
                         "Statistical Base" = reactable::colDef(headerVAlign = "center"),
                         "Conditionals" = reactable::colDef(headerVAlign = "center"),
                         "Target Period" = reactable::colDef(headerVAlign = "center"),
                         "TMDL Element" = reactable::colDef(headerVAlign = "center"),
                         "TMDL Reference" = reactable::colDef(headerVAlign = "center"),
                         "TMDL" = reactable::colDef(headerVAlign = "center"),
                         "EPA Action ID" = reactable::colDef(minWidth = 125, maxWidth = 170,
                                                             align = "right", headerVAlign = "center"),
                         "URL" = reactable::colDef(show = FALSE)),
                       sortable = TRUE,
                       showSortIcon = TRUE,
                       searchable = TRUE,
                       compact = TRUE,
                       bordered = TRUE)

#- AU GNIS table Reactive --------------------------------------------------

au_gnis_data <- function(){

  fau_gnis %>%
    dplyr::select("Assessment Unit GNIS ID" = AU_GNIS,
                  "Assessment Unit GNIS Name" = AU_GNIS_Name,
                  "303(d) Parameter Addressed" = TMDL_parameter,
                  "TMDL Pollutant" = TMDL_pollutant,
                  "TMDL Scope" = TMDL_scope,
                  "Fish Use Period" = Period,
                  "Percent GNIS Assessment Unit Addressed by TMDL" = TMDL_AU_GNIS_Percent,
                  "Percent GNIS Assessment Unit Addressed by Allocation Only" = Allocation_AU_GNIS_Percent,
                  "TMDL" = TMDL_name,
                  "EPA Action ID" = action_id)
}

#- AU GNIS table Render ----------------------------------------------------

reactable::reactable(data = shiny::isolate(au_gnis_data()),
                     columns = list(
                       "Assessment Unit GNIS ID" = reactable::colDef(width = 400, headerVAlign = "center"),
                       "Assessment Unit GNIS Name"  = reactable::colDef(width = 275, headerVAlign = "center"),
                       "303(d) Parameter Addressed" = reactable::colDef(minWidth = 150, maxWidth = 150, headerVAlign = "center"),
                       "TMDL Pollutant" = reactable::colDef(minWidth = 150, maxWidth = 150, headerVAlign = "center"),
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

#- AU table Reactive -------------------------------------------------------
au_data <- function(){

  fau %>%
    dplyr::arrange(AU_ID, TMDL_parameter, TMDL_pollutant) %>%
    dplyr::select("Assessment Unit ID" = AU_ID,
                  "Assessment Unit Name" = AU_Name,
                  "Assessment Unit Description" = AU_Description,
                  "303(d) Parameter Addressed" = TMDL_parameter,
                  "TMDL Pollutant" = TMDL_pollutant,
                  "TMDL Scope" = TMDL_scope,
                  "Fish Use Period" = Period,
                  "Percent Assessment Unit Addressed by TMDL" = TMDL_AU_Percent,
                  "Percent Assessment Unit Addressed by Allocation Only" = Allocation_AU_Percent,
                  "TMDL" = TMDL_name,
                  "EPA Action ID" = action_id,
                  URL)

}

#- AU table Render ---------------------------------------------------------

reactable::reactable(data = shiny::isolate(au_data()),
                     columns = list(
                       "Assessment Unit ID" = reactable::colDef(width = 250, headerVAlign = "center",
                                                                cell = function(value, index) {
                                                                  htmltools::tags$a(href = au_data()[index, "URL"], target = "_blank", as.character(value))
                                                                }),
                       "Assessment Unit Name"  = reactable::colDef(width = 200, headerVAlign = "center"),
                       "Assessment Unit Description" = reactable::colDef(width = 200, headerVAlign = "center"),
                       "303(d) Parameter Addressed" = reactable::colDef(minWidth = 150, maxWidth = 150, headerVAlign = "center"),
                       "TMDL Pollutant" = reactable::colDef(minWidth = 150, maxWidth = 150, headerVAlign = "center"),
                       "TMDL Scope" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                       "Fish Use Period" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                       "Percent Assessment Unit Addressed by TMDL" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                       "Percent Assessment Unit Addressed by Allocation Only" = reactable::colDef(minWidth = 125, maxWidth = 125, headerVAlign = "center"),
                       "TMDL" = reactable::colDef(minWidth = 325, maxWidth = 325, headerVAlign = "center"),
                       "EPA Action ID" = reactable::colDef(minWidth = 125, maxWidth = 170,
                                                           align = "right", headerVAlign = "center"),
                       "URL" = reactable::colDef(show = FALSE)),
                     sortable = TRUE,
                     showSortIcon = TRUE,
                     searchable = TRUE,
                     compact = TRUE,
                     bordered = TRUE)

#- WLA table Reactive -------------------------------------------------------
wla_data <- function(){

  fwla %>%
    dplyr::arrange(facility_name, TMDL_pollutant, AU_ID, TMDL_name) %>%
    dplyr::select("Facility Name" = facility_name,
                  "EPA Number" = EPANum,
                  "File Number" = WQFileNum,
                  "Assessment Unit ID" = AU_ID,
                  "TMDL Pollutant" = TMDL_pollutant,
                  "TMDL" = TMDL_name,
                  "EPA Action ID" = action_id)

}

#- WLA table Render ---------------------------------------------------------

  reactable::reactable(data = shiny::isolate(wla_data()),
                       columns = list(
                         "Facility Name" = reactable::colDef(width = 350, align = "left", headerVAlign = "center"),
                         "EPA Number" = reactable::colDef(width = 95, align = "right", headerVAlign = "center"),
                         "File Number" = reactable::colDef(width = 95, align = "right", headerVAlign = "center"),
                         "Assessment Unit ID" = reactable::colDef(width = 250, headerVAlign = "center"),
                         "TMDL Pollutant" = reactable::colDef(minWidth = 160, maxWidth = 170, headerVAlign = "center"),
                         "TMDL" = reactable::colDef(minWidth = 325, maxWidth = 325, headerVAlign = "center"),
                         "EPA Action ID" = reactable::colDef(minWidth = 125, maxWidth = 170,
                                                             align = "right", headerVAlign = "center")),
                       sortable = TRUE,
                       showSortIcon = TRUE,
                       searchable = TRUE,
                       compact = TRUE,
                       bordered = TRUE)


# -Download --------------------------------------------------------------------

filename = paste0("TMDL_query_result_", Sys.Date(),".xlsx")

options(openxlsx.dateFormat = "mm/dd/yyyy")
openxlsx::write.xlsx(list(Query = query_table,
                          Column_Descriptions = col_desc_app,
                          TMDL_actions = action_data,
                          TMDL_Pollutant_Targets = target_data |>
                            dplyr::select(-matches("TMDL Target"),
                          Assessment_Units = au_data,
                          GNIS_Assessment_Units = au_gnis_data,
                          Point_Sources = wla_data)),
                     file = file,
                     colWidths = "auto",
                     firstActiveRow = c(2,2,2,2,2,2,2),
                     firstRow = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                     rowNames = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                     borders = "rows",
                     startCol = c(1), startRow = c(1),
                     headerStyle = openxlsx::createStyle(fgFill = "#000000",
                                                         halign = "LEFT",
                                                         textDecoration = "Bold",
                                                         wrapText = TRUE,
                                                         border = "Bottom",
                                                         fontColour = "white",
                                                         fontName = "Arial",
                                                         fontSize = 10))


