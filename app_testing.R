
library(dplyr)
library(odeqtmdl)
library(reactable)
library(htmltools)

getOption("openxlsx.dateFormat", "mm/dd/yyyy")
options(dplyr.summarise.inform = FALSE)

odeqtmdl_version <- "0.8.4"

# Load data --------------------------------------------------------------------

load(file = file.path("data", "tmdl_actions_app.rda"))
load(file = file.path("data", "tmdl_targets_app.rda"))
load(file = file.path("data", "tmdl_parameters_app.rda"))
load(file = file.path("data", "tmdl_au_app.rda"))
load(file = file.path("data", "tmdl_au_gnis_app.rda"))
load(file = file.path("data", "tmdl_au_gnis_LU.rda"))
load(file = file.path("data", "tmdl_geo_id_app.rda"))

tmdl_names <- c(sort(unique(tmdl_au_app$TMDL_name)))
tmdl_statuses <- c("Active", "Not Active", "In Development")
tmdl_scopes <- c("TMDL", "Allocation only", "Advisory allocation")
tmdl_huc6 <- c(sort(unique(tmdl_au_app$HUC6_full)))
tmdl_huc8 <- c(sort(unique(tmdl_au_app$HUC8_full)))
tmdl_parameters <- c(sort(unique(tmdl_au_app$TMDL_wq_limited_parameter)))
tmdl_pollutants <- c(sort(unique(tmdl_au_app$TMDL_pollutant)))
tmdl_au_ids <- c(sort(unique(tmdl_au_app$AU_ID)))

tmdl_au_names <- unique(tmdl_au_app$AU_Name)
tmdl_au_gnis_names <- unique(tmdl_au_gnis_app$AU_GNIS_Name)

tmdl_au_names_all <- sort(unique(c(tmdl_au_names,
                                   tmdl_au_gnis_names)))

#- Query -----------------------------------------------------------------------

select_tmdl_status <- "Active"
select_tmdl_scope <- NULL
select_tmdl_names <- NULL #c("Tenmile Lakes Watershed Total Maximum Daily Load (TMDL)")
select_wql_param <- NULL
select_tmdl_polluntant <- NULL
select_au_name <- c("Reeder Reservoir")
select_au <- NULL
select_huc6 <- NULL
select_huc8 <- NULL

input <- data.frame(select_tmdl_status = I(list(select_tmdl_status)),
                    select_tmdl_scope = I(list(select_tmdl_scope)),
                    select_tmdl_names = I(list(select_tmdl_names)),
                    select_wql_param = I(list(select_wql_param)),
                    select_tmdl_polluntant = I(list(select_tmdl_polluntant)),
                    select_au_name = I(list(select_au_name)),
                    select_au = I(list(select_au)),
                    select_huc6 = I(list(select_huc6)),
                    select_huc8 = I(list(select_huc8)))

#- App ------------------------------------------------------------------------

# Get names from the right field
select_au_name_filter <- tmdl_au_names[tmdl_au_names %in% select_au_name]
select_au_gnis_name_filter <- tmdl_au_gnis_names[tmdl_au_gnis_names %in% select_au_name]

if (length(select_au_gnis_name_filter) > 0) {

  select_au_name_filter2 <- tmdl_au_gnis_LU %>%
    dplyr::filter(AU_GNIS_Name %in% select_au_gnis_name_filter) %>%
    dplyr::pull(AU_Name) %>%
    unique()

  select_au_name_filter <- unique(c(select_au_name_filter, select_au_name_filter2))

}


# Filter to geo_ids based on inputs = fr
{
  fr <- tmdl_geo_id_app

  if (!is.null(select_tmdl_status)) {

    fr <- fr %>%
      dplyr::filter(TMDL_status %in% select_tmdl_status)
  }

  if (!is.null(select_tmdl_names)) {
    fr <- fr %>%
      dplyr::filter(TMDL_name %in% select_tmdl_names)
  }

  if (!is.null(select_tmdl_scope)) {
    fr <- fr %>%
      dplyr::filter(TMDL_scope %in% select_tmdl_scope)
  }

  if (!is.null(select_wql_param)) {
    fr <- fr %>%
      dplyr::filter(TMDL_wq_limited_parameter %in% select_wql_param)
  }

  if (!is.null(select_tmdl_polluntant )) {
    fr <- fr %>%
      dplyr::filter(TMDL_pollutant %in% select_tmdl_polluntant)
  }

  if (length(select_au_name_filter) > 0) {
    fr <- fr %>%
      dplyr::filter(AU_Name %in% select_au_name_filter)
  }

  if (length(select_au_gnis_name_filter) > 0) {
    fr <- fr %>%
      dplyr::filter(AU_GNIS_Name %in% select_au_gnis_name_filter)
  }

  if (!is.null(select_au)) {
    fr <- fr %>%
      dplyr::filter(AU_ID %in% select_au)
  }

  if (!is.null(select_huc6)) {
    fr <- fr %>%
      dplyr::filter(HUC6_full %in% select_huc6)
  }

  if (!is.null(select_huc8)) {
    fr <- fr %>%
      dplyr::filter(HUC8_full %in% select_huc8)
  }

  fr <- fr %>%
    dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")"))

}

# Filter tmdl_aus based on inputs = fau
{
  fau <- tmdl_au_app

  if (!is.null(select_tmdl_status)) {
    fau <- fau %>%
      dplyr::filter(TMDL_status %in% select_tmdl_status)
  }

  if (!is.null(select_tmdl_names)) {
    fau <- fau %>%
      dplyr::filter(TMDL_name %in% select_tmdl_names)
  }

  if (!is.null(select_tmdl_scope)) {
    fau <- fau %>%
      dplyr::filter(TMDL_scope %in% select_tmdl_scope)
  }

  if (!is.null(select_wql_param)) {
    fau <- fau %>%
      dplyr::filter(TMDL_wq_limited_parameter %in% select_wql_param)
  }

  if (!is.null(select_tmdl_polluntant)) {
    fau <- fau %>%
      dplyr::filter(TMDL_pollutant %in% select_tmdl_polluntant)
  }

  if (!is.null(select_au)) {
    fau <- fau %>%
      dplyr::filter(AU_ID %in% select_au)
  }

  if (!is.null(select_au)) {
    fau <- fau %>%
      dplyr::filter(AU_ID %in% unique(c(select_au, f_au_ids)))
  }

  if (length(select_au_name_filter) > 0) {
    fau <- fau %>%
      dplyr::filter(AU_Name %in% select_au_name_filter)
  }

  if (!is.null(select_huc6)) {
    fau <- fau %>%
      dplyr::filter(HUC6_full %in% select_huc6)
  }

  if (!is.null(select_huc8)) {
    fau <- fau %>%
      dplyr::filter(HUC8_full %in% select_huc8)
  }

  f_au_ids <- fau %>%
    dplyr::pull(AU_ID) %>%
    unique()

  fau <- fau %>%
    dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")"))
}

# Filter tmdl_au_gnis based on inputs = fau_gnis
{
  fau_gnis <- tmdl_au_gnis_app %>%
    dplyr::filter(AU_ID %in% f_au_ids)

  # if (!is.null(select_tmdl_status)) {
  #   fau_gnis <- fau_gnis %>%
  #     dplyr::filter(TMDL_status %in% select_tmdl_status)
  # }
  #
  # if (!is.null(select_tmdl_names)) {
  #   fau_gnis <- fau_gnis %>%
  #     dplyr::filter(TMDL_name %in% select_tmdl_names)
  # }
  #
  # if (!is.null(select_tmdl_scope)) {
  #   fau_gnis <- fau_gnis %>%
  #     dplyr::filter(TMDL_scope %in% select_tmdl_scope)
  # }
  #
  # if (!is.null(select_wql_param)) {
  #   fau_gnis <- fau_gnis %>%
  #     dplyr::filter(TMDL_wq_limited_parameter %in% select_wql_param)
  # }
  #
  # if (!is.null(select_tmdl_polluntant)) {
  #   fau_gnis <- fau_gnis %>%
  #     dplyr::filter(TMDL_pollutant %in% select_tmdl_polluntant)
  # }
  #
  # if (!is.null(select_au)) {
  #   fau_gnis <- fau_gnis %>%
  #     dplyr::filter(AU_ID %in% select_au)
  # }
  #
  # if (!is.null(select_huc6)) {
  #   fau_gnis <- fau_gnis %>%
  #     dplyr::filter(HUC6_full %in% select_huc6)
  # }
  #
  # if (!is.null(select_huc8)) {
  #   fau_gnis <- fau_gnis %>%
  #     dplyr::filter(HUC8_full %in% select_huc8)
  # }

  if (length(select_au_gnis_name_filter) > 0) {
    fau_gnis <- fau_gnis %>%
      dplyr::filter(AU_GNIS_Name %in% select_au_gnis_name_filter)

  }

  fau_gnis <- fau_gnis %>%
    dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")"))

}

#- Render TMDL action table ------------------------------------------------

# - Query Table

query_table <- data.frame(Query_Date = Sys.time(),
                          #Session = session$token,
                          R_package_version = odeqtmdl_version,
                          TMDL_Name = paste(collapse =  "; ", select_tmdl_names),
                          TMDL_Status = paste(collapse = "; ", select_tmdl_status),
                          TMDL_Scope = paste(collapse = "; ", select_tmdl_scope),
                          Parameter_303d = paste(collapse =  "; ", select_wql_param),
                          TMDL_Pollutant = paste(collapse =  "; ", select_tmdl_polluntant),
                          AU_Name = paste(collapse =  "; ", select_au_name),
                          AU = paste(collapse =  "; ", select_au),
                          Basin = paste(collapse =  "; ", select_huc6),
                          Subbasin = paste(collapse =  "; ", select_huc8))


#- Action ID query

action_ids <- fau %>%
  dplyr::select(action_id) %>%
  dplyr::distinct() %>%
  dplyr::pull(action_id)

#- Action AU parameter count Reactive
action_param_count_data <- fau %>%
  dplyr::select(action_id, TMDL_wq_limited_parameter, AU_ID) %>%
  dplyr::distinct() %>%
  dplyr::group_by(action_id, TMDL_wq_limited_parameter) %>%
  dplyr::summarise(AU_count = dplyr::n())

#- Action Parameter table Reactive
# action_param_data <-  fau %>%
#   dplyr::filter(TMDL_scope == "TMDL") %>%
#   dplyr::group_by(TMDL_name, TMDL_issue_date, EPA_action_date,
#                   action_id, TMDL_wq_limited_parameter) %>%
#   dplyr::summarise(TMDL_pollutant = paste0(sort(unique(TMDL_pollutant)), collapse = ", ")) %>%
#   dplyr::distinct() %>%
#   dplyr::left_join(action_param_count_data,
#                    by = c("action_id", "TMDL_wq_limited_parameter")) %>%
#   dplyr::arrange(TMDL_name, TMDL_wq_limited_parameter) %>%
#   dplyr::select(TMDL = TMDL_name,
#                 "TMDL Completion Date" = TMDL_issue_date,
#                 "EPA Approval Date" = EPA_action_date,
#                 "EPA Action ID" = action_id,
#                 "303(d) Parameters Addressed" = TMDL_wq_limited_parameter,
#                 "TMDL Pollutants" = TMDL_pollutant,
#                 "Count of Assessment Units Addressed by TMDLs" = AU_count)

df <- fau %>%
  dplyr::select(TMDL_name, TMDL_issue_date, EPA_action_date,
                action_id, TMDL_wq_limited_parameter) %>%
  dplyr::distinct() %>%
  dplyr::left_join(action_param_count_data,
                   by = c("action_id", "TMDL_wq_limited_parameter")) %>%
  dplyr::arrange(TMDL_name, TMDL_wq_limited_parameter)

action_param_data <- tmdl_parameters_app %>%
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

#- Action AU count Reactive
action_count_data <- action_param_count_data %>%
  dplyr::group_by(action_id) %>%
  dplyr::summarise(AU_count = sum(AU_count, na.rm = TRUE))


#- Action table Reactive

action_data <- tmdl_actions_app %>%
  dplyr::filter(action_id %in% action_ids) %>%
  dplyr::left_join(action_count_data, by = "action_id") %>%
  dplyr::arrange(TMDL_name) %>%
  dplyr::select("TMDL" = TMDL_name,
                "TMDL Completion Date" = TMDL_issue_date,
                "EPA Approval Date" = EPA_action_date,
                "EPA Action ID" = action_id,
                "303(d) Parameters Addressed" = TMDL_wq_limited_parameter,
                "TMDL Pollutants" = TMDL_pollutant,
                "Count of Assessment Units Based on Query" = AU_count,
                "Total Count of Assessment Units Addressed by TMDL" = AU_count_total)

#- Action table Render ---------------------------------------------------------

reactable::reactable(action_data,
                     columns = list(
                       "TMDL" = reactable::colDef(minWidth = 325,
                                                  maxWidth = 650,
                                                  headerVAlign = "center",),
                       "TMDL Completion Date" = reactable::colDef(maxWidth = 110,
                                                                  align = "center", headerVAlign = "center",
                                                                  format = reactable::colFormat(date = TRUE)),
                       "EPA Approval Date" = reactable::colDef(maxWidth = 110,
                                                               align = "center", headerVAlign = "center",
                                                               format = reactable::colFormat(date = TRUE)),
                       "EPA Action ID" = reactable::colDef(minWidth = 160, maxWidth = 170, headerVAlign = "center",
                                                           align = "right"),
                       "303(d) Parameters Addressed" = reactable::colDef(minWidth = 250, maxWidth = 550, headerVAlign = "center"),
                       "TMDL Pollutants" = reactable::colDef(minWidth = 250, maxWidth = 550, headerVAlign = "center"),
                       "Count of Assessment Units Based on Query" = reactable::colDef(minWidth = 100, maxWidth = 175, headerVAlign = "center"),
                       "Total Count of Assessment Units Addressed by TMDL" = reactable::colDef(minWidth = 100, maxWidth = 175, headerVAlign = "center")),
                     sortable = TRUE,
                     showSortIcon = TRUE,
                     searchable = TRUE,
                     compact = TRUE,
                     bordered = TRUE,
                     details = function(index) {
                       dt1 <- action_param_data[action_param_data$'EPA Action ID' == action_data$'EPA Action ID'[index],
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


#- Target Table -----------------------------------------------------------

pollus_geo_ids <- fr %>%
  dplyr::select(action_id, TMDL_pollutant, geo_id, TMDL_name) %>%
  dplyr::distinct()

pollus_no_geo_ids <- fau %>%
  dplyr::mutate(geo_id = NA_character_) %>%
  dplyr::select(action_id, TMDL_pollutant, geo_id, TMDL_name) %>%
  dplyr::distinct() %>%
  dplyr::anti_join(pollus_geo_ids, by = c("action_id", "TMDL_pollutant", "TMDL_name"))

target_data <- fr %>%
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
                "TMDL" = TMDL_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange("Field Parameter", Location)

# target_data <- fr %>%
#     dplyr::select(action_id, TMDL_pollutant, geo_id, TMDL_name) %>%
#     dplyr::distinct() %>%
#     dplyr::left_join(tmdl_targets_tbl, by = c("action_id", "TMDL_pollutant")) %>%
#     tidyr::unite("stat_base", target_time_base:target_stat_base, sep = " ", na.rm = TRUE) %>%
#     dplyr::mutate(Location = dplyr::case_when(is.na(geo_description) ~ "See TMDL",
#                                               TRUE ~ geo_description),
#                   field_parameter = dplyr::if_else(is.na(field_parameter),
#                                                    TMDL_pollutant,
#                                                    field_parameter),
#                   target = dplyr::case_when(is.na(target_value) ~ "See TMDL",
#                                             target_type == "percent reduction" ~ paste(target_value, target_units, "reduction"),
#                                             TRUE ~ paste(target_value, target_units)),
#                   tmdl_period = dplyr::case_when(is.na(season_start) | is.na(season_end) ~ "See TMDL",
#                                                  TRUE ~ paste(season_start,"-", season_end))) %>%
#     dplyr::select(Location,
#                   "Field Parameter" = field_parameter,
#                   "TMDL Target" = target,
#                   "Statistical Base" = stat_base,
#                   "Target Period" = tmdl_period,
#                   "TMDL" = TMDL_name) %>%
#     dplyr::distinct() %>%
#     dplyr::arrange(Location, "TMDL Pollutant")


#- Render AU GNIS table ----------------------------------------------------

au_gnis_data <- fau_gnis %>%
    dplyr::select("Assessment Unit GNIS ID" = AU_GNIS,
                  "Assessment Unit GNIS Name" = AU_GNIS_Name,
                  "303(d) Parameter Addressed" = TMDL_wq_limited_parameter,
                  "TMDL Pollutant" = TMDL_pollutant,
                  "TMDL Scope" = TMDL_scope,
                  "Fish Use Period" = Period,
                  "Percent GNIS Assessment Unit Addressed by TMDL" = TMDL_AU_GNIS_Percent,
                  "TMDL" = TMDL_name,
                  "EPA Action ID" = action_id)

DT::datatable(data = shiny::isolate(au_gnis_data),
              selection = "none",
              # options = list(lengthChange = FALSE,
              #                searching = FALSE,
              #                searchable = FALSE,
              #                scrollY = 400,
              #                scrollX = TRUE,
              #                scroller = TRUE,
              #                #autoWidth = TRUE,
              #                columnDefs = list(list(width = '10%',
              #                                       targets = c(1:9)))),
              class = "nowrap cell-border hover stripe",
              rownames = FALSE)

#- Render AU table ---------------------------------------------------------

au_data <- fau %>%
    dplyr::arrange(AU_ID, TMDL_wq_limited_parameter, TMDL_pollutant) %>%
    dplyr::select("Assessment Unit ID" = AU_ID,
                  "Assessment Unit Name" = AU_Name,
                  "Assessment Unit Description" = AU_Description,
                  "303(d) Parameter Addressed" = TMDL_wq_limited_parameter,
                  "TMDL Pollutant" = TMDL_pollutant,
                  "TMDL Scope" = TMDL_scope,
                  "Fish Use Period" = Period,
                  "Percent Assessment Unit Addressed by TMDL" = TMDL_AU_Percent,
                  "TMDL" = TMDL_name,
                  "EPA Action ID" = action_id)


DT::datatable(data = shiny::isolate(au_data),
              selection = "none",
              # options = list(lengthChange = FALSE,
              #                searching = FALSE,
              #                searchable = FALSE,
              #                scrollY = 400,
              #                scrollX = TRUE,
              #                scroller = TRUE,
              #                #autoWidth = TRUE,
              #                columnDefs = list(list(width = '10%',
              #                                       targets = c(1:9)))),
              class = "nowrap cell-border hover stripe",
              rownames = FALSE)



# Render Target table -----------------------------------------------------


DT::datatable(data = shiny::isolate(target_data),
              selection = "none",
              options = list(dom = "t",
                             ordering = FALSE,
                             paging = FALSE,
                             searching = FALSE,
                             headerCallback = htmlwidgets::JS(headerCallbackRemoveHeaderFooter)),
              callback = htmlwidgets::JS(
                "$('table.dataTable.no-footer').css('border-bottom', 'none');"
              ),
              class = 'row-border',
              escape = FALSE,
              rownames = FALSE,
              filter = "none",
              width = 500)

# -Download --------------------------------------------------------------------

filename = paste0("TMDL_query_result_", Sys.Date(),".xlsx")

openxlsx::write.xlsx(list(TMDL_actions = action_data,
                          TMDL_Pollutant_Targets = target_data,
                          Assessment_Units = au_data,
                          GNIS_Assessment_Units = au_gnis_data),
                     file = filename,
                     colWidths = "auto",
                     firstActiveRow = c(2,2,2,2),
                     firstRow = c(TRUE,TRUE,TRUE,TRUE),
                     rowNames = c(FALSE, FALSE, FALSE, FALSE),
                     borders = "rows",
                     startCol = c(1,1,1,1), startRow = c(1,1,1,1),
                     headerStyle = openxlsx::createStyle(fgFill = "#000000",
                                                         halign = "LEFT",
                                                         textDecoration = "Bold",
                                                         wrapText = TRUE,
                                                         border = "Bottom",
                                                         fontColour = "white",
                                                         fontName = "Arial",
                                                         fontSize = 10))


