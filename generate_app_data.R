# saves data files for app.

library(dplyr)
library(tidyr)
library(vroom)
library(DBI)
library(duckdb)
library(odeqtmdl)
library(readxl)

# Column Descriptions ----------------------------------------------------------

col_desc_app <- read_xlsx("column_definitions.xlsx", sheet = "Column_Descriptions")

save(col_desc_app, file = file.path("data", "col_desc_app.rda"))

# tmdl_reaches -----------------------------------------------------------------

# tmdl_reaches <- odeqtmdl::tmdl_reaches() %>%
#   dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name", "citation_abbreviated")],
#                    by = "action_id") %>%
#   dplyr::left_join(odeqtmdl::tmdl_parameters[, c("action_id", "TMDL_parameter", "TMDL_pollutant", "TMDL_status")],
#                    by = c("action_id", "TMDL_parameter", "TMDL_pollutant")) %>%
#   dplyr::select(any_of(c("action_id", "TMDL_parameter", "TMDL_pollutant",
#                          "TMDL_scope", "Period", "geo_id", "HUC6_full", "HUC8_full",
#                          "GNIS_Name", "AU_ID", "AU_GNIS", "LengthKM",
#                          "TMDL_name", "citation_abbreviated", "TMDL_status")))
#
# saveRDS(tmdl_reaches, compress = TRUE, file = file.path("data", "tmdl_reaches_app.RDS"))

#vroom_write(x = tmdl_reaches, file = file.path("data", "tmdl_reaches_app_vroom.csv"))


# read using duckdb

package_path <- "C:/Users/rmichie/OneDrive - Oregon/GitHub/odeqtmdl"

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(package_path, "data_raw", "tmdl_reaches.duckdb"))
tmdl_reaches <- DBI::dbReadTable(con, "tmdl_reaches")
duckdb::dbDisconnect(con, shutdown = TRUE)

tmdl_geo_id_app <- tmdl_reaches %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name",
                                              "TMDL_issue_date", "EPA_action_date",
                                              "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::filter(!is.na(geo_id)) %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  dplyr::select(any_of(c("action_id", "TMDL_parameter", "TMDL_pollutant",
                         "TMDL_scope", "geo_id", "HUC6_full", "HUC8_full",
                         "AU_ID", "AU_Name", "AU_GNIS", "AU_GNIS_Name",
                         "TMDL_name", "TMDL_issue_date", "EPA_action_date",
                         "citation_abbreviated", "TMDL_status"))) %>%
  dplyr::distinct()

save(tmdl_geo_id_app, file = file.path("data", "tmdl_geo_id_app.rda"))

# tmdl targets -----------------------------------------------------------------

tmdl_targets_app <- odeqtmdl::tmdl_geo_ids %>%
  dplyr::left_join(odeqtmdl::tmdl_targets, by = c("action_id", "geo_id")) %>%
  tidyr::unite("stat_base", tidyr::matches(c("target_time_base", "target_stat_base")), sep = " ", na.rm = TRUE) %>%
  dplyr::mutate(URL = paste0("https://www.arcgis.com/apps/webappviewer/index.html?id=d3c176c743c042b7a92f91070ddfaa5c&query=1925b4926d1-layer-20,geo_id,",geo_id))

save(tmdl_targets_app, file = file.path("data", "tmdl_targets_app.rda"))

# tmdl_au table ----------------------------------------------------------------

tmdl_au_app <- odeqtmdl::tmdl_au %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name",
                                              "TMDL_issue_date", "EPA_action_date",
                                              "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  dplyr::mutate(URL = case_when(TMDL_status %in% c("Active", "Not Active") ~
                                  paste0("https://www.arcgis.com/apps/webappviewer/index.html?id=d3c176c743c042b7a92f91070ddfaa5c&query=d37ee6195856496a85e09dba11c183a3,AU_ID,",AU_ID),
                                TMDL_status == "In Development" ~
                                  paste0("https://www.arcgis.com/apps/webappviewer/index.html?id=d3c176c743c042b7a92f91070ddfaa5c&query=1925b43a169-layer-15,AU_ID,",AU_ID),
                                TRUE ~ NA_character_))

save(tmdl_au_app, file = file.path("data", "tmdl_au_app.rda"))

tmdl_au_gnis_app <- odeqtmdl::tmdl_au_gnis %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name",
                                              "TMDL_issue_date", "EPA_action_date",
                                              "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")"))

save(tmdl_au_gnis_app, file = file.path("data", "tmdl_au_gnis_app.rda"))

tmdl_au_gnis_LU <- tmdl_au_gnis_app %>%
  dplyr::filter(!is.na(AU_GNIS_Name)) %>%
  dplyr::select(AU_ID, AU_Name, AU_GNIS_Name) %>%
  dplyr::distinct()

save(tmdl_au_gnis_LU, file = file.path("data", "tmdl_au_gnis_LU.rda"))

# AU counts and Length------------------------------------------------------------

# Number of TMDLs
AU_count_param <- tmdl_au_app %>%
  dplyr::filter(TMDL_scope == "TMDL" & TMDL_status %in% c("Active", "In Development")) %>%
  dplyr::group_by(action_id, TMDL_parameter, AU_ID) %>%
  dplyr::summarise(TMDL_length_km = max(TMDL_length_km, na.rm = TRUE)) %>%
  dplyr::select(action_id, TMDL_parameter, AU_ID, TMDL_length_km) %>%
  dplyr::distinct() %>%
  dplyr::group_by(action_id, TMDL_parameter) %>%
  dplyr::summarise(AU_count_total = dplyr::n(),
                   AU_TMDL_length_km = sum(TMDL_length_km))

# Number of TMDL actions
AU_count_actions1 <- AU_count_param %>%
  dplyr::group_by(action_id) %>%
  dplyr::summarise(AU_count_total = sum(AU_count_total),
                   AU_TMDL_length_km = sum(AU_TMDL_length_km))

# Number of TMDLs by action
AU_count_actions <- tmdl_au_app %>%
  dplyr::group_by(action_id) %>%
  dplyr::summarise(TMDL_parameter =
                     paste0(sort(unique(TMDL_parameter)), collapse = ", "),
                   TMDL_pollutant = paste0(sort(unique(TMDL_pollutant)), collapse = ", ")) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::left_join(AU_count_actions1, by = c("action_id"))

#save(AU_count_actions, file = file.path("data", "AU_count_actions.rda"))

# tmdl_actions -----------------------------------------------------------------

# tmdl_status_comment <- odeqtmdl::tmdl_parameters %>%
#   select(action_id, TMDL_status_comment) %>%
#   drop_na(TMDL_status_comment) %>%
#   group_by(action_id) %>%
#   summarise(TMDL_status_comment = unique(TMDL_status_comment)) %>%
#   distinct()

tmdl_status_comment <- odeqtmdl::tmdl_actions %>%
  select(action_id, TMDL_status_comment = TMDL_comment) %>%
  drop_na(TMDL_status_comment) %>%
  distinct()


tmdl_status <- odeqtmdl::tmdl_au %>%
  select(action_id, TMDL_status) %>%
  distinct() %>%
  group_by(action_id) %>%
  summarise(TMDL_status = paste0(sort(unique(TMDL_status)), collapse = " and ")) %>%
  distinct() %>%
  ungroup() %>%
  mutate(TMDL_status = if_else(TMDL_status == "Active and Not Active", "Some parameters Active", TMDL_status)) %>%
  left_join(tmdl_status_comment, by = "action_id")

tmdl_actions_app <- odeqtmdl::tmdl_actions %>%
  dplyr::left_join(tmdl_status, by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  dplyr::select(action_id, TMDL_name, TMDL_issue_date, EPA_action_date, TMDL_status, TMDL_status_comment, citation_abbreviated, URL) %>%
  #dplyr::mutate(URL = dplyr::if_else(is.na(URL),
  #                                   "https://www.oregon.gov/deq/wq/tmdls/Pages/default.aspx",
  #                                   URL)) %>%
  dplyr::left_join(AU_count_actions, by = "action_id")

save(tmdl_actions_app, file = file.path("data", "tmdl_actions_app.rda"))

# tmdl parameters --------------------------------------------------------------

tmdl_parameters_app <- odeqtmdl::tmdl_parameters %>%
  dplyr::group_by(action_id, TMDL_parameter) %>%
  dplyr::summarise(TMDL_pollutant = paste0(sort(unique(TMDL_pollutant)), collapse = ", ")) %>%
  dplyr::distinct() %>%
  dplyr::left_join(AU_count_param, by = c("action_id", "TMDL_parameter"))

save(tmdl_parameters_app, file = file.path("data", "tmdl_parameters_app.rda"))


# tmdl_wla ---------------------------------------------------------------------

tmdl_pollu <- odeqtmdl::tmdl_au %>%
  dplyr::select(action_id, TMDL_pollutant, TMDL_status) %>%
  dplyr::distinct()

tmdl_wla_app <- odeqtmdl::tmdl_wla %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name",
                                              "TMDL_issue_date", "EPA_action_date",
                                              "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  dplyr::left_join(tmdl_pollu,
                   by = c("action_id", "TMDL_pollutant"))

save(tmdl_wla_app, file = file.path("data", "tmdl_wla_app.rda"))
