# saves data files for app.

library(dplyr)
library(tidyr)
library(vroom)
library(odeqtmdl)

# tmdl_reaches -----------------------------------------------------------------

# tmdl_reaches <- odeqtmdl::tmdl_reaches() %>%
#   dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name", "citation_abbreviated")],
#                    by = "action_id") %>%
#   dplyr::left_join(odeqtmdl::tmdl_parameters[, c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
#                    by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
#   dplyr::select(any_of(c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant",
#                          "TMDL_scope", "Period", "geo_id", "HUC6_full", "HUC8_full",
#                          "GNIS_Name", "AU_ID", "AU_GNIS", "LengthKM",
#                          "TMDL_name", "citation_abbreviated", "TMDL_status")))
#
# saveRDS(tmdl_reaches, compress = TRUE, file = file.path("data", "tmdl_reaches_app.RDS"))

#vroom_write(x = tmdl_reaches, file = file.path("data", "tmdl_reaches_app_vroom.csv"))

tmdl_geo_id_app <- odeqtmdl::tmdl_reaches() %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name", "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters[, c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::filter(!is.na(geo_id)) %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  dplyr::select(any_of(c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant",
                         "TMDL_scope", "geo_id", "HUC6_full", "HUC8_full",
                         "AU_ID", "AU_Name", "AU_GNIS", "AU_GNIS_Name",
                         "TMDL_name", "citation_abbreviated", "TMDL_status"))) %>%
  dplyr::distinct()

save(tmdl_geo_id_app, file = file.path("data", "tmdl_geo_id_app.rda"))

# tmdl targets -----------------------------------------------------------------

tmdl_targets_app <- odeqtmdl::tmdl_geo_ids %>%
  dplyr::left_join(odeqtmdl::tmdl_targets, by = c("action_id", "geo_id")) %>%
  tidyr::unite("stat_base", tidyr::matches(c("target_time_base", "target_stat_base")), sep = " ", na.rm = TRUE)

save(tmdl_targets_app, file = file.path("data", "tmdl_targets_app.rda"))

# tmdl_au table ----------------------------------------------------------------

tmdl_au_app <- odeqtmdl::tmdl_au %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name",
                                              "TMDL_issue_date", "EPA_action_date",
                                              "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters,
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant"))

save(tmdl_au_app, file = file.path("data", "tmdl_au_app.rda"))

tmdl_au_gnis_app <- odeqtmdl::tmdl_au_gnis %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name", "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters,
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant"))

save(tmdl_au_gnis_app, file = file.path("data", "tmdl_au_gnis_app.rda"))

tmdl_au_gnis_LU <- tmdl_au_gnis_app %>%
  dplyr::filter(!is.na(AU_GNIS_Name)) %>%
  dplyr::select(AU_ID, AU_Name, AU_GNIS_Name) %>%
  dplyr::distinct()

save(tmdl_au_gnis_LU, file = file.path("data", "tmdl_au_gnis_LU.rda"))

# AU counts --------------------------------------------------------------------

# Number of TMDLs
AU_count_param <- tmdl_au_app %>%
  dplyr::filter(TMDL_scope == "TMDL" & TMDL_status == "Active") %>%
  dplyr::select(action_id, TMDL_wq_limited_parameter, AU_ID) %>%
  dplyr::distinct() %>%
  dplyr::group_by(action_id, TMDL_wq_limited_parameter) %>%
  dplyr::summarise(AU_count_total = dplyr::n())

# Number of TMDL actions
AU_count_actions1 <- AU_count_param %>%
  dplyr::group_by(action_id) %>%
  dplyr::summarise(AU_count_total = sum(AU_count_total))

# Number of TMDLs by action
AU_count_actions <- tmdl_au_app %>%
  dplyr::group_by(action_id) %>%
  dplyr::summarise(TMDL_wq_limited_parameter =
                     paste0(sort(unique(TMDL_wq_limited_parameter)), collapse = ", "),
                   TMDL_pollutant = paste0(sort(unique(TMDL_pollutant)), collapse = ", ")) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::left_join(AU_count_actions1, by = c("action_id"))

#save(AU_count_actions, file = file.path("data", "AU_count_actions.rda"))

# tmdl_actions -----------------------------------------------------------------

tmdl_status_comment <- odeqtmdl::tmdl_parameters %>%
  select(action_id, TMDL_status_comment) %>%
  drop_na(TMDL_status_comment) %>%
  group_by(action_id) %>%
  summarise(TMDL_status_comment = unique(TMDL_status_comment)) %>%
  distinct()


tmdl_status <- odeqtmdl::tmdl_parameters %>%
  select(action_id, TMDL_status) %>%
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
  dplyr::group_by(action_id, TMDL_wq_limited_parameter) %>%
  dplyr::summarise(TMDL_pollutant = paste0(sort(unique(TMDL_pollutant)), collapse = ", ")) %>%
  dplyr::distinct() %>%
  dplyr::left_join(AU_count_param, by = c("action_id", "TMDL_wq_limited_parameter"))

save(tmdl_parameters_app, file = file.path("data", "tmdl_parameters_app.rda"))


# tmdl_wla ---------------------------------------------------------------------

tmdl_pollu <- odeqtmdl::tmdl_parameters %>%
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
