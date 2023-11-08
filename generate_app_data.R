# saves data files for app.

library(dplyr)
library(tidyr)
library(vroom)
library(odeqtmdl)

# tmdl_reaches -----------------------------------------------------------------

tmdl_reaches <- odeqtmdl::tmdl_reaches() %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name", "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters[, c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::select(any_of(c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant",
                         "TMDL_scope", "Period", "geo_id", "HUC6_full", "HUC8_full",
                         "AU_ID", "AU_GNIS_Name", "LengthKM",
                         "TMDL_name", "citation_abbreviated", "TMDL_status")))

saveRDS(tmdl_reaches, compress = TRUE, file = file.path("inst", "extdata", "tmdl_reaches_app.RDS"))

#vroom_write(x = tmdl_reaches, file = file.path("inst", "extdata", "tmdl_reaches_vroom.csv"))

# tmdl targets -----------------------------------------------------------------

tmdl_targets_app <- odeqtmdl::tmdl_geo_ids %>%
  dplyr::left_join(odeqtmdl::tmdl_targets, by = c("action_id", "geo_id")) %>%
  tidyr::unite("stat_base", target_time_base:target_stat_base, sep = " ", na.rm = TRUE)

save(tmdl_targets_app, file = file.path("data", "tmdl_targets_app.rda"))

# tmdl_au table ----------------------------------------------------------------

tmdl_au_app <- odeqtmdl::tmdl_au %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name",
                                              "TMDL_issue_date", "EPA_action_date",
                                              "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters,
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant"))

save(tmdl_au_app, file = file.path("data", "tmdl_au_app.rda"))

tmdl_au_gnis_app <- odeqtmdl::tmdl_au_gnis %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[, c("action_id", "TMDL_name", "citation_abbreviated")],
                   by = "action_id") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters,
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant"))

save(tmdl_au_gnis_app, file = file.path("data", "tmdl_au_gnis_app.rda"))

# AU counts --------------------------------------------------------------------

AU_count_param <- tmdl_au_app %>%
  dplyr::filter(TMDL_scope == "TMDL") %>%
  dplyr::select(action_id, TMDL_wq_limited_parameter, AU_ID) %>%
  dplyr::distinct() %>%
  dplyr::group_by(action_id, TMDL_wq_limited_parameter) %>%
  dplyr::summarise(AU_count_total = dplyr::n())

AU_count_actions1 <- AU_count_param %>%
  dplyr::group_by(action_id) %>%
  dplyr::summarise(AU_count_total = sum(AU_count_total))

AU_count_actions <- tmdl_au_app %>%
  dplyr::group_by(action_id) %>%
  dplyr::summarise(TMDL_wq_limited_parameter =
                     paste0(sort(unique(TMDL_wq_limited_parameter)), collapse = ", "),
                   TMDL_pollutant = paste0(sort(unique(TMDL_pollutant)), collapse = ", ")) %>%
  dplyr::left_join(AU_count_actions1, by = c("action_id"))

#save(AU_count_actions, file = file.path("data", "AU_count_actions.rda"))

# tmdl_actions -----------------------------------------------------------------

tmdl_actions_app <- odeqtmdl::tmdl_actions %>%
  dplyr::select(action_id, TMDL_name, TMDL_issue_date, EPA_action_date, citation_abbreviated) %>%
  dplyr::left_join(AU_count_actions, by = "action_id")

save(tmdl_actions_app, file = file.path("data", "tmdl_actions_app.rda"))

# tmdl parameters --------------------------------------------------------------

tmdl_parameters_app <- odeqtmdl::tmdl_parameters %>%
  dplyr::group_by(action_id, TMDL_wq_limited_parameter) %>%
  dplyr::summarise(TMDL_pollutant = paste0(sort(unique(TMDL_pollutant)), collapse = ", ")) %>%
  dplyr::distinct() %>%
  dplyr::left_join(AU_count_param, by = c("action_id", "TMDL_wq_limited_parameter"))

save(tmdl_parameters_app, file = file.path("data", "tmdl_parameters_app.rda"))
