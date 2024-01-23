#' FitbitDailySummary
#'
#' @description A function to summarise Fitbit data downloaded from the Web API
#'
#' @param file_path file path to the Fitbit data downloaded from the Web API
#' @param age age of the participant in years
#' @param resting_hr resting heart rate of the participant in bpm
#'
#' @return List of 4 dataframes: a dataframe with each row representing when an activity
#' snack was performed, a dataframe of the number of each bout duration performed during each day,
#' a dataframe with the total number of minutes of sedentary time performed each day, a dataframe
#' with min, mean, standard deviation, max and count of sedentary bouts performed each day
#' @export
#'
#' @examples
#' 

FitbitDailySummary <- function(file_path, age, resting_hr) {
  sed <- readxl::read_xlsx(file_path, sheet = 1) %>%
    dplyr::mutate(
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date)
    )
  hr <- readxl::read_xlsx(file_path, sheet = 2)
  mets <- readxl::read_xlsx(file_path, sheet = 5)

  fitbit <- dplyr::inner_join(mets, hr, by = "date_time")

  max_hr <- round(211 - (0.64 * age), digits = 0)
  resting_hr <- resting_hr
  mod_hr <- round((max_hr - resting_hr) * 0.40 + resting_hr, digits = 0)
  sed_hr <- round((max_hr - resting_hr) * 0.30 + resting_hr, digits = 0)

  fitbit <- fitbit %>%
    dplyr::mutate(
      sedentary = dplyr::if_else(heart_rate < sed_hr, 1, 0),
      mets = mets / 10,
      met_mvpa = dplyr::if_else(mets >= 3.0, 1, 0),
      level_mvpa = dplyr::if_else(level == 2, 1, 0),
      hr_light = dplyr::if_else(heart_rate > sed_hr & heart_rate < mod_hr, 1, 0),
      hr_mvpa = dplyr::if_else(heart_rate >= mod_hr, 1, 0)
    )

  snacks <- rle(fitbit$hr_mvpa)

  bouts <- data.frame(lengths = snacks$lengths, values = snacks$values) %>%
    dplyr::filter(values == 1) %>%
    dplyr::mutate(
      sporadic = dplyr::if_else(lengths == 1, 1, 0),
      snack = dplyr::if_else(lengths >=2 & lengths <= 5, 1, 0),
      longer = dplyr::if_else(lengths >= 6 & lengths <= 10, 1, 0),
      bouted = dplyr::if_else(lengths >10, 1, 0)
    )

  mins <- data.frame(lengths = snacks$lengths, values = snacks$values) %>%
    dplyr::filter(values == 1) %>%
    dplyr::mutate(
      sporadic = dplyr::if_else(lengths == 1, lengths, 0),
      snack = dplyr::if_else(lengths >=2 & lengths <= 5, lengths, 0),
      longer = dplyr::if_else(lengths >= 6 & lengths <= 10, lengths, 0),
      bouted = dplyr::if_else(lengths >10, lengths, 0)
    )

  date <- fitbit %>%
    dplyr::mutate(
      lag_hr_mvpa = dplyr::lag(hr_mvpa)
    ) %>%
    dplyr::filter(hr_mvpa != lag_hr_mvpa) %>%
    dplyr::filter(hr_mvpa == 1) %>%
    cbind(bouts) %>%
    dplyr::mutate(
      detected_by = "fitbit"
    ) %>%
    dplyr::mutate(
      year = lubridate::year(date_time),
      month = lubridate::month(date_time),
      day = lubridate::day(date_time)
    )

  summary <- date %>%
    dplyr::group_by(year, month, day) %>%
    dplyr::summarise(dplyr::across(.cols = sporadic:bouted, ~sum(., na.rm = TRUE))) %>%
    dplyr::mutate(
      date = lubridate::make_date(year, month, day)
    )

  non_wear <- hr %>%
    dplyr::mutate(
      day = lubridate::day(date_time),
      month = lubridate::month(date_time),
      year = lubridate::year(date_time),
    ) %>%
    dplyr::group_by(day, month, year) %>%
    dplyr::summarise(
      n = 1440 - dplyr::n()
    )

  sedentary <- fitbit %>%
    dplyr::mutate(
      year = lubridate::year(date_time),
      month = lubridate::month(date_time),
      day = lubridate::day(date_time)
    ) %>%
    dplyr::group_by(year, month, day) %>%
    dplyr::summarise(
      sedentary_time = sum(sedentary, na.rm = TRUE),
      light_time = sum(hr_light, na.rm = TRUE),
      mvpa_time = sum(hr_mvpa, na.rm = TRUE)
    )

  sedentary_bouts <- rle(fitbit$sedentary)

  sedentary_bouts <- data.frame(lengths = sedentary_bouts$lengths, values = sedentary_bouts$values) %>%
    dplyr::filter(values == 1) %>%
    dplyr::slice(-1)

  sedentary_date <- fitbit %>%
    dplyr::mutate(
      year = lubridate::year(date_time),
      month = lubridate::month(date_time),
      day = lubridate::day(date_time)
    ) %>%
    dplyr::mutate(
      lag_sed = dplyr::lag(sedentary)
    ) %>%
    dplyr::filter(sedentary != lag_sed) %>%
    dplyr::filter(sedentary == 1) %>%
    cbind(sedentary_bouts)

  sedentary_bouts_summary <- sedentary_date %>%
    dplyr::group_by(year, month, day) %>%
    dplyr::summarise(
      min_duration = min(lengths, na.rm = TRUE),
      mean_duration = mean(lengths, na.rm = TRUE),
      sd_duration = sd(lengths, na.rm = TRUE),
      max_duration = max(lengths, na.rm = TRUE),
      number_bouts = dplyr::n()
    )

  x <- list()

  for (i in 1:length(snacks$lengths)) {
    x[[i]] <- data.frame(length = rep(snacks$lengths[i], each = snacks$lengths[i]))
  }

  length <- dplyr::bind_rows(x)

  x <- list()

  for (i in 1:length(snacks$lengths)) {
    x[[i]] <- data.frame(value = rep(snacks$values[i], each = snacks$lengths[i]))
  }

  value <- dplyr::bind_rows(x)

  fitbit <- cbind(fitbit, length)
  fitbit <- cbind(fitbit, value)

  return(list(date, summary, sedentary, sedentary_bouts_summary, sed, non_wear, fitbit))
}
