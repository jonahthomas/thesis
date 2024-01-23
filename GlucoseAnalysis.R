#' GlucoseAnalysis
#'
#' @description a wrapper around the iglu::all_metrics function to correctly format the data and
#' provide daily metrics
#'
#' @param file_path A file path to the glucose data - expected format is a download from the
#' Freestyle Libre View platform
#'
#' @return a dataframe with glucose metrics for each day which data is present
#' @export
#'
#' @examples

GlucoseAnalysis <- function(file_path){
  data <- read.csv(file_path, skip = 2)

  first_date <- as.POSIXct(dplyr::first(data$Device.Timestamp), "%d-%m-%Y", tz = "")

  data <- data %>%
    dplyr::filter(!as.POSIXct(Device.Timestamp, "%d-%m-%Y", tz = "") %in% first_date)

  trimmed <- data %>%
    dplyr::filter(Record.Type == 0) %>%
    dplyr::select(Device.Timestamp, Historic.Glucose.mmol.L) %>%
    dplyr::mutate(
      id = 1
    ) %>%
    dplyr::rename("time" = Device.Timestamp, "gl" = Historic.Glucose.mmol.L) %>%
    dplyr::mutate(gl = as.numeric(gl) * 18) %>% # convert to mg/dl
    dplyr::mutate(time = as.POSIXct(time, "%d-%m-%Y %H:%M", tz = "")) %>%
    dplyr::relocate(id, time, gl)

  analysed <- iglu::all_metrics(trimmed)

  daily <- trimmed %>%
    dplyr::mutate(
      year = lubridate::year(time),
      month = lubridate::month(time),
      day = lubridate::day(time),
      id = paste(year, month, day, sep = "-")
    ) %>%
    dplyr::select(id, time, gl)

  daily_analysed <- iglu::all_metrics(daily)

  daily_analysed <- daily_analysed %>%
    dplyr::mutate(
      date = as.POSIXct(id, format = "%Y-%m-%d"),
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date)
    )

  return(daily_analysed)
}
