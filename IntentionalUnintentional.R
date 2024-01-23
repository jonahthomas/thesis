#' IntentionalUnitentional
#'
#' @description A function to identify intentional activity snacks (from Fitbit event data) and
#' unintentional snacks from Fitbit heart rate data and calculate the 2 hour AUC, mean, standard
#' deviation and coefficient of variance from each snack.
#'
#' @param fitbit_event_path File path for the Fitbit event data download from the Snacktivity
#' dashboard
#' @param fitbit_pa_path File path to the Fitbit data downloaded from the Web API
#' @param glucose_path File path to the glucose data downloaded from Freestyle Libre View
#' @param age Age of participants in years
#' @param resting_hr Resting heart rate of the participant in bpm
#'
#' @return
#' @export
#'
#' @examples

IntentionalUnintentional <- function(fitbit_event_path, fitbit_pa_path, glucose_path, age, resting_hr){

  # set-up work

  age <- age
  resting_hr <- resting_hr
  max_hr <- round(211 - (0.64 * age_n), digits = 0)
  resting_hr <- resting_hr
  mod_hr <- round((max_hr - resting_hr) * 0.40 + resting_hr, digits = 0)
  vig_hr <- round((max_hr - resting_hr) * 0.60 + resting_hr, digits = 0)

  fitbit_event <- read.csv(fitbit_event_path)

  fitbit <- readxl::read_xlsx(fitbit_pa_path, sheet = 2)

  glucose <- read.csv(glucose_path, skip = 2) %>%
    dplyr::filter(Record.Type == 0) %>%
    dplyr::mutate(
      Device.Timestamp = as.POSIXct(Device.Timestamp, format = "%d-%m-%Y %H:%M")
    )

  food <- read.csv(glucose_path, skip = 2) %>%
    dplyr::filter(Non.numeric.Food == 1) %>%
    dplyr::mutate(
      Device.Timestamp = as.POSIXct(Device.Timestamp, format = "%d-%m-%Y %H:%M")
    )

  # intentional snacks

  # this is not done using "activity-snack-started" as it produces a different number of rows for some participants
  # only activity-snack-ended is used and the duration is subtratced from the end time to give the start time
  intentional_number <- fitbit_event %>%
    dplyr::filter(`Metric` == "activity-snack-ended") %>%
    dplyr::mutate(
      Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"),
      Record = as.numeric(lubridate::seconds(lubridate::ms(Record))),
      Date = Date - Record
    )

  intentional_duration <- fitbit_event %>%
    dplyr::filter(`Metric` == "activity-snack-ended") %>%
    dplyr::mutate(
      Record = as.numeric(lubridate::seconds(lubridate::ms(Record))),
      Record = ceiling(Record / 60)
    )

  intentional_number <- intentional_number %>%
    dplyr::mutate(
      Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"),
      Date_2hr = Date + (60 * 120)
    ) %>%
    cbind(intentional_duration$Record)

  intentional_number <- intentional_number %>%
    dplyr::mutate(
      snack_end = Date + intentional_duration$Record * 60
    )

  snacks <- list()

  for(i in 1:nrow(intentional_number)){
    snacks[[i]] <- fitbit %>% dplyr::filter(date_time >= intentional_number$Date[i] & date_time <= intentional_number$snack_end[i])
  }

  intentional_intensity <- list()

  for(i in 1:length(snacks)){
    intentional_intensity[[i]] <- snacks[[i]] %>% dplyr::summarise(
      intensity = mean(heart_rate, na.rm = TRUE)
    )
  }

  intentional <- list()

  for(i in 1:nrow(intentional_number)) {
    start <- intentional_number[i, 1]
    end <- intentional_number[i, 4]
    intentional[[i]] <- glucose %>%
      dplyr::filter(Device.Timestamp >= start & Device.Timestamp <= end)
  }

  auc <- list()

  for(i in 1:length(intentional)){
    auc[[i]] <- intentional[[i]] %>%
      dplyr::reframe(
        area = nrow(intentional[[i]])/60 * ((Historic.Glucose.mmol.L[2:length(Historic.Glucose.mmol.L)] + Historic.Glucose.mmol.L[1:length(Historic.Glucose.mmol.L) - 1])/2)
      ) %>%
      dplyr::reframe(
        auc = sum(area, na.rm = TRUE)
      )
  }

  other_metrics <- list()

  for(i in 1:length(intentional)){
    other_metrics[[i]] <- intentional[[i]] %>%
      dplyr::reframe(
        sd = sd(Historic.Glucose.mmol.L, na.rm = TRUE),
        mean = mean(Historic.Glucose.mmol.L, na.rm = TRUE),
        cv = sd/mean
      )
  }

  intentional <- cbind(intentional_number$Date,dplyr::bind_rows(auc), dplyr::bind_rows(other_metrics),
                       round(unlist(intentional_intensity), digits = 0), intentional_duration$Record) %>%
    setNames(c("date_time", "auc", "sd", "mean", "cv", "mean_hr", "duration"))

  nearest_food <- list()

  for(i in 1:nrow(intentional)){
    nearest_food[[i]] <- as.data.frame(max(food$Device.Timestamp[food$Device.Timestamp <= intentional$date_time[i]]))
  }

  nearest_food <- dplyr::bind_rows(nearest_food)

  intentional <- intentional %>%
    cbind(nearest_food$`max(food$Device.Timestamp[food$Device.Timestamp <= intentional$date_time[i]])`) %>%
    setNames(c("date_time", "auc", "sd", "mean", "cv", "mean_hr", "duration", "last_food")) %>%
    dplyr::mutate(
      time_since_last_food = as.numeric(round(date_time - last_food, digits = 0))
    )

  # unintentional snacks

  fitbit <- fitbit %>%
    dplyr::mutate(
      hr_mvpa = dplyr::if_else(heart_rate >= vig_hr, 2, dplyr::if_else(heart_rate >= mod_hr, 1, 0))
    )

  snacks <- rle(fitbit$hr_mvpa)

  bouts <- data.frame(lengths = snacks$lengths, values = snacks$values) %>%
    dplyr::filter(values == 1 | values == 2) %>%
    dplyr::mutate(
      sporadic = dplyr::if_else(lengths == 1, 1, 0),
      snack = dplyr::if_else(lengths >=2 & lengths <= 5, 1, 0),
      longer = dplyr::if_else(lengths >= 6 & lengths <= 10, 1, 0),
      bouted = dplyr::if_else(lengths >10, 1, 0)
    )

  unintentional_snacks <- fitbit %>%
    dplyr::mutate(
      lag_hr_mvpa = dplyr::lag(hr_mvpa)
    ) %>%
    dplyr::filter(hr_mvpa != lag_hr_mvpa) %>%
    dplyr::filter(hr_mvpa == 1 | hr_mvpa == 2) %>%
    cbind(bouts) %>%
    dplyr::mutate(
      detected_by = "fitbit"
    ) %>%
    dplyr::mutate(
      year = lubridate::year(date_time),
      month = lubridate::month(date_time),
      day = lubridate::day(date_time)
    )

  unintentional_number <- unintentional_snacks %>%
    dplyr::mutate(
      date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%S"),
      time_2hr = date_time + (60 * 120)
    ) %>%
    cbind(bouts$lengths)

  unintentional_number <- unintentional_number %>%
    dplyr::mutate(
      snack_end = date_time + unintentional_number$`bouts$lengths` * 60
    )

  snacks <- list()

  for(i in 1:nrow(unintentional_number)){
    snacks[[i]] <- fitbit[fitbit$date_time >= unintentional_number$date_time[i] & fitbit$date_time <= unintentional_number$snack_end[i], ]
    print(paste("file", i))
  }

  unintentional_intensity <- list()

  for(i in 1:length(snacks)){
    unintentional_intensity[[i]] <- snacks[[i]] %>% dplyr::summarise(
      intensity = mean(heart_rate, na.rm = TRUE)
    )
  }

  unintentional <- list()

  for(i in 1:nrow(unintentional_number)) {
    start <- unintentional_number[i, 1]
    end <- unintentional_number[i, 16]
    unintentional[[i]] <- glucose %>%
      dplyr::filter(Device.Timestamp >= start & Device.Timestamp <= end)
  }

  auc <- list()

  for(i in 1:length(unintentional)){
    auc[[i]] <- unintentional[[i]] %>%
      dplyr::reframe(
        area = nrow(unintentional[[i]])/60 * ((Historic.Glucose.mmol.L[2:length(Historic.Glucose.mmol.L)] + Historic.Glucose.mmol.L[1:length(Historic.Glucose.mmol.L) - 1])/2)
      ) %>%
      dplyr::reframe(
        auc = sum(area, na.rm = TRUE)
      )
  }

  other_metrics <- list()

  for(i in 1:length(unintentional)){
    other_metrics[[i]] <- unintentional[[i]] %>%
      dplyr::summarise(
        sd = sd(Historic.Glucose.mmol.L, na.rm = TRUE),
        mean = mean(Historic.Glucose.mmol.L, na.rm = TRUE),
        cv = sd/mean
      )
  }

  unintentional <- cbind(unintentional_number$date_time, dplyr::bind_rows(auc), dplyr::bind_rows(other_metrics),
                         round(unlist(unintentional_intensity), digits = 0),  unintentional_number$`bouts$lengths`) %>%
    setNames(c("date_time", "auc", "sd", "mean", "cv", "mean_hr", "duration"))


  nearest_food <- list()

  for(i in 1:nrow(unintentional)){
    nearest_food[[i]] <- as.data.frame(max(food$Device.Timestamp[food$Device.Timestamp <= unintentional$date_time[i]]))
  }

  nearest_food <- dplyr::bind_rows(nearest_food)

  unintentional <- unintentional %>%
    cbind(nearest_food$`max(food$Device.Timestamp[food$Device.Timestamp <= unintentional$date_time[i]])`) %>%
    setNames(c("date_time", "auc", "sd", "mean", "cv", "mean_hr", "duration", "last_food")) %>%
    dplyr::mutate(
      time_since_last_food = as.numeric(round(date_time - last_food, digits = 0))
    )

  return(list(intentional, unintentional))
}
