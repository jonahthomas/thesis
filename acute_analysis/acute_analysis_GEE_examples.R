# MVPA GEE

gee_data <- final_data %>% 
  select(ax_mvpa, ID, condition, gender, age) %>%
  na.omit() %>%
  mutate(
    ID = as.double(dense_rank(ID)), 
    condition = if_else(condition == "control", 0, 1),
    gender = dplyr::if_else(gender == "Male", 0, 1)
  ) 

form <- formula(ax_mvpa ~ condition + gender + age)
gee_ar1 <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "ar1")
summary(gee_ar1)

gee_unstructured <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "unstructured")
summary(gee_unstructured)
geepack::QIC(gee_unstructured, gee_ar1)

# mean glucose with Axivity GEE

gee_data <- final_data %>%
  select(Mean, ID, dur_spt_sleep_min, dur_day_total_IN_min, dur_day_total_LIG_min, ax_mvpa, condition, VO2max, gender, age, body_fat_percent) %>%
  na.omit() %>%
  mutate(
    ID = as.double(dense_rank(ID)), 
    condition = if_else(condition == "control", 0, 1),
    gender = dplyr::if_else(gender == "Male", 0, 1)
  ) 

form <- formula(Mean ~ dur_spt_sleep_min + dur_day_total_IN_min + dur_day_total_LIG_min + ax_mvpa + condition + gender + age + body_fat_percent + VO2max)
gee_ar1 <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "ar1")
summary(gee_ar1)

gee_unstructured <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "unstructured")
summary(gee_unstructured)
geepack::QIC(gee_unstructured, gee_ar1)

# mean glucose with bouted MVPA GEE

gee_data <- final_data %>%
  select(Mean, ID, dur_spt_sleep_min, dur_day_total_IN_min, dur_day_total_LIG_min, dur_day_MVPA_bts_1_2_min, dur_day_MVPA_bts_2_5_min, dur_day_MVPA_bts_5_10_min, dur_day_MVPA_bts_10_min, condition, VO2max, gender, age, body_fat_percent) %>%
  na.omit() %>%
  mutate(
    ID = as.double(dense_rank(ID)), 
    condition = if_else(condition == "control", 0, 1),
    gender = dplyr::if_else(gender == "Male", 0, 1)
  ) 

form <- formula(Mean ~ dur_spt_sleep_min + dur_day_total_IN_min + dur_day_total_LIG_min + dur_day_MVPA_bts_1_2_min + dur_day_MVPA_bts_2_5_min + dur_day_MVPA_bts_5_10_min + dur_day_MVPA_bts_10_min + condition + VO2max + gender + age + body_fat_percent)
gee_ar1 <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "ar1")
summary(gee_ar1)

gee_unstructured <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "unstructured")
summary(gee_unstructured)
geepack::QIC(gee_unstructured, gee_ar1)

# Mean glucose with Fitbit GEE

gee_data <- final_data %>%
  select(Mean, ID, Duration, sedentary_time.y, light_time, mvpa_time.y, condition, VO2max, gender, age, body_fat_percent) %>%
  na.omit() %>%
  mutate(
    ID = as.double(dense_rank(ID)), 
    sedentary_minutes = as.numeric(sedentary_time.y),
    light_minutes = as.numeric(light_time), 
    mvpa_time = mvpa_time.y,
    condition = if_else(condition == "control", 0, 1),
    gender = dplyr::if_else(gender == "Male", 0, 1)
  ) 

form <- formula(Mean ~ Duration + sedentary_minutes + light_minutes + mvpa_time + condition + VO2max + gender + age + body_fat_percent)
gee_ar1 <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "ar1")
summary(gee_ar1)

gee_unstructured <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "unstructured")
summary(gee_unstructured)
geepack::QIC(gee_unstructured, gee_ar1)

# Mean glucose with bouted Fitbit MVPA GEE

gee_data <- final_data %>%
  select(Mean, ID, Duration, sedentary_time.y, light_time, sporadic, snack, longer, bouted, condition, VO2max, gender, age, body_fat_percent) %>%
  na.omit() %>%
  mutate(
    ID = as.double(dense_rank(ID)), 
    sedentary_minutes = as.numeric(sedentary_time.y),
    light_minutes = as.numeric(light_time), 
    condition = if_else(condition == "control", 0, 1),
    gender = dplyr::if_else(gender == "Male", 0, 1)
  ) 

form <- formula(Mean ~ Duration + sedentary_minutes + light_minutes + sporadic + snack + longer + bouted + condition + VO2max + gender + age + body_fat_percent)
gee_ar1 <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "ar1")
summary(gee_ar1)

gee_unstructured <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "unstructured")
summary(gee_unstructured)
geepack::QIC(gee_unstructured, gee_ar1)

# Intentional vs unintentional GEE analysis

gee_data <- IntentionalUnintentionalData %>%
  select(ID, mean, Intentional, mean_hr, duration, time_since_last_food) %>%
  mutate(
    mean = as.numeric(mean),
    mean_hr = as.numeric(mean_hr),
    duration = as.numeric(duration),
    time_since_last_food = as.numeric(time_since_last_food)
  ) %>%
  na.omit() %>%
  mutate(
    ID = as.double(dense_rank(ID))
  )

gee_data[sapply(gee_data, is.infinite)] <- NA

gee_data <- gee_data %>% na.omit()

form <- formula(mean ~ Intentional + mean_hr + duration + time_since_last_food)
gee_ar1 <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "ar1")
summary(gee_ar1)

gee_unstructured <- geepack::geeglm(form, data = gee_data, id = ID, family = "gaussian", corstr = "unstructured")
summary(gee_unstructured)
geepack::QIC(gee_unstructured, gee_ar1)