# creating iso-metric log ratios 

final_data <- final_data %>%
  dplyr::mutate(
    comp_sed = `Sedentary.Minutes.Valid.Days.Total.Per.Day`/`Wear.Minutes.Valid.Days.Total.Per.Day`,
    comp_light = `Light.Minutes.Valid.Days.Total.Per.Day`/`Wear.Minutes.Valid.Days.Total.Per.Day`,
    comp_mvpa = `MVPA.Minutes.Valid.Days.Total.Per.Day`/`Wear.Minutes.Valid.Days.Total.Per.Day`,
    z1_sed = sqrt(2/3) * log(Sedentary.Minutes.Valid.Days.Total.Per.Day / sqrt(Light.Minutes.Valid.Days.Total.Per.Day * MVPA.Minutes.Valid.Days.Total.Per.Day)),
    z2_sed = sqrt(1/2) * log(Light.Minutes.Valid.Days.Total.Per.Day / MVPA.Minutes.Valid.Days.Total.Per.Day),
    z1_light = sqrt(2/3) * log(Light.Minutes.Valid.Days.Total.Per.Day / sqrt(Sedentary.Minutes.Valid.Days.Total.Per.Day * MVPA.Minutes.Valid.Days.Total.Per.Day)),
    z2_light = sqrt(1/2) * log(MVPA.Minutes.Valid.Days.Total.Per.Day / Sedentary.Minutes.Valid.Days.Total.Per.Day),
    z1_mvpa = sqrt(2/3) * log(MVPA.Minutes.Valid.Days.Total.Per.Day / sqrt(Sedentary.Minutes.Valid.Days.Total.Per.Day * Light.Minutes.Valid.Days.Total.Per.Day)),
    z2_mvpa = sqrt(1/2) * log(Sedentary.Minutes.Valid.Days.Total.Per.Day / Light.Minutes.Valid.Days.Total.Per.Day)
  )

# three part unadjusted

weight_sed_model <- lm(harmonised_weight ~ z1_sed + z2_sed, final_data)
weight_light_model <- lm(harmonised_weight ~ z1_light + z2_light, final_data)
weight_mvpa_model <- lm(harmonised_weight ~ z1_mvpa + z2_mvpa, final_data)
weight_sed_model_sum <- summary(weight_sed_model)
weight_light_model_sum <- summary(weight_light_model)
weight_mvpa_model_sum <- summary(weight_mvpa_model)

weight_summary <- rbind(weight_sed_model_sum$coefficients[2, ], weight_light_model_sum$coefficients[2, ], weight_mvpa_model_sum$coefficients[2, ])
row.names(weight_summary) <- c("sed", "light", "mvpa")
weight_summary

# three part adjusted

weight_sed_model_ad <- lm(harmonised_weight ~ z1_sed + z2_sed + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_light_model_ad <- lm(harmonised_weight ~ z1_light + z2_light + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_mvpa_model_ad <- lm(harmonised_weight ~ z1_mvpa + z2_mvpa + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_sed_model_ad_sum <- summary(weight_sed_model_ad)
weight_light_model_ad_sum <- summary(weight_light_model_ad)
weight_mvpa_model_ad_sum <- summary(weight_mvpa_model_ad)

weight_ad_summary <- rbind(weight_sed_model_ad_sum$coefficients[2, ], weight_light_model_ad_sum$coefficients[2, ], weight_mvpa_model_ad_sum$coefficients[2, ])
row.names(weight_ad_summary) <- c("sed", "light", "mvpa")
weight_ad_summary

# four part unadjusted 

weight_sed_model <- lm(harmonised_weight ~ z1_sed_4 + z2_sed_4 + z3_sed_4, final_data)
weight_light_model <- lm(harmonised_weight ~ z1_light_4 + z2_light_4 + z3_light_4, final_data)
weight_lessthan10_model <- lm(harmonised_weight ~ z1_less10_4 + z2_less10_4 + z3_less10_4, final_data)
weight_morethan10_model <- lm(harmonised_weight ~ z1_more10_4 + z2_more10_4 + z3_more10_4, final_data)
weight_sed_model_sum <- summary(weight_sed_model)
weight_light_model_sum <- summary(weight_light_model)
weight_lessthan10_model_sum <- summary(weight_lessthan10_model)
weight_morethan10_model_sum <- summary(weight_morethan10_model)

weight_summary <- rbind(weight_sed_model_sum$coefficients[2, ], weight_light_model_sum$coefficients[2, ], weight_lessthan10_model_sum$coefficients[2, ], weight_morethan10_model_sum$coefficients[2, ])
row.names(weight_summary) <- c("sed", "light", "mvpa < 10", "mvpa > 10")
weight_summary

# four part adjusted 

weight_sed_model_ad <- lm(harmonised_weight ~ z1_sed_4 + z2_sed_4 + z3_sed_4 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_light_model_ad <- lm(harmonised_weight ~ z1_light_4 + z2_light_4 + z3_light_4 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_lessthan10_model_ad <- lm(harmonised_weight ~ z1_less10_4 + z2_less10_4 + z3_less10_4 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_morethan10_model_ad <- lm(harmonised_weight ~ z1_more10_4 + z2_more10_4 + z3_more10_4 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_sed_model_ad_sum <- summary(weight_sed_model_ad)
weight_light_model_ad_sum <- summary(weight_light_model_ad)
weight_lessthan10_model_ad_sum <- summary(weight_lessthan10_model_ad)
weight_morethan10_model_ad_sum <- summary(weight_morethan10_model_ad)

weight_ad_summary <- rbind(weight_sed_model_ad_sum$coefficients[2, ], weight_light_model_ad_sum$coefficients[2, ],
                           weight_lessthan10_model_ad_sum$coefficients[2, ],
                           weight_morethan10_model_ad_sum$coefficients[2, ])
row.names(weight_ad_summary) <- c("sed", "light", "mvpa < 10", "mvpa > 10")
weight_ad_summary

# six part unadjusted 

weight_sed_model <- lm(harmonised_weight ~ z1_sed_6 + z2_sed_6 + z3_sed_6 + z4_sed_6 + z5_sed_6, final_data)
weight_light_model <- lm(harmonised_weight ~ z1_light_6 + z2_light_6 + z3_light_6 + z4_light_6 + z5_light_6, final_data)
weight_lesssnack_model <- lm(harmonised_weight ~ z1_lesssnack_6 + z2_lesssnack_6 + z3_lesssnack_6 + z4_lesssnack_6 + z5_lesssnack_6, final_data)
weight_snack_model <- lm(harmonised_weight ~ z1_snack_6 + z2_snack_6 + z3_snack_6 + z4_snack_6 + z5_snack_6, final_data)
weight_moresnack_model <- lm(harmonised_weight ~ z1_moresnack_6 + z2_moresnack_6 + z3_moresnack_6 + z4_moresnack_6 + z5_moresnack_6, final_data)
weight_bout_model <- lm(harmonised_weight ~ z1_bout_6 + z2_bout_6 + z3_bout_6 + z4_bout_6 + z5_bout_6, final_data)
weight_sed_model_sum <- summary(weight_sed_model)
weight_light_model_sum <- summary(weight_light_model)
weight_lesssnack_model_sum <- summary(weight_lesssnack_model)
weight_snack_model_sum <- summary(weight_snack_model)
weight_moresnack_model_sum <- summary(weight_moresnack_model)
weight_bout_model_sum <- summary(weight_bout_model)

weight_summary <- rbind(weight_sed_model_sum$coefficients[2, ], weight_light_model_sum$coefficients[2, ], weight_lesssnack_model_sum$coefficients[2, ], weight_snack_model_sum$coefficients[2, ], weight_moresnack_model_sum$coefficients[2, ], weight_bout_model_sum$coefficients[2, ])
row.names(weight_summary) <- c("sed", "light", "lesssnack", "snack", "moresnack", "bout")
weight_summary

# six part adjusted

weight_sed_model_ad <- lm(harmonised_weight ~ z1_sed_6 + z2_sed_6 + z3_sed_6 + z4_sed_6 + z5_sed_6 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_light_model_ad <- lm(harmonised_weight ~ z1_light_6 + z2_light_6 + z3_light_6 + z4_light_6 + z5_light_6 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_lesssnack_model_ad <- lm(harmonised_weight ~ z1_lesssnack_6 + z2_lesssnack_6 + z3_lesssnack_6 + z4_lesssnack_6 + z5_lesssnack_6 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_snack_model_ad <- lm(harmonised_weight ~ z1_snack_6 + z2_snack_6 + z3_snack_6 + z4_snack_6 + z5_snack_6 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_moresnack_model_ad <- lm(harmonised_weight ~ z1_moresnack_6 + z2_moresnack_6 + z3_moresnack_6 + z4_moresnack_6 + z5_moresnack_6 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_bout_model_ad <- lm(harmonised_weight ~ z1_bout_6 + z2_bout_6 + z3_bout_6 + z4_bout_6 + z5_bout_6 + harmonised_age + harmonised_ethnicity1 + harmonised_gender, final_data)
weight_sed_model_ad_sum <- summary(weight_sed_model_ad)
weight_light_model_ad_sum <- summary(weight_light_model_ad)
weight_lesssnack_model_ad_sum <- summary(weight_lesssnack_model_ad)
weight_snack_model_ad_sum <- summary(weight_snack_model_ad)
weight_moresnack_model_ad_sum <- summary(weight_moresnack_model_ad)
weight_bout_model_ad_sum <- summary(weight_bout_model_ad)

weight_ad_summary <- rbind(weight_sed_model_ad_sum$coefficients[2, ], weight_light_model_ad_sum$coefficients[2, ],
                           weight_lesssnack_model_ad_sum$coefficients[2, ], weight_snack_model_ad_sum$coefficients[2, ], weight_moresnack_model_ad_sum$coefficients[2, ], weight_bout_model_ad_sum$coefficients[2, ])
row.names(weight_ad_summary) <- c("sed", "light", "lesssnack", "snack", "moresnack", "bout")
weight_ad_summary