# three part unadjusted

weight <- codaredistlm::predict_delta_comps(
  dataf = final_data,
  y = "harmonised_weight",
  comps = c("sed", "light", "mvpa"),
  covars = c(),
  deltas = seq(-15, 15, by = 5) / (1440 - (8 * 60)),
  comparisons = "prop-realloc",
  alpha = 0.05)

codaredistlm::plot_delta_comp(weight,
                              comp_total = 1440 - (8 * 60),
                              units_lab = "min")

# three part adjusted

weight <- codaredistlm::predict_delta_comps(
  dataf = final_data,
  y = "harmonised_weight",
  comps = c("sed", "light", "mvpa"),
  covars = c("harmonised_gender", "harmonised_age", "harmonised_ethnicity1"),
  deltas = seq(-15, 15, by = 5) / (1440 - (8 * 60)),
  comparisons = "prop-realloc",
  alpha = 0.05)

codaredistlm::plot_delta_comp(weight,
                              comp_total = 1440 - (8 * 60),
                              units_lab = "min")

# four part unadjusted

weight4 <- codaredistlm::predict_delta_comps(
  dataf = four_final_data, 
  y = "harmonised_weight",
  comps = c("sed", "light", "less_than_10", "bout.MVPA.Minutes.Valid.Days.Total.Per.Day"),
  covars = c(),
  deltas = seq(-10, 10, by = 5) / (1440 - (8 * 60)),
  comparisons = "prop-realloc",
  alpha = 0.05)

codaredistlm::plot_delta_comp(weight4,
                              comp_total = (1440 - (8 * 60)),
                              units_lab = "min")

# four part adjusted

weight4 <- codaredistlm::predict_delta_comps(
  dataf = four_final_data, 
  y = "harmonised_weight",
  comps = c("sed", "light", "less_than_10", "bout.MVPA.Minutes.Valid.Days.Total.Per.Day"),
  covars = c("harmonised_gender", "harmonised_age", "harmonised_ethnicity1"),
  deltas = seq(-10, 10, by = 5) / (1440 - (8 * 60)),
  comparisons = "prop-realloc",
  alpha = 0.05)

codaredistlm::plot_delta_comp(weight4,
                              comp_total = (1440 - (8 * 60)),
                              units_lab = "min")

# six part unadjusted

weight6 <- codaredistlm::predict_delta_comps(
  dataf = six_final_data,
  y = "harmonised_weight",
  comps = c("sed", "light", "lesssnack.MVPA.Minutes.Valid.Days.Total.Per.Day", "snack.MVPA.Minutes.Valid.Days.Total.Per.Day", "moresnack.MVPA.Minutes.Valid.Days.Total.Per.Day", "bout.MVPA.Minutes.Valid.Days.Total.Per.Day"),
  covars = c(),
  deltas = seq(-5, 5, by = 5) / (1440 - (8 * 60)),
  comparisons = "prop-realloc",
  alpha = 0.05)

codaredistlm::plot_delta_comp(weight6,
                              comp_total = 1440 - (8 * 60),
                              units_lab = "min")

# six part adjusted

weight6 <- codaredistlm::predict_delta_comps(
  dataf = six_final_data,
  y = "harmonised_weight",
  comps = c("sed", "light", "lesssnack.MVPA.Minutes.Valid.Days.Total.Per.Day", "snack.MVPA.Minutes.Valid.Days.Total.Per.Day", "moresnack.MVPA.Minutes.Valid.Days.Total.Per.Day", "bout.MVPA.Minutes.Valid.Days.Total.Per.Day"),
  covars = c("harmonised_gender", "harmonised_age", "harmonised_ethnicity1"),
  deltas = seq(-5, 5, by = 5) / (1440 - (8 * 60)),
  comparisons = "prop-realloc",
  alpha = 0.05)

codaredistlm::plot_delta_comp(weight6,
                              comp_total = 1440 - (8 * 60),
                              units_lab = "min")
