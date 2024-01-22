harmonisation <- function(nhanes_03_04 = nhanes_03_04, nhanes_05_06 = nhanes_05_06, hse_2008 = hse_2008, walking_away = walking_away, erma = erma) {

  nhanes_03_04 <- nhanes_03_04
  nhanes_05_06 <- nhanes_05_06
  hse_2008 <- hse_2008
  walking_away <- walking_away
  erma <- erma

  # harmonising height
  
  nhanes_03_04$height <- round(nhanes_03_04$height, digits = 0)
  nhanes_05_06$height <- round(nhanes_05_06$height, digits = 0)
  hse_2008$height <- round(hse_2008$height, digits = 0)
  walking_away$height <- round(walking_away$height * 100, digits = 0)
  erma$Height <- round(erma$Height * 100, digits = 0)
  
  # harmonising weight
  
  nhanes_03_04$weight <- round(nhanes_03_04$weight, digits = 0)
  nhanes_05_06$weight <- round(nhanes_05_06$weight, digits = 0)
  hse_2008$weight <- round(hse_2008$weight, digits = 0)
  walking_away$wt <- round(walking_away$wt, digits = 0)
  erma$Body_mass <- round(erma$Body_mass, digits = 0)

  # harmonising waist circumference
  
  hse_2008 <- hse_2008 %>%
    dplyr::mutate(
      waist = dplyr::if_else(!is.na(waist3), round((waist2 + waist3) / 2, digits = 0), round((waist1 + waist2) / 2, digits = 0))
    )
  
  nhanes_03_04$wc <- round(nhanes_03_04$wc, digits = 0)
  nhanes_05_06$wc <- round(nhanes_05_06$wc, digits = 0)
  hse_2008$waist <- round(hse_2008$waist, digits = 0)
  walking_away$waist <- round(walking_away$waist, digits = 0)
  erma$WaistCirc <- round(erma$WaistCirc, digits = 0)
  
  # harmonising systolic blood pressure
  
  nhanes_03_04 <- nhanes_03_04 %>%
    dplyr::mutate(
      sys_final = dplyr::if_else(!is.na(sys4), (sys3 + sys4)/2, (sys2 + sys3)/2)
    )
  
  nhanes_05_06 <- nhanes_05_06 %>%
    dplyr::mutate(
      sys_final = dplyr::if_else(!is.na(sys4), (sys3 + sys4)/2, (sys2 + sys3)/2)
    )

  
  # harmonising diastolic blood pressure
  
  nhanes_03_04 <- nhanes_03_04 %>%
    dplyr::mutate(
      dia_final = dplyr::if_else(!is.na(dia4), (dia3 + dia4)/2, (dia2 + dia3)/2)
    )
  
  nhanes_05_06 <- nhanes_05_06 %>%
    dplyr::mutate(
      dia_final = dplyr::if_else(!is.na(dia4), (dia3 + dia4)/2, (dia2 + dia3)/2)
    )
  
  # harmonising fasting
  
  nhanes_03_04 <- nhanes_03_04 %>%
    dplyr::mutate(
      harmonised_fasting_duration = (PHAFSTHR * 60) + PHAFSTMN
    )
  
  nhanes_05_06 <- nhanes_05_06 %>%
    dplyr::mutate(
      harmonised_fasting_duration = (PHAFSTHR * 60) + PHAFSTMN
    )
  
  hse_2008 <- hse_2008 %>%
    dplyr::mutate(
      harmonised_fasting_duration = dplyr::if_else(consbx11 == 0, 30, 0)
    )
  
  walking_away <- walking_away %>%
    dplyr::mutate(
      harmonised_fasting_duration = 12 * 60
    )
  
  erma <- erma %>%
    dplyr::mutate(
      harmonised_fasting_duration = 8 * 60  
    )
  
  # harmonising sex
  
  walking_away <- walking_away %>%
    dplyr::mutate(
      sex = dplyr::if_else(sex == "Male", 1, dplyr::if_else(sex == "Female", 2, 0))
    ) 
  
  erma$sex <- 2
  
  # harmonise ethnicity
  
  nhanes_03_04 <- nhanes_03_04 %>%
    dplyr::mutate(
      ethnicity2 = dplyr::if_else(ethnicity1 == 3, 1, dplyr::if_else(ethnicity1 == 4, 2, 3)),
      ethnicity1 = dplyr::if_else(ethnicity1 == 3, 1, 2)
      
    )
  
  nhanes_05_06 <- nhanes_05_06 %>%
    dplyr::mutate(
      ethnicity1 = dplyr::if_else(ethnicity1 == 3, 1, 2),
      ethnicity2 = dplyr::if_else(ethnicity1 == 3, 1, dplyr::if_else(ethnicity1 == 4, 2, 3))
    )
  
  hse_2008 <- hse_2008 %>%
    dplyr::mutate(
      ethnicity1 = dplyr::if_else(origin == 1 | origin == 2 | origin == 3, 1, 2),
      ethnicity2 = dplyr::if_else(origin == 1 | origin == 2 | origin == 3, 1, dplyr::if_else(origin == 12 | origin == 13 | origin == 14, 2,3)),
    )
  
  # tidy up walking away ethnicity names
  
  walking_away$ethnicity_txt <- stringr::str_replace(walking_away$ethnicity_txt, "White and Black Carribian", "White and Black Caribbean")
  walking_away$ethnicity_txt <- stringr::str_replace(walking_away$ethnicity_txt, "white british", "White British")
  
  walking_away <- walking_away %>%
    dplyr::mutate(
      ethnicity1 = dplyr::if_else(ethnicity_txt == "White British" | ethnicity_txt == "White Irish" | ethnicity_txt == "An other white background", 1, 2),
      ethnicity2 = dplyr::if_else(ethnicity_txt == "White British" | ethnicity_txt == "White Irish" | ethnicity_txt == "An other white background", 1, dplyr::if_else(ethnicity_txt == "Caribbean" | ethnicity_txt == "Any other black background", 2, 3))
    )
  
  erma <- erma %>%
    dplyr::mutate(
      ethnicity1 = 1,
      ethnicity2 = 1
    )
  
  # harmonise annual household income 
  
  # renaming for joining
  
  nhanes_03_04 <- nhanes_03_04 %>%
    dplyr::rename(harmonised_gender = gender, harmonised_age = age_yr, harmonised_ethnicity1 = ethnicity1, harmonised_ethnicity2 = ethnicity2, harmonised_height = height, harmonised_weight = weight, harmonised_wc = wc, harmonised_sysbp = sys_final, harmonised_diabp = dia_final, harmonised_totalChol = total_chol, harmonised_hdlChol =  hdl, harmonised_ldlChol = ldl, harmonised_triglyc = triglycerides, harmonised_glucose = glucose, harmonised_hba1c = hba1c)
  
  nhanes_05_06 <- nhanes_05_06 %>%
    dplyr::rename(harmonised_gender = gender, harmonised_age = age_yr, harmonised_ethnicity1 = ethnicity1, harmonised_ethnicity2 = ethnicity2, harmonised_height = height, harmonised_weight = weight, harmonised_wc = wc, harmonised_sysbp = sys_final, harmonised_diabp = dia_final, harmonised_totalChol = total_chol, harmonised_hdlChol =  hdl, harmonised_ldlChol = ldl, harmonised_triglyc = triglycerides, harmonised_glucose = glucose, harmonised_hba1c = hba1c)
  
  hse_2008 <- hse_2008 %>%
    dplyr::rename(harmonised_gender = sex, harmonised_age = age, harmonised_ethnicity1 = ethnicity1, harmonised_ethnicity2 = ethnicity2, harmonised_height = height, harmonised_weight = weight, harmonised_wc = waist, harmonied_sysbp = omsysval, harmonised_diabp = omdiaval, harmonised_totalChol = cholval, harmonised_hdlChol = hdlval, harmonised_hba1c = glyhbval)
  
  walking_away <- walking_away %>%
    dplyr::rename(harmonised_gender = sex, harmonised_age = age, harmonised_ethnicity1 = ethnicity1, harmonised_ethnicity2 = ethnicity2, harmonised_height = height, harmonised_weight = wt, harmonised_wc = waist, harmonised_sysbp = sbp, harmonised_diabp = dbp, harmonised_totalChol = cholesterol, harmonised_hdlChol = hdl, harmonised_ldlChol = ldl, harmonised_triglyc = trigs, harmonised_glucose = fasting_glucose, harmonised_hba1c = `a1c_%`)
  
  erma <- erma %>%
    dplyr::rename(harmonised_gender = sex, harmonised_age = Age, harmonised_ethnicity1 = ethnicity1, harmonised_ethnicity2 = ethnicity2, harmonised_height = Height, harmonised_weight = Body_mass, harmonised_wc = WaistCirc, harmonised_sysbp = SBP, harmonised_diabp = DBP, harmonised_totalChol = Total_cholesterol, harmonised_hdlChol = HDL_C, harmonised_ldlChol = LDL_C, harmonised_triglyc = Triglycerides, harmonised_glucose = Glucose)
  
  # bind datasets together
  
  final_data <- dplyr::bind_rows(nhanes_03_04 %>% dplyr::select(SEQN, contains("harmonised")), nhanes_05_06 %>% dplyr::select(SEQN, contains("harmonised"))) %>%
    dplyr::bind_rows(hse_2008 %>% dplyr::select(seriali, contains("harmonised"))) %>%
    dplyr::bind_rows(walking_away %>% dplyr::select(id, contains("harmonised"))) %>%
    dplyr::bind_rows(erma %>% dplyr::select(ID, contains("harmonised"))) %>%
    dplyr::relocate(SEQN, seriali, id, ID)
  
  return(final_data)
}
