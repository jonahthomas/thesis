# three part

tiff("three_part.png", units="in", width=5, height=5, res=300)
ggplot(data, aes(x = Estimate, y = factor(`Cardiometabolic.marker`, levels = c("Weight", "BMI", "WC", "SBP", "DBP", "TC", "HDL", "LDL", "Triglycerides", "Glucose", "HbA1c", "MetS")), color = cutpoint)) +
  geom_point(aes(alpha = 0.05)) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("Blue", "Orange")) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_up, color = cutpoint, alpha = 0.05)) +
  guides(alpha = "none", color = guide_legend(title="Cutpoint")) +
  labs(x = "Predicted change in health outcome", y = "Cardiometabolic marker") +
  theme_linedraw()
dev.off()

# four part

tiff("four_part.png", units="in", width=5, height=5, res=300)
ggplot(data, aes(x = Estimate, y = factor(`Cardiometabolic.marker`, levels = c("Weight", "BMI", "WC", "SBP", "DBP", "TC", "HDL", "LDL", "Triglycerides", "Glucose", "HbA1c", "MetS")), color = cutpoint)) +
  facet_wrap(~Behaviour) +
  geom_point(aes(alpha = 0.05)) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("Blue", "Orange")) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_up, color = cutpoint, alpha = 0.05)) +
  guides(alpha = "none", color = guide_legend(title="Cutpoint")) +
  labs(x = "Predicted change in health outcome", y = "Cardiometabolic marker") +
  theme_linedraw()
dev.off()

# six part

tiff("six_part.png", units="in", width=5, height=5, res=300)
ggplot(data, aes(x = Estimate, y = factor(`Cardiometabolic.marker`, levels = c("Weight", "BMI", "WC", "SBP", "DBP", "TC", "HDL", "LDL", "Triglycerides", "Glucose", "HbA1c", "MetS")), color = cutpoint)) +
  facet_wrap(~Behaviour) +
  geom_point(aes(alpha = 0.05)) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("Blue", "Orange")) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_up, color = cutpoint, alpha = 0.05)) +
  guides(alpha = "none", color = guide_legend(title="Cutpoint")) +
  labs(x = "Predicted change in health outcome", y = "Cardiometabolic marker") +
  theme_linedraw()
dev.off()
