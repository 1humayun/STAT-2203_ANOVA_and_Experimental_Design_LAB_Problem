
#################################### Example-2 (Page-88) #########################
df = data.frame(
   Treatment = factor(rep(c("A", "B", "C", "D", "E", "F"), times = 5)),
   Response = c(
      39, 26, 35, 26, 24, 28,
      45, 43, 28, 27, 28, 36,
      28, 32, 36, 30, 31, 45,
      32, 40, 24, 25, 29, 41,
      38, 29, 31, 42, 44, 25)
)

#................................. (i)...
df
fit = aov(Response~Treatment, data = df)
anova_model = summary(fit)
anova_model

#...............................(ii)...
t_test = t.test(Response~Treatment, data = subset(df, Treatment %in% c("A", "E")), conf.level = .95)
t_test

CI = t_test[["conf.int"]]
CI




