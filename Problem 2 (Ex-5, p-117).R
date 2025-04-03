############################# Problem-5 | Example-5 (page-117) ######################
df = data.frame(
   Block = factor(rep(1:5, each = 5)),
   Treatment = factor(rep(c("A", "B", "C", "D", "E"), times = 5)),
   Yeild = c(18, 20, 20, 21, 21,
             17, 19, 19, 20, 20,
             16, 17, 18, 19, 20,
             16, 16, 17, 18, 18,
             16, 16, 15, 17, 16)
)

#..............(i)...
fit = aov(Yeild~Block + Treatment, data = df)
anova_model = summary(fit)
anova_model

#...............(ii)...
t_test = t.test(Yeild~Treatment, data = subset(df, Treatment %in% c("A","D")), conf.level = 0.90)
t_test
t_test[["conf.int"]]
summary(t_test)

#...............(iii)..

