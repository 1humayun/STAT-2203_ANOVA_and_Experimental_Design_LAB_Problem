#################################### Exercise-28 (Page-159) ################
df = data.frame(
   Row = factor(rep(1:6, each = 6)),
   Column = factor(rep(1:6, times = 6)),
   Treatment = factor(c("F", "E", "D", "C", "B", "A",
                        "E", "C", "A", "D", "F", "B",
                        "B", "A", "F", "E", "D", "C",
                        "A", "B", "E", "F", "C", "D",
                        "D", "F", "C", "B", "A", "E",
                        "C", "D", "B", "A", "E", "F")),
   Yield = c(219, 250, 227, 162, 182, 89,
             224, 141, 91, 191, 213, 195,
             204, 94, 225, 229, 250, 207,
             77, 204, 240, 199, 182, 250,
             250, 231, 209, 204, 92, 227,
             152, 186, 191, 77, 230, 198)
)

#............................(i)...
fit = aov(Yield~Row + Column + Treatment, data= df)
anova_model = summary(fit)
anova_model

