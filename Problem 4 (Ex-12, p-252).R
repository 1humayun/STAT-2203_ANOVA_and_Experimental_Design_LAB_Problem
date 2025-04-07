############################# Problem-4 | Example-12 (Page-252) ################

df <- data.frame(
   Block = factor(rep(1:4, each = 8)),
   A = factor(c(0, 1, 1, 0, 0, 1, 1, 0,         # 0 represents absence of factor effect
                1, 0, 0, 1, 1, 0, 1, 0,         # 1 represents presence of factor effect
                0, 0, 0, 1, 1, 0, 1, 1,
                1, 1, 0, 0, 1, 0, 0, 1)),
   B = factor(c(0, 0, 1, 1, 0, 0, 1, 1, 
                0, 0, 1, 1, 0, 0, 1, 1,
                1, 1, 0, 0, 1, 0, 1, 0,
                1, 0, 0, 1, 0, 1, 0, 1)),
   C = factor(c(0, 0, 0, 0, 1, 1, 1, 1, 
                1, 0, 0, 1, 0, 1, 0, 1,
                0, 1, 1, 0, 0, 0, 1, 1,
                0, 1, 0, 1, 0, 0, 1, 1)),
   Yield = c(257, 232, 230, 211, 210, 176, 186, 175,
             267, 276, 262, 220, 256, 269, 285, 272,
             188, 186, 160, 188, 164, 214, 182, 166,
             204, 206, 239, 224, 254, 269, 252, 301)
)
df

#.............................. (ii) ...
fit = aov(Yield~Block + A*B*C, data = df)
anova_model = summary(fit)
anova_model


#.............................. (i) ...
# To estimate the main effects and interactions:
model_coefficients <- coef(fit)
model_coefficients

#.............................. (iii) ...
# Fit the ANOVA model (confounding ABC with blocks)
model <- aov(Yield ~ Block + A * B * C - A:B:C, data = df)

# Display ANOVA table
anova_table <- summary(model)
print(anova_table)
