############################# Problem-6 | Example-16 (page-286) ######################
# Create the dataset
data = data.frame(
   Fertilizer = factor(rep(1:3, each = 12)),  # 3 fertilizer levels
   Variety = factor(rep(rep(1:2, each = 6), times = 3)),  # 2 varieties
   Block = factor(rep(1:6, times = 6)),  # 6 blocks
   Yield = c(
      161, 166, 113, 103, 132, 180,  # F1V1
      192, 253, 208, 171, 196, 198,  # F1V2
      145, 231, 131, 168, 176, 216,  # F2V1
      232, 231, 190, 171, 242, 238,  # F2V2
      172, 204, 104, 135, 178, 175,  # F3V1
      227, 214, 144, 146, 186, 230   # F3V2
   )
)
#View the dataset
head(data)
# Fit the initial ANOVA model
fit = aov(Yield ~ Block + Fertilizer * Variety, data = data)
anova_model = summary(fit)
anova_model

#Partition Treatment SS into 5 Orthogonal Components...
# Set orthogonal polynomial contrasts for fertilizer
contrasts(data$Fertilizer) = contr.poly(3)

# Fit the model with contrasts
model = aov(Yield ~ Block + Fertilizer * Variety, data = data)

# Partition treatment SS into 5 orthogonal components (each with 1 df)
ortho_model = summary(model, split = list(
   Fertilizer = list(Linear = 1, Quadratic = 2),
   `Fertilizer:Variety` = list(V.Linear = 1, V.Quadratic = 2)
))

five_ortho_component = ortho_model[[1]][c(3,4,5,7,8),]
five_ortho_component



