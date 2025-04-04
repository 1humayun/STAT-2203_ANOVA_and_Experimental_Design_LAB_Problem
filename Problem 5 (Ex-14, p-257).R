
data = data.frame(
   Replicate = rep(1:2, each = 16),
   Block = factor(rep(rep(c("B1", "B2"), each = 8), times = 2)),
   Treatment = c("dnk", "p", "k", "dnp", "d", "npk", "dpk", "n",
                 "np", "dn", "dnpk", "nk", "dk", "pk", "(1)", "dp",
                 "npk", "d", "p", "dnp", "dpk", "n", "k", "dnk",
                 "nk", "dp", "np", "pk", "dk", "dnpk", "(l)", "dn"),
   Yield = c(40, 46, 52, 49, 50, 38, 55, 41,
             53, 40, 43, 43, 40, 52, 55, 50,
             45, 40, 40, 52, 45, 47, 50, 35,
             43, 51, 52, 58, 52, 56, 58, 42)
)


# Alternatively, if factors are encoded in 'Treatment':
# Extract factor levels (example: 'dnk' => D=1, N=1, K=1, P=0)
data$N = ifelse(grepl("n", data$Treatment), 1, 0)
data$P = ifelse(grepl("p", data$Treatment), 1, 0)
data$K = ifelse(grepl("k", data$Treatment), 1, 0)
data$D = ifelse(grepl("d", data$Treatment), 1, 0)
data
# Fit a linear model with main effects and interactions
model = aov(Yield ~ Replicate + Block + N*P*K*D, data = data)
summary(model)

#Simplified model...
#The model Yield ~ Replicate + Block + d * n * p * k includes all possible interactions (up to 4-way: d:n:p:k).
#With limited data (only 32 observations), this leads to overfitting and insufficient degrees of freedom for error estimation.
#Hence, Test only main effects and 2-way interactions:
model = aov(Yield ~ Replicate+ Block+ N + P + K + D + N:P + N:K + P:K + D:N + D:P + D:K, data = data)

# Summarize ANOVA results
summary(model)


