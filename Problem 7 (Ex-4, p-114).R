# Create the data frame from the given experiment
df = data.frame(
   Block = factor(rep(1:4, each = 5)),
   Treatment = factor(c("B", "C", "A", "D", "E", 
                        "A", "B", "D", "E", "C", 
                        "B", "D", "C", "A", "E", 
                        "E", "A", "B", "C", "D")),
   Response = c(30, 23, 20, 18, 24,
                28, 26, 19, 15, 19,
                22, 24, 29, 37, 25,
                21, 18, 23, 28, 25))

# i) Test significance of difference between treatments
# Using ANOVA for randomized block design
model = aov(Response~Block+Treatment, data = df)
anova_model = summary(model)
anova_model

# ii) Estimate efficiency relative to CRD
# Calculate MSE for RBD and CRD
mse_rbd = anova_model[[1]]["Residuals", "Mean Sq"]

crd_model = aov(Response ~ Treatment, data = df)
mse_crd = summary(crd_model)[[1]]["Residuals", "Mean Sq"]
mse_crd 

efficiency = (mse_crd/mse_rbd) * 100
efficiency
# Since 104.47 > 100, So Block factor (RBD) has been effective slightly in reducing error variance.

#.............(iii)...
#(a) H0: mu_A = mu_C

t_ac = t.test(Response~Treatment, data = subset(df, Treatment %in% c("A", "C"),conf.level = 0.95))
t_ac

# (b) mu_B = mu_E
t_be = t.test(Response~Treatment, data = subset(df, Treatment %in% c("B", "E"),conf.level = 0.95))
t_be

#.................. (iv)...
# H0: mu_B = mu_D | Find 95% CI
t_bd = t.test(Response~Treatment, data = subset(df, Treatment %in% c("B", "D"),conf.level = 0.95))
t_bd[["conf.int"]][c(1,2)]

# suppose the value in 3rd col and 4th row are missing. now estimate the missing value
# data frame with missing value
df = data.frame(
   Block = factor(rep(1:4, each = 5)),
   Treatment = factor(c("B", "C", "A", "D", "E", 
                        "A", "B", "D", "E", "C", 
                        "B", "D", "C", "A", "E", 
                        "E", "A", "B", "C", "D")),
   Response = c(30, 23, 20, 18, 24,
                28, 26, 19, 15, 19,
                22, 24, 29, NA, 25,
                21, 18, 23, 28, 25))

df
mis.index = which(is.na(df$Response))
mis.block = df$Block[mis.index]
mis.treatment = df$Treatment[mis.index]

b.mean = mean(df$Response[df$Block==mis.block], na.rm = TRUE)
t.mean = mean(df$Response[df$Treatment == mis.treatment], na.rm=TRUE)
g.mean = mean(df$Response, na.rm = TRUE)
missing.v = t.mean + b.mean - g.mean
missing.v

#Another way...
n.b = 4
n.t = 6
b.sum = sum(df$Response[df$Block==mis.block], na.rm = TRUE)
t.sum = sum(df$Response[df$Treatment == mis.treatment], na.rm=TRUE)
g.sum = sum(df$Response, na.rm = TRUE)
missing.value = (n.b*b.sum + n.t*t.sum - g.sum)/((n.b-1)*(n.t-1))
missing.value


#Find Standard Error 
# replace missing value.
df$Response[mis.index] = missing.value

fit = aov(Response~Block+Treatment, data = df)
anova.table = summary(fit)
anova.table
MSE = anova.table[[1]]["Residuals", "Mean Sq"]
r = 4  # number of rep of a treatment
SE = sqrt(2*MSE/r)
SE
