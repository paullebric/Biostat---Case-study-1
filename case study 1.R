library(ggplot2)
library(readxl)
library (car)
library(emmeans)
library(dplyr)

Data <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Data")
Summary <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Summary")
Info <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Info")
Codebook <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Codebook")

#---------------------------------------------------------------------------------------
# this code was build with the help of the entire group using a github repository and 
# github desktop.
# The git repository is available at : https://github.com/paullebric/Biostat---Case-study-1
#---------------------------------------------------------------------------------------




#=======================================================================================

#I)DATA IMPORTATION
#=======================================================================================
All_Data <- list(
  Data = Data,
  Summary = Summary,
  Info = Info,
  Codebook = Codebook
)
#=======================================================================================
#II) DATA EXPLORATION
#=======================================================================================
lapply(All_Data,head)
lapply(All_Data,dim)
lapply(All_Data, function(x){str(x); head(x)})
lapply(All_Data,str)
n_all <- nrow(Data)
n_na <- sum(is.na(Data$Response))
tab <- with(Data, table(Group, Timepoint))
desc1 <- tapply(Data$Response, Data$Group, function(x) c(n=length(x),
                                                  mean=mean(x,na.rm=TRUE), sd=sd(x,na.rm=TRUE)))
desc2 <- tapply(Data$Response, list(Data$Timepoint, Data$Group), function(x) c(n=length(x),
                                                         mean=mean(x,na.rm=TRUE), sd=sd(x,na.rm=TRUE)))
n_all; n_na; tab; desc1; desc2
#=======================================================================================
#III) ASSUMPTION CHECK / CTL
#=======================================================================================
spec0 <- aov(Response ~ Group * Timepoint, data = Data)
check_1 <- shapiro.test(residuals(spec0))
check_2 <- leveneTest(Response ~ interaction(Group, Timepoint), data = Data)

spec0; check_1; check_2

#=======================================================================================
#IV) TWO-LEVEL COMPARISONS (at least two)
#=======================================================================================
test_1 <- t.test(Response ~ Timepoint, data = subset(Data, Timepoint %in% c("T1", "T2")))                                                     
test_2 <- t.test(Response ~ Timepoint, data = subset(Data, Timepoint %in% c("T3", "T4"))) 
test_1; test_2
#here it's useless but hey we have to do it we compare T1 and T2 then T3 and T4
#=======================================================================================
#V)MULTI-LEVEL COMPARISONS
#=======================================================================================

test_multi <- aov(Response ~ Group * Timepoint, data = Data)
test_multi
summary(test_multi)

# we see that there is a highly significant effect of the group on the response, but
# also of the timepoint on the response, and also of the timepoint*group interaction on the response.
#=======================================================================================
#VI)REGRESSION MODEL
#=======================================================================================
model <- lm(Response ~ Group * Timepoint, data = Data)
model_out <- summary(model)
model; model_out


# Here we look at the difference between the timepoint groups and the timepoint groups compared to
# group A Timepoint 1.
# In summary, we see that groups B and C have a coefficient of +7 and +9 compared to group A T1
#=======================================================================================
#VII) POST-HOC ANALYSIS
#=======================================================================================
posthoc <- TukeyHSD(test_multi)
posthoc
#Graph summarizing the results of the turkeyHSD not requested
emm_all <- as.data.frame(emmeans(model, ~ Group * Timepoint))
ggplot(emm_all, aes(Timepoint, emmean, color = Group, group = Group)) +
  geom_line() + geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .1) +
  labs(y = "Moyenne ajustée (IC95%)",
       title = "Évolution de Response par Groupe (estimations du modèle)") +
  theme_minimal()

#Here we start to get to the heart of the matter, comparing each of the gr
#timepoints two by two. This gives us some interesting information.

#=======================================================================================
#VIII) P-value matrix and comparison visual
#=======================================================================================
f <- model_out$fstatistic
p_1 <- test_1$p.value
  p_2 <- test_2$p.value
  p_multi <- summary(test_multi)[[1]]["Group:Timepoint", "Pr(>F)"]
  p_model <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  p_mat <- matrix(c(p_1, p_2, p_multi, p_model), nrow=1)
colnames(p_mat) <- c("two_1","two_2","multi","model")
rownames(p_mat) <- "p"
p_mat
barplot(-log10(p_mat), beside=TRUE, ylab="-log10(p)")

# Here on this graph we see the different p-values (-log10). The higher the bar, the more significant
# It is, so we see that the more advanced the test, the more significant it is.

image(t(-log10(p_mat)), axes=FALSE)
axis(1, at=seq(0,1,length.out=ncol(p_mat)), labels=colnames(p_mat))
axis(2, at=0.5, labels=rownames(p_mat))


#=======================================================================================
#VIII) BOXPLOTS
#=======================================================================================


ggplot(Data, aes(x = Timepoint, y = Response, fill = Timepoint)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution de la Response selon le Timepoint",
    x = "Timepoint",
    y = "Response"
  ) +
  theme_minimal()

ggplot(Data, aes(x = Group, y = Response, fill = Group)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution de la Response selon le Gr",
    x = "Groupe",
    y = "Response"
  ) +
  theme_minimal()

ggplot(Data, aes(x = Group, y = Response, fill = Timepoint)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1) +
  theme_minimal() +
  labs(
    title = "Distribution de la Response selon le Gr et le Timepoint",
    x = "Groupe",
    y = "Response"
  ) +
  theme_minimal()
#Les points gris sont les valeurs aberrantes (outlier)


#=======================================================================================
#IX) VIOLON PLOT
#=======================================================================================

ggplot(Data, aes(x = Timepoint, y = Response, fill = Timepoint)) +
  geom_violin(alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution de la Response selon le Timepoint",
    x = "Timepoint",
    y = "Response"
  ) +
  theme_minimal()

ggplot(Data, aes(x = Group, y = Response, fill = Group)) +
  geom_violin(alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribution de la Response selon le Gr",
    x = "Groupe",
    y = "Response"
  ) +
  theme_minimal()

ggplot(Data, aes(x = Group, y = Response, fill = Timepoint)) +
  geom_violin(alpha = 0.8,position = position_dodge(width = 0.8)) +
  stat_summary(aes(group = Timepoint), FUN = mean, color = "black",geom = "point", size = 2 ,position = position_dodge(width = 0.8))+
  theme_minimal() +
  labs(
    title = "Distribution de la Response selon le Gr et le Timepoint",
    x = "Groupe",
    y = "Response"
  ) +
  theme_minimal()


#=======================================================================================
#X) MEAN + CI
#=======================================================================================


summary_data <- Data %>%
  group_by(Group, Timepoint) %>%
  summarise(mean = mean(Response, na.rm = TRUE),
            se = sd(Response, na.rm = TRUE) / sqrt(sum(!is.na(Response))),
            .groups = "drop") %>%
  mutate(lo = mean - 1.96*se,
         hi = mean + 1.96*se)

ggplot(summary_data, aes(x = Timepoint, y = mean, color = Group, group = Group)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.1) +
  labs(y = "Mean ± 95% CI", title = "Moyennes et intervalles de confiance") +
  theme_minimal()

#=======================================================================================
#XI) MODEL VIEW
#=======================================================================================
Data_clean <- na.omit(Data)
model_view <- data.frame(
  Observed = Data_clean$Response,
  Predicted = fitted(model)
)
#Missing data needs to be cleaned before doing this graph because for each predicted 
#response we need an observed response

ggplot(model_view, aes(x = Observed, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +  # nuage de points
  geom_abline(intercept = 0, slope = 1,                     # diagonale y = x
              color = "red", linetype = "dashed", alpha = 0.6, linewidth = 1.2) +
  labs(
    title = "Valeurs observées vs prédictions du modèle",
    x = "Valeurs observées (Response)",
    y = "Valeurs prédites (fitted)"
  )+
  theme_minimal()

#Here we look at how the model predict the responses compared to the responses.
# In red there is the diagonal and as we can see the predicted and observed 
# responses follow this diagonal wich means that the model is working

#=======================================================================================
#XII) CONCLUSION
#=======================================================================================
# We used three approaches:
#   
# T-tests to compare two groups or timepoints,
# 
# ANOVA to test overall effects and interactions,
# 
# Linear regression to measure how each Group and Timepoint affects the Response.
# 
# All methods showed the same result:
#   ==> Response increases with time and differs between groups, with a clear interaction effect.
# 
# The Central Limit Theorem (CLT) supported using parametric tests because our sample size was large and data were roughly normal with equal variances.
# So, the regression model gave consistent and reliable results with the simpler tests.











