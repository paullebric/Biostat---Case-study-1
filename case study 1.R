library(ggplot2)
library(readxl)
library (car)

Data <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Data")
Summary <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Summary")
Info <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Info")
Codebook <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Codebook")

#I)DATA IMPORTATION
All_Data <- list(
  Data = Data,
  Summary = Summary,
  Info = Info,
  Codebook = Codebook
)

#II) DATA EXPLORATION
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

#III) ASSUMPTION CHECK / CTL
spec0 <- aov(Response ~ Group * Timepoint, data = Data)
check_1 <- shapiro.test(residuals(spec0))
check_2 <- leveneTest(Response ~ interaction(Group, Timepoint), data = Data)

spec0; check_1; check_2


#IV) TWO-LEVEL COMPARISONS (at least two)
test_1 <- t.test(Response ~ Timepoint, data = subset(Data, Timepoint %in% c("T1", "T2")))                                                     
test_2 <- t.test(Response ~ Timepoint, data = subset(Data, Timepoint %in% c("T3", "T4"))) 
test_1; test_2
#ici ca sert a rien mais bon on doit le faire


#V)MULTI-LEVEL COMPARISONS

test_multi <- aov(Response ~ Group * Timepoint, data = Data)
test_multi
summary(test_multi)
"""
on voit qu'il y a un effet hautement significatif du groupe sur la reponses mais
aussi du timepoint sur la reponse et aussi de l'interaction timepoint*group sur la reponse
"""

#VI)REGRESSION MODEL
model <- lm(Response ~ Group * Timepoint, data = Data)
model_out <- summary(model)
model; model_out


# Ici on regarde la difference des group des timepoint et des group+timepoint par rapport au 
# groupe A Timepoint 1.
# En résumé on voit que les groupes B et C on un coeff de + 7 et + 9 par rapport au gr A T1


#VI) POST-HOC ANALYSIS
posthoc <- TukeyHSD(test_multi)
posthoc


#ici on commence a rentrer dans le vif du sujet on compare 2 à 2 chacun des gr timepoint. Ce qui nous donne des informations interressantes.

#VII) P-value matrix and comparison visual
p_1 <- test_1
  p_2 <- test_2
  p_multi <- test_multi
  p_model <- model_out
  p_mat <- matrix(c(p_1, p_2, p_multi, p_model), nrow=1)
colnames(p_mat) <- c("two_1","two_2","multi","model")
rownames(p_mat) <- "p"
p_mat
barplot(-log10(p_mat), beside=TRUE, ylab="-log10(p)")
image(t(-log10(p_mat)), axes=FALSE)
axis(1, at=seq(0,1,length.out=ncol(p_mat)), labels=colnames(p_mat))
axis(2, at=0.5, labels=rownames(p_mat))
#VIII) Visualisations (end only)
Boxplots
boxplot(?? ~ ??, ??a = subset(??, ?? == "??"))
boxplot(?? ~ ??, ??a = subset(??, ?? == "??"))
boxplot(?? ~ ??, ??a = ??)
