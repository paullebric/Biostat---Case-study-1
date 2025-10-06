library(ggplot2)
library(readxl)

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
check_2 <- bartlett.test(Response ~ interaction(Group, Timepoint), data = Data)

spec0; check_1; check_2


#IV) TWO-LEVEL COMPARISONS (at least two)
test_1 <- t.test(Data$Response ~ sub, Data = subset(Data, Timepoint == "T1"))
test_2 <- ??(?? ~ ??, ??a = subset(??, ?? == "??"))
test_1; test_2

TukeyHSD(spec0)

#V)MULTI-LEVEL COMPARISONS

test_multi <- ??(?? ~ ??, ??a = ??)
test_multi
#VI)REGRESSION MODEL
model <- ??(?? ~ ?? * ??, ??a = ??)
model_out <- ??(model)
model; model_out


#VI) POST-HOC ANALYSIS
posthoc <- ??(??)
posthoc

#VII) P-value matrix and comparison visual
p_1 <- ??
  p_2 <- ??
  p_multi <- ??
  p_model <- ??
  p_mat <- matrix(c(p_two_1, p_two_2, p_multi, p_model), nrow=1)
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
