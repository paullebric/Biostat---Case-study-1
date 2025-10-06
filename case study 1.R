library(ggplot2)
library(readxl)
Data <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Data")
Summary <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Summary")
Info <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Info")
Codebook <- read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Codebook")

All_Data <- list(
  Data = Data,
  Summary = Summary,
  Info = Info,
  Codebook = Codebook
)

#$$$1 Data Exploration
lapply(All_Data,head)
lapply(All_Data,dim)
lapply(All_Data, function(x){str(x); head(x)})
lapply(All_Data,str)



#$$$2 Assumption checks



#$$$3 Test or Model Selection



#$$$4 Results



#$$$5 Post-Hoc Analysis



#$$$6 
