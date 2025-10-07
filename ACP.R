library(MASS)
library(factoextra)
library(ggplot2)
library(readxl)

data <- data.frame(read_excel("Biostatistics 5A 1st Case Study dataset.xlsx",sheet = "Data"))
dim(data)
str(data)
