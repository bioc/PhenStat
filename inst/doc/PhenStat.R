### R code from vignette source 'PhenStat.Rnw'

###################################################
### code chunk number 1: R_hide_000
###################################################
#Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.21\\bin\\gswin64.exe")
#tools::compactPDF(paths = 'C:\\Users\\hamedhm\\Google Drive\\At work\\ Publications. Shared with Terry and Jeremy\\PhenStat changes\\PhenStat_2.8.0\\PhenStat\\inst\\doc',gs_quality = "ebook")
suppressPackageStartupMessages(library(PhenStat))


###################################################
### code chunk number 2: R_hide_001
###################################################
PhenStat:::WhatIsNew()


###################################################
### code chunk number 3: R_hide002
###################################################
library(PhenStat)

dataset1 <- system.file("extdata", "test1.csv", package = "PhenStat")

dataset2 <- system.file("extdata", "test1.txt", package = "PhenStat")


###################################################
### code chunk number 4: R_hide003
###################################################
# Default behaviour with messages
library(PhenStat)
dataset1 <- system.file("extdata", "test1.csv", package = "PhenStat")
test <- PhenList(dataset = read.csv(dataset1, na.strings = '-'),
                 testGenotype = "Sparc/Sparc")

# Out-messages are switched off
test <- PhenList(
  dataset = read.csv(dataset1, na.strings = '-'),
  testGenotype = "Sparc/Sparc",
  outputMessages = FALSE
)


###################################################
### code chunk number 5: R_hide004
###################################################
library(PhenStat)
dataset1 <- system.file("extdata", "test3.csv", package="PhenStat")

test <- PhenList(dataset=read.csv(dataset1,na.strings = '-'),
                 dataset.clean=TRUE,
                 dataset.values.female=1,
                 dataset.values.male=2,
                 testGenotype="Mysm1/+")


###################################################
### code chunk number 6: R_hide005
###################################################
library(PhenStat)
dataset1 <- system.file("extdata", "test3.csv", package="PhenStat")

test <- PhenList(dataset=read.csv(dataset1,na.strings = '-'),
                 dataset.clean=TRUE,
                 dataset.values.female=1,
                 dataset.values.male=2,
                 testGenotype="Mysm1/+")

PhenStat:::getDataset(test)
test


###################################################
### code chunk number 7: R_hide006
###################################################
library(PhenStat)
dataset2 <- system.file("extdata", "test2.csv", package="PhenStat")

test2 <- PhenList(dataset=read.csv(dataset2,na.strings = '-'),
                  testGenotype="Arid4a/Arid4a",
                  dataset.colname.weight="Weight.Value")

PhenStat:::testGenotype(test2)
PhenStat:::refGenotype(test2)


###################################################
### code chunk number 8: R_hide007 (eval = FALSE)
###################################################
## file <- system.file("extdata", "test1.csv", package = "PhenStat")
## test = PhenStat:::PhenList(dataset = read.csv(file, na.strings = '-'),
##                            testGenotype = "Sparc/Sparc")
## PhenStatReport(test,
##                depVariable = 'Bone.Area',
##                open = TRUE)


###################################################
### code chunk number 9: R_hide008
###################################################
library(PhenStat)
dataset1 <- system.file("extdata", "test1.csv", package="PhenStat")

test <- PhenList(dataset=read.csv(dataset1,na.strings = '-'),
                 testGenotype="Sparc/Sparc",
                 outputMessages=FALSE)

# Default behaviour
result <- testDataset(test,
                      depVariable="Bone.Area",
                      equation="withoutWeight")

# Perform each step of the MM framework separatly
result <- testDataset(test,
                      depVariable="Bone.Area",
                      equation="withoutWeight",callAll=FALSE)

# Estimated model effects
linearRegressionResults <- PhenStat:::analysisResults(result)
linearRegressionResults$model.effect.batch

linearRegressionResults$model.effect.variance

linearRegressionResults$model.effect.weight

linearRegressionResults$model.effect.sex

linearRegressionResults$model.effect.interaction

# Change the effect values: interaction effect will stay in the model
result <- testDataset(test,
                      depVariable="Bone.Area",
                      equation="withoutWeight",
                      keepList=c(TRUE,TRUE,FALSE,TRUE,TRUE),
                      callAll=FALSE)

result <- PhenStat:::finalModel(result)

summary(result)


###################################################
### code chunk number 10: R_hide009
###################################################
PhenStat:::testFinalModel(result)

PhenStat:::classificationTag(result)


###################################################
### code chunk number 11: R_hide010
###################################################
file <- system.file("extdata", "test7_TFE.csv", package="PhenStat")
test <- PhenList(dataset=read.csv(file,na.strings = '-'),
                 testGenotype="het",
                 refGenotype = "WT",
                 dataset.colname.sex="sex",
                 dataset.colname.genotype="Genotype",
                 dataset.values.female="f",
                 dataset.values.male= "m",
                 dataset.colname.weight="body.weight",
                 dataset.colname.batch="Date_of_procedure_start")

# TFDataset function creates cleaned dataset - concurrent controls dataset
test_TF <- PhenStat:::TFDataset(test,depVariable="Cholesterol")

# TF method is called
result  <- testDataset(test_TF,
                       depVariable="Cholesterol",
                       method="TF")
summary(result)


###################################################
### code chunk number 12: R_hide011
###################################################
library(PhenStat)
file <- system.file("extdata", "test1.csv", package="PhenStat")
test <- PhenList(dataset=read.csv(file,na.strings = '-'),
                 testGenotype="Sparc/Sparc")

# RR method is called
result <- testDataset(test,
                      depVariable="Lean.Mass",
                      method="RR")
summary(result)


###################################################
### code chunk number 13: R_hide012
###################################################
library(PhenStat)
dataset_cat <- system.file("extdata", "test_categorical.csv", package="PhenStat")

test_cat <- PhenList(read.csv(dataset_cat,na.strings = '-'),testGenotype="Aff3/Aff3")

result_cat <- testDataset(test_cat,
                          depVariable="Thoracic.Processes",
                          method="FE")

PhenStat:::getVariable(result_cat)

PhenStat:::method(result_cat)

summary(result_cat)


###################################################
### code chunk number 14: R_hide013
###################################################
library(PhenStat)
dataset1 <- system.file("extdata", "test1.csv", package="PhenStat")

# MM framework
test <- PhenList(dataset=read.csv(dataset1,na.strings = '-'),
                 testGenotype="Sparc/Sparc",outputMessages=FALSE)

result <- testDataset(test,
                      depVariable="Lean.Mass",
                      outputMessages=FALSE)

summary(result)


###################################################
### code chunk number 15: R_hide014
###################################################
library(PhenStat)
dataset_cat <- system.file("extdata", "test_categorical.csv", package="PhenStat")

test2 <- PhenList(dataset=read.csv(dataset_cat,na.strings = '-'),
                  testGenotype="Aff3/Aff3",outputMessages=FALSE)

result2 <- testDataset(test2,
                       depVariable="Thoracic.Processes",
                       method="FE",outputMessages=FALSE)

summary(result2)


###################################################
### code chunk number 16: R_hide015
###################################################
library(PhenStat)
dataset_cat <- system.file("extdata", "test_categorical.csv", package="PhenStat")

test_cat <- PhenList(dataset=read.csv(dataset_cat,na.strings = '-'),
                     testGenotype="Aff3/Aff3",outputMessages=FALSE)

result_cat <- testDataset(test_cat,
                          depVariable="Thoracic.Processes",
                          method="FE",outputMessages=FALSE)

PhenStat:::vectorOutput(result_cat)


###################################################
### code chunk number 17: R_hide016
###################################################
library(PhenStat)
dataset_cat <- system.file("extdata", "test_categorical.csv", package="PhenStat")

test_cat <- PhenList(dataset=read.csv(dataset_cat,na.strings = '-'),
                     testGenotype="Aff3/Aff3",outputMessages=FALSE)

result_cat <- testDataset(test_cat,
                          depVariable="Thoracic.Processes",
                          method="FE",outputMessages=FALSE)

#vectorOutputMatrices(result_cat)


###################################################
### code chunk number 18: R_hide017
###################################################
library(PhenStat)
dataset_cat <- system.file("extdata", "test_categorical.csv", package="PhenStat")

test_cat <- PhenList(dataset=read.csv(dataset_cat,na.strings = '-'),
                     testGenotype="Aff3/Aff3",outputMessages=FALSE)

result_cat <- testDataset(test_cat,
                          depVariable="Thoracic.Processes",
                          method="FE",outputMessages=FALSE)

plot(result_cat)


###################################################
### code chunk number 19: R_hide018
###################################################
library(PhenStat)
dataset_cat <- system.file("extdata", "test_categorical.csv", package="PhenStat")

test_cat <- PhenList(dataset=read.csv(dataset_cat,na.strings = '-'),
                     testGenotype="Aff3/Aff3",outputMessages=FALSE)

result_cat <- testDataset(test_cat,
                          depVariable="Thoracic.Processes",
                          method="FE",outputMessages=FALSE)

plot(result_cat)


###################################################
### code chunk number 20: R_hide019
###################################################
library(PhenStat)
dataset1 <- system.file("extdata", "test1.csv", package="PhenStat")

# MM framework
test <- PhenList(dataset=read.csv(dataset1,na.strings = '-'),
                 testGenotype="Sparc/Sparc",outputMessages=FALSE)

result <- testDataset(test,
                      depVariable="Lean.Mass",
                      outputMessages=FALSE)

PhenStat:::boxplotSexGenotype(test,
                              depVariable="Lean.Mass",
                              graphingName="Lean Mass")

PhenStat:::scatterplotSexGenotypeBatch(test,
                                       depVariable="Lean.Mass",
                                       graphingName="Lean Mass")

PhenStat:::scatterplotGenotypeWeight(test,
                                     depVariable="Bone.Mineral.Content",
                                     graphingName="BMC")

# All in one
#plot(
#  test,
#  depVariable = 'Lean.Mass',
#  type = c(
#  'boxplotSexGenotype',
#  'scatterplotSexGenotypeBatch',
#  'scatterplotGenotypeWeight'
#  )
#  )


###################################################
### code chunk number 21: R_hide020
###################################################
library(PhenStat)
dataset1 <- system.file("extdata", "test1.csv", package="PhenStat")

# MM framework
test <- PhenList(dataset=read.csv(dataset1,na.strings = '-'),
                 testGenotype="Sparc/Sparc",outputMessages=FALSE)

result <- testDataset(test,
                      depVariable="Lean.Mass",
                      outputMessages=FALSE)

# All plots together
# plot(result)

PhenStat:::qqplotGenotype(result)

PhenStat:::qqplotRandomEffects(result)

PhenStat:::qqplotRotatedResiduals(result)

PhenStat:::plotResidualPredicted(result)

PhenStat:::boxplotResidualBatch(result)


