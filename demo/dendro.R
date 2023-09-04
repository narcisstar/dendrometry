# Demo for dendrometry ####

## ----package loading----------------------------------------------------------
library(dendrometry)

## ----data, collapse=TRUE------------------------------------------------------
data("Tree")
head(Tree)

## ----data description, eval=FALSE---------------------------------------------
#  help("Tree")

## ----dbh----------------------------------------------------------------------
Tree$DBH <- dbh(Tree$circum)
head(Tree)

## ----Height estimation--------------------------------------------------------
Tree$upHeight <- height(distance = Tree$dist, top = Tree$up, base = Tree$down,
                        type = "angle", angleUnit = "deg")
Tree$futHeight <- height(distance = Tree$dist, top = Tree$fut, base = Tree$down,
                         type = "angle", angleUnit = "deg")
head(Tree)

## ----round-------------------------------------------------------
Tree$DBH <- round(Tree$DBH, 2)
Tree$upHeight <- round(Tree$upHeight, 2)
Tree$futHeight <- round(Tree$futHeight, 2)
Tree

## ----slope--------------------------------------------------------------------
Tree$up.slope <- angle2slope(Tree$up)
Tree$up.slope <- round(Tree$up.slope)
Tree

## ----ind basal----------------------------------------------------------------
Tree$basal <- basal_i(dbh = Tree$DBH/100)
Tree$basal <- round(Tree$basal, 4)
Tree

## ----Lorey--------------------------------------------------------------------
Lorey <- loreyHeight(basal = Tree$basal, height = Tree$upHeight)
Lorey

## ---- Remark--------------------------------------------------------------
Height.Mean <- mean(Tree$upHeight)
Height <- c(Height.Mean, Lorey)
names(Height) <- c("Mean of Height(m)" ,"Lorey height(m)")
Height

## ----Mean diameter------------------------------------------------------------
Dm <- diameterMean(Tree$DBH)
Dm

## ---- Remark 2--------------------------------------------------------------
Diam <- mean(Tree$DBH)
Diameter <- c(Diam, Dm)
names(Diameter) <- c("Simple mean of Diameter(cm)" ,"Mean diameter(cm)")
Diameter

rm(Diam, Diameter, Dm, Height, Height.Mean, Lorey, Tree)
