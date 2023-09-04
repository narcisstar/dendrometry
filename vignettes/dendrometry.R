## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----Citation-----------------------------------------------------------------
citation("dendrometry")

## ----package------------------------------------------------------------------
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

## ----round, eval=TRUE---------------------------------------------------------
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

## ----Lorey \' s and simple mean diffrence, echo=FALSE-------------------------
Height.Mean <- mean(Tree$upHeight)
Height <- c(Height.Mean, Lorey)
names(Height) <- c("Mean of Height(m)" ,"Lorey height(m)")
Height

## ----Mean diameter------------------------------------------------------------
Dm <- diameterMean(Tree$DBH)
Dm

## ----Mean diameter vs simple mean of dbh, echo=FALSE--------------------------
Diam <- mean(Tree$DBH)
Diameter <- c(Diam, Dm)
names(Diameter) <- c("Simple mean of Diameter(cm)" ,"Mean diameter(cm)")
Diameter

## ----Logging dataset----------------------------------------------------------
data("Logging")
summary(Logging)

## ----factorize----------------------------------------------------------------
class(Logging$tree)
Logging <- factorize(data = Logging)
class(Logging$tree)
summary(Logging)
head(Logging, 4)
attach(Logging)

## ----decrease-----------------------------------------------------------------
Decrease <- decrease(middle = diametreMedian, breast = diametreBase) 
Decrease

## ----reducecoef---------------------------------------------------------------
Reduce <- reducecoef(middle = diametreMedian, breast = diametreBase)
Reduce

## ----decreaseMetric-----------------------------------------------------------
DecMetric <- decreaseMetric(dmh = diametreMedian, dbh = diametreBase, 
                            mh = hauteur/2)
DecMetric

## ----decreaseMetric 1.5-------------------------------------------------------
DecMetric1.5 <- decreaseMetric(dmh = diametreMedian, dbh = diametreBase, 
                               mh = hauteur/2, bh = 1.5)
DecMetric1.5

## ----volume-------------------------------------------------------------------
#HUBER
VolHub <- volume(height = hauteur, dm = diametreMedian, method = "huber")
#SMALIAN
VolSmal <- volume(height = hauteur, do = diametreBase, ds = diametreSection, 
       method = "smalian")
#CONE
VolCone <- volume(height = hauteur, do = diametreBase, ds = diametreSection, 
       method = "cone")
#NEWTON 
VolNew <- volume(height = hauteur, do = diametreBase, dm = diametreMedian, 
       ds = diametreSection, method = "newton")

## ----Trees volume-------------------------------------------------------------
TreeVol <- data.frame(tree, VolHub, VolSmal, VolCone, VolNew)
head(TreeVol)

## ----volume successive--------------------------------------------------------
#HUBER
VolHubSuc <- volume(height = hauteur, dm = diametreMedian, method = "huber", 
       successive = TRUE, log = tree)
#SMALIAN
VolSmalSuc <- volume(height = hauteur, do = diametreBase, ds = diametreSection, 
       method = "smalian", successive = TRUE, log = tree)
#CONE
VolConSuc <- volume(height = hauteur, do = diametreBase, ds = diametreSection, 
       method = "cone", successive = TRUE, log = tree)
#NEWTON
VolNewSuc <- volume(height =  hauteur, do = diametreBase, dm = diametreMedian, 
       ds = diametreSection, method = "newton", successive = TRUE, 
       log = tree)
VolNewSuc
volume(height =  hauteur, do = diametreBase, dm = diametreMedian, 
       ds = diametreSection, method = "newton", successive = TRUE, log = tree)

## ----Tree volume successive---------------------------------------------------
TreeVolSuc <- data.frame(tree = unique(tree), VolHubSuc, VolSmalSuc, VolConSuc, VolNewSuc)
TreeVolSuc

## ----shape coefficient--------------------------------------------------------
Shape <- shape(volume = VolNewSuc, height = hauteur, dbh = perimetreMedian)
Shape

