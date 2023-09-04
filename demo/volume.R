## Volume computation ##
library(dendrometry)
#data(Logging)
attach(Logging)

head(Logging)
volume(height = hauteur, dm = diametreMedian, method = "huber")
volume(height = hauteur, dm = diametreMedian, method = "huber",
       successive = TRUE, log = tree)

#SMALIAN
volume(height = hauteur, do = diametreBase, ds = diametreSection,
       method = "smalian")
volume(height = hauteur, do = diametreBase, ds = diametreSection,
       method = "smalian", successive = TRUE, log = tree)

#CONE
volume(height = hauteur, do = diametreBase, ds = diametreSection,
       method = "cone")
volume(height = hauteur, do = diametreBase, ds = diametreSection,
       method = "cone", successive = TRUE, log = tree)

#NEWTON
volume(height = hauteur, do = diametreBase, dm = diametreMedian,
       ds = diametreSection, method = "newton")
volume(height =  hauteur, do = diametreBase, dm = diametreMedian,
       ds = diametreSection, method = "newton", successive = TRUE,
       log = tree)

detach(Logging)