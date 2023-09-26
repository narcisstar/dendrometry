# Data in Package ####
#' @details
#' Type \code{RShowDoc("dendrometry", package = "dendrometry")} to read a HTML
#' user guide vignette.
#'
# Type \code{RShowDoc("dendrometry_pdf", package = "dendrometry")} to read a PDF user guide vignette.
#'
#' Type \code{demo(dendro, package = "dendrometry")} for a demo of dendrometric computations. Click on \code{Index} bellow to see the index of the package.
#'
#' Type \code{demo(volume, package = "dendrometry")} for a demo of dendrometric computations. Click on \code{Index} bellow to see the index of the package.
#' @keywords internal
"_PACKAGE"


#' Dendrometric measures on tree
#' @docType data
#' @description Data frame of 10 rows and 5 columns containing tree measures.
#' @format Data frame with ten observations and five variables:
#' \describe{
#'   \item{circum}{Tree circumference in centimeter (cm).}
#'   \item{dist}{Horizontal distance between the observer (person who measure angles) and the tree circumference in centimeter (cm).}
#'   \item{up}{Up angle measure in degree (°).}
#'   \item{down}{Down angle measure in degree (°).}
#'   \item{fut}{Bole angle measure in degree (°); Bole is where the first branch occurs on the trunk. This measure is usually useful for timber estimation on wood market.}
#' }
#' @usage data(Tree)
#' @examples #demo(dendro)
#' @source Fake data simulated for tutorial purposes.
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
"Tree"

#' Tree metrics for logging
#' @docType data
#' @description Data frame of 24 rows and 8 columns containing tree measures.
#' @format Data frame with twenty five observations and eight variables:
#' \describe{
#'   \item{tree}{Tree name}
#'   \item{hauteur}{Stem lenght in meter (m).}
#'   \item{diametreMedian}{Tree median diameter in centimeter (cm).}
#'   \item{perimetreMedian}{Tree median circumference in centimeter (cm).}
#'
#'   \item{diametreSection}{Tree diameter at the end in centimeter (cm).}
#'   \item{perimetreSection}{Tree circumference at the end in centimeter (cm).}
#'
#'   \item{diametreBase}{Tree diameter at the base in centimeter (cm).}
#'   \item{perimetreBase}{Tree circumference at the base in centimeter (cm).}
#' }
#' @usage data(Logging)
#' @examples #demo(volume)
#' @source Fake data simulated for tutorial purposes.
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
"Logging"

# Useful functions ####
#' Sample size
#'
#' @param confLev numeric, the confidence level. Default is \code{0.05}.
#' @param popPro numeric, proportion of population which have considered factor.
#' Default is \code{0.5}.
#'
#' @param errorMargin numeric, margin error. Default is \code{0.05}.
#' @param size integer, population size when it is known. If not specified,
#' simple random sampling will be used. Allows infinite.
#'
#' @param method optional character string specifying method to use if not
#' simple adjusted is desired. Only "cauchran" is implemented now.
#'
#' @param cv variation coefficient.
#' @note Population size to be considered as large or infinite heavily depends on error margin. Lower error margin increases population size to be conidered as large or infinite. For errorMargin = .05, size = 152 231 and cauchran 151 760 when confLev = .05
#' @return The sample size.

#' @examples sampleSize(confLev = .95, popPro = 0.4, errorMargin = .05)
#' sampleSize(confLev = .95, popPro = 0.5, errorMargin = .05, size = 150)
#' sampleSize(confLev = .95, popPro = 0.5, errorMargin = .05, size = 150,
#' method = "cauchran")
#' sampleSize()
#'
#' @export
sampleSize <- function(confLev = .95, popPro = .5, errorMargin = .05,
                       size = NULL, method = "", cv = NULL){
  alpha <- 1 - confLev
  if (!missing(cv)) {
    n <- qt(p = 1 - (alpha/2), df = size -1)**2 * cv**2 /errorMargin**2
    return(n)
  }
  if(is.null(size) || is.infinite(size)){
    n <- qnorm(p = 1 - (alpha/2))**2 * popPro * (1 - popPro)/errorMargin**2
    return(n)
  }
  else if(method == "cauchran"){
    if(size >= 30)
      n <- qnorm(p = 1 - (alpha/2))**2 * popPro * (1 - popPro)/errorMargin**2
    else
      n <- qt(p = 1 - (alpha/2), df = size -1)**2 * popPro * (1 - popPro)/errorMargin**2
    nc <- n/((n - 1)/size + 1)
    return(nc)
  }
  else{
    if(size >= 30)
      n <- qnorm(p = 1 - (alpha/2))**2 * popPro * (1 - popPro)/errorMargin**2
    else
      n <- qt(p = 1 - (alpha/2), df = size -1)**2 * popPro * (1 - popPro)/errorMargin**2
    np <- size * n/(size + n)
    return(np)
  }
}

#' Make stand data
#' @description Make data of stands according to defined \code{factor1,factor2,factor3}.
#' @param data data frame containing optional factors \code{factor1,factor2,factor3}.
#' @param factor1,factor2,factor3 optional variables of the data frame that define subsets to consider.
#' @return A list of data.
#' @examples
#' # require(BiodiversityR)
#' # data(ifri, package = "BiodiversityR")
#' #a1=makedata(ifri, factor1 = "forest", factor2 = "plotID", factor3 = "species")
#' #a2=makedata(ifri, factor1 = "species")
#' #makedata(ifri, factor2 = "")
#' #identical(makedata(ifri), ifri)
#' @export
makedata <- function(data, factor1 = "", factor2 = "", factor3 = ""){
  if(!is.character(c(factor1, factor2, factor3)))
    stop("When difined, 'factor1', 'factor2' and/or 'factor3' should be character.
         Please, quote them when using.")
  if(factor1 != "")
    pos1 <- match(factor1, names(data))
  if(factor2 != "")
    pos2 <- match(factor2, names(data))
  if(factor3 != "")
    pos3 <- match(factor3, names(data))

  if((factor1 != "")){
    dataf <- lapply(unique(data[[pos1]]), function(i){
      dataf <- subset(data, data[[pos1]] == i)

      if(factor2 != ""){
        datafp <- lapply(unique(dataf[[pos2]]), function(j){
          datafp <- subset(dataf, dataf[[pos2]] == j)

          if(factor3 != ""){
            datafps <- lapply(unique(datafp[[pos3]]), function(k){
              subset(datafp, datafp[[pos3]] == k)
            })
            names(datafps) <- unique(datafp[[pos3]])
            datafps
          }
          else
            datafp
        })
        names(datafp) <- unique(dataf[[pos2]])
        datafp
      }
      else if(factor3 != ""){
        datafs <- lapply(unique(dataf[[pos3]]), function(k){
          subset(dataf, dataf[[pos3]] == k)
        })
        names(datafs) <- unique(dataf[[pos3]])
        datafs
      }
      else
        dataf
    })
    names(dataf) <- unique(data[[pos1]])
    return(dataf)
  }
  else if(factor2 != ""){
    datap <- lapply(unique(data[[pos2]]), function(j){
      datap <- subset(data, data[[pos2]] == j)

      if(factor3 != ""){
        dataps <- lapply(unique(datap[[pos3]]), function(k){
          subset(datap, datap[[pos3]] == k)
        })
        names(dataps) <- unique(datap[[pos3]])
        dataps
      }
      else
        datap
    })
    names(datap) <- unique(data[[pos2]])
    return(datap)
  }
  else if(factor3 != ""){
    datas <- lapply(unique(data[[pos3]]), function(k){
      subset(data, data[[pos3]] == k)
    })
    names(datas) <- unique(data[[pos3]])
    return(datas)
  }
  else
    warning("Any of 'factor1', 'factor2' and 'factor3' are not defined. The same data are returned.")
  #stop("At least one of 'factor1', 'factor2' and 'factor3' should be defined.")
  return(data)
}

#' Making factor vectors
#' @description Changes character vectors of a data set to factor vectors.
#' @aliases factorise
#' @param data data frame or tibble data set.
#' @param binary logical indicating if binary data should be considered as factor.
#' Default is \code{FALSE}.
#' @return Data frame with all character vectors changed to factor vectors.
#' @details When \code{binary = TRUE}, variables stored as numeric and which have
#' exactly two levels are changed to factor.
#' @export
factorize <- function(data, binary = FALSE){
  for (i in 1:dim(data)[2]) {
    if(is.character(data[[i]]) ||
       (binary && is.numeric(data[[i]]) &&
        length(levels(as.factor(data[[i]]))) == 2))
      data[[i]] <-  as.factor(data[[i]])

    else data[[i]] <- data[[i]]
  }
  return(as.data.frame(data))
}

#' @export
factorise <- factorize

#' Stack all vectors of a data frame or list
#' @description Stacking all columns of a data frame or vectors of a list into a single vector.
#' @param data data frame, tibble or list.
#' @return A vector of all element of the argument \code{data}.
#' @export
stacking <- function(data){
  if(inherits(data, what = "list"))
    data <- list2DF(data)
  as.vector(as.matrix(data))
}


#' Fibonacci series
#' @description Generates numbers from Fibonacci series.
#' @param n integer, the size of the series.
#' @param Uo,U1 integer, the first two numbers of the series.
#' @param PrintFib logical, indicating if the series should be printed.
#' @return Either an integer, result of the function or a vector of \code{n} first
#' numbers of the series.
#' @examples fibonacci(n = 10, PrintFib = TRUE)
#' fibonacci(n = 10, Uo = 1, U1 = 3, PrintFib = FALSE)
#' @details The series equation is Un = U_(n-2) /U_(n-1).
#' @seealso \code{\link{fiboRate}}
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
#' @export
fibonacci <- function(n, PrintFib = FALSE, Uo = 0, U1 = 1){
  Un <- numeric(length = n)
  Un[1:2] <- c(Uo, U1)

  if(n < 2) return(Uo)

  else if (n > 2){
    for (i in 3:n){
      Un[i] <- Un[i-1] + Un[i-2]
    }
    Fib <- Un
  }
  else
    Fib <- Un

  if(!PrintFib)
    Fib <- Fib[n]

  return(Fib)
}

#' Fibonacci series ratio
#' @description Computes rates from Fibonacci series.
#' @param n integer, the size of the series.
#' @param Uo,U1 integer, the first number of the series.
#' @param PrintSer logical, indicating if the series should be printed.
#' @return Either a numeric, result of the rate of \code{nth} and \code{(n-1)th} numbers
#' in Fibonacci series or all \code{(n-1)th} those rates.
#' @examples ##Golden number (Le Nombre d'Or)
#' fiboRate(n = 18, PrintSer = FALSE, Uo = 0, U1 = 1)
#' ##(1+sqrt(5))/2
#' fiboRate(n = 10, PrintSer = TRUE, Uo = 0, U1 = 1)
#' @details The series equation is Un = U_(n-2) /U_(n-1).
#' The function returns golden number when Uo = 0, and U1 = 1. Larger n is, more precise the number (result) is.
#' @seealso \code{\link{fibonacci}}
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
#' @export

fiboRate <- function(n, PrintSer = FALSE, Uo = 0, U1 = 1){
  a <- fibonacci(n = n, Uo = Uo, U1 = U1, PrintFib = TRUE)
  #$$U_n = \frac{U_{n-2}}{U_{n-1}}$$
  serie <- a[2:n]/a[1:n-1]
  if(PrintSer) return(serie)
  else return(serie[n-1])
}

#' Radians to degrees
#' @description Converts angle values from  radian to degree.
#' @param radian A vector of radian values to be converted.
#' @return A vector of degree values.
#' @examples deg(pi/2)
#' @seealso \code{\link{rad}}, the reciprocal of \code{deg}.
#' @export

deg <- function (radian)
{
  radian * (180/pi)
}

#' Degrees to radians
#' @description Converts angle values from degree to radian.
#' @param degree A numeric vector of degree values to be converted.
#' @return A vector of radian values.
#' @examples rad(180)
#' @seealso \code{\link{deg}}, the reciprocal of \code{rad}.
#' @export
rad <- function (degree)
{
  degree * (pi/180)
}

#' Height of tree or vertical object.
#' @description Computes the height of tree, pillar, girder, mast or any
#' vertical object. Allows both slope (in per cent) and angle measures
#' (in degree or radian) . No matter the relative position of the persons who
#' measures angle/slope.
#' @usage height(distance, top, base, type = c("angle", "slope"),
#'        angleUnit = c("deg", "rad"))
#' @param distance numeric vector of the horizontal distance between object
#' and the person who measures angle.
#' @param top,base numeric vector of top angle and ground angle respectively
#' (readings from a clinometer).
#' @param type the type of \code{top} and \code{base} measures. Either
#' \code{"angle"} or \code{"slope"}. Default is \code{"slope"}.
#' @param  angleUnit the unit of \code{top} and \code{base} measures when
#' \code{type = "angle"}. Either \code{"deg"} for degree or \code{"rad"} for
#' radian. Default is \code{"deg"}.
#' @return A vector of heights.
#' @examples height(10, 80, 17)
#' height(17, top = -18, base = -113)
#' height(distance = 18, top = 42, base = -12, type = "angle", angleUnit = "deg")
#' height(distance = 18:21, top = 42:45, base = -12:-15, type = "angle", angleUnit = "deg")
#' ## Bellow shows warning messages
#' height(distance = 18:21, top = -42:-45, base = -12:-15, type = "angle", angleUnit = "deg")
#' @author Narcisse Yehouenou \email{narcisstar211@gmail.com}
#' @export
height <- function(distance, top, base, type = c("angle", "slope"),
                   angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg") {
    VH <- rad(top)
    VB <- rad(base)
  }
  else if (!(angleUnit %in% c("deg", "rad"))) {
    stop("angleUnit should be either  'deg' or 'rad'")
  }

  if (prod(type == c("angle", "slope"))) {
    type <- "slope"
  }
  else if (!(type %in% c("angle", "slope"))) {
    stop("type should be either  'angle' or 'slope'")
  }

  if (sum(top <= base))
    warning("One or more top angles are less than their down angles.
          Please check your data")

  if (type ==  "slope") {
    h <- 0.01*distance * (top - base)
  }
  else
    h <- distance * (tan(VH) - tan(VB))
  return(h)
}

#' Diameter or DBH
#' @description Computes diameter based on circumference. If circumference (perimeter) at breast height is given, then Diameter at Breast Height (\code{DBH}) is obtained. Used in dendrometry for trees' \code{DBH} calculation.
#' @param circum numeric vector of circumference.
#' @return A vector of diameter or \code{DBH}.
#' @examples x = seq(1, 5, .4)
#' dbh(x)
#' @seealso \code{\link{height}} for tree  height, \code{\link{circum}} for diameter.
#' @export
dbh <- function(circum){
  if (is.numeric(circum))
    return(circum/pi)
  else stop("must be numeric")
}

#' Circumference or perimeter
#' @description Computes circumference based on diameter.
#' @param dbh numeric vector of diameter
#' @return A vector of circumference.
#' @examples x = seq(1, 5, .4)
#' circum(x)
#' @seealso \code{\link{height}} for tree  height, \code{\link{dbh}} for diameter.
#' @export
circum <- function(dbh){
  if (is.numeric(dbh))
    return(dbh * pi)
  else stop("must be numeric")
}

#' Angle to slope
#' @description Converts angle values to  slope values.
#' @param angle numeric vector of angle to be converted to slope.
#' @param  angleUnit The unit of \code{angle}. Either \code{"deg"}, \code{"rad"}. Default is \code{"deg"}.
#' @return A vector of slope values.
#' @examples angle2slope(10)
#' angle2slope(angle = 45)
#' angle2slope(angle = 50, angleUnit = "deg")
#' angle2slope(1.047198, "rad")
#' angle2slope(0.2617994, angleUnit = "rad")
#' @seealso \code{\link{slope2angle}}, the reciprocal of \code{angle2slope}.
#' @export
angle2slope <- function(angle, angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg")
    angle <- rad(angle)

  else if (!(angleUnit %in% c("deg", "rad")))
    stop("angleUnit should be either  'deg' or 'rad'")
  slp <- 100 * tan(angle)
  return(slp)
}

#' Slope to angle
#' @description Converts slope values to angle values.
#' @param slope numeric vector of slope to be converted to angle.
#' @param  angleUnit the desired unit for the returned angle value. Either \code{"deg"} or \code{"rad"}. Default is \code{"deg"}.
#' @return A vector of angle values in specified unit.
#' @examples slope2angle(100)
#' slope2angle(17.6327)
#' slope2angle(angle2slope(30))
#' @seealso \code{\link{angle2slope}}, the reciprocal of \code{slope2angle}
#' @export
slope2angle <- function(slope, angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg")
    return(deg(atan(.01 * slope)))

  else if (!(angleUnit %in% c("deg", "rad")))
    stop("angleUnit should be either  'deg' or 'rad'")

  return(atan(.01 * slope))
}

#' Horizontal distance
#' @description Horizontal distance calculation for sloping area.
#' @usage distanceH(distance, angle, type = c("angle", "slope"),
#'           angleUnit = c("deg", "rad"))
#' @param distance numeric vector of the distance measured on sloping area.
#' @param angle numeric vector of angle values.
#' @param type type of \code{angle}. Either \code{"angle"} or
#' \code{"slope"}. Default is \code{"slope"}.
#' @param  angleUnit unit of \code{angle} measures when
#' \code{type = "angle"}. Either \code{"deg"} for degree or \code{"rad"} for
#' radian. Default is \code{"deg"}.
#' @return A vector of horizontal distance.
#' @examples distanceH(20, 30)
#' distanceH(20, angle = 30, type = "slope")
#' distanceH(20, angle = 25, type = "angle")
#' @export
#'
distanceH <- function(distance, angle, type = c("angle", "slope"),
                      angleUnit = c("deg", "rad")){
  if (prod(type == c("angle", "slope")) || type == "slope")
    d <- distance * cos(slope2angle(slope = angle, angleUnit = "rad"))
  else{
    if(prod(angleUnit == c("deg", "rad")) || angleUnit == "deg")
      d <- distance * cos(rad(angle))
    else if (angleUnit == "rad")
      d <- distance * cos(angle)
  }
  return(d)
}


#' Principal measure
#' @description Principal measure of an angle value.
#' Principal measure ranges from -pi to pi for radian unit while it ranges from
#' -180 to 180 for degree unit.
#' @param angle numeric vector of angle.
#' @param  angleUnit The unit of \code{angle}. Either "deg" or "rad".
#' Default is "deg".
#' @return A matrix of principal measure of angle in both radian and in degree units.
#' @export
#' @seealso \code{\link{rad}} for radian, \code{\link{deg}} for degree,
#' \code{\link{slope2angle}} for slope to angle conversion,
#' \code{\link{angle2slope}} for angle to slope conversion.
#' @examples principal(303)
#' principal(23 * pi/8, "rad")
#' @note Use \code{principal} in position computations, not distance computations.
principal <- function(angle, angleUnit = c("deg", "rad")){
  if (prod(angleUnit == c("deg", "rad")) || angleUnit == "deg")
    angle <- rad(angle)

  else if (!(angleUnit %in% c("deg", "rad")))
    stop("angleUnit should be either  'deg' or 'rad'")
  radian <- Arg(complex(real = cos(angle), imaginary = sin(angle)))
  degree <- deg(radian)
  return(cbind(radian, degree))
}

# Tree parameter ####
#' Individual basal area
#' @aliases basal2dbh
#' @usage basal2dbh(basal)
#' @description The area of a circle of diameter \code{dbh}.
#' @usage basal_i(dbh, circum = NULL)
#' @param dbh numeric vector of diameter.
#' @param circum numeric vector of circumference. Is used only when \code{dbh}
#' is not given.
#' @param basal numeric, individual basal area.
#' @return \code{basal_i} returns individual basal area while \code{basal2dbh} returns DBH.
#' @examples basal_i(dbh = 10)
#' basal_i(circum = 31.41)
#' basal2dbh(78.53982)
#' @details If \code{circum} is given, \code{dbh} is not used.
#' @export
basal_i <- function(dbh = NULL, circum = NULL){
  if(is.numeric(circum))
    dbh <- dbh(circum = circum)
  else if(!is.numeric(dbh))
    stop("Should be numeric.")
  return(pi * .25 * dbh**2)
}

#' @export
basal2dbh <- function(basal) {
  2 * sqrt(basal/pi)
}


#' The decrease coefficient
#' @description This coefficient expresses the ratio between the diameter
#' (or circumference) at mid-height of the bole and the diameter
#' (or circumference) measured at breast height.
#' @param middle numeric, the diameter or circumference at middle height.
#' @param breast numeric, the diameter or circumference at breast height.
#' @details Both \code{middle} and \code{breast} arguments should be of the
#' same type (either diameter or circumference). Not mixture.
#' @return A vector of decrease coefficients.
#' @examples decrease(30, 120)
#' decrease(middle = 40, breast = 90)
#' @export
decrease <- function(middle, breast){
  if(!is.numeric(middle))
    stop("'middle' should be numeric")

  if(!is.numeric(breast))
    stop("'breast' should be numeric")

  if(any(middle > breast))
    warning("One or more breast value are less than their middle values.
          Please check your data")
  return(middle/breast)
}


#' The reduction coefficient
#' @description The reduction coefficient is the ratio between the difference
#' in size at breast height and mid-height on the one hand, and the size at
#' breast height on the other. It is thus the complement to 1 of the
#' coefficient of decrease.
#' @param middle numeric, the diameter or circumference at middle height.
#' @param breast numeric, the diameter or circumference at breast height.
#' @details Both \code{middle} and \code{breast} arguments should be of the
#' same type (either diameter or circumference). Not mixture.
#' @return The reduction coefficient.
#' @examples reducecoef(30, 120)
#' reducecoef(middle = 40, breast = 90)
#' @seealso \code{decrease}
#' @export
reducecoef <- function(middle, breast){
  if(!is.numeric(middle))
    stop("'middle' should be numeric")

  if(!is.numeric(breast))
    stop("'breast' should be numeric")

  if(any(middle > breast))
    warning("One or more breast value are less than their middle values.
          Please check your data")
  r <- (breast - middle)/breast
  return(r)
}


#' Metric scrolling or decay
#' @description The average metric decay expresses the difference, in
#' centimeters per meter, between the diameter (or circumference) at breast
#' height and its diameter at mid-height of a stem related to the difference
#' between the height at mid-height and that at breast height.
#' @param dmh numeric, the diameter at middle height in centimeter (cm).
#' @param dbh numeric, the diameter at breast height in centimeter (cm).
#' @param mh numeric, the middle (or cut) height in meter (m).
#' @param bh Either a numeric value standing for the breast height in meter (m)
#' of all trees or a numeric vector standing for the breast height of each tree.
#' Default is \code{1.3}.
#' @return Metric decay
#' @examples decreaseMetric(dmh = 40, dbh = 90, mh = 7)
#' decreaseMetric(45, 85, 9)
#' @seealso \code{reducecoef}
#' @export
decreaseMetric <- function(dmh, dbh, mh, bh = 1.3){
  if(!is.numeric(dbh))
    stop("'dbh' should be numeric")
  if(!is.numeric(dmh))
    stop("'dmh' should be numeric")
  if(!is.numeric(mh))
    stop("'mh' should be numeric")

  if(any(dmh > dbh))
    warning("One or more middle height diameter are greater than their DBH.
            Please check your data")
  if(any(mh <= bh))
    warning("One or more middle height are greater or equal to their breast height.
            Please check your data")
  d <- (dbh - dmh)/(mh - bh)
  return(d)
}


.huberMethod <- function(height, dm, circum, successive, log){

  if(is.null(dm) && is.null(circum))
    stop("Specify either 'dm' or 'circum'")
  else if(!is.null(dm) && !is.null(circum))
    warning("Don't specify both 'dm' (diameter) and 'circum' (circumference).
            Only 'dm' is considered.")

  if(is.null(dm))
    dm <- dbh(circum)
  v <- .25 * pi * dm**2 * height

  if(successive)
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))

  return(v)
}
.smalianMethod <- function(height, do, ds, circumo, circums, successive,
                           log){
  # Don't specify mixture of args
  if(all(any(is.null(circumo), is.null(circums)),
         any(is.null(do), is.null(ds))))
    stop("Specify either both 'circumo' and 'circums' or both 'do' and 'ds'
           when using 'smalian' method.")

  else if(sum(!is.null(do), !is.null(ds), !is.null(circumo),
              !is.null(circums)) > 2)
    warning("Don't specify both diameters and circumferences.")
  # le && est inutile
  if(!is.null(do) && !is.null(ds))
    v <- .125 * pi * (do**2 + ds**2) * height
  else if(!is.null(circumo) && !is.null(circums))
    v <- .125 * pi * (dbh(circumo)**2 + dbh(circums)**2) * height

  if(successive)
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  return(v)
}
.coneMethod <- function(height, do, ds, circumo, circums, successive, log){
  # Don't specify mixture of args
  if(all(any(is.null(circumo), is.null(circums)),
         any(is.null(do), is.null(ds))))
    stop("Specify either 'circumo' and 'circums' or 'do' and 'ds'
           when using 'cone' method.")

  else if(sum(!is.null(do), !is.null(ds), !is.null(circumo),
              !is.null(circums)) > 2)
    warning("Don't specify both diameters and circumferences.")
  # le && est inutile
  if(!is.null(do) && !is.null(ds))
    v <- pi * (do**2 + do * ds + ds**2) * height/12
  else if(!is.null(circumo) && !is.null(circums))
    v <- pi * (dbh(circumo)**2 + dbh(circumo) * dbh(circums) + dbh(circums)**2) *
      height/12
  if(successive)
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  return(v)
}
.newtonMethod <- function(height, do, dm, ds, circumo, circum, circums,
                          successive, log){
  if(all(any(is.null(circum), is.null(circumo), is.null(circums)),
         any(is.null(dm), is.null(do), is.null(ds))))
    stop("Specify either 'circum', 'circumo' and 'circums' or 'dm', 'do' and 'ds'
           when using 'newton' method.")
  #"Specify either only diameters or only circumferences."
  else if(sum(is.null(dm), !is.null(do), !is.null(ds), is.null(circum),
              !is.null(circumo), !is.null(circums)) > 3)
    warning("Don't specify both diameters and circumferences.")

  if(!is.null(dm) && !is.null(do) && !is.null(ds))
    v <- pi * (do**2 + 4 * dm**2 + ds**2) * height/24
  else if(!is.null(circum) && !is.null(circumo) && !is.null(circums))
    v <- pi * (dbh(circumo)**2 + 4 * dbh(circum)**2 + dbh(circums)**2) *
      height/24

  if(successive)
    v <- sapply(unique(log), FUN = function(i) sum(v[log == i]))
  return(v)
}


#' Tree stem and log Volume
#' @description Determining the volume of the log or of the tree.
#' @usage volume(height, dm, do, ds, circum, circumo, circums,
#'        method = "huber", successive = FALSE, log)
#' @param height numeric, stem (whole bole) length. When \code{successive} is "\code{TRUE}",
#' it stands for log length.
#' @param do,dm,ds numeric, respectively base, median and end diameter.
#' @param circumo,circum,circums numeric, respectively base, median and end
#' circumference.
#' @param method character string, the method of volume computation. Can be one
#' of "\code{huber}", "\code{smalian}", "\code{cone}", or "\code{newton}".
#' Default is "\code{huber}".
#' @param successive logical. If \code{TRUE}, Successive method is applied.
#' is applied. Default is \code{FALSE}.
#' @param log a vector indicating tree to which belongs each log.
#' Is used only when \code{successive} is "\code{TRUE}".
#' @examples ## huber method
#' volume(height = 10, dm = 35)
#' volume(height = 10, circum = 100)
#'
#' ## smalian method
#' volume(height = 10, do = 45, ds = 15, method = "smalian")
#' volume(height = 10, circumo = 200, circums = 110, method = "smalian")
#'
#' ## cone method
#' volume(height = 10, do = 45, ds = 15, method = "cone")
#' volume(height = 10, circumo = 200, circums = 110, method = "cone")
#'
#' ## newton method
#' volume(height = 10, dm = 35, do = 45, ds = 15, method = "newton")
#' volume(height = 10, circum = 100, circumo = 200, circums = 110, method = "newton")
#' @return A numeric vector of logs or trees volume.
#' @details Using \code{method = cone} refers to truncated cone method.
#' @seealso \code{\link{shape}}, for shape coefficient.
#' @export
volume <- function(height, dm = NULL, do = NULL, ds = NULL, circum = NULL,
                   circumo = NULL, circums = NULL, method = "huber",
                   successive = FALSE, log = NULL){
  if(!(method %in% c("huber", "smalian", "cone", "newton")))
    stop("'method' should be one of 'huber', 'smalian', 'cone', or 'newton'.")

  if(all(!successive, !is.null(log)))
    warning("Don't specify 'log' when 'successive' is not TRUE")# unused arg ...
  if(method == "huber") return(.huberMethod(height = height, dm = dm,
                                            circum = circum, log = log,
                                            successive = successive))
  else if(method == "smalian") return(.smalianMethod(height = height, do = do,
                                                     circumo = circumo,
                                                     ds = ds,
                                                     circums = circums,
                                                     log = log,
                                                     successive = successive))

  else if (method == "cone") return(.coneMethod(height = height, do = do,
                                                ds = ds, circumo = circumo,
                                                circums = circums,
                                                successive = successive,
                                                log = log))
  else if (method == "newton") return(.newtonMethod(height = height, do = do,
                                                    dm = dm, ds = ds,
                                                    circumo = circumo,
                                                    circum = circum,
                                                    circums = circums,
                                                    successive = successive,
                                                    log = log))
}


#' The shape coefficient
#' @description The shape coefficient of the tree is the ratio of the actual
#' volume of the tree to the volume of a cylinder having as base the surface of
#' the section at 1.3 m (or a given breast height) and as length, the height
#' (at bole level) of the tree.
#' @usage shape(volume, height, dbh, basal = NULL)
#' @param volume numeric, tree real volume.
#' @param height numeric, tree height.
#' @param dbh numeric, diameter at breast height (DBH).
#' @param basal numeric, basal area. Is used when \code{dbh} is not specified.
#' @examples shape(volume = 10000, 11, dbh = 40)
#' shape(volume = 10000, 11, 40)
#' shape(volume = 10000, 11, basal = 2256.637)
#' ## Bellow gives warning
#' shape(volume = 10000, height = 11, dbh = 40, basal = 2256.637)
#' @return The shape coefficient.
#' @seealso \code{\link{volume}}, for tree real volume.
#' @export
shape <- function(volume, height, dbh = NULL, basal = NULL){
  if(all(is.null(dbh), is.null(basal)))
    stop("Specify either 'dbh' or 'basal'")
  else if((!any(is.null(dbh), is.null(basal))))
    warning("Both of 'dbh' and 'basal' are specified. Only 'dbh' is considered.")

  if(!is.null(dbh))
    f <- volume/(basal_i(dbh = dbh) * height)
  else
    f <- volume/(basal * height)
  return(f)
}

# Stand parameter ####

#' Tree density
#' @description Density per plot.
#' @param number numeric vector of individual count in each plot.
#' @param area numeric, area of a plot.
#' @param overall logical. If \code{TRUE}, an overall density is
#' computed; if \code{FALSE}, density is computed for each plot.
#' Default is \code{TRUE}.
#' @return Vector of density.
#' @details If every plot have same area, \code{area} is a numeric otherwise
#' \code{area} is a vector of each plot area.
#' @export
densityTree <- function(number, area, overall = TRUE){
  if (!overall) {
    return(number/area)
  } else return(mean(number/area))
}

#' Mean diameter
#' @description Mean diameter of a forestry stand.
#' @param dbh numeric vector of diameter.
#' @param factor1,factor2,factor3 optional variables of the data frame that define subsets to consider.
#' @param data data frame containing optional factors \code{factor1,factor2,factor3}.
#' @return Mean diameter.
#' @seealso \code{\link{dbh}}, \code{\link{basal_i}}
#' @examples set.seed(1)
#' diameter = rnorm(10, 100, 20)
#' diameterMean(dbh = diameter)
#' @export
diameterMean <- function(dbh, factor1 = "", factor2 = "", factor3 = "", data){
  if(!missing(data)){
    pos <- match(dbh, names(data))
    data <- makedata(data = data, factor1 = factor1, factor2 = factor2, factor3 = factor3)
    lev <- sum(factor1 != "", factor2 != "", factor3 != "")

    if(lev == 3)
      d <- lapply(data, function(i){
        lapply(i, function(j){
          sapply(j, function(k){
            diamMean(k[[pos]])
          })
        })
      })

    else if(lev == 2)
      d <- lapply(data, function(i){
        sapply(i, function(j){
          diamMean(j[[pos]])
        })
      })

    else if(lev == 1)
      d <- sapply(data, function(i){
        diamMean(i[[pos]])
      })
  }
  else if(any(c(factor1, factor2, factor3) != ""))
    stop("Any of 'factor1', 'factor2'and/or 'factor3' is not used when 'data' is not defined.")
  else
    d <- diamMean(dbh = dbh)

  return(d)
}

#' Root mean square
#' @param x numeric vector
#' @return The root mean square.
#' @export
rms <- function(dbh){
  sqrt(mean(dbh**2))
}
diamMean <- rms

#' Lorey's mean height
#' @description The average height of the trees in a plot, weighted by their basal area.
#' @param basal numeric, individual basal areas.
#' @param  height numeric vector of individual heights.
#' @return Average Lorey height of a species.
#' @seealso \code{\link{height}}, \code{\link{basal_i}}
#' @examples set.seed(1)
#' donnee <- data.frame(hauteur = rnorm(10, 12, 3), area = basal_i(rnorm(10, 100, 20)))
#' loreyHeight(basal = donnee$area, height = donnee$hauteur)
#' @export
loreyHeight <- function(basal, height){
  sum(basal * height)/sum(basal)
}

#' Skewness coefficient
#' @param x numeric vector.
#' @examples data("Logging")
#' skewness(Logging$hauteur)
#' hist(Logging$hauteur,3)
#' @return The skewness coefficient.
#' @import stats
#' @export
skewness <- function(x){
  x <- na.omit(x)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  sk <- n * (sum(((x - m)/s)**3))/((n - 1) * (n - 2))
  return(sk)
}

#' Index of Blackman
#' @param density numeric vector of the density.
#' @return Index of Blackman.
#' @export
blackman <- function(density) {
  b <- var(density)/mean(density)
  return(b)
}

#' Index of Green
#' @param density numeric vector of the density.
#' @return Index of Green.
#' @export
green <- function(density) {
  g <- (blackman(density) - 1)/(length(density) - 1)
  return(g)
}

#' The basal area of stands
#' @description The basal area is the cross sectional area of the bole or stem of a tree at breast height.
#' @param dbh numeric vector of diameter. If \code{data} is specified, character indicating name of variable of \code{data} containing dbh values.
#' @param area numeric, area of a plot (see \code{details} for unit).
#' @param factor1,factor2,factor3 character, optional variables of the data frame that define subsets to consider.
#' @param data data frame containing optional factors \code{factor1,factor2,factor3}.
#' @param constant numeric, used to convert diameter unit. Default is \code{100} (see \code{details})
#' @return A vector of basal area of stands. If more than one factor set, a list.
#'
#' @details If \code{area} is expressed in ha and \code{dbh} expressed in cm, the basal area unit is cm²/ha when \code{constant = 1}.
#' In order to convert centimeter (cm) to meter (m) for \code{dbh}, set \code{constant = 100}. Because 1m = 100 cm. Then, basal area unit will be \code{m²/ha}.
#'
#' If \code{dbh} is in meter (m), and \code{area} in in hectare (ha), setting \code{constant = 1} returns basal area in m²/ha.
#'
#' If \code{dbh} is in feet, and \code{area} in acre, setting \code{constant = 1} returns basal area in ft²/ac.
#'
#' If \code{dbh} is in inch, and \code{area} in acre, setting \code{constant = 12} returns basal area in feet²/acres (ft²/ac).
#' @export
basal <- function(dbh, area, factor1 = "", factor2 = "", factor3 = "", data, constant = 100){
  if(!missing(data)){
    pos <- match(dbh, names(data))
    data <- makedata(data = data, factor1 = factor1, factor2 = factor2, factor3 = factor3)
    lev <- sum(factor1 != "", factor2 != "", factor3 != "")

    if(lev == 3)
      b <- lapply(data, function(i){
        lapply(i, function(j){
          sapply(j, function(k){
            a <- (basal_i(dbh = k[[pos]]))
            sum(a)/(area * constant**2)
          })
        })
      })

    else if(lev == 2)
      b <- lapply(data, function(i){
        sapply(i, function(j){
          a <- basal_i(dbh = j[[pos]])
          sum(a)/(area * constant**2)
        })
      })

    else if(lev == 1)
      b <- sapply(data, function(i){
        a <- basal_i(dbh = i[[pos]])
        sum(a)/(area * constant**2)
      })
  }
  else
    b <- sum(basal_i(dbh = dbh))/(area * constant**2)
  return(b)
}

# In developpement ####

#' param for general; To be deleted
paramDendro <- function(data, Site = "Releve", area = .15, DBH = "DBH",
                        height = "Height", houppier = "houppier", group = TRUE){
  data <- na.omit(data)
  DIM <- dim(data)
  #head(data)

  res <-setNames(as.data.frame(table(data[[Site]])/area), c("Site", "Density"))

  res$MeanDBH <- diameterMean(dbh = DBH, factor1 = Site, data = data)
  res$Houppier <- diameterMean(dbh = houppier, factor1 = Site, data = data)

  res$basal <- basal(dbh = DBH, area = area, factor1 = Site, data = data)
  browser()
  #head(data[height])
  #head(res)

  res$MeanHeight <- mean(data[[height]])
  a <- (levels(res$Site))
  b <- numeric(length(a))
  for(i in a){
    b[i] = mean(data[data[Site] == i, height])
  }
  b
  sapply(data[[Site]], mea)
  #res$LoreyHeight <- loreyHeight() # adjust pkg function
  #head(res)
}

#' Structural parameter
#' @usage param(data, factor1 = "Commune", factor2 = "Releve", factor3 = "Species",
#' area = 0.15, DBH = "DBH", height = "Height", houppier = "houppier", group = TRUE)
#' @export
param <- function(data, plot = "Releve", area = .15, factor1 = "", factor2 = "",
                  factor3 = "", DBH = "DBH", height = "Height",
                  houppier = "houppier"){
  lev <- sum(factor1 != "", factor2 != "", factor3 != "")
  if(lev == 1){
    data <- makedata(data = data, factor1 = factor1)
    res <- sapply(X = data, function(data){
      nbPlot <- sum(!duplicated(data[[plot]]))

      MeanDBH <- diameterMean(dbh = data$DBH)
      Basal <- basal(dbh = data$DBH, area = area * nbPlot)

      MeanHoup <- diameterMean(dbh = data$houppier)
      BasalHoup <- basal(dbh = data$houppier, area = area * nbPlot, constant = 1)

      Height <- mean(data$Height)
      LoreyHeight <- loreyHeight(basal = basal_i(dbh = data$DBH),
                                 height = data$Height)

      Density <- densityTree(number = dim(data)[1], area = area * nbPlot)

      c(MeanDBH = MeanDBH, Basal = Basal, MeanHoup = MeanHoup,
        BasalHoup = BasalHoup, Height = Height, LoreyHeight = LoreyHeight,
        Density = Density)

    })
    return(t(res))
  }

  nm <- names(data)
  #nm %in% c(factor1, factor2, factor3)
  pos <- match(c(factor1, factor2, factor3), nm)
  names(data)[pos] <- c("factor1", "factor2", "factor3")

  res <- aggregate(DBH ~ factor1 + factor2 + factor3, diameterMean, data = data)
  names(res)[length(names(res))] <- "MeanDBH"

  res$Basal <- aggregate(DBH ~ factor1 + factor2 + factor3, data = data, basal,
                         area = area)[[4]] # problem with area if basal is not computed by plot (releve)
  res$MeanHoup <- aggregate(houppier ~ factor1 + factor2 + factor3, diameterMean,
                            data = data)[[4]]
  res$BasalHoup <- aggregate(houppier ~ factor1 + factor2 + factor3, data = data,
                             basal, area = area, constant = 1)[[4]] # problem with area if basal is not computed by plot (releve)


  res$MeanHeight <- aggregate(Height ~ factor1 + factor2 + factor3, data = data,
                              mean)[[4]]


  data$basal_i <- basal_i(dbh = data[[DBH]])
  data$basalHeight <- data$basal_i * data[[height]]
  res$LoreyHeight <- aggregate(basalHeight ~ factor1 + factor2 + factor3, data = data,
                               sum)[[4]]
  res$LoreyHeight <- res$LoreyHeight/aggregate(basal_i ~ factor1 + factor2 +
                                                 factor3, data = data, sum)[[4]]

  res$Density <- aggregate(DBH ~ factor1 + factor2 + factor3, data = data,
                           length)[[4]]
  res$Density <- densityTree(number = res$Density, area = area, overall = FALSE)

  names(res)[1:3] <- nm[pos]
  return(res)
}

#' Biomass
#'
#' @export
biomass <- function(DBH, height, wdensity){
  AGB <- 0.0673 * (wdensity * DBH**2 * height)^.976
  BGB <- 0.37 * AGB
  TB <- 1.37 + AGB
  return(setNames(c(TB, AGB, BGB), c("TB", "AGB", "BGB")))
}


#' To abbreviate  species name from 'Gender epithet' to 'G. epithet'
#' @description To abbreviate  species name from 'Gender epithet' to 'G. epithet'.
#' Useful in plots with species names.
#' @param name a factor coerceable vector of species name in forms 'Gender epithet'.
#' @param sep character string which separates Gender and epithet. Default is space " ".
#' @details Returned reduced names are made unique.
#' @return A factor vector of species reduced names in forms 'G. epithet'.
#' @seealso \code{make.cepnames} in \code{vegan} package.
#' @export
spNmReduce <- function(name, sep = " "){
  name <- as.factor(name)
  nm <- strsplit(levels(name), sep)
  nm <- sapply(nm, function(sp)
    paste(substr(sp[1], 1, 1), ". ", paste(sp[-1], collapse = " "),sep = ""))
  levels(name) <- make.unique(nm)
  return(name)
}

#' Create Data Frame from Raster
#' @description
#' Create a data frame from (stack) raster
#'
#' @param biodata Stack raster
#' @param ext Spatial extent ...
#' @param smdup Logical. Should duplicates be removed ?
#' @param plot Logical. Should raters be plotted ?
#' @param which character, indicates data in which duplicates should be checked.
#' 'c' for coordinates, 'd' for data without coordinates, 'b' for both.
#' @importFrom raster getValues stack ncell
#' @importFrom terra plot crop xyFromCell
#' @export
raster2df <- function(biodata, ext = c(0, 4, 6, 14), rmdup = TRUE, plot = TRUE,
                      which = "c"){
  bio <- list()
  for (i in seq_len(dim(biodata)[3]))
    bio[[i]] <- crop(biodata[[i]], ext)
  biodata <- stack(bio)

  if(plot) plot(biodata) # needs debug !!!
  myvar <- getValues(biodata) #we will have NA for non-land points
  myxy <- xyFromCell(biodata[[1]],cell=1:ncell(biodata[[1]])) #central coordinates of cells
  biodata <-cbind.data.frame(myxy, myvar)
  biodata <- na.omit(biodata) #It will remain only Benin points

  if(rmdup){
    dup <- switch(which, c = duplicated(biodata[c(1, 2)]),
                  d = duplicated(biodata[-c(1, 2)]),
                  b = duplicated(biodata))
    if (sum(dup))
      warning(sum(dup), " duplicates found and removed.")
    #sum(dup)
    biodata <- biodata[!dup,]
  }
  return(biodata)
}

#' Climograph
#' Plots the climograph of a location. The climograph is a representation of basic climate of a location. It displays monthly average precipitation and monthly average temperature.
#' @param prec description
#' @param temp description
#' @param month description
#' @param title description
#' @export
ombrothermique <- function(prec, temp, month, title = "Climograph"){
  plot(prec ~ month, main = title,
       xlab = "Month", ylab = "y", lty = 0)
  lines(prec, col = "blue", lwd = 2)
  points(prec, col = "blue", pch = 7)
  lines(temp*2, col = "red", lwd = 2)
  points(temp*2, col = "red", pch = 16)
}

#' Relative Frequency
#' @description
#' Relative Frequency in percentage.
#' @param x a numeric vector.
#' @export
rfreq <- function(x) 100 * x/sum(x)


#' Adjust three-parameter Weibull distribution to data
#' @param x a numeric vector of observations.
#' @param amplitude a numeric value indicating the amplitude of classes.
#' @param shape optional description
#' @examples
#' d <- rweibull(1000, shape = 2.5, scale = 15) + 80
#' res = adjWeibull(d, amplitude = 3, mid = FALSE, shape = 6,
#'                  main = "Weibull adjustment", line.col = "red",
#'                  legendPos = "right")
#' res
#' @import ForestFit graphics
#' @export
adjWeibull <- function(x, amplitude = 10, shape, main = NULL,
                       xlab = 'Diameter class (cm)',
                       ylab='Relative frequency (%)', mid = TRUE,
                       line.col = "blue", bar.col = "grey80",
                       cex.axis = 0.6, cex.lab = 0.8, las = 1,
                       legendPos = "topright", ...){
  if(missing(shape)) shape = 3
  #if("main" %in% ...names())
  #main = ...elt(which(...names() == "main"))
  #else
  #main = NULL

  qq <- hist(x, breaks = seq(min(x), max(x) + 1, 1), plot = FALSE)

  #intial values determination
  loc <-  min(x)
  scale <- which(cumsum(qq$density) >= 0.632)[1]
  scale <- qq$mids[scale] - loc #bb
  starts <- c(shape, scale, loc)

  #estimation of parameteres
  est = ForestFit::fitWeibull(data = x, location = TRUE, method = "mps",
                              starts = starts)
  estimate <- setNames(c(est$estimate), nm = c("shape", "scale", "location"))
  measures <- setNames(c(est$measures), nm = colnames(est$measures))
  est <- list(estimate = estimate, measures = measures)

  loc = est$estimate[3]
  scale = est$estimate[2]
  shape = est$estimate[1]

  f <- function(x, ampl = amplitude){
    den <- dweibull(x - loc, shape = shape, scale = scale, log = FALSE)
    100 * ampl * den
  }

  qq <- hist(x, breaks = seq(min(x), max(x) + amplitude, amplitude), plot = FALSE)

  qq$density <- rfreq(qq$density)
  plot(qq, freq = FALSE, col = bar.col, xlim = c(min(qq$breaks), max(qq$breaks)),
       las = las, xlab = xlab, ylab= ylab, cex.axis = cex.axis, cex.lab = cex.lab,
       ylim = c(0, min(max(qq$density) + diff(range(qq$density)) * .2, 100)), #xlim = c(0, 100) #labels = paste0(qq$density, "%"),
       #fg = "red", sub = "sub title"#, yaxp = c(0, 70, 7) # Ctrl la graduation
       main = main, ...)

  if(mid)
    graphics::curve(f, col = line.col, add = T, from = min(qq$mids), to = max(qq$mids))
  else
    curve(f, col = line.col, add = T, from = min(qq$breaks), to = max(qq$breaks))
  graphics::legend(legendPos, title = "Legend", title.col = "black", lty = c(1, 0, 0, 0),
                   lwd = 1, col = line.col, text.font = c(4, 5, 5, 5), box.lty = 0,
                   bty = "o", bg = "aliceblue", cex = 1,
                   legend = c("weibull", paste("a =", round(loc, 2)),
                              paste("b =", round(scale, 2)), paste("g =", round(shape, 2))))

  return(invisible(est))
}

#' Shared elements or intersection
#' @description
#' Determine the number of shared elements by two vector
#' as well as the shared elements.
#' @param x,y vectors which shared elements (intersection) are needed.
#' @return A list whose first element is the length of shared elements
#' between two vectors and second element is a vector of shared elements.
#' @export
shared <- function(x, y){
  elmt <- intersect(x, y)
  len <- length(elmt)
  return(list(length = len, element = elmt))
}

#' Sorensen index
#' @param x,y vectors which Sorensen index is needed.
#' @return Sorensen index.
#' @export
sorensen <- function(x, y){
  2 * shared(x, y)$length/sum(length(unique(x)), length(unique(y)))
}


#' Sorensen index for entire study area
#' @param data data frame
#' @param species a character specifying species column name.
#' @param env a character specifying environment column name.
#' @return Matix of Sorensen index.
#' @export

sorensenAll <- function(data, species, env){
  envLev <- as.character(unique(data[env])[,1])
  sapply(envLev, function(envi){
    sapply(envLev, function(envj){
      sorensen(x = data[species][data[env] == envi],
               y = data[species][data[env] == envj])
    })
  })
}
#' Function modified from 'BiodiversityR' package to allow 'base' argument
#' @import vegan
#' @export
diversityvariables <- function (x, y, digits = 8, base = 2)
{
  y$richness <- diversityresult(x, index = "richness", method = "each site",
                                digits = digits)[, 1]
  y$Shannon <- diversityresult(x, index = "Shannon", method = "each site",
                               digits = digits, base = base)[, 1]
  y$Simpson <- diversityresult(x, index = "Simpson", method = "each site",
                               digits = digits, base = base)[, 1]
  y$Pielou <- y$Shannon/log(y$richness, base = base)
  return(y)
}

#' Function modified from 'BiodiversityR' package to allow 'base' argument
#' @importFrom bootstrap jackknife
#' @export
diversityresult <- function (x, y = NULL, factor = NULL, level = NULL, base = 2,
                             index = c("Shannon",
                                       "Simpson", "inverseSimpson", "Logalpha", "Berger", "richness",
                                       "abundance", "Jevenness", "Eevenness", "jack1", "jack2",
                                       "chao", "boot"),
                             method = c("pooled", "each site", "mean",
                                        "sd", "max", "jackknife"),
                             sortit = FALSE, digits = 8)
{
  INDEX <- c("Shannon", "Simpson", "inverseSimpson", "Logalpha",
             "Berger", "richness", "abundance", "Jevenness", "Eevenness",
             "jack1", "jack2", "chao", "boot")
  if ((index %in% INDEX) == F) {
    stop(paste("choose an accepted index, not index: ",
               index, sep = ""))
  }
  METHOD <- c("pooled", "each site", "mean", "sd", "max",
              "jackknife")
  if ((method %in% METHOD) == F) {
    stop(paste("choose an accepted method, not method: ",
               method, sep = ""))
  }
  if (is.null(y) == F) {
    if ((factor %in% names(y)) == F) {
      stop("specified factor '", factor, "' is not a variable of the environmental data frame")
    }
    if (is.factor(y[, factor]) == F) {
      stop("specified factor '", factor, "' is not a factor")
    }
    levels1 <- as.character(levels(as.factor(as.character(y[,
                                                            factor]))))
    if ((level %in% levels1) == F) {
      stop("specified level '", level, "' is not an available factor level")
    }
  }
  if (index %in% c("jack1", "jack2", "chao", "boot") && method !=
      "pooled") {
    cat(paste("\n", "Note that default method for index '",
              index, "' is method 'pooled'", "\n\n", sep = ""))
    method <- "pooled"
  }
  diversityresult0 = function(x, index, method, base) {
    marg <- 1
    if (method == "pooled" && index != "jack1" && index !=
        "jack2" && index != "chao" && index != "boot") {
      x <- apply(x, 2, sum)
      marg <- 2
    }
    if (index == "Shannon") {
      result <- vegan::diversity(x, index = "shannon", MARGIN = marg, base = base)
    }
    if (index == "Simpson") {
      result <- vegan::diversity(x, index = "simpson", MARGIN = marg)
    }
    if (index == "inverseSimpson") {
      result <- vegan::diversity(x, index = "invsimpson", MARGIN = marg)
    }
    if (index == "Logalpha") {
      result <- fisher.alpha(x, MARGIN = 1)
    }
    if (index == "Berger") {
      if (marg == 2) {
        result <- max(x)/sum(x)
      }
      else {
        tots <- as.matrix(apply(x, marg, sum))
        result <- as.matrix(apply(x, marg, max))
        result <- as.matrix(result/tots)[, 1]
      }
    }
    if (index == "richness") {
      if (marg == 2) {
        result <- sum(x > 0)
      }
      else {
        result <- as.matrix(apply(x > 0, marg, sum))
        result <- result[, 1]
      }
    }
    if (index == "abundance") {
      if (marg == 2) {
        result <- sum(x)
      }
      else {
        result <- as.matrix(apply(x, marg, sum))
        result <- result[, 1]
      }
    }
    if (index == "Jevenness") {
      result1 <- vegan::diversity(x, index = "shannon", MARGIN = marg)
      if (marg == 2) {
        result2 <- sum(x > 0)
      }
      else {
        result2 <- as.matrix(apply(x > 0, marg, sum))
        result2 <- result2[, 1]
      }
      result <- result1/log(result2)
    }
    if (index == "Eevenness") {
      result1 <- vegan::diversity(x, index = "shannon", MARGIN = marg)
      if (marg == 2) {
        result2 <- sum(x > 0)
      }
      else {
        result2 <- as.matrix(apply(x > 0, marg, sum))
        result2 <- result2[, 1]
      }
      result <- exp(result1)/result2
    }
    if (index == "jack1") {
      result <- specpool(x)$jack1
    }
    if (index == "jack2") {
      result <- specpool(x)$jack2
    }
    if (index == "chao") {
      result <- specpool(x)$chao
    }
    if (index == "boot") {
      result <- specpool(x)$boot
    }
    return(result)
  }
  options(digits = digits)
  if (is.null(y) == F) {
    subs <- y[, factor] == as.character(level)
    for (q in 1:length(subs)) {
      if (is.na(subs[q])) {
        subs[q] <- F
      }
    }
    x <- x[subs, , drop = F]
    freq <- apply(x, 2, sum)
    subs <- freq > 0
    x <- x[, subs, drop = F]
  }
  x <- as.matrix(x)
  if (dim(x)[1] == 0) {
    result <- array(NA, dim = c(1, 1))
    colnames(result) <- index
    rownames(result) <- "none"
    return(result)
  }
  if (method == "jackknife") {
    thetadiv <- function(x, xdata, index, base) {
      xdata2 <- xdata[x, 1:ncol(xdata)]
      diversityresult0(xdata2, index = index, method = "pooled", base = base)
    }
    if (nrow(x) > 1) {
      result2 <- jackknife(1:nrow(x), thetadiv,
                           x, index = index)
      result2$jack.estimate <- mean(as.numeric(result2$jack.values),
                                    na.rm = T)
    }
    else {
      result2 <- list(jack.values = NA, jack.estimate = NA)
    }
  }
  if (method != "jackknife") {
    result <- diversityresult0(x, index = index, method = method, base = base)
  }
  if (method == "mean") {
    result2 <- result
    result <- result2[1]
    result[1] <- mean(result2)
  }
  if (method == "max") {
    result2 <- result
    result <- result2[1]
    result[1] <- max(result2)
  }
  if (method == "sd") {
    result2 <- result
    result <- result2[1]
    result[1] <- sd(result2)
  }
  if (sortit == T && method != "jackknife" && method != "pooled") {
    result <- sort(result)
  }
  if (method != "jackknife") {
    result2 <- round(result, digits = digits)
    result2 <- data.frame(result2)
    colnames(result2) <- index
  }
  if (method == "pooled") {
    rownames(result2) <- "pooled"
  }
  if (method == "mean") {
    rownames(result2) <- "mean"
  }
  if (method == "max") {
    rownames(result2) <- "max"
  }
  if (method == "sd") {
    rownames(result2) <- "sd"
  }
  if (method != "pooled" && method != "jackknife" && method !=
      "mean" && method != "max" && method != "sd") {
    rownames(result2) <- names(result)
  }
  return(result2)
}



# Basal area contribution (not implemented)
#basalContr <- function(dbh, species, choice, area, factor1 = "", factor2 = "", factor3 = "", data, constant = 100){

#}
