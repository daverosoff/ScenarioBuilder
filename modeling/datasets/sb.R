library(mosaic)
library(tidyverse)

options(digits=4)
wks <- seq(1,12)

sn <- function(x,digits)
{
  if (x==0) return("0")
  ord <- floor(log(abs(x),10))
  x <- x / 10^ord
  if (!missing(digits)) x <- format(x,digits=digits)
  if (ord==0) return(as.character(x))
  return(paste(x,"\\\\times 10^{",ord,"}",sep=""))
}

fuzz <- function(pointlist) {
  n <- length(pointlist)
  fz <- runif(n, min = -0.2, max = 0.2)
  pointlist + fz
}

vpos <- function(v) {
  pmax(rep(0, length(v)), v)
}

getFormula <- function(modeltype) {
  if (modeltype == "S") { 
    "$at e^{-bt}$" 
  } else if (modeltype == "T") { 
    "$A \\\\sin{(0.25t)} + C$"
  } else if (modeltype == "E") { 
    "$A e^{bt}$"
  } else if (modeltype == "P") { 
    "$a_0 + a_1 t + a_2 t^2 + a_3 t^3 + a_4 t^4$"
  }
}

printFormula <- function(modeltype, mod) {
  if (modeltype == "S") {
    
  }
}

randomSurge <- function(pointlist) {
  a <- runif(1, min = 8, max = 14)
  b <- runif(1, min = 0.4, max = 0.6)
  mod <- makeFun(a*t*exp(-b*t)~t, a=a, b=b)
  vpos(mod(pointlist))
}

randomTrig <- function(pointlist)  {
  A <- runif(1, min = 6, max = 8)
  B <- 0.25
  C <- runif(1, min = 0.5, max = 2.5)
  mod <- makeFun(A*sin(B*t) + C~t, A=A, B=B, C=C)
  vpos(mod(pointlist))
}

randomExp <- function(pointlist) {
  A <- runif(1, min = 1.8, max = 2.4)
  b <- runif(1, min = 0.07, 0.12)
  mod <- makeFun(A*exp(b*t)~t, A=A, b=b)
  vpos(mod(pointlist))
}

randomPoly <- function(pointlist) {
  a <- -0.003
  b1 <- runif(1, min=1.05, max=1.20)
  b2 <- runif(1, min=1.95, max=2.05)
  b3 <- runif(1, min=13, max=15)
  mod <- makeFun(a*(t-b1)*(t-b2)^2*(t-b3)~t, a=a, b1=b1, b2=b2, b3=b3)
  vpos(mod(pointlist))
}

res <- function(pointlist, vals, mdl) {
  result <- vals - mdl(pointlist)
  sqrt(sum(result*result))
}

extract <- function(scenario, num=TRUE) {
  if (num) {
    as.numeric(scenario[,2:13])
  } else {
    scenario[,2:13]
  }
}

getModel <- function(xs, ys, mtype, visible=FALSE) {
  if (mtype == "S" | mtype == "surge") {
    mxs <- -1*xs
    project(log(ys)~1+log(xs)+mxs) -> S
    S <- as.vector(S)
    a <- S[1]
    b <- S[2]
    d <- S[3]
    mod <- makeFun(exp(a)*b*t*exp(-d*t)~t, a=a, b=b, d=d)
    if (visible) {
      paste("$", exp(a)*b, "t e^{", -d, "t}$")
    } else { 
      mod 
    }
  }
  else if (mtype == "T" | mtype == "trig") {
    project(ys~1+sin(0.25*xs)) -> TT
    TT <- as.vector(TT)
    a <- TT[1]
    b <- TT[2]
    mod <- makeFun(a + b*sin(0.25*t)~t, a=a, b=b)
    if (visible) {
      paste("$", a, " + ", b, "\\\\sin{(0.25t)}$")
    } else { 
      mod 
    }
  }
  else if (mtype == "E" | mtype == "exp") {
    project(log(ys)~1+xs) -> E
    E <- as.vector(E)
    a <- E[1]
    b <- E[2]
    mod <- makeFun(exp(a)*exp(b*t)~t, a=a, b=b)
    if (visible) {
      paste("$", exp(a), "e^{", b, "t}$")
    } else { 
      mod 
    }
  }
  else if (mtype == "P" | mtype == "poly") {
    xs2 <- xs*xs
    xs3 <- xs2*xs
    xs4 <- xs3*xs
    project(ys~1+xs+xs2+xs3+xs4) -> P
    P <- as.vector(P)
    a1 <- P[1]
    a2 <- P[2]
    a3 <- P[3]
    a4 <- P[4]
    a5 <- P[5]
    mod <- makeFun(a1+a2*t+a3*t^2+a4*t^3+a5*t^4~t,
                   a1=a1, a2=a2, a3=a3, a4=a4, a5=a5)
    if (visible) {
      paste("\\\\begin{multline}", a1, " + ", a2, "t + \\\\\\\\", a3, "t^2 + ", a4, "t^3 + ", a5, "t^4\\\\end{multline}")
    } else { 
      mod 
    }
  } else { # bad input
    -1
  }
}

testData <- function(xs, ys, mtype) {
  mod <- getModel(xs, ys, mtype)
  res(xs, ys, mod)
}

getWinningType <- function(xs, ys) {
  resS <- testData(xs, ys, "S")
  resT <- testData(xs, ys, "T")
  resE <- testData(xs, ys, "E")
  resP <- testData(xs, ys, "P")
  labs <- c("S", "T", "E", "P")
  vals <- c(resS, resT, resE, resP)
  pos <- which.min(vals)
  labs[pos]
}

randomScenario <- function(modeltype, pointlist) {
  if (modeltype == "S") {modelVec <- randomSurge}
  if (modeltype == "T") {modelVec <- randomTrig}
  if (modeltype == "E") {modelVec <- randomExp}
  if (modeltype == "P") {modelVec <- randomPoly}
  repeat {
    p <- modelVec(pointlist)
    if (getWinningType(pointlist, p) == modeltype)
    {
      break
    }
  }
  p <- vpos(fuzz(p))
  pp <- tibble(modelType=modeltype,
               t01=p[1],
               t02=p[2],
               t03=p[3],
               t04=p[4],
               t05=p[5],
               t06=p[6],
               t07=p[7],
               t08=p[8],
               t09=p[9],
               t10=p[10],
               t11=p[11],
               t12=p[12]
  )
  pp
}

# scenarios <- tibble(
#   modelType = character(),
#   t01 = numeric(),
#   t02 = numeric(),
#   t03 = numeric(),
#   t04 = numeric(),
#   t05 = numeric(),
#   t06 = numeric(),
#   t07 = numeric(),
#   t08 = numeric(),
#   t09 = numeric(),
#   t10 = numeric(),
#   t11 = numeric(),
#   t12 = numeric()
#   )

# for (i in seq(1, 10)) { scenarios <- bind_rows(scenarios, randomScenario('S', wks)) }
# for (i in seq(1, 10)) { scenarios <- bind_rows(scenarios, randomScenario('T', wks)) }
# for (i in seq(1, 10)) { scenarios <- bind_rows(scenarios, randomScenario('E', wks)) }
# for (i in seq(1, 10)) { scenarios <- bind_rows(scenarios, randomScenario('P', wks)) }

# scenarios %>% print(n = Inf)

# write_scenarios <- function() {
#   currtime <- gsub(pattern="[\\s :]", replacement = "-", x = lubridate::now())
#   write_csv(scenarios, path=paste0("~/ScenarioBuilder/scenarios/", currtime, "-scenarios.csv"), append=FALSE)
# }
