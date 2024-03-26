## ----include = FALSE----------------------------------------------------------
Sys.setenv(LANGUAGE="en")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE-----------------------------------------------------
library(pricelevels) # load package
library(data.table) # load data.table-package

## -----------------------------------------------------------------------------
# example price data with gaps:
dt <- data.table(
  "r"=c(rep(c("a","b"), each=5), rep(c("c","d"), each=3)),
  "n"=as.character(c(rep(1:5, 2), rep(1:3, 2))),
  "p"=round(runif(16, min=10, max=20), 1))

# price matrix:
dt[, tapply(X=p, INDEX=list(n,r), FUN=mean)]

## -----------------------------------------------------------------------------
dt[, sparsity(r=r, n=n)] # sparsity

dt[, is.connected(r=r, n=n)] # is connected

dt[, comparisons(r=r, n=n)] # one group of regions

## -----------------------------------------------------------------------------
# non-connected example price data:
dt2 <- dt[!(n%in%1:3 & r%in%c("a","b")),]

# price matrix:
dt2[, tapply(X=p, INDEX=list(n,r), FUN=mean)]

## -----------------------------------------------------------------------------
dt2[, sparsity(r=r, n=n)] # sparsity

dt2[, is.connected(r=r, n=n)] # is not connected

dt2[, comparisons(r=r, n=n)] # two groups of regions

## -----------------------------------------------------------------------------
# group regions into groups of connected regions:
dt2[, "block" := neighbors(r=r, n=n, simplify=TRUE)]
dt2[, is.connected(r=r, n=n), by="block"]

# subset to biggest group of connected regions:
dt2[connect(r=r, n=n), is.connected(r=r, n=n)]

## -----------------------------------------------------------------------------
R <- 4 # number of regions
N <- 5 # number of products

# set frame:
dt <- data.table("r"=as.character(rep(1:R, each=N)),
                 "n"=as.character(rep(1:N, times=R)))

# add random prices:
set.seed(1)
dt[, "p":=round(x=rnorm(n=.N, mean=as.integer(n), sd=0.1), digits=1)]

# price matrix:
dt[, tapply(X=p, INDEX=list(n,r), FUN=mean)]

## -----------------------------------------------------------------------------
# introduce gaps:
set.seed(1)
dt.gaps <- dt[!rgaps(r=r, n=n, amount=0.2), ]

# price matrix:
dt.gaps[, tapply(X=p, INDEX=list(n,r), FUN=mean)]

## -----------------------------------------------------------------------------
# introduce gaps but not for region 1:
set.seed(1)
dt.gaps <- dt[!rgaps(r=r, n=n, amount=0.2, exclude=data.frame(r="1", n=NA)), ]

# price matrix:
dt.gaps[, tapply(X=p, INDEX=list(n,r), FUN=mean)]

## -----------------------------------------------------------------------------
# introduce gaps with probability:
set.seed(1)
dt.gaps <- dt[!rgaps(r=r, n=n, amount=0.2, prob=as.integer(n)^2), ]

# price matrix:
dt.gaps[, tapply(X=p, INDEX=list(n,r), FUN=mean)]

## -----------------------------------------------------------------------------
# constant expenditure weights:
dt.gaps[, "w1" := rweights(r=r, b=n, type=~1)]
dt.gaps[, tapply(X=w1, INDEX=list(n,r), FUN=mean)]

# weights different for basic headings but same among regions:
dt.gaps[, "w2" := rweights(r=r, b=n, type=~n)]
dt.gaps[, tapply(X=w2, INDEX=list(n,r), FUN=mean)]

# weights different for basic headings and regions:
dt.gaps[, "w3" := rweights(r=r, b=n, type=~n+r)]
dt.gaps[, tapply(X=w3, INDEX=list(n,r), FUN=mean)]

## -----------------------------------------------------------------------------
# weights not necessarily add up to 1 if there are gaps:
dt.gaps[, list("w1"=sum(w1),"w2"=sum(w2),"w3"=sum(w3)), by="r"]

## -----------------------------------------------------------------------------
# simulate random price data:
set.seed(123)
srp <- rdata(
  R=9, # number of regions
  B=4, # number of product groups
  N=c(2,2,3,3), # number of individual products per basic heading
  gaps=0.2, # share of gaps
  weights=~b, # same varying expenditure weights for regions
  sales=0.1, # share of sales
  settings=list(par.add=TRUE) # add true parameters to function output
  )

# true parameters:
srp$param

# price data:
head(srp$data)

## ----echo=FALSE, fig.width=7, fig.height=4, fig.align='center'----------------
# plot prices by basic heading and product:
boxplot(log(price)~paste(group, product, sep=":"), 
        data=srp$data, 
        xlab="basic heading / product",
        col="skyblue")
title("Prices by basic heading and individual product")

## ----fig.width=7, fig.align='center'------------------------------------------
# simulate random price data:
set.seed(1)
dt <- rdata(R=5, B=1, N=4)
head(dt)

## -----------------------------------------------------------------------------
dt[, jevons(p=price, r=region, n=product, base="1")]

## -----------------------------------------------------------------------------
dt[, dutot(p=price, r=region, n=product, base="2")]

## -----------------------------------------------------------------------------
# harmonic mean index fails:
all.equal(
 dt[, harmonic(p=price, r=region, n=product, base="1")][2],
 1/dt[, harmonic(p=price, r=region, n=product, base="2")][1],
 check.attributes=FALSE)

# carli fails:
all.equal(
 dt[, carli(p=price, r=region, n=product, base="1")][2],
 1/dt[, carli(p=price, r=region, n=product, base="2")][1],
 check.attributes=FALSE)

# jevons index ok:
all.equal(
 dt[, jevons(p=price, r=region, n=product, base="1")][2],
 1/dt[, jevons(p=price, r=region, n=product, base="2")][1],
 check.attributes=FALSE)

# dutot index ok:
all.equal(
 dt[, dutot(p=price, r=region, n=product, base="1")][2],
 1/dt[, dutot(p=price, r=region, n=product, base="2")][1],
 check.attributes=FALSE)


## -----------------------------------------------------------------------------
# unit value index:
dt[, uvalue(p=price, r=region, n=product, q=quantity, base="1")]

## -----------------------------------------------------------------------------
dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="1")]

## -----------------------------------------------------------------------------
dt[, "share" := price*quantity/sum(price*quantity), by="region"]
dt[, laspeyres(p=price, r=region, n=product, w=share, base="1")]

## -----------------------------------------------------------------------------
# CPD estimation with respect to regional average:
dt[, cpd(p=price, r=region, n=product, q=quantity, base=NULL)]

# CPD estimation with respect to region 1:
dt[, cpd(p=price, r=region, n=product, q=quantity, base="1")]

# same price levels with shares as weights:
dt[, cpd(p=price, r=region, n=product, w=share, base="1")]

# NLCPD estimation with shares as weights:
dt[, nlcpd(p=price, r=region, n=product, w=share, base="1")]

## -----------------------------------------------------------------------------
# full CPD regression output:
dt[, cpd(p=price, r=region, n=product, w=share, simplify=FALSE)]

## -----------------------------------------------------------------------------
set.seed(123)
dt.big <- rdata(R=50, B=1, N=30, gaps=0.25)

# don't use jacobian matrix:
system.time(m1 <- dt.big[, nlcpd(p=price, r=region, n=product, q=quantity,
                              settings=list(use.jac=FALSE), simplify=FALSE,
                              control=minpack.lm::nls.lm.control("maxiter"=200))])

# use jacobian matrix:
system.time(m2 <- dt.big[, nlcpd(p=price, r=region, n=product, q=quantity,
                              settings=list(use.jac=TRUE), simplify=FALSE,
                              control=minpack.lm::nls.lm.control("maxiter"=200))])

# less computation time needed for m2, but same results as m1:
all.equal(m1$par, m2$par, tol=1e-05)

## -----------------------------------------------------------------------------
# introduce 20% data gaps:
set.seed(1)
dt.gaps <- dt[!rgaps(r=region, n=product, amount=0.2)]

# estimate CPD model using different base regions and check transitivity:
P1.cpd <- dt.gaps[, cpd(p=price, r=region, n=product, q=quantity, base="1")]
P3.cpd <- dt.gaps[, cpd(p=price, r=region, n=product, q=quantity, base="3")]
all.equal(P1.cpd[2], P1.cpd[3]*P3.cpd[2], check.attributes=FALSE)

# estimate NLCPD model using different base regions and check transitivity:
P1.nlcpd <- dt.gaps[, nlcpd(p=price, r=region, n=product, q=quantity, base="1")]
P3.nlcpd <- dt.gaps[, nlcpd(p=price, r=region, n=product, q=quantity, base="3")]
all.equal(P1.nlcpd[2], P1.nlcpd[3]*P3.nlcpd[2], check.attributes=FALSE)

## -----------------------------------------------------------------------------
# GEKS using Törnqvist:
dt.gaps[, geks(p=price, r=region, n=product, q=quantity, settings=list(type="toernqvist"))]

# GEKS using Jevons so quantities have no impact:
dt.gaps[, geks(p=price, r=region, n=product, q=quantity, settings=list(type="jevons"))]

## -----------------------------------------------------------------------------
# estimate GEKS-Törnqvist using different base regions and 
# applying weights in second aggregation stage:
P1.geks <- dt.gaps[, geks(p=price, r=region, n=product, q=quantity, base="1",
                          settings=list(type="toernqvist", wmethod="obs"))]
P3.geks <- dt.gaps[, geks(p=price, r=region, n=product, q=quantity, base="3",
                          settings=list(type="toernqvist", wmethod="obs"))]

# check transitivity:
all.equal(P1.geks[2], P1.geks[3]*P3.geks[2], check.attributes=FALSE)

## -----------------------------------------------------------------------------
# sample data with gaps:
set.seed(123)
dt.big <- rdata(R=99, B=1, N=50, gaps=0.25)

# iterative processing:
system.time(m1 <- dt.big[, gkhamis(p=price, r=region, n=product, q=quantity,
                                   settings=list(solve="iterative"))])

# matrix algebra:
system.time(m2 <- dt.big[, gkhamis(p=price, r=region, n=product, q=quantity,
                                   settings=list(solve="matrix"))])

# compare results:
all.equal(m1, m2, tol=1e-05)

## -----------------------------------------------------------------------------
# Geary-Khamis index transitive:
P1.gk <- dt.gaps[, gkhamis(p=price, r=region, n=product, q=quantity, base="1")]
P3.gk <- dt.gaps[, gkhamis(p=price, r=region, n=product, q=quantity, base="3")]
all.equal(P1.gk[2], P1.gk[3]*P3.gk[2], check.attributes=FALSE)

# multilateral Carli index transitive:
P1.carli <- dt.gaps[, mcarli(p=price, r=region, n=product, base="1")]
P3.carli <- dt.gaps[, mcarli(p=price, r=region, n=product, base="3")]
all.equal(P1.carli[2], P1.carli[3]*P3.carli[2], check.attributes=FALSE)

## -----------------------------------------------------------------------------
dt.gaps[, gerardi(p=price, r=region, n=product, q=quantity, base="1", settings=list(variant="adjusted"))]

## -----------------------------------------------------------------------------
# Gerardi index transitive:
P1 <- dt.gaps[, gerardi(p=price, r=region, n=product, q=quantity, base="1")]
P3 <- dt.gaps[, gerardi(p=price, r=region, n=product, q=quantity, base="3")]
all.equal(P1[2], P1[3]*P3[2], check.attributes=FALSE)

## -----------------------------------------------------------------------------
# example data:
dt <- data.table(
  "region"=rep(letters[1:5], c(3,3,7,4,4)),
  "product"=as.character(c(1:3, 1:3, 1:7, 4:7, 4:7)))
set.seed(123)
dt[, "price":=rnorm(n=.N, mean=20, sd=2)]
dt[, "quantity":=rnorm(n=.N, mean=999, sd=100)]

# price matrix:
with(dt, tapply(X=price, list(product, region), mean))

## -----------------------------------------------------------------------------
dt[, is.connected(r=region, n=product)] # true

## -----------------------------------------------------------------------------
# bilateral jevons index:
dt[, jevons(p=price, r=region, n=product, base="a")]

# multilateral unweighted cpd index:
dt[, cpd(p=price, r=region, n=product, base="a")]

## -----------------------------------------------------------------------------
# drop region 'c':
dt2 <- dt[!region%in%"c",]

# price matrix:
with(dt2, tapply(X=price, list(product, region), mean))

# check if connected:
dt2[, is.connected(r=region, n=product)]

## -----------------------------------------------------------------------------
# bilateral jevons index:
dt2[, jevons(p=price, r=region, n=product, base="a")]

# multilateral unweighted cpd index:
dt2[, cpd(p=price, r=region, n=product, base="a")]

## -----------------------------------------------------------------------------
dt2[, cpd(p=price, r=region, n=product, base="a", settings=list(chatty=FALSE))]

## ----error=TRUE---------------------------------------------------------------
dt2[, cpd(p=price, r=region, n=product, base="a", settings=list(connect=FALSE))]

## -----------------------------------------------------------------------------
# example data:
dt <- data.table(
  "region"=c("a","a","a","b","b","b","b"),
  "product"=as.character(c(1,1,2,1,1,2,2)))
set.seed(123)
dt[, "price":=rnorm(n=.N, mean=20, sd=2)]
dt[, "quantity":=rnorm(n=.N, mean=999, sd=100)]
dt[1, "price" := NA_real_]

dt[, cpd(p=price, r=region, n=product)]

## -----------------------------------------------------------------------------
# example data:
set.seed(123)
dt <- rdata(R=5, B=1, N=7, gaps=0.2)

# cpd:
dt[, cpd(p=price, r=region, n=product, base="1")]

# geks-jevons:
dt[, geks(p=price, r=region, n=product, base="1", setting=list(type="jevons"))]

# jevons:
dt[, jevons(p=price, r=region, n=product, base="1")]

## -----------------------------------------------------------------------------
dt[, pricelevels(p=price, r=region, n=product, base="1", settings=list(type=c("jevons","cpd","geks-jevons")))]

## -----------------------------------------------------------------------------
dt[, pricelevels(p=price, r=region, n=product, base="1")]

## -----------------------------------------------------------------------------
dt[, pricelevels(p=price, r=region, n=product, q=quantity, base="1")]

