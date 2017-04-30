mod <- R.rspss("data/child2.sav") ## numeric variables ##
mod <- na.omit(dat2.n)
mod.c <- within(mod, {## could also do this with an "apply()" function ##
    boyfriend <- boyfriend - mean(boyfriend)
    program <- program - mean(program)
    welfare <- welfare - mean(welfare)
    white <- white - mean(white)
})

names(mod) <- c("y", "x1", "x2", "x3", "x4")
names(mod.c) <- c("y", "x1", "x2", "x3", "x4")
R.msmm(mod)
R.msmm(mod.c)

library(lavaan) ## "sem()" ##
m1 <-  'y ~ x1 + x2 + x3 + x4'
m1.fit <- sem(model = m1, data = mod)
summary(m1.fit)

cov(mod)

mod <- specify.model()
 1: AF -> D, NA, 1
 2: STW -> D, NA, 1
 3: D -> MFIPV, lam1, NA
 4: AF <-> STW, psi1, NA
 5: STW <-> AF, psi2, NA