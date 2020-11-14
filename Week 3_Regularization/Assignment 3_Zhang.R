library(ncvreg)
library(bigmemory)
library(biglasso)

data(colon)
X <- colon$X
y <- colon$y
dim(X)

X[1:5, 1:5]

# convert X to a big.matrix.object
X.bm <- as.big.matrix(X)
str(X.bm) # X.bm is a pointer to the data matrix
dim(X.bm)

X.bm[1:5, 1:5] #same results as X[1:5, 1:5]

# fit entire solution path, using our newly proposed screening rule "SSR-BEDPP"
fit <- biglasso(X.bm, y, screen = "SSR-BEDPP")
plot(fit)

# 10-fold cross-valiation in parallel
cvfit <- cv.biglasso(X.bm, y, seed = 1234, nfolds = 10, ncores = 4)

# plot the cross-validation plots
par(mfrow = c(2,2), mar = c(3.5, 3.5, 3, 1), mgp = c(2.5, 0.5, 0))
plot(cvfit, type = "all")

# summarize CV object
summary(cvfit)

# Extract non-zero coefficients at the optimal λ value 0.0165448
coef(cvfit)[which(coef(cvfit) !=0)]
