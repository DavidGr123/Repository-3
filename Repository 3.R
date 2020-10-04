
## Exercise 1: Optimzing portfolios


## Exercise 2: Do it yourself

In this exercise you first download the IBoxx Euro Corporate All Maturities ("IBCRPAL") and the EuroStoxx ("DJES50I") index from Datastream - monthly data as long as possible. We will check the calculations of `R`. Calculate discrete monthly returns.



```{r load_packs}
pacman::p_load(tidyverse,tidyquant,FFdownload,PortfolioAnalytics,tsibble,matrixcalc,Matrix,timetk,dplyr,devtools,ggrepel)
pacman::p_load(tidyverse,tidyquant,FFdownload,PortfolioAnalytics,tsibble,matrixcalc,Matrix,timetk,dplyr,devtools,ggrepel,readxl)
```
%>% to pipe

@@ -216,7 +216,7 @@ opt_maxret <- optimize.portfolio(R=stock.returns.timeseries.xts, portfolio=maxre
print(opt_maxret)
plot(opt_maxret, risk.col="StdDev", return.col="mean",
       main="Maximum Return Optimization", chart.assets=TRUE,
       xlim=c(0, 0.15), ylim=c(0,0.013))
       xlim=c(0, 0.3), ylim=c(0,0.013))
```

@@ -275,14 +275,422 @@ chart.EfficientFrontierOverlay(R=stock.returns.timeseries.xts,

In this exercise you first download the IBoxx Euro Corporate All Maturities ("IBCRPAL") and the EuroStoxx ("DJES50I") index from Datastream - monthly data as long as possible. We will check the calculations of `R`. Calculate discrete monthly returns.



downloaded it from datastream --> citrix (Thomson Reuters)

uploaded it on the right side --> as a excel (xlsx)

import dataset on the right side above --> the two of them


```{r}
Eurostoxx_correct <- read_xlsx("eurostoxx.xlsx")
Eurostoxx_correct
View(Eurostoxx_correct)
Iboxx_correct <- read_xlsx("iboxx.xlsx")
Iboxx_correct
View(Iboxx_correct)
```

Calculate discrete monthly returns for eurostoxx

```{r}
monthly_returns_eurostoxx <- Eurostoxx_correct %>%
  mutate(date=as.yearmon(date), price=as.numeric(price))%>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period="monthly",
               type="arithmetic",
               col_rename = "monthly_returns"
               )
monthly_returns_eurostoxx
```

then the same for iboxx

```{r}
monthly_returns_iboxx <- Iboxx_correct %>%
  mutate(date=as.yearmon(date), price=as.numeric(price)) %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period="monthly",
               type="arithmetic",
               col_rename = "monthly_returns"
               )
monthly_returns_iboxx
```

to use portfolioanalytics package we need our data in xts format

```{r}
eurostoxx_returns_xts <- monthly_returns_eurostoxx %>%
  select(date,monthly_returns) %>%
  tk_xts(silent = TRUE)
eurostoxx_returns_xts
```

```{r}
iboxx_returns_xts <- monthly_returns_iboxx %>%
  select(date,monthly_returns) %>%
  tk_xts(silent = TRUE)
iboxx_returns_xts
```

merge them together

```{r}
index_final <- left_join(monthly_returns_iboxx, monthly_returns_eurostoxx, by = "date")
index_final
returns_index_final_xts <- index_final %>%
  select(date, monthly_returns.x, monthly_returns.y) %>%
  tk_xts(silent = TRUE)
returns_index_final_xts
```

a)	Stats/Normality (see A1)
Check the summary/basic statistics and moments of the assets. Plot and check for (multivariate) normality (OPTIONAL).

```{r}
monthly_returns_eurostoxx %>%
  tq_performance(Ra = monthly_returns, Rb = NULL, performance_fun = table.Stats)
monthly_returns_eurostoxx
```

```{r}
monthly_returns_iboxx %>%
  tq_performance(Ra = monthly_returns, Rb = NULL, performance_fun = table.Stats)
monthly_returns_iboxx
```

plot a histogram to check normality

```{r}
monthly_returns_eurostoxx %>%
  ggplot(aes(x=monthly_returns)) +
  geom_histogram(aes(y=..density..), colour="darkblue", fill="white") 
```


it is skewed to the left --> it is right steep , negatively skewed --> we see that in thhe skewness in table.Stats as well

but it is almost normally distributed

```{r}
monthly_returns_iboxx %>%
  ggplot(aes(x=monthly_returns)) +
  geom_histogram(aes(y=..density..), colour="darkblue", fill="white") 
```
same as above

is more normally distributed

it kind of has just one outliner on the left

```{r}
qqnorm(monthly_returns_iboxx$monthly_returns)
```
we can see here as well that it is almost normally distributed --> almost a linear regression

```{r}
qqnorm(monthly_returns_eurostoxx$monthly_returns)
```

almost a straight line


b)	Get the necessary input parameters (mu, sigma, please using variables, I don't want to see manual numbers in your code) and calculate the Minimum-Variance-Portfolio (manually in R). Then do it using the `portfolioAnalytics`-package.

Calculate "mu" for each index

monthly_returns.x = iboxx
monthly_returns.y = eurostoxx

```{r}
mu_returns_index_final_xts <- lapply(returns_index_final_xts, FUN=mean)
mu_returns_index_final_xts
```
Calculate "sigma" for each index

```{r}
sigma_returns_index_final_xts <- lapply(returns_index_final_xts,FUN=sd)
sigma_returns_index_final_xts
```
calculate with the package

calculate the minimum-variance-portfolio

-vignette("portfolio_vignette")
-do not allow for short selling


```{r}
labels <- c("iboxx", "eurostoxx")
returns_index_final
port_l <- portfolio.spec(assets = colnames(returns_index_final_xts), category_labels = labels)
port_l <- add.constraint(portfolio=port_l,type="long_only")
minvar <- add.objective(portfolio=port_l, type="risk", name="var")
opt_minvar <- optimize.portfolio(R=returns_index_final_xts, portfolio=minvar, optimize_method="ROI", trace=TRUE)
print(opt_minvar)
```
we would invest all in iboxx

```{r}
# allow for shortselling (NOT WORKING)
portf_minvar <- portfolio.spec(assets=returns_index_final_xts)
# Add full investment constraint to the portfolio object
portf_minvar <- add.constraint(portfolio=portf_minvar, type="full_investment")
minvar <- add.objective(portfolio=portf_minvar, type="risk", name="var")
opt_minvar <- optimize.portfolio(R=returns_index_final_xts, portfolio=minvar, optimize_method="ROI", trace=TRUE)
print(opt_minvar)
```

#allow for short selling (delete the long_only condition)

```{r}
#allow for short selling (delete the long_only condition)
mu <- colMeans(returns_index_final_xts); Sigma <- cov(returns_index_final_xts); 
ones <- rep(1,ncol(returns_index_final_xts))
wMVP <- t(solve(Sigma) %*% ones)/drop(ones %*% solve(Sigma) %*% ones)
muMVP <- drop(wMVP%*%mu) 
sigmaMVP <- drop(wMVP %*% Sigma %*% t(wMVP))^0.5
srMVP <- muMVP/sigmaMVP
round(cbind(wMVP,"mean"=muMVP,"sd"=sigmaMVP,"sr"=srMVP),4)
```
do it manually

Calculate "mu" for each index separately to use them for calculation
```{r}
returns_eurostoxx <- monthly_returns_eurostoxx%>%
  select(monthly_returns)
returns_iboxx <- monthly_returns_iboxx%>%
  select(monthly_returns)
mu_iboxx <- lapply(returns_iboxx, FUN=mean)
mu_iboxx
mu_iboxx_numeric <- as.numeric(mu_iboxx)
mu_eurostoxx <- lapply(returns_eurostoxx, FUN=mean)
mu_eurostoxx
mu_eurostoxx_numeric <- as.numeric(mu_eurostoxx)
```
Calculate "sigma" for each index separately
```{r}
sigma_iboxx <- as.numeric(lapply(returns_iboxx, FUN=sd))
sigma_iboxx
sigma_eurostoxx <- as.numeric(lapply(returns_eurostoxx, FUN=sd))
sigma_eurostoxx
```
```{r}
cor <- cor(returns_index_final_xts, y=NULL)
cor_xy <- cor [1,2]
cor_xy
```

```{r}
abc <- sigma_iboxx^2-(sigma_eurostoxx*sigma_iboxx*cor_xy)
covarianz_xy <- sigma_eurostoxx*sigma_iboxx*cor_xy
xyz <- sigma_eurostoxx^2+sigma_iboxx^2-(2*sigma_eurostoxx*sigma_iboxx*cor_xy)
MVP <- abc/xyz
MVP
```

we do not invest in eurostoxx
we invest everything in iboxx and sell eurostoxx to buy more iboxx

for the minumum varianze portfolio




c)	Now assume a risk-free rate of 0 and calculate the Tangency-Portfolio manually and with the `portfolioAnalytics`-package. What is the slope of the CAL? Plot a mu-sigma-diagram including all relevant information. What are your portfolio weights and weighted returns? Additionally allow for shortselling and check for changes.

?portfolioAnalytics

```{r}
# construct the data
asset.names = c("MSFT", "NORD", "SBUX")
er = c(0.0427, 0.0015, 0.0285)
names(er) = asset.names
covmat = matrix(c(0.0100, 0.0018, 0.0011,
                  0.0018, 0.0109, 0.0026,
                  0.0011, 0.0026, 0.0199),
                nrow=3, ncol=3)
r.free = 0.005
dimnames(covmat) = list(asset.names, asset.names)
# compute tangency portfolio
tan.port <- tangency.portfolio(er, covmat, r.free)
tan.port
summary(tan.port, risk.free=r.free)
plot(tan.port, col="blue")
# compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(er, covmat, r.free, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=r.free)
plot(tan.port.ns, col="blue")
```
?tangency.portfolio()

```{r}
# construct the data
asset.names = c("Eurostxx_correct", "Iboxx_correct")
er = c("mu_eurostoxx", "mu_iboxx")
names(er) = asset.names
covmat = matrix(c(0.0100, 0.0018, 0.0011,
                  0.0018, 0.0109, 0.0026,
                  0.0011, 0.0026, 0.0199),
                nrow=3, ncol=3)
r.free = 0.005
dimnames(covmat) = list(asset.names, asset.names)
# compute tangency portfolio
tan.port <- tangency.portfolio(er, covmat, r.free)
tan.port
summary(tan.port, risk.free=r.free)
plot(tan.port, col="blue")
# compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(er, covmat, r.free, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=r.free)
plot(tan.port.ns, col="blue")
```


__tangency portfolio with package__
```{r }
wTP <- t(solve(Sigma) %*% (mu*ones))/drop(ones %*% solve(Sigma) %*% (mu*ones))
muTP <- drop(wTP%*%mu); sigmaTP <- drop(wTP %*% Sigma %*% t(wTP))^0.5
srTP <- (muTP)/sigmaTP; srTP2 <- sqrt(drop((mu*ones) %*% solve(Sigma) %*% (mu*ones)))
round(cbind(wTP,"mean"=muTP,"sd"=sigmaTP,"sr"=srTP),4)
```
__tangency portfolio manually__

maximize the sharp ratio
```{r}
weight_eurostoxx1 <- (mu_eurostoxx_numeric*sigma_iboxx^2)-(mu_iboxx_numeric*covarianz_xy)
weight_eurostoxx2 <- (mu_eurostoxx_numeric*sigma_iboxx^2)+(mu_iboxx_numeric*sigma_eurostoxx^2)-((mu_eurostoxx_numeric+mu_iboxx_numeric)*covarianz_xy)
weight_eurostoxx <- weight_eurostoxx1/weight_eurostoxx2
weight_eurostoxx
```
__calculating sharpratio manually__

mu tangency portfolio_ we calculate manually the sharp ratio}
```{r}
mean_tangencyportfolio <- (weight_eurostoxx)*mu_eurostoxx_numeric+((1-(weight_eurostoxx))*mu_iboxx_numeric)
varianz_tangencyportfolio <- sqrt(((weight_eurostoxx)^2*(sigma_eurostoxx)^2)+(((1-(weight_eurostoxx))^2)*(sigma_iboxx)^2)+(2*weight_eurostoxx*(1-(weight_eurostoxx))*covarianz_xy))
sr_tangencyportfolio <- mean_tangencyportfolio/varianz_tangencyportfolio
sr_tangencyportfolio
```
__calculate slope__

slope of the CAL would be the Sharpratio = -0.0334

mu sigma diagram
```{r}
allsigmamu <- bind_rows(merge(sigma_eurostoxx, mu_eurostoxx_numeric), merge( sigma_iboxx,mu_iboxx_numeric))
name <- c("EuroStoxx", "Iboxx")
allsigmamuwithname <- allsigmamu %>% add_column(name)
allsigmamuwithname
```

```{r}
#rename the columns
colnames(allsigmamuwithname) <- c("sigma", "mu", "name")
allsigmamuwithname
```

```{r}
ggplot(allsigmamuwithname, aes(sigma, mu)) +
  geom_point() +
  theme_classic() + geom_label_repel(aes(label=name),
                            box.padding = 0.4,
                            point.padding = 0.3,
                            size=6)
```
Plot the efficient frontier
```{r}
port <- portfolio.spec(assets = colnames(returns_index_final_xts),
                        category_labels = labels)
port <- add.constraint(portfolio=port,
                        type="full_investment")
meanvar.portf <- add.objective(portfolio=port, 
                       type="return",
                       name="mean")
meanvar.portf <- add.objective(portfolio=port, 
                       type="risk",
                       name="StDev")
summary(meanvar.portf, digits=2)
prt_ef <- create.EfficientFrontier(R=returns_index_final_xts, portfolio=port, type="mean-StdDev", match.col = "StdDev")
chart.EfficientFrontier(prt_ef, match.col="StdDev", type="b", rf=NULL, pch.assets = 1)
chart.EF.Weights(prt_ef, colorset=rainbow(n = length(labels)), match.col="StdDev", cex.lab = 1, main = "StdDev")
```

__calculate the weighted return manually__ 
```{r}
2.8329*mu_eurostoxx_numeric + -1.8329*mu_iboxx_numeric
#our weighted return would be about 0.004
```

d)	Now, assume a risk-aversion of A=1, 2 or 3 and calculate your optimal complete portfolio (see lecture slides).


```{r}
mean_tangencyportfolio/(1*varianz_tangencyportfolio)
```
```{r}
mean_tangencyportfolio/(2*varianz_tangencyportfolio)
```

```{r}
mean_tangencyportfolio/(3*varianz_tangencyportfolio)
```


## Exercise 3: Covariance Problems

In the first part of this exercise we will be checking covariances and portfolios that might occur from faulty correlation matrices. We use the covariance matrix from our example


```{r cov, echo=FALSE, fig.cap="Faulty covariance matrix", out.width = '60%'}
knitr::include_graphics("cov.png")
```
where we additionally assume mean returns of 10% for all three assets.
If we define $\mu$ to be the vector of mean returns and $\sigma$ the vector of standard deviations, we can calculate the covariance matrix $\Sigma$ as $\Sigma=diag(\sigma)\cdot R\cdot diag(\sigma)$, where $R$ is the correlation matrix (as in the table above) and $diag$ puts the three standard deviations into the diagonal of a matrix.

Now we can calculate the Minimum-Variance-Portfolio using matrix calculus as
$w_MP=\frac{\Sigma^{-1}\cdot 1'}{1\cdot\Sigma^{-1}\cdot 1'}$
where 1 is a vector of ones with dimension equal to the number of assets. Similarly one can calculate the tangency portfolio as
$w_TP=\frac{\Sigma^{-1}\cdot (\mu-r_f)'}{1\cdot\Sigma^{-1}\cdot (\mu-r_f)'}$.

So to get used to the necessary tools, we use the package "matrixcalc" wherein we have a function `is.positive.semi.definite()` that can check covariance/correlation matrices for positive semidefiniteness. In the package `Matrix` we find a function `nearPD` that can help us to create a valid correlation matrix. Try and calculate the weights of the MVP and the TP, and then calculate portfolio mean and variance using $\mu_P=w\cdot \mu'$ and $\sigma_P^2=w\cdot \Sigma\cdot w'$ for the MVP and the TP as well as the weight vector w=(-1,1,1). Do this for the faulty matrix as well as the corrected one. What do you observe?


```{r}
# Create the Correlation-Matrix "R"
x1 <- c(1.00, 0.90, 0.90, 0.90, 1.00, 0.00, 0.90, 0.00, 1.00)
R <- matrix(x1, 3)
colnames(R) <- c("A", "B", "C")
rownames(R) <- c("A", "B", "C")
R

#Define Mu (10%) and Standard Deviation (20%)
mu <- matrix(c(.1, .1, .1), 3)
sd <- matrix(c(.20, .20, .20), 3)
mu
sd
```

```{r}
#Create a Covariance Matrix
covariance_matrix <- diag(sd)*R*diag(sd)
covariance_matrix

--------------------

#Now we can calculate the Minimum-Variance-Portfolio using matrix calculus as
  
$w_MP=\frac{\Sigma^{-1}\cdot 1'}{1\cdot\Sigma^{-1}\cdot 1'}$
where 1 is a vector of ones with dimension equal to the number of assets. Similarly one can calculate the tangency portfolio as
$w_TP=\frac{\Sigma^{-1}\cdot (\mu-r_f)'}{1\cdot\Sigma^{-1}\cdot (\mu-r_f)'}$.
```

```{r}
#Minimum-Variance Portfolio
onevector <- matrix(c(1, 1, 1), 1)
wmvpcalctop <- solve(covariance_matrix)%*%t(onevector)
wmvpcalcbottom <- as.numeric(onevector%*%solve(covariance_matrix)%*%t(onevector))
wmvp <- wmvpcalctop/wmvpcalcbottom
wmvp
```

#Similarly one can calculate the tangency portfolio as
$w_TP=\frac{\Sigma^{-1}\cdot (\mu-r_f)'}{1\cdot\Sigma^{-1}\cdot (\mu-r_f)'}$.

```{r}
#Tangency Portfolio (rf = 3%)
wtpcalctop <- (solve(covariance_matrix)%*%(mu-0.03))
wtpcalcbottom <- as.numeric(onevector%*%solve(covariance_matrix)%*%(mu-0.03))
wtp <- wtpcalctop/wtpcalcbottom
wtp #Weights equal to MVP
```


```{r}
#Test: Are the Matrices definite?
is.positive.semi.definite(R) #FALSE means that the matrix is not positive semi-definite. -> One Eigenvalue is less than zero.
is.positive.definite(covariance_matrix) # FALSE means as well that the matrix is not positive definite. As soon as one eigenvalues is less than zero, then the matrix is nox positive semi-definite. 
```
```{r}
#Compute the nearest positive definite matrix with the help of nearPD and create a new covariance matrix
R2 <- nearPD(R,keepDiag = TRUE)
R2 <- matrix(c( 1.00000, 0.74341, 0.74341,
0.74341, 1.00000, 0.10532,
0.74341, 0.10532, 1.00000)
, 3)
covmat2 <- diag(sd)*R2*diag(sd)
```

```{r}
#test: did it work?
is.positive.definite(R2) #Yes
#test: did it work?
is.positive.definite(covmat2) #Yes
```

```{r}
#Calculate new Minimum-Variance Portfolio
wmvpcalctop2 <- solve(covmat2)%*%t(onevector)
wmvpcalcbottom2 <- as.numeric(onevector%*%solve(covmat2)%*%t(onevector))
wmvp2 <- wmvpcalctop2/wmvpcalcbottom2
wmvp2
```

```{r}
#Mu
mumvp <- t(wmvp)%*%mu
mumvp2 <- wmvp2[,1]%*%mu
mumvp2 #Mu didn't change, still 10%
  ```
  
  ```{r}
  #Standard Deviation
  sdmvpcalc <- t(wmvp)%*%R%*%wmvp
  sdmvp <- sqrt(sdmvpcalc)
  sdmvpcalc2 <- t(wmvp2)%*%R2%*%wmvp2
  sdmvp2 <- sqrt(sdmvpcalc2)
  sdmvp2 #Standard Deviation didn't change, still .48%
  ```
  ```{r}
  #Calculate new Tangency Portfolio (rf = 3%)
  wtpcalctop2 <- (solve(covmat2)%*%(mu-0.03))
  wtpcalcbottom2 <- as.numeric(onevector%*%solve(covmat2)%*%(mu-0.03))
  wtp2 <- wtpcalctop2/wtpcalcbottom2
  wtp2 #Weights again equal to MVP
  ```
  
  ```{r}
  #Mu
  muwtp <- t(wtp)%*%mu
  muwtp2 <- wtp2[,1]%*%mu
  muwtp2 #Mu didn't change, still 10%
  ```
  
  ```{r}
  #Standard Deviation
  sdwtpcalc <- t(wtp)%*%R%*%wtp
  sdwtp <- sqrt(sdwtpcalc)
  sdwtpcalc2 <- t(wtp2)%*%R2%*%wtp2
  sdwtp2 <- sqrt(sdwtpcalc2)
  sdwtp2 #Standard Deviation didn't change, still .48%
  ```
  
  ```{r}
  #-1,1,1 portfolio
  #create the weight vector
  wv <- matrix(c(-1, 1, 1),3)
  wv
  ```
  
  ```{r}
  #Mu
  muwv <- wv[,1]%*%mu
  muwv
  ```
  ```{r}
  #Standard Deviation
  sdwvcalc <- t(wv)%*%R%*%wv
  sdwv <- sqrt(sdwvcalc)
  #In sqrt(sdwvcalc) : NaNs produced
  sdwvcalc <- t(wmvp2)%*%R2%*%wmvp2
  sdwv2 <- sqrt(sdwvcalc)
  sdwv2 #Standard Deviation also .48%
  ```
  

