---
title: "R package processR"
author: "Keon-Woong Moon"
date: "2019-02-28"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{processR}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



The `processR` package aims to be a user-friendly way to perform moderation, mediation, moderated mediation and moderated moderation in R. This package is inspired from famous PROCESS macro for SPSS and SAS created by Andrew Hayes. 

## PROCESS macro and R package `processR`

Andrew F. Hayes was not involved in the development of this R package or application and cannot attest to the quality of the computations implemented in the code you are using. Use at your own risk.

## Installation

You can install the `processR` package from github.


```r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("cardiomoon/processR")
```
 
## What does this package cover ? 

The `processR` package covers moderation, mediation, moderated mediation and moderated moderation with R. Supporting models are as follows.


```r
library(processR)
sort(pmacro$no)
```

```
 [1]  0.0  1.0  2.0  3.0  4.0  4.2  5.0  6.0  6.3  6.4  7.0  8.0  9.0 10.0
[15] 11.0 12.0 13.0 14.0 15.0 16.0 17.0 18.0 19.0 20.0 21.0 22.0 23.0 24.0
[29] 28.0 29.0 30.0 31.0 35.0 36.0 40.0 41.0 45.0 49.0 50.0 58.0 59.0 60.0
[43] 61.0 62.0 63.0 64.0 65.0 66.0 67.0 74.0 75.0 76.0
```

Currently, 52 models are supported.

## Example: Moderated Mediation (PROCESS macro model 8)

I will explain functions of processR package by a example.

## Concept Diagram and Statistical Diagram

You can draw concept diagram and statistical diagram easily. For example, you can draw the concept diagram for PROCESS macro model 8.


```r
pmacroModel(8)
```

<img src="https://github.com/cardiomoon/processR/blob/master/figure/fig1.png?raw=true"  style="display: block; margin: auto;" />


You can draw statistical diagram of this model.


```r
statisticalDiagram(8)
```

<img src="https://github.com/cardiomoon/processR/blob/master/figure/fig2.png?raw=true"  style="display: block; margin: auto;" />


## Make model equation for analysis

This package uses `lavaan` and `mediation` packages for analysis. The `lavaan` package is extremely customizable and can also model latent variables if your measurement model requires it. But it is difficult to figure out the model equation for analysis. You can make model equation easily. For example, if you want to perform moderated mediation with data `disaster` with the following concept model.


```r
labels=list(X="frame",M="justify",Y="donate",W="skeptic")
pmacroModel(8,labels=labels)
```

<img src="https://github.com/cardiomoon/processR/blob/master/figure/fig3.png?raw=true"  style="display: block; margin: auto;" />


```r
moderator=list(name="skeptic",site=list(c("a","c")))
model=tripleEquation(X="frame",M="justify",Y="donate",moderator=moderator)
cat(model)
```

```
justify~a1*frame+a2*skeptic+a3*frame:skeptic
donate~b1*justify+c1*frame+c2*skeptic+c3*frame:skeptic
skeptic ~ skeptic.mean*1
skeptic ~~ skeptic.var*skeptic
indirect :=(a1+a3*skeptic.mean)*(b1)
direct :=c1+c3*skeptic.mean
total := direct + indirect
prop.mediated := indirect / total
indirect.below :=(a1+a3*(skeptic.mean-sqrt(skeptic.var)))*(b1)
indirect.above :=(a1+a3*(skeptic.mean+sqrt(skeptic.var)))*(b1)
direct.below:=c1+c3*(skeptic.mean-sqrt(skeptic.var))
direct.above:=c1+c3*(skeptic.mean+sqrt(skeptic.var))
total.below := direct.below + indirect.below
total.above := direct.above + indirect.above
prop.mediated.below := indirect.below / total.below
prop.mediated.above := indirect.above / total.above
```



With this model syntax, you can analyze moderated mediation with sem() function of lavaan package. 


```r
library(lavaan)
semfit=sem(model=model,data=disaster,se="bootstrap",bootstrap=10)
summary(semfit,fit.measures=FALSE,standardize=TRUE,rsquare=TRUE)
```

```
lavaan 0.6-3 ended normally after 36 iterations

  Optimization method                           NLMINB
  Number of free parameters                         18

  Number of observations                           211

  Estimator                                         ML
  Model Fit Test Statistic                     136.428
  Degrees of freedom                                 2
  P-value (Chi-square)                           0.000

Parameter Estimates:

  Standard Errors                            Bootstrap
  Number of requested bootstrap draws               10
  Number of successful bootstrap draws              10

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
  justify ~                                                             
    frame     (a1)   -0.562    0.148   -3.792    0.000   -0.562   -0.319
    skeptic   (a2)    0.105    0.036    2.902    0.004    0.105    0.242
    frm:skptc (a3)    0.201    0.048    4.161    0.000    0.201    0.504
  donate ~                                                              
    justify   (b1)   -0.923    0.045  -20.659    0.000   -0.923   -0.636
    frame     (c1)    0.160    0.239    0.671    0.502    0.160    0.063
    skeptic   (c2)   -0.043    0.044   -0.957    0.339   -0.043   -0.068
    frm:skptc (c3)    0.015    0.071    0.212    0.832    0.015    0.026

Covariances:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
  frame ~~                                                              
    frame:skeptic     0.854    0.043   19.974    0.000    0.854    0.774

Intercepts:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    skeptic (skp.)    3.378    0.131   25.767    0.000    3.378    1.666
   .justify           2.452    0.124   19.813    0.000    2.452    2.781
   .donate            7.291    0.218   33.387    0.000    7.291    5.704
    frame             0.479    0.035   13.694    0.000    0.479    0.958
    frm:skp           1.637    0.142   11.552    0.000    1.637    0.741

Variances:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    skeptic (skp.)    4.113    0.537    7.659    0.000    4.113    1.000
   .justify           0.648    0.063   10.351    0.000    0.648    0.835
   .donate            0.943    0.152    6.186    0.000    0.943    0.577
    frame             0.250    0.002  159.040    0.000    0.250    1.000
    frm:skp           4.877    0.551    8.852    0.000    4.877    1.000

R-Square:
                   Estimate
    justify           0.165
    donate            0.423

Defined Parameters:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    indirect         -0.108    0.076   -1.417    0.156   -0.108   -0.331
    direct            0.211    0.139    1.514    0.130    0.211    0.106
    total             0.103    0.156    0.658    0.511    0.103   -0.226
    prop.mediated    -1.053    2.061   -0.511    0.610   -1.053    1.468
    indirect.below    0.268    0.108    2.475    0.013    0.268   -0.011
    indirect.above   -0.485    0.152   -3.196    0.001   -0.485   -0.652
    direct.below      0.180    0.177    1.020    0.308    0.180    0.080
    direct.above      0.241    0.229    1.051    0.293    0.241    0.131
    total.below       0.449    0.219    2.051    0.040    0.449    0.069
    total.above      -0.244    0.303   -0.802    0.422   -0.244   -0.521
    prop.medtd.blw    0.598    1.058    0.565    0.572    0.598   -0.154
    prop.meditd.bv    1.990   21.871    0.091    0.928    1.990    1.252
```


You can also call for bootstrapped confidence interval parameter estimates of all of our effects.


```r
res=parameterEstimates(semfit,
                   boot.ci.type = "bca.simple",
                   level = .95, ci = TRUE,
                   standardized = FALSE)
select=stringr::str_detect(res$label,"dir|tot|prop")
res[select,]
```

```
                   lhs op                                           rhs
19            indirect :=                     (a1+a3*skeptic.mean)*(b1)
20              direct :=                            c1+c3*skeptic.mean
21               total :=                               direct+indirect
22       prop.mediated :=                                indirect/total
23      indirect.below := (a1+a3*(skeptic.mean-sqrt(skeptic.var)))*(b1)
24      indirect.above := (a1+a3*(skeptic.mean+sqrt(skeptic.var)))*(b1)
25        direct.below :=        c1+c3*(skeptic.mean-sqrt(skeptic.var))
26        direct.above :=        c1+c3*(skeptic.mean+sqrt(skeptic.var))
27         total.below :=                   direct.below+indirect.below
28         total.above :=                   direct.above+indirect.above
29 prop.mediated.below :=                    indirect.below/total.below
30 prop.mediated.above :=                    indirect.above/total.above
                 label    est     se      z pvalue ci.lower ci.upper
19            indirect -0.108  0.076 -1.417  0.156   -0.308   -0.074
20              direct  0.211  0.139  1.514  0.130   -0.113    0.363
21               total  0.103  0.156  0.658  0.511   -0.210    0.261
22       prop.mediated -1.053  2.061 -0.511  0.610   -1.831   -0.670
23      indirect.below  0.268  0.108  2.475  0.013    0.148    0.432
24      indirect.above -0.485  0.152 -3.196  0.001   -0.788   -0.408
25        direct.below  0.180  0.177  1.020  0.308   -0.159    0.414
26        direct.above  0.241  0.229  1.051  0.293   -0.072    0.525
27         total.below  0.449  0.219  2.051  0.040    0.057    0.710
28         total.above -0.244  0.303 -0.802  0.422   -0.843    0.023
29 prop.mediated.below  0.598  1.058  0.565  0.572    0.115    3.795
30 prop.mediated.above  1.990 21.871  0.091  0.928    0.717   64.265
```

The estimatesTable2 make a flextable object of this model.


```r
estimatesTable2(semfit)
```

<img src="https://github.com/cardiomoon/processR/blob/master/figure/table1.png?raw=true" width="700" style="display: block; margin: auto;" />


If you want to get black and white table for publication purpose, please set the argument vanilla=TRUE.


```r
estimatesTable2(semfit,vanilla = TRUE)
```

<img src="https://github.com/cardiomoon/processR/blob/master/figure/table2.png?raw=true" width="700" style="display: block; margin: auto;" />


You can draw statistical diagram with the analysis result.


```r
statisticalDiagram(8,labels=labels,fit=semfit,whatLabel="est")
```

<img src="https://github.com/cardiomoon/processR/blob/master/figure/fig4.png?raw=true"  style="display: block; margin: auto;" />



## Analysis with simple regression models

You can analyze this model using lm() function. You can make regression equations for moderator and dependent variables. 


```r
equations=regEquation(X="frame",M="justify",Y="donate",moderator=moderator)
cat(equations)
```

```
justify ~ frame
donate ~  frame+justify+frame*skeptic
```

With this equations, you can perform linear regression. First, you can use moderator as a dependent variable. 


```r
eq=unlist(strsplit(equations,"\n"))
fit=lapply(1:2,function(i) {
    lm(as.formula(eq[i]),data=disaster)
})
summary(fit[[1]])
```

```

Call:
lm(formula = as.formula(eq[i]), data = disaster)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.8024 -0.6867 -0.0024  0.5976  3.8633 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.80236    0.08862  31.623   <2e-16 ***
frame        0.13437    0.12809   1.049    0.295    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.9294 on 209 degrees of freedom
Multiple R-squared:  0.005238,	Adjusted R-squared:  0.0004783 
F-statistic:   1.1 on 1 and 209 DF,  p-value: 0.2954
```

The second regression model uses dependent variable.  


```r
summary(fit[[2]])
```

```

Call:
lm(formula = as.formula(eq[i]), data = disaster)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.4326 -0.5205  0.1216  0.6898  3.1508 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)    7.29147    0.27368  26.642   <2e-16 ***
frame          0.16032    0.26766   0.599    0.550    
justify       -0.92269    0.08403 -10.981   <2e-16 ***
skeptic       -0.04257    0.04693  -0.907    0.365    
frame:skeptic  0.01492    0.06892   0.217    0.829    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.9828 on 206 degrees of freedom
Multiple R-squared:  0.454,	Adjusted R-squared:  0.4434 
F-statistic: 42.82 on 4 and 206 DF,  p-value: < 2.2e-16
```

## Table summarizing coefficients of two models

You can make table summarizing two models.


```r
x=modelsSummary(fit,labels=labels)
modelsSummaryTable(x)
```

<img src="https://github.com/cardiomoon/processR/blob/master/figure/table3.png?raw=true" width="700"  style="display: block; margin: auto;" />

## Conditional direct and indirect effects 

You can make table summarizing the conditional direct and indirect effects. By default, the equation uses mean $\pm$ sd of moderator. The following table summarizes the direct and indirect effect when the moderator is mean, mean + sd and mean - sd. 

```r
x=modmedSummary(semfit,mod="skeptic")
modmedSummaryTable(x)
```

<img src="https://github.com/cardiomoon/processR/blob/master/figure/table4.png?raw=true" width="700" style="display: block;margin: auto;" />

## Plots for conditional direct and indirect effects 

You can draw summarizing the conditional direct and indirect effects.


```r
conditionalEffectPlot(semfit,data=disaster,mod="skeptic")
```

<img src="https://github.com/cardiomoon/processR/blob/master/figure/fig5.png?raw=true"  style="display: block; margin: auto;" />

## Shiny App

I have developed a shiny app. You can test the app at http://web-r.space:3838/processR.
I will appreciate any comment.

## How to perform this analysis with shiny app

You can see how to perform this analysis at http://rpubs.com/cardiomoon/468600

## Sample powerpoint file

In the shiny app, you can download the analysis results as a powerpoint file. You can download the sample file [model8.pptx](https://github.com/cardiomoon/processRDocs/blob/master/model8/model8.pptx?raw=true) - view with [office web viewer](https://view.officeapps.live.com/op/view.aspx?src=https://github.com/cardiomoon/processRDocs/blob/master/model8/model8.pptx?raw=true). 
