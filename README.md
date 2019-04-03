R package processR  <img src="inst/figures/imgfile.png" align="right" height="120" width="103.6"/>
=========================================================
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/processR)](https://cran.r-project.org/package=processR)


The `processR` package aims to be a user-friendly way to perform moderation, mediation, moderated mediation and moderated moderation in R. This package is inspired from famous PROCESS macro for SPSS and SAS created by Andrew Hayes. 

**processR is under the GPL-3 license. For a commercial license, please
[contact me](mailto: cardiomoon@gmail.com).**

**If you find processR
useful, please consider supporting its development\!**

<p align="center">
<a href="https://www.paypal.me/cardiomoon/20">
<img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" />
</a>

</p>

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


## Full vignette

You can see full vignette for model 8 at http://rpubs.com/cardiomoon/468602


## Shiny App

I have developed a shiny app. You can test the app at http://web-r.space:3838/processR.
I will appreciate any comment.

## How to perform this analysis with shiny app

You can see how to perform this analysis at http://rpubs.com/cardiomoon/468600

## Sample powerpoint file

In the shiny app, you can download the analysis results as a powerpoint file. You can download the sample file [model8.pptx](https://github.com/cardiomoon/processRDocs/blob/master/model8/model8.pptx?raw=true) - view with [office web viewer](https://view.officeapps.live.com/op/view.aspx?src=https://github.com/cardiomoon/processRDocs/blob/master/model8/model8.pptx?raw=true). 
