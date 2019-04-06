# Model Summary Table
require(processR)
fit=lm(justify~skeptic*frame,data=disaster)
labels=list(X="skeptic",W="frame",Y="justify")
summary(fit)

## Model Summary Table
modelsSummaryTable(list(fit),labels=labels) %>% width(j=1,width=1.6) %>%
    align(j=1,align="center",part="all")

## Moderation effect plot
condPlot(fit,rangemode=2,xpos=0.7)

---
condPlot(fit,mode=2,xpos=0.6)

----
condPlot(fit,mode=3,rangemode=2,xpos=0.5)

## Johnson-Neyman Plot
res=jnPlot(fit,plot=FALSE)
res$p
