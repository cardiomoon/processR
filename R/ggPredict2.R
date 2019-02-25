#' Decide whether a vector can be treated as a numeric variable
#' @param x A vector
#' @param maxylev An integer indicating the maximum number of levels of numeric variable be treated as a categorial variable
#' @export
is.mynumeric=function(x,maxylev=6){
    ifelse((is.numeric(x) & (length(unique(x))>maxylev)),TRUE,FALSE)
}

#' Make a new data set for prediction
#'@param fit An object of calss "lm" or "glm"
#'@param predictors Names of predictor variables in string
#'@param mode A numeric. Useful when the variables are numeric. If 1, c(-1,0,1)*sd + mean is used. If 2, the 14th, 50th, 86th percentile values used. If 3 sequence over a the range of a vector used
#'@param pred.values For which values of the predictors should be used? Default is NULL. If NULL, 20 seq_range is used.
#'@param modx.values For which values of the moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param mod2.values For which values of the second moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param colorn The numer of regression lines when the modifier variable(s) are numeric.
#'@param maxylev An integer indicating the maximum number of levels of numeric variable be treated as a categorial variable
#'@importFrom prediction seq_range
#'@importFrom tidyr crossing
#'@importFrom purrr reduce
#'@importFrom modelr typical
#'@importFrom stats sd
#'@export
#'@examples
#'fit=lm(mpg~hp*wt*cyl+carb+am,data=mtcars)
#'fit2newdata(fit,predictors=c("hp","wt","am"))
#'fit2newdata(fit,predictors=c("hp","wt","cyl"))
#'fit2newdata(fit,predictors=c("hp"))
fit2newdata=function(fit,predictors,mode=1,pred.values=NULL,modx.values=NULL,mod2.values=NULL,colorn=3,maxylev=6){
    # mode=1;modx.values=NULL;mod2.values=NULL;colorn=3;maxylev=6
    # predictors="c12hour";modx.values=c(0,20,45,65,85,105,125,170)
    # fit
    df=fit$model[-1]
    yvar=names(fit$model)[1]

    df1<-df[predictors]
    df2<-df[setdiff(names(df),predictors)]

    if(is.mynumeric(df1[[1]],maxylev=maxylev)) {
        newdf=seq_range(df1[[1]],20)
    } else{
        newdf=unique(df1[[1]])
    }
    if(!is.null(pred.values)) newdf=pred.values
    newdf=data.frame(newdf)
        if(length(df1)>1){
        newdf2<-lapply(df1[2:length(df1)],function(x) {
            if(is.mynumeric(x,maxylev=maxylev)) {
                if(mode==1) mean(x,na.rm=TRUE)+c(-1,0,1)*sd(x,na.rm=TRUE)
                else if(mode==2) quantile(x,probs=c(0.14,0.50,0.86),type=6)
                else if(mode==3) seq_range(x,colorn)
            } else{
                unique(x)
            }
        })
        if(!is.null(modx.values)) newdf2[[1]]=modx.values
        if(!is.null(mod2.values)) newdf2[[2]]=mod2.values
        newdf2<-newdf2 %>% reduce(crossing)
        newdf=crossing(newdf,newdf2)
    }
    colnames(newdf)=colnames(df1)

    caption<-NULL
    if(length(df2)>0){

        newdf3<-lapply(df2,modelr::typical) %>% reduce(crossing)
        newdf3=data.frame(newdf3,stringsAsFactors = FALSE)
        if(nrow(newdf3)>1) newdf3<-newdf3[1,]
        colnames(newdf3)=names(df2)
        caption<-paste(names(newdf3),newdf3[1,],sep="=",collapse=",")

        newdf<-crossing(newdf,newdf3)
    }
    result <- predict(fit, newdata = newdf, type = "response",se.fit=TRUE)
    newdf[[yvar]]<-result$fit
    newdf$se.fit<-result$se.fit
    newdf$ymax<-newdf[[yvar]]+result$se.fit
    newdf$ymin<-newdf[[yvar]]-result$se.fit
    if(!is.null(caption)) attr(newdf,"caption")=caption
    newdf
}


#' Visualize predictions from the multiple regression models.
#'@param fit An object of calss "lm" or "glm"
#'@param pred The name of predictor variable
#'@param modx Optional. The name of moderator variable
#'@param mod2 Optional. The name of second moderator variable
#'@param modx.values For which values of the moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param mod2.values For which values of the second moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param mode A  numeric. Useful when the variables are numeric. If 1, c(-1,0,1)*sd + mean is used. If 2, the 14th, 50th, 86th percentile values used. If 3 sequence over a the range of a vector used
#'@param colorn The numer of regression lines when the modifier variable(s) are numeric.
#'@param maxylev An integer indicating the maximum number of levels of numeric variable be treated as a categorial variable
#'@param show.point Logical. Whether or not add points
#'@param jitter logical Whether or not use geom_jitter
#'@param se Logical. Whether or not add confidence interval
#'@param alpha A numeric. Transparency
#'@param show.text  Logical. Whether or not add regression equation as label
#'@param add.modx.values Logical. Whether or not add moderator values to regression equation
#'@param labels labels on regression lines
#'@param angle angle of text
#'@param xpos x axis position of label
#'@param vjust vertical aligment of labels
#'@param digits integer indicating the number of decimal places
#'@param ... additional arguments to be passed to geom_text
#'@importFrom rlang enquo "!!" quo_name enexpr
#'@importFrom dplyr group_by do
#'@importFrom stats as.formula glm lm predict
#'@importFrom ggplot2 geom_line geom_ribbon geom_point labs facet_grid geom_jitter
#'@export
#'@examples
#'fit=lm(mpg~hp*wt,data=mtcars)
#'ggPredict2(fit,pred=hp,modx=wt,xpos=0.3,vjust=c(-0.5,-0.5,1.5))
#'ggPredict2(fit,modx=wt,labels=paste0("label",1:3),xpos=c(0.3,0.6,0.4),vjust=c(-0.5,-0.5,1.5))
#'ggPredict2(fit,modx=wt,se=TRUE,xpos=0.3)
#'ggPredict2(fit,modx=wt,mode=3,colorn=40,show.text=FALSE)
#'fit=lm(mpg~hp*wt*cyl,data=mtcars)
#'ggPredict2(fit,modx=wt,modx.values=c(2,3,4,5),mod2=cyl,show.text=FALSE)
#'ggPredict2(fit,pred=hp,modx=wt,show.point=FALSE,se=TRUE,xpos=0.5)
#'ggPredict2(fit,modx=wt,xpos=0.3)
#'ggPredict2(fit)
#'require(TH.data)
#'fit=glm(cens~pnodes*horTh,data=GBSG2,family=binomial)
#'ggPredict2(fit,pred=pnodes,modx=horTh,se=TRUE,xpos=c(0.6,0.3),angle=c(40,60),vjust=c(2,-0.5))
#'fit1=glm(cens~pnodes,data=GBSG2,family=binomial)
#'ggPredict2(fit1,pred=pnodes,vjust=0)
#'fit3=glm(cens~pnodes*age,data=GBSG2,family=binomial)
#'ggPredict2(fit3,pred=pnodes,modx=age,mode=3,colorn=10,show.text=FALSE)
#'fit2=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
#'ggPredict2(fit2,pred=pnodes,modx=age,mod2=horTh,mode=3,colorn=10,show.text=FALSE)
ggPredict2=function(fit,pred=NULL,modx=NULL,mod2=NULL,modx.values=NULL,mod2.values=NULL,
                    mode=1,colorn=3,maxylev=6,show.point=TRUE,jitter=NULL,se=FALSE,alpha=0.1,
                    show.text=TRUE, add.modx.values=TRUE,
                    labels=NULL,angle=NULL,xpos=NULL,vjust=-0.5,digits=3,...) {

    # mod2=NULL;modx.values=NULL;mod2.values=NULL
    # mode=1;colorn=3;maxylev=6;show.point=TRUE;se=FALSE;alpha=0.1
    # show.text=TRUE; add.modx.values=TRUE
    # labels=NULL;xpos=0.7;vjust=-0.5;digits=3
    # predictors=c("hp","wt")
    predc <- quo_name(enexpr(pred))
    if(predc=="NULL"){
        pred<-NULL
        predc=names(fit$model)[2]
    } else{
        pred=enquo(pred)
    }
    modxc <- quo_name(enexpr(modx))
    modx<-enquo(modx)
    if(modxc=="NULL"){
        modx<-NULL
        if(length(names(fit$model))>2){
            modxc=names(fit$model)[3]
        } else{
            modxc<-NULL
        }
    }
    mod2c <- quo_name(enexpr(mod2))
    mod2<-enquo(mod2)
    if(mod2c=="NULL"){
        mod2<-NULL
        if(length(names(fit$model))>3){
            mod2c=names(fit$model)[4]
        } else{
            mod2c<-NULL
        }
    }


    predictors=c(predc,modxc,mod2c)
    # str(predictors)

    yvar=names(fit$model)[1]
    newdata=fit2newdata(fit,predictors,mode=mode,modx.values=modx.values,
                        mod2.values=mod2.values,colorn=colorn,maxylev=maxylev)

    fitted=newdata
    if(!is.null(modxc) & !is.null(mod2c)) {
        fitted<-eval(parse(text=paste0("group_by(fitted,",modxc,",",mod2c,")")))
    } else if(!is.null(mod2c)) {
        fitted<-eval(parse(text=paste0("group_by(fitted,",mod2c,")")))
    } else if(!is.null(modxc)) {
        fitted<-eval(parse(text=paste0("group_by(fitted,",modxc,")")))
    }
    # str(fitted)

    predictors=unique(c(modxc,mod2c))
    if(length(predictors)>0){
         formulaString=getNewFormula(fit,predictors)
         newFormula=as.formula(formulaString)
    } else{
        newFormula=fit$terms

    }

    if(class(fit)[1]=="lm") {
        fitted<-fitted %>% do(coef=lm(newFormula,data=.)$coef[1:2])
    } else if(class(fit)[1]=="glm") {
        fitted<-fitted %>% do(coef=glm(newFormula,data=.,family=fit$family$family)$coef[1:2])
    }
    coef=unlist(fitted$coef)
    fitted$intercept=coef[seq(1,by=2,length.out=nrow(fitted))]
    fitted$slope=coef[seq(2,by=2,length.out=nrow(fitted))]

    if(is.null(xpos)){
        if(class(fit)[1]=="lm") xpos=0.7
        else if(class(fit)[1]=="glm") xpos=0.4
    }



    if(is.null(modxc)){
    p<-ggplot(data=newdata,aes_string(x=predc,y=yvar))
    }  else {
    p<-ggplot(data=newdata,aes_string(x=predc,y=yvar,color=modxc,fill=modxc,group=modxc))
    }
    p<-p+  geom_line()
    if(is.null(jitter)){
        if(class(fit)[1]=="glm") jitter=TRUE
        else jitter=FALSE
    }
    if(show.point==TRUE) {
        if(jitter) p<-p+geom_jitter(data=fit$model,width=0,height=0.05)
        else p<-p+geom_point(data=fit$model)
    }

    if(se==TRUE) p<-p+ geom_ribbon(aes_string(ymax="ymax",ymin="ymin",color=NULL),alpha=alpha)
    if(!is.null(mod2c)) p<-p+eval(parse(text=paste0("facet_grid(~",mod2c,")")))

    facetno<-NULL
    if(!is.null(mod2c)) facetno=length(unique(fit$model[[mod2c]]))
    fitted<-slope2angle(fitted,predc,p,method=class(fit)[1],xpos=xpos,digits=digits,
                        facetno=facetno,add.modx.values=add.modx.values)
    if(!is.null(angle)) fitted$angle=angle
    if(!is.null(labels)){
        if(length(labels)==nrow(fitted)) fitted$label=labels
    }

    # str(fitted)
    if(show.text) {
        if(class(fit)[1]=="lm"){
            p <- p+ geom_text(data=fitted,
                          aes_string(x="x",y="y",angle="angle",label="label"),vjust=vjust,...)
        } else{
            p <- p+ geom_text(data=fitted,
                              aes_string(x="x",y="y",angle="angle",label="label"),vjust=vjust,
                              parse=TRUE,...)
        }
    }
    if(class(fit)[1]=="lm") p<-p+ coord_fixed(ratio=attr(fitted,"ratio"))
    p<-p+theme_bw()
    if(!is.null(attr(newdata,"caption"))) {
        p<-p+labs(caption=paste0("Analysis assuming ",attr(newdata,"caption")))
    }
    p
}

#'Make angle data with slope data
#'@param df A data.frame
#'@param predc Name of predictor variable
#'@param p An object of class ggplot
#'@param method String. Choices are one of "lm" and "glm".
#'@param xpos The relative x-axis position of labels. Should be within 0 to 1
#'@param digits integer indicating the number of decimal places
#'@param facetno Tne number of facets
#'@param add.modx.values Whether add name of moderator variable
slope2angle=function(df,predc,p,method="lm",xpos=0.7,digits=3,facetno=NULL,add.modx.values=TRUE){
    # digits=3;xpos=0.7
    info=getAspectRatio(p)
    ratio=info$ratio
    if(!is.null(facetno)) ratio=ratio*facetno
    df$slope2=df$slope*ratio
    df$radian=atan(df$slope2)
    df$angle=df$radian*180/pi
    if(method=="lm"){
    df$label=paste0(round(df$slope,digits),predc,
                    ifelse(df$intercept>=0," + "," - "),
                    round(df$intercept,digits))
    } else if(method=="glm"){

    df$label=paste0("frac(1,1+ plain(e)^(-(",round(df$slope,digits),"*",predc,
                    ifelse(df$intercept>=0,"+","-"),abs(round(df$intercept,digits)),")))")

    }

    if(is.numeric(df[[1]])){
        df$labels2=round(df[[1]],digits)
    } else if(is.factor(df[[1]])){
        df$labels2=levels(df[[1]])[df[[1]]]
    } else{
        df$labels2=df[[1]]
    }
    if(add.modx.values) {
        if(method=="lm"){
        if(colnames(df)[1]!="coef"){
        df$label=paste0(df$label," | ",colnames(df)[1]," = ",df$labels2)
        }
        }
    }
    count=nrow(df)
    x=info$xmin+(info$xmax-info$xmin)*xpos
    if(length(xpos)==1){
        x=rep(x,count)
    }
    y=c()
    for(i in seq_along(df$slope)){
        if(method=="lm"){
           y=c(y,df$slope[i]*x[i]+df$intercept[i])
        }else if(method=="glm"){
            y=c(y,1/(1+exp(-(df$slope[i]*x[i]+df$intercept[i]))))
        }
    }
    y

    df$x=x
    df$y=y
    if(method=="glm") df$angle=0
    attr(df,"ratio")=ratio
    # print(df)
    df
}


#'Make new formula
#'@param fit An object of class lm or glm
#'@param predictors Names of variables to exclude
#'@export
#'@examples
#'fit=lm(mpg~hp*wt*cyl+carb+am,data=mtcars)
#'getNewFormula(fit,predictors=c("wt","cyl"))
getNewFormula=function(fit,predictors=NULL){
    # predictors<-NULL
    temp=names(fit$coef)[-1]
    temp
    for( i in seq_along(predictors)){
        select=which(!str_detect(temp,predictors[i]))
        select
        temp=temp[select]
    }
    # str(fit)
    result=paste0(names(fit$model)[1],"~",paste(temp,collapse="+"))
    result

}

