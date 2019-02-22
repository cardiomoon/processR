#' Add dummy vars to data.frame
#' @param df A data.frame
#' @param varnames Variable name to be converted as factor and add dummies
#' @param groupLetter A character
#' @param mode Numeric. One of 1:4. 1= simple indicator coding, 2= sequential coding, 3= Helmert coding, 4= effect coding
#' @export
#' @examples
#' mtcars1=addCatVars(mtcars,c("cyl","carb"))
#' mtcars1[c(3:5),]
#' mtcars2=addCatVars(mtcars,c("cyl","carb"),mode=3)
#' mtcars2[c(3:5),]
#' protest1=addCatVars(protest,"protest")
#' head(protest1)
#' iris1=addCatVars(iris,c("Species"),mode=3)
#' (iris1[c(1,51,101),])
addCatVars=function(df,varnames,groupLetter="D",mode=1){

    start=grep(groupLetter,LETTERS[])-1

    for(i in seq_along(varnames)){
        if(is.factor(df[[varnames[i]]])) {
            temp=df[[varnames[i]]]
        } else{
            temp<-factor(df[[varnames[i]]])
        }
        res=unique(temp)
        count=length(res)-1
        groupLetter=LETTERS[start+i]
        if (mode %in% c(1,4)) {
            for(j in 1:count){
                df[[paste0(groupLetter,j)]]=ifelse(as.numeric(temp)==j+1,1,0)
            }
        } else if(mode==2){
            for(j in 1:count){
                df[[paste0(groupLetter,j)]]=ifelse(as.numeric(temp)>=j+1,1,0)
            }

        } else if(mode==3){
            for(j in 1:count){
                df[[paste0(groupLetter,j)]]=getHelmert(as.numeric(temp),j,count)
            }
        }
    }
    df
}

#' Get Helmert Coding of column j of group with length of unique values (count-1)
#' @param x a numeric vector
#' @param j column no
#' @param count length unique values of group minus 1
#' @return A numeric vector
#' @export
#' @examples
#' x=c(1:4,4:2,2,3,5)
#' getHelmert(x,1)
#' getHelmert(mtcars$cyl,1)
#' @source
#' Andrew F. Hayes.(2018) Introduction to Mediation, Moderation and Conditional Process Analysis(2nd Ed.). New York, NY: The Guilford Press. p584
getHelmert=function(x,j,count=NULL){
    # if(!is.factor(x)) x=factor(x)
    if(is.null(count)) count=length(unique(x))-1

    A=c(rep(0,j-1),-1,rep(1,count+1-j))
    B=c(rep(0,j-1),(count+1-j)/(count+2-j),rep(1/(count+2-j),count+1-j))
    res=A*B
    res[x]
}

#'Get coding table for dummy variables
#'@param count number of unique length of categorical variable
#'@param mode Numeric. One of 1:4. 1= simple indicator coding, 2= sequential coding, 3= Helmert coding, 4= effect coding
#'@export
#'@examples
#'getRatioTable(count=3)
#'getRatioTable(count=4,mode=3)
getRatioTable=function(count=3,mode=1){
    if(mode==1){
      ratio=matrix(rep(0,(count-1)^2),byrow=FALSE,nrow=(count-1))
      for(i in 1:(count-1)){
        ratio[i,i]=1
      }
      ratio=rbind(rep(0,(count-1)),ratio)
      ratio
    } else if(mode==2){
        ratio=matrix(rep(0,(count-1)^2),byrow=FALSE,nrow=(count-1))
        for(i in 1:(count-1)){
            for(j in 1:(count-1)){
                ratio[i,j]=ifelse(i>=j,1,0)
            }
        }
        ratio=rbind(rep(0,(count-1)),ratio)
        ratio
    } else if(mode==3){
        x=1:count
        res=unlist(lapply(1:(count-1),function(i){getHelmert(x,i)}))
        ratio=matrix(res,byrow=FALSE,nrow=count)
        ratio
    } else {
        ratio=matrix(rep(0,(count-1)^2),byrow=FALSE,nrow=(count-1))
        for(i in 1:(count-1)){
            ratio[i,i]=1
        }
        ratio=rbind(rep(-1,(count-1)),ratio)
        ratio
    }
}



#' Get predicted value from object of class "lm"
#' @param fit Object of class "lm"
#' @param group names of dummy variables in  formula
#' @param mode Numeric. One of 1:4. 1= simple indicator coding, 2= sequential coding, 3= Helmert coding, 4= effect coding
#' @importFrom stringr str_detect
#' @export
#' @examples
#'iris1=addCatVars(iris,c("Species"))
#'iris3=addCatVars(iris,c("Species"),mode=3)
#'fit1=lm(Sepal.Length~Sepal.Width+D1+D2,data=iris1)
#'getYhat(fit1)
#'fit1=lm(Sepal.Length~D2*Sepal.Width+Sepal.Width*D1+Petal.Width,data=iris1)
#'getYhat(fit1)
#'fit3=lm(Sepal.Length~D2*Sepal.Width+Sepal.Width*D1+Petal.Width*D1+Petal.Width*D2,data=iris3)
#'getYhat(fit3,mode=3)
getYhat=function(fit,group="D",mode=1){
     # fit=fit1;group="D";mode=1
    count=length(fit$coeff)
    interact=which(str_detect(names(fit$coef),":"))
    pattern=paste0("^",group,"[1-9]")
    dummy=which(str_detect(names(fit$coef),pattern))
    dummy=setdiff(dummy,interact)
    dummyCount=length(dummy)
    (dummyNames=names(fit$coef)[dummy])

    vars=setdiff(1:count,c(1,dummy,interact))
    varnames=names(fit$coef)[vars]
    df=fit$model

    (moderator=setdiff(unlist(strsplit(names(fit$coef)[interact],":")),dummyNames))
    ratio=getRatioTable(count=dummyCount+1,mode=mode)
    eq="fit$coef[1]"
    for(i in 2:count){
        eq=c(eq,paste0(names(fit$coef[i]),"*fit$coef[",i,"]"))
    }
    eq=str_replace_all(eq,":","*")

    slopemod<-interceptmod<-list()
    eq

    moderator
    for(i in seq_along(moderator)){
        values=strGrouping(eq,moderator[i])
        slopemod[[i]]=paste0(values$yes,collapse="+")
        interceptmod[[i]]=paste0(values$no,collapse="+")
    }

    slopemod
    slope<-intercept<-list()
    for(k in seq_along(slopemod)){
        temp1=slopemod[[k]]
        temp2=interceptmod[[k]]
    for(i in 1:(dummyCount+1)){
        for(j in 1:dummyCount){
            temp3=paste0("ratio[",i,",",j,"]")
            slope[[(k-1)*3+i]]=str_replace_all(temp1,paste0(group,j),temp3)
            intercept[[(k-1)*3+i]]=str_replace_all(temp2,paste0(group,j),temp3)
        }
    }
    }

    eq=paste0(eq,collapse="+")
    for(i in seq_along(varnames)) {
        eq=str_replace_all(eq,varnames[i],paste0("mean(df$",varnames[i],",na.rm=TRUE)"))
        slope=str_replace_all(slope,varnames[i],paste0("mean(df$",varnames[i],",na.rm=TRUE)"))
        intercept=str_replace_all(intercept,varnames[i],paste0("mean(df$",varnames[i],",na.rm=TRUE)"))
    }
    eq
    slope
    intercept
    equation=list()

    for(i in 1:(dummyCount+1)){
        equation[[i]]=eq
        for(j in 1:dummyCount){
            temp=paste0("ratio[",i,",",j,"]")
            equation[[i]]=str_replace_all(equation[[i]],paste0(group,j),temp)
            for(k in seq_along(slopemod)){
            slope[[(k-1)*3+i]]=str_replace_all(slope[[(k-1)*3+i]],paste0(group,j),temp)
            intercept[[(k-1)*3+i]]=str_replace_all(intercept[[(k-1)*3+i]],paste0(group,j),temp)
            }
        }
    }

    result=unlist(lapply(1:length(equation),function(i){eval(parse(text=equation[[i]]))}))
    names(result)=paste0("\u0176",0:(length(result)-1))
    result

    for(i in seq_along(slopemod)){
        for(j in 1:3){
        slope[[(i-1)*3+j]]=eval(parse(text=slope[[(i-1)*3+j]]))
        intercept[[(i-1)*3+j]]=eval(parse(text=intercept[[(i-1)*3+j]]))
        }
        result=cbind(result,c(slope[[((i-1)*3+1)]],slope[[((i-1)*3+2)]],slope[[((i-1)*3+3)]]),
                     c(intercept[[((i-1)*3+1)]],intercept[[((i-1)*3+2)]],intercept[[((i-1)*3+3)]]))

    }
    temp=c("slope","intercept")
    names=c()
    for(i in seq_along(moderator)){
         names=c(names,paste0(temp,moderator[i]))
    }
    result=as.data.frame(result,stringsAsFactors=FALSE)
    if(length(moderator)>0) {
        colnames(result)=c("\u0176",names)
    } else{
        colnames(result)=c("\u0176")
    }
    result
}


