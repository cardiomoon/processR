#' Add dummy vars to data.frame
#' @param df A data.frame
#' @param varname Variable name to be converted as factor and add dummies
#' @param groupLetter A character
#' @param mode if mode is 2, apply different coding system
#' @export
#' @examples
#' addCatVar(mtcars,"cyl")
#' protest1=addCatVar(protest,"protest",mode=2)
#' head(protest1)
addCatVar=function(df,varname,groupLetter="d",mode=1){
    if(is.factor(df[[varname]])) {
        temp=df[[varname]]
    } else{
        temp<-factor(df[[varname]])
    }
    res=sort(as.numeric(unique(df[[varname]])))
    if((mode==2)&(length(res)==3)){
        df[[paste0(groupLetter,"1")]]=ifelse(as.numeric(temp)==1,-2/3,1/3)
        df[[paste0(groupLetter,"2")]]=ifelse(as.numeric(temp)==1,0,ifelse(as.numeric(temp)==2,-1/2,1/2))
    } else {
        for(i in 2:length(res)){
           df[[paste0(groupLetter,i-1)]]=ifelse(as.numeric(df[[varname]])==i,1,0)
        }
    }
    df
}


#'Make interaction equation with dummy categorical independent variable
#'@param Y Name of dependent variable
#'@param X Optional.Name of independent variable
#'@param W Name of moderator variable
#'@param data A data.frame
#'@param count length of unique values of independent variable
#'@param prefix A prefix
#'@param covar A list
#'@param corr A logical
#'@export
#'@examples
#'cat(catInteraction1(Y="mpg",W="wt",count=3))
#'cat(catInteraction1(Y="mpg",X="cyl",W="wt",data=mtcars))
catInteraction1=function(Y="liking",X=NULL,W="sexism",data=NULL,
                            count=NULL,prefix="b",covar=list(),corr=TRUE){
    if(is.null(count)) count=length(unique(data[[X]]))
    no=1
    res=c()
    for(i in 2:count){
        res=c(res,paste0(prefix,i-1,"*d",i-1))
        no=no+1
    }
    res=c(res,paste0(prefix,no,"*",W))

    for(i in 2:count){
        res=c(res,paste0(prefix,no+i-1,"*d",i-1,":",W))
    }
    temp=paste0(Y," ~ ",paste0(res,collapse="+"))
    temp=addCovarEquation(temp,covar,prefix="h")
    if(corr==TRUE){
    temp=paste0(temp,"\n",W," ~ ",W,".mean*1")
    temp=paste0(temp,"\n",W," ~~ ",W,".var*",W)
    }
    temp
}

#'Make interaction equation with dummy categorical antecedent and moderator
#'@param Y Name of dependent variable
#'@param X Name of independent variable
#'@param W Optional.Name of moderator variable
#'@param data A data.frame
#'@param xcount length of unique values of independent variable
#'@param wcount length of unique values of moderator variable
#'@param prefix A prefix
#'@param covar A list
#'@export
#'@examples
#'cat(catInteraction2(Y="mpg",X="carb",W="cyl",data=mtcars))
catInteraction3=function(Y,X=NULL,W=NULL,data=NULL,
                        xcount=NULL,wcount=NULL,prefix="b",covar=list()){

     # Y="mpg";X="carb";W="cyl";data=mtcars
     # xcount=NULL;wcount=NULL;prefix="b";covar=list();corr=TRUE

    if(is.null(xcount)) xcount=length(unique(data[[X]]))
    if(is.null(wcount)) wcount=length(unique(data[[W]]))


    res=c()
    dgroup=paste0("d",1:(xcount-1))
    dgroup
    egroup=paste0("e",1:(wcount-1))
    egroup

    for(i in 2:xcount){
        res=c(res,paste0(prefix,i-1,"*d",i-1))

    }
    res
    no=xcount-1
    for(i in 2:wcount){
        res=c(res,paste0(prefix,no+i-1,"*e",i-1))
    }
    res
    no=xcount+wcount-1

    for(i in 2:xcount){
        for(j in 2:wcount){
            res=c(res,paste0(prefix,no,"*d",i-1,":","e",j-1))
            no=no+1
        }
    }
    res
    temp=paste0(Y," ~ ",paste0(res,collapse="+"))
    temp=addCovarEquation(temp,covar,prefix="h")

    temp
}

#'Make interaction equation with dummy categorical moderator
#'@param Y Name of dependent variable
#'@param X Optional.Name of independent variable
#'@param W Name of moderator variable
#'@param data A data.frame
#'@param count length of unique values of independent variable
#'@param prefix A prefix
#'@param covar A list
#'@param corr A logical
#'@export
#'@examples
#'cat(catInteraction2(Y="mpg",X="wt",count=3))
#'cat(catInteraction2(Y="mpg",X="wt",W="cyl",data=mtcars))
catInteraction2=function(Y,X,W=NULL,data=NULL,
                        count=NULL,prefix="b",covar=list(),corr=TRUE){

    if(is.null(count)) count=length(unique(data[[W]]))
    no=1
    res=c()
    for(i in 2:count){
        res=c(res,paste0(prefix,i-1,"*d",i-1))
        no=no+1
    }
    res
    res=c(res,paste0(prefix,no,"*",X))

    for(i in 2:count){
        res=c(res,paste0(prefix,no+i-1,"*d",i-1,":",X))
    }
    temp=paste0(Y," ~ ",paste0(res,collapse="+"))
    temp=addCovarEquation(temp,covar,prefix="h")
    if(corr==TRUE){
        temp=paste0(temp,"\n",X," ~ ",X,".mean*1")
        temp=paste0(temp,"\n",X," ~~ ",X,".var*",X)
    }
    temp
}

#'Make interaction equation with dummy categorical and/or moderator
#'@param Y Name of dependent variable
#'@param X Name of independent variable
#'@param W Name of moderator variable
#'@param data A data.frame
#'@param prefix A prefix
#'@param covar A list
#'@param corr A logical
#'@param maxylev maximal unique length of categorial variable
#'@export
#'@examples
#'cat(catInteraction(Y="mpg",X="wt",W="cyl",data=mtcars))
#'cat(catInteraction(Y="mpg",X="carb",W="cyl",data=mtcars))
#'cat(catInteraction(Y="mpg",X="wt",W="hp",data=mtcars))
#'cat(catInteraction(Y="mpg",X="cyl",W="hp",data=mtcars))
catInteraction=function(Y,X,W,data=NULL,
                        prefix="b",covar=list(),corr=TRUE,maxylev=6){
    # Y="mpg";X="carb";W="cyl";data=mtcars
    # prefix="b";covar=list();corr=TRUE;maxylev=6
    cat=c()
    xcount=length(unique(data[[X]]))
    wcount=length(unique(data[[W]]))

    if(is.factor(data[[X]])) {
        cat=c(cat,"X")
    } else if((xcount>2)&(xcount<=maxylev)) cat=c(cat,"X")
    if(is.factor(data[[W]])) {
        cat=c(cat,"W")
    } else if((wcount>2)&(wcount<=maxylev)) cat=c(cat,"W")

    if(length(cat)==0){
        temp=c(X,W,paste0(X,":",W))
        temp=paste0(prefix,1:3,"*",temp)
        temp=paste0(Y,"~",paste0(temp,collapse="+"))
        eq=addCovarEquation(temp,covar,prefix="h")
    } else if(length(cat)==2){
        eq=catInteraction3(Y=Y,X=X,W=W,data=data,prefix=prefix,covar=covar)
    } else if(cat=="X"){
        eq=catInteraction1(Y=Y,X=X,W=W,data=data,prefix=prefix,covar=covar,corr=corr)
    } else{
        eq=catInteraction2(Y=Y,X=X,W=W,data=data,prefix=prefix,covar=covar,corr=corr)
    }
    eq
}
