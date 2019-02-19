#' Add dummy vars to data.frame
#' @param df A data.frame
#' @param varname Variable name to be converted as factor and add dummies
#' @export
#' @examples
#' addCatVar(mtcars,"cyl")
addCatVar=function(df,varname){
    if(!is.factor(df[[varname]])) {
        df[[varname]]<-factor(df[[varname]])
    }
    res=sort(as.numeric(unique(df[[varname]])))
    for(i in 2:length(res)){
        df[[paste0("d",i-1)]]=ifelse(as.numeric(df[[varname]])==i,1,0)
    }
    df
}


#'Make interaction equation with dummy categorical variable"
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
#'cat(catInteraction(Y="mpg",W="wt",count=3))
#'cat(catInteraction(Y="mpg",X="cyl",W="wt",data=mtcars))
catInteraction=function(Y="liking",X=NULL,W="sexism",data=NULL,
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


