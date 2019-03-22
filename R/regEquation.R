#'Make regression equation
#' @param X A character vectors indicating independent variables
#' @param M A character vectors indicating mediators
#' @param Y A character vectors indicating dependent variables
#' @param moderator moderator
#' @param covar covariates
#' @param secondIndirect A logical
#' @export
#' @examples
#' X="X";M=NULL;Y="Y"; moderator=list(name="W",site=list("c"))
#' regEquation(X,M,Y,moderator)
#' M=c("M1","M2")
#' regEquation(X,M,Y,moderator,secondIndirect=TRUE)
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M1","Y"),"Y","Y"))
#' regEquation(X,M,Y,moderator,covar=covar)
#' covar=list(name=c("ese","sex","tenure"),site=list(c("M","Y"),c("M","Y"),c("M","Y")))
#' regEquation(X="estress",M="affect",Y="withdraw",covar=covar)
regEquation=function(X="X",M=NULL,Y="Y",moderator=list(),covar=list(),secondIndirect=FALSE){
    # moderator=list();secondIndirect=FALSE
    # X="estress";M="affect";Y="withdraw"
    (XM=moderator$name[str_detect2(moderator$site,"a")])
    (MY=moderator$name[str_detect2(moderator$site,"b")])
    (XY=moderator$name[str_detect2(moderator$site,"c")])

    equation=list()
    count=1
    if(!is.null(M)){
         for(i in 1:length(M)){
             equation[[count]]=paste0(M[i],"~",X )
             if((i>1)&(secondIndirect)){
                 temp=M[1:(i-1)]
                 temp=paste0(temp,collapse="+")
                 equation[[count]]=paste0(equation[[count]],"+",temp)
             }
             count=count+1
         }
    }
    temp=c(X)
    if(!is.null(M)){
        temp=c(temp,M)
    }
    if(length(XY)>0) {
        for(i in 1:length(XY)){
            temp=c(temp,paste0(X,"*",XY[i]))
        }
    }
    temp=paste0(temp,collapse="+")
    equation[[count]]=paste(Y,"~",temp)
    equation2=paste0(unlist(equation),collapse="\n")

    equation2=addCovarEquation(equation2,covar=covar,prefix=NULL)
    equation2
}

#' Make a list of objects of class lm
#' @param equations equations for linear regression
#' @param data A data.frame
#' @return a list of objects of class lm
#' @importFrom stats as.formula lm
#' @export
eq2fit=function(equations,data){
    eq=unlist(strsplit(equations,"\n"))
    count=length(eq)
    fit=lapply(1:count,function(i) {
        lm(as.formula(eq[i]),data=data)
    })
    fit
}
