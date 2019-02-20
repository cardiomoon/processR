#'Make equation for sem and lm
#'@param X Name of indpendent variable
#'@param Y Name of dependent variable
#'@param W Name of moderators
#'@param data a data.frame
#'@param prefix a character
#'@param maxylev maximal unique length of categorial variable
#'@param grouplabels A list
#'@param mode A numeric
#'@export
#'@examples
#'makeCatEquation(X="wt",Y="mpg",data=mtcars)
#'makeCatEquation(X="wt",Y="mpg",W="cyl",data=mtcars)
#'makeCatEquation(X="wt",Y="mpg",W=c("cyl","hp"),data=mtcars)
#'grouplabels=list(carb="f")
#'makeCatEquation(X="carb",Y="mpg",W=c("cyl","hp"),data=mtcars,maxylev=6)
#'makeCatEquation(X="carb",Y="mpg",W=c("cyl","hp"),data=mtcars)
#'cat(makeCatEquation(X="wt",Y="carb",W=c("am","hp"),data=mtcars,maxylev=6,grouplabels=grouplabels))
makeCatEquation=function(X="X",Y="Y",W=NULL,data,prefix="b",maxylev=6,grouplabels=list(),mode=0){

     # X="wt";Y="carb";W=c("am","hp");data=mtcars;maxylev=6;prefix="b";group="d";ygroup="z"
    xgroup<-wgroup<-c()
    xcount<-wcount<-ycount<-0
    groupstart=4
    if(length(grouplabels)==0) {
        group="d"
        groupstart=4
    }

    xcount=length(unique(data[[X]]))
    if(is.factor(data[[X]])|((xcount>2)&(xcount<=maxylev))) {
        group=ifelse(!is.null(grouplabels[[X]]),grouplabels[[X]],group)
        xgroup=paste0(group,1:(xcount-1))
        groupstart=5
    } else{
        xgroup=X
    }
    for(i in seq_along(W)){
        wcount=length(unique(data[[W[i]]]))
        if(is.factor(data[[W[i]]])|((wcount>2)&(wcount<=maxylev))) {
            group=ifelse(!is.null(grouplabels[[W[i]]]),grouplabels[[W[i]]],letters[groupstart])
            wgroup=c(wgroup,paste0(letters[groupstart],1:(wcount-1)))
            groupstart=groupstart+1
        } else{
            wgroup=c(wgroup,W[i])
        }
    }

    res=xgroup
    if(length(wgroup)>0){
    res=c(res,wgroup)
    for(i in seq_along(xgroup)){
        for(j in seq_along(wgroup)){
            res=c(res,paste0(xgroup[i],":",wgroup[j]))
        }
    }
    }
    ycount=length(unique(data[[Y]]))
    res
    ycount
    if(is.factor(data[[Y]])|((ycount>2)&(ycount<=maxylev))){
        no=1
        temp=c()
        for(i in 1:ycount){
            ygroup=ifelse(!is.null(grouplabels[[Y]]),grouplabels[[Y]],letters[groupstart])
            if(mode==0) {
                temp1=paste0(prefix,no:(no+length(res)-1),"*",res)
            } else{
                temp1=res
            }
            temp=c(temp,paste0(ygroup,i,"~",paste0(temp1,collapse="+")))
            no=no+length(res)
        }
        eq=paste0(temp,collapse="\n")

    } else{
         if(mode==0) res=paste0(prefix,1:length(res),"*",res)
         eq=paste0(Y,"~",paste0(res,collapse="+"))
    }
    eq
}

