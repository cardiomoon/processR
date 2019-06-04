#'Make equation for sem and lm for categorical variables
#'@param X Name of independent variable
#'@param Y Name of dependent variable
#'@param W Name of moderators
#'@param labels optional list
#'@param data a data.frame
#'@param prefix a character
#'@param maxylev maximal unique length of categorical variable
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
makeCatEquation=function(X=NULL,Y=NULL,W=NULL,labels=list(),data,prefix="b",maxylev=6,grouplabels=list(),mode=0){

     # X="wt";Y="carb";W=c("am","hp");data=mtcars;maxylev=6;prefix="b";group="d";ygroup="z"

    if(is.null(X)) X=labels$X
    if(is.null(W)) if(!is.null(labels$W)) W=labels$W
    if(is.null(Y)) Y=labels$Y

    xgroup<-wgroup<-c()
    xcount<-wcount<-ycount<-0
    groupstart=4
    if(length(grouplabels)==0) {
        group="D"
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
            group=ifelse(!is.null(grouplabels[[W[i]]]),grouplabels[[W[i]]],LETTERS[groupstart])
            wgroup=c(wgroup,paste0(LETTERS[groupstart],1:(wcount-1)))
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
            ygroup=ifelse(!is.null(grouplabels[[Y]]),grouplabels[[Y]],LETTERS[groupstart])
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

#'Make equation for sem and lm for multiple X or multiple Y
#'@param X Names of independent variable
#'@param Y Names of dependent variable
#'@param W Names of moderators
#'@param labels optional list
#'@param prefix a character
#'@param mode A numeric
#'@param pos Numeric moderator position
#'@export
#'@examples
#'makeCatEquation2(X="wt",Y="mpg")
#'makeCatEquation2(X="wt",Y="mpg",W="cyl")
#'makeCatEquation2(X="wt",Y=c("hp","vs"),W="cyl",prefix="a")
#'makeCatEquation2(X="wt",Y=c("hp","vs"),W=c("cyl","am"),prefix="a",pos=list(1,2))
#'makeCatEquation2(X="wt",Y=c("hp","vs"),W=c("cyl"),prefix="a",pos=list(1))
#'makeCatEquation2(X=c("hp","vs"),Y="mpg",W=c("cyl"),prefix="b",pos=2)
#'makeCatEquation2(X=c("hp","vs"),Y="mpg",W=c("cyl"),prefix="b")
#'cat(makeCatEquation2(X="wt",Y="carb",W=c("am","hp")))
makeCatEquation2=function(X=NULL,Y=NULL,W=NULL,labels=list(),prefix="b",mode=0,pos=list()){

  # X="wt";Y=c("hp","vs");W=c("am");data=mtcars;prefix="a";mode=0;pos=NULL
   # X=c("hp","vs");Y="mpg";W=c("cyl");prefix="b";mode=0;pos=list()
  # X="wt";Y="mpg";W="cyl";labels=list();prefix="b";mode=0;pos=list()

  if(is.null(X)) X=labels$X
  if(is.null(W)) if(!is.null(labels$W)) W=labels$W
  if(is.null(Y)) Y=labels$Y

  xgroup<-wgroup<-c()
  xcount<-wcount<-ycount<-0

  xcount=length(X)
  wcount=length(W)
  ycount=length(Y)

  temp=c()


  for(j in 1:ycount){
    res1=c()
    for(i in 1:xcount){
      res=c()
      no=max(i,j)
      res=c(res,X[i])
      if(xcount==1){
      for(l in seq_along(W)){
        if(length(pos)<i){
          if(!is.null(W[l])) res=c(res,W[l],paste0(X[i],":",W[l]))
        } else if(no %in% pos[[l]]){
          if(!is.null(W[l])) res=c(res,W[l],paste0(X[i],":",W[l]))
        }
      }
      } else{
         for(l in seq_along(W)){
           if(length(pos)==0){
             res=c(res,W[l],paste0(X[i],":",W[l]))
           } else if(i %in% pos){
              res=c(res,W[l],paste0(X[i],":",W[l]))
            }
         }
      }
      if(mode==0){
        temp1=c()
        for(k in 1:length(res)){
          temp1=c(temp1,paste0(prefix,k,ifelse(xcount>1,i,""),
                               ifelse(ycount>1,j,""),"*",res[k]))
        }
      } else{
        temp1=res
      }
      res1=c(res1,temp1)
    }
    res1

    temp=c(temp,paste0(Y[j],"~",paste0(res1,collapse="+")))
  }
  temp

  eq=paste0(temp,collapse="\n")
  eq
}

