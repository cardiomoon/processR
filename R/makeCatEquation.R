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
#'@param serial logical If TRUE, serial variables are added
#'@export
#'@examples
#'makeCatEquation2(X="wt",Y="mpg")
#'makeCatEquation2(X="wt",Y="mpg",W="cyl")
#'makeCatEquation2(X="wt",Y=c("hp","vs"),W="cyl",prefix="a")
#'makeCatEquation2(X="wt",Y=c("hp","vs"),W=c("cyl","am"),prefix="a",pos=list(1,2))
#'makeCatEquation2(X="wt",Y=c("hp","vs"),W=c("cyl"),prefix="a",pos=list(1))
#'makeCatEquation2(X="wt",Y=c("hp","vs"),W=c("cyl"),prefix="a",pos=list(c(1,2)))
#'makeCatEquation2(X=c("hp","vs"),Y="mpg",W=c("cyl"),prefix="b",pos=list(c(1)))
#'makeCatEquation2(X=c("hp","vs"),Y="mpg",W=c("cyl"),prefix="b")
#'makeCatEquation2(X=c("hp","vs"),Y="mpg",W=c("cyl"),prefix="b",pos=list(c(1,2)))
#'cat(makeCatEquation2(X="wt",Y="carb",W=c("am","hp")))
#'cat(makeCatEquation2(X="X",Y=c("M1","M2","M3"),W=NULL,prefix="a",serial=TRUE))
#'cat(makeCatEquation2(X="X",Y=c("M1","M2","M3"),W=NULL,prefix="a"))
makeCatEquation2=function(X=NULL,Y=NULL,W=NULL,labels=list(),prefix="b",mode=0,pos=list(),serial=FALSE){

  # X="wt";Y=c("hp","vs");W=c("am");data=mtcars;prefix="a";mode=0;pos=list()
  # X=c("hp","vs");Y="mpg";W=c("cyl","wt");prefix="b";mode=0;pos=list(c(1,2),c(1))
  # X="wt";Y="mpg";W="cyl";labels=list();prefix="b";mode=0;pos=list()
         # X="X";Y=c("M1","M2","M3");W=NULL;labels=list();prefix="a";mode=1;pos=list();serial=TRUE

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
      res=c(res,X[i])
      res
      for(l in seq_along(W)){
        if(length(pos)==0){
          res=c(res,W[l],paste0(X[i],":",W[l]))
        } else if(length(pos[[l]])==0){
          res=c(res,W[l],paste0(X[i],":",W[l]))
        } else if(i %in% pos[[l]]){
          res=c(res,W[l],paste0(X[i],":",W[l]))
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
      if(serial) {
        if(j>1) {
          for(k in 1:(j-1)){
              if(mode==0) {
                res1=c(res1,paste0("d",j,k,"*",Y[k]))
              } else{
                res1=c(res1,Y[k])
              }
          }
        }
      }
    }
    res1
    temp=c(temp,paste0(Y[j],"~",paste0(res1,collapse="+")))
  }
  temp

  eq=paste0(temp,collapse="\n")
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
#'@param bmatrix integer specifying causal relations among mediators
#'@param depy logical
#'@param depx logical
#'@export
#'@examples
#'cat(makeCatEquation3(X="X",Y=c("M1","M2","M3"),prefix="a",bmatrix=c(1,1,0,1,0,0,1,1,1,1)))
#'cat(makeCatEquation3(X="X",Y=c("M1","M2","M3"),prefix="a",bmatrix=c(1,1,0,1,0,1,1,1,1,1)))
#'cat(makeCatEquation3(X="X",Y=c("M1","M2","M3"),prefix="a",bmatrix=c(1,1,0,1,1,0,1,1,1,1)))
#'cat(makeCatEquation3(X="X",Y=c("M1","M2","M3"),prefix="a",bmatrix=c(1,1,1,1,1,1,1,1,1,1)))
#'cat(makeCatEquation3(X=c("M1","M2","M3"),Y="Y",prefix="a",bmatrix=c(1,1,1,1,1,1,1,1,1,1),depy=TRUE))
#'cat(makeCatEquation3(X="X",Y="Y",prefix="a",bmatrix=c(1,1,1,1,1,1,1,1,1,1),depy=TRUE,depx=TRUE))
#'cat(makeCatEquation3(X="X",Y="Y",prefix="a",bmatrix=c(1,1,1,1,1,1,0,1,1,1),depy=TRUE,depx=TRUE))
#'cat(makeCatEquation3(X=c("M1","M2"),Y="Y",prefix="a",bmatrix=c(1,1,1,1,0,1),depy=TRUE))
#'cat(makeCatEquation3(X=c("M1","M2"),Y="Y",prefix="a",bmatrix=c(1,1,1,1,1,0),depy=TRUE))
#'cat(makeCatEquation3(X="X",Y=c("M1","M2"),prefix="a",bmatrix=c(1,1,1,0,0,1),depy=FALSE))
#'cat(makeCatEquation3(X="X",Y=c("M1","M2"),prefix="a",bmatrix=c(1,1,1,1,1,1),depy=FALSE))
#'cat(makeCatEquation3(X=c("M1","M2"),Y="Y",prefix="a",bmatrix=c(1,1,1,1,0,1),depy=TRUE))
makeCatEquation3=function(X=NULL,Y=NULL,W=NULL,labels=list(),prefix="b",mode=0,pos=list(),bmatrix,depy=FALSE,depx=FALSE){

  # X="X";Y=c("M1","M2","M3");W=NULL;labels=list();prefix="a";mode=0;pos=list();
  # bmatrix=c(1,1,0,1,1,0,1,1,1,1);depy=FALSE
  # bmatrix=c(1,1,1,1,1,1,1,1,1,1);depy=FALSE
   # X=c("M1","M2","M3");Y="Y";W=NULL;labels=list();prefix="a";mode=0;pos=list();
   # bmatrix=c(1,1,1,1,1,1,0,1,1,1);depy=TRUE
    # X="X";Y="Y";W=NULL;prefix="a";bmatrix=c(1,1,1,1,1,1,0,1,1,1);depy=TRUE;depx=TRUE;mode=0;pos=list()
     # X=c("M1","M2");W=NULL;Y="Y";prefix="a";bmatrix=c(1,1,1,0,0,1);depy=TRUE;depx=FALSE;labels=list();mode=0

  if(is.null(X)) X=labels$X
  if(is.null(W)) if(!is.null(labels$W)) W=labels$W
  if(is.null(Y)) Y=labels$Y

  xgroup<-wgroup<-c()
  xcount<-wcount<-ycount<-0

  xcount=length(X)
  wcount=length(W)
  ycount=length(Y)

  temp=c()

  j=1
  for(j in 1:ycount){
    res1=c()
    if(depy==FALSE){
    for(i in 1:xcount){
      res=c()
      res=c(res,X[i])
      for(l in seq_along(W)){
        if(length(pos)==0){
          res=c(res,W[l],paste0(X[i],":",W[l]))
        } else if(length(pos[[l]])==0){
          res=c(res,W[l],paste0(X[i],":",W[l]))
        } else if(i %in% pos[[l]]){
          res=c(res,W[l],paste0(X[i],":",W[l]))
        }
      }
      if(mode==0){
        temp1=c()
        for(k in 1:length(res)){
          temp1=c(temp1,paste0(prefix,ifelse(xcount>1,i,""),
                               ifelse(ycount>1,j,""),"*",res[k]))
        }
      } else{
        temp1=res
      }
      res1=c(res1,temp1)
      if(j>1){
          for(k in 2:j){
              pos=1+sum(1:(j-1))+(k-1)
               #cat("j=",j,",k=",k,"\n")
               #cat("pos=",pos,",bmatrix[pos]=",bmatrix[pos],"\n")
              if(bmatrix[pos]==1){
                if(mode==0) {
                  res1=c(res1,paste0("d",j,k-1,"*",Y[k-1]))
                } else{
                  res1=c(res1,Y[k-1])
                }
              }
          }
      }
    }
    } else if(depx==TRUE){
      count=length(bmatrix)
      count
      if(count==1) {
        xcount=1
      } else if(count==3) {
        xcount=2
      } else if(count==6) {
        xcount=3
      } else if(count==10) {
        xcount=4
      } else if(count==15) {
        xcount=5
      } else if(count==21) {
         xcount=6
      } else xcount=7
      xcount
      pos=1+sum(1:(xcount-1))
      pos
      bmatrix[pos]
      if(bmatrix[pos]==1){
        if(mode==0) {
          res1=c(res1,paste0("c*",X))
        } else{
          res1=c(res1,X)
        }

      }
    } else{
      pos=1+sum(1:(xcount))
      pos=pos+1
      pos
      l=1
      for(k in pos:length(bmatrix)){
        if(bmatrix[k]==1){
          if(mode==0) {
            res1=c(res1,paste0("b",l,"*",X[l]))
          } else{
            res1=c(res1,X[l])
          }

        }
        l=l+1
      }
    }
    res1
    if(length(res1)>0) temp=c(temp,paste0(Y[j],"~",paste0(res1,collapse="+")))
  }
  temp

  eq=paste0(temp,collapse="\n")
  eq
}
