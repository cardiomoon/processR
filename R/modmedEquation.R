#' Perform mean centering
#' @param data A data.frame
#' @param names column names to mean centering
#' @examples
#' library(processR)
#' newData=meanCentering(education,colnames(education)[1:3])
#' @export
meanCentering=function(data,names){
  for(i in seq_along(names)){
    data[[paste0(names[i],".c")]]=scale(data[[names[i]]],center=TRUE,scale=FALSE)
  }
  data
}

#'make interaction equation
#'@param x character vector
#'@param prefix prefix
#'@param skip whether or not skip
#'@param count Numeric
#'@param addPrefix A logical
#'@examples
#'interactStr(LETTERS[1])
#'interactStr(LETTERS[1:3])
#'interactStr(LETTERS[1:3],skip=TRUE)
#'@export
interactStr=function(x,prefix="a",skip=FALSE,count=1,addPrefix=TRUE){
    res=c()
    for(i in seq_along(x)){
        if((i!=2) | (skip==FALSE)){

            if(addPrefix) {
                temp=paste0(prefix,ifelse(length(x)>1,count,
                                          ifelse(count>1,count,"")),"*",x[i])}
            else{
                temp=x[i]
            }
        res=c(res,temp)
        count=count+1
        }
        if(i>1){
            if(addPrefix){
            temp=paste0(prefix,ifelse(length(x)>1,count,ifelse(count>1,count,""))
                        ,"*",x[1],":",x[i])
            } else{
                temp=paste0(x[1],":",x[i])
            }
            res=c(res,temp)
            count=count+1
        }
    }
    res
}

#' Extract group by string
#' @param string character vector
#' @param groupby name of groupby
#' @importFrom stringr fixed
#' @export
extractX=function(string,groupby="X"){
   for(i in seq_along(string)){
      if(string[i]==groupby) string[i]=paste0("1*",groupby)
   }
  str_replace(string,fixed(paste0("*",groupby)),"")
}

#' Make Grouping equation
#' @param x  character vector
#' @param groupby name of groupby
#' @importFrom stringr str_detect
#' @export
strGrouping=function(x,groupby="X"){

    yes=x[str_detect(x,groupby)]
    yes=extractX(yes,groupby=groupby)
    no=x[!str_detect(x,groupby)]
    no
    list(yes=yes,no=no)
}

#'Extension of str_detect to list
#'@param list a list
#'@param pattern pattern to look for
#'@export
#'@examples
#'site=list(c("a","c"),c("a","b","c"))
#'str_detect2(site,"b")
str_detect2=function(list,pattern){


  str_detect_any=function(string,pattern){
    any(str_detect(string,pattern))
  }

  unlist(lapply(list,str_detect_any,pattern))
}


#'Make moderated mediation equation
#' @param X A character vectors indicating independent variables
#' @param M A character vectors indicating mediators
#' @param Y A character vectors indicating dependent variables
#' @param moderator moderator
#' @param labels labels
#' @param range Whether or not add range equation
#' @param covar Optional list of covariates
#' @importFrom stringr str_replace_all str_detect
#' @export
#' @examples
#' X="X";Y="Y"
#' moderator=list(name=c("Z"),site=list(c("a","c")))
#' cat(modmedEquation(X=X,Y=Y,moderator=moderator,range=TRUE))
#' X="X";M="M";Y="Y"
#' cat(modmedEquation(X=X,M=M,Y=Y,range=TRUE))
#' X="X";M="M";Y="Y"
#' moderator=list(name=c("Z"),site=list(c("a","c")))
#' cat(modmedEquation(X=X,M=M,Y=Y,moderator=moderator,range=TRUE))
#' X="X";M="M";Y="Y";labels=NULL;range=FALSE
#' moderator=list(name=c("X"),site=list(c("b")))
#' cat(modmedEquation(X=X,M=M,Y=Y,moderator=moderator,range=FALSE))
#' X="X";Y="Y"
#' moderator=list(name=c("Z"),site=list(c("c")))
#' cat(modmedEquation(X=X,Y=Y,moderator=moderator,range=FALSE))
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' cat(modmedEquation(X=X,M="M",Y=Y,moderator=moderator,range=FALSE,covar=covar))
modmedEquation=function(X="",M=NULL,Y="",moderator=list(),labels=NULL,range=FALSE,covar=list()){

  # M=NULL; labels=NULL;range=FALSE
  # labels=NULL;range=FALSE;covar=list()

  (XM=moderator$name[str_detect2(moderator$site,"a")])
  (MY=moderator$name[str_detect2(moderator$site,"b")])
  (XY=moderator$name[str_detect2(moderator$site,"c")])

  # Regression of Moderator
  XM=c(X,XM)
  XMstr=interactStr(XM,prefix="a")
  XMstr
  if(!is.null(M)) {
    equation=paste(M,"~",stringr::str_flatten(XMstr,"+"))
  } else{
    equation=""
  }
  if(length(covar$name)>0){
    j=1
    for(i in 1:length(covar$name)){
      if("Mi" %in% covar$site[[i]]){
        equation=paste0(equation," + ","f",j,"*",covar$name[i])
        j=j+1
      }
    }
  }
  equation=paste0(equation,"\n")
  MY=c(M,MY)
  XY=c(X,XY)
  skip=ifelse(X %in% MY,TRUE,FALSE)
  MYstr=interactStr(MY,prefix="b",skip=skip)
  MYstr
  skip=FALSE
  if(!is.null(M)) skip=ifelse(M %in% XY,TRUE,FALSE)
  XYstr=interactStr(XY,prefix="c",skip=skip)
  XYstr
  if(!is.null(M)){
    temp=paste(Y,"~",stringr::str_flatten(XYstr,"+"),"+",
               stringr::str_flatten(MYstr,"+"))
  } else{
    temp=paste(Y,"~",stringr::str_flatten(XYstr,"+"))
  }
  if(length(covar$name)>0){
    j=1
    for(i in 1:length(covar$name)){
      if("Y" %in% covar$site[[i]]){
        temp=paste0(temp," + ","g",j,"*",covar$name[i])
        j=j+1
      }
    }
  }
  temp=paste0(temp,"\n")
  equation=paste0(equation,temp)

  for(i in seq_along(moderator$name)){
    name=moderator$name[i]
    temp=paste0(name," ~ ",name,".mean*1\n")
    temp=paste0(temp,name," ~~ ",name,".var*",name,"\n")
    equation=paste0(equation,temp)
  }
  if(!is.null(M)){
    XMstr
    XMstr=stringr::str_replace_all(XMstr,":","*")
    ind1=strGrouping(XMstr,X)$yes
    ind1
    MYstr=stringr::str_replace_all(MYstr,":","*")
    MYstr
    ind2=strGrouping(MYstr,M)$yes
    ind2
    ind=paste0("(",str_flatten(ind1,"+"), ")*(",str_flatten(ind2,"+"),")")
    ind
    ind.below<-ind.above<-ind
    for(i in seq_along(moderator$name)){
      temp=paste0(moderator$name[i],".mean")
      ind=str_replace_all(ind,moderator$name[i],temp)
      temp=paste0("(",moderator$name[i],".mean-sqrt(",moderator$name[i],".var))")
      ind.below=str_replace_all(ind.below,moderator$name[i],temp)
      temp=paste0("(",moderator$name[i],".mean+sqrt(",moderator$name[i],".var))")
      ind.above=str_replace_all(ind.above,moderator$name[i],temp)
    }
    ind.below
    ind.above
    ind
    equation=paste0(equation,"indirect :=",ind,"\n")

    XYstr=stringr::str_replace_all(XYstr,":","*")
    XYstr
    direct=strGrouping(XYstr,X)$yes
    dir=paste0(str_flatten(direct,"+"))
    dir.below<-dir.above<-dir
    for(i in seq_along(moderator$name)){
      temp=paste0(moderator$name[i],".mean")
      dir=str_replace_all(dir,moderator$name[i],temp)
      temp=paste0("(",moderator$name[i],".mean-sqrt(",moderator$name[i],".var))")
      dir.below=str_replace_all(dir.below,moderator$name[i],temp)
      temp=paste0("(",moderator$name[i],".mean+sqrt(",moderator$name[i],".var))")
      dir.above=str_replace_all(dir.above,moderator$name[i],temp)
    }
    dir
    equation=paste0(equation,"direct :=",dir,"\n")
    equation=paste0(equation,"total := direct + indirect\n")
    if(range){
      equation=paste0(equation,"indirect.SDbelow :=",ind.below,"\n")
      equation=paste0(equation,"indirect.SDabove :=",ind.above,"\n")
      equation=paste0(equation,"direct.SDbelow:=",dir.below,"\n")
      equation=paste0(equation,"direct.SDabove:=",dir.above,"\n")
      equation=paste0(equation,"total.SDbelow := direct.SDbelow + indirect.SDbelow\n",
                      "total.SDabove := direct.SDabove + indirect.SDabove\n")
      equation=paste0(equation,"prop.mediated.SDbelow := indirect.SDbelow / total.SDbelow\n",
                      "prop.mediated.SDabove := indirect.SDabove / total.SDabove\n")


      if(length(moderator$name)==1) {
        temp=ind1[str_detect(ind1,fixed("*"))]
        temp=unlist(str_split(temp,fixed("*")))
        temp[seq(1,length(temp),2)]
        ind2
        equation=paste0(equation,"index.mod.med := ",
                        temp[seq(1,length(temp),2)],"*",str_flatten(ind2,"+"),"\n")

      }
    }
  }
  equation

}

#' Remove parentheses
#' @param string A character vector
#' @export
removeParentheses=function(string){
  res=c()
  for(i in seq_along(string)){
    x=string[i]
    x=str_replace(x,fixed(")"),"")
    x=unlist(str_split(x,fixed("*(")))
    x=unlist(str_split(x,fixed("+")))
    x

    for(i in seq_along(x)){
      if(i==1){
        if(length(x)==1) {
          temp=x[1]
        } else{
          temp=NULL
        }
      } else {
        temp=paste0(x[1],"*",x[i])
      }
      res=c(res,temp)
    }
  }
  res
}

