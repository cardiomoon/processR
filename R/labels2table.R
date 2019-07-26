#'add name to labels
#'@param labels A list
#'@param id label id
#'@param name A character
#'@export
#'@examples
#'labels=c(X="X",M="M",Y="Y")
#'addLabels(labels,"W","X")
#'addLabels(labels,"W","W")
addLabels=function(labels,id,name){
   if(!(name %in% unlist(labels))){
       labels[[id]]=name
   }
   labels
}

#' Append labels from vars, moderator and covar
#' @param labels A list
#' @param vars A list
#' @param moderator  A list
#' @param covar A list
appendLabels=function(labels,vars=list(),moderator=list(),covar=NULL){
  if(length(vars)>0){
    if(is.null(vars$label)){
      if(length(vars$name)==1){
        labels=addLabels(labels,"W",vars$name[[1]][1])
        labels=addLabels(labels,"Z",vars$name[[1]][2])

      } else{
        for(i in seq_along(vars$name)){
          labels=addLabels(labels,paste0("W",i),vars$name[[i]][1])
          labels=addLabels(labels,paste0("Z",i),vars$name[[i]][2])
        }
      }
    } else{
      if(length(vars$name)==1){
        labels=addLabels(labels,vars$label[[1]][1],vars$name[[1]][1])
        labels=addLabels(labels,vars$label[[1]][2],vars$name[[1]][2])

      } else{
        for(i in seq_along(vars$name)){
          labels=addLabels(labels,vars$label[[i]][1],vars$name[[i]][1])
          labels=addLabels(labels,vars$label[[i]][2],vars$name[[i]][2])
        }
      }
    }
  }
  if(length(moderator)>0){

    # prefix=ifelse(length(vars)==0,"W","V")
    # if(length(moderator$name)==1){
    #   labels=addLabels(labels,prefix,moderator$name)
    # } else{
    # for(i in seq_along(moderator$name)){
    #   labels=addLabels(labels,paste0(prefix,i),moderator$name[i])
    # }
    # }
    if(is.null(moderator$label)){
      prefix=ifelse(length(vars)==0,"W","V")
      if(length(moderator$name)==1){
        labels=addLabels(labels,prefix,moderator$name)

      } else{
        for(i in 1:length(moderator$name)){
          labels=addLabels(labels,paste0(prefix,i),moderator$name[i])
        }
      }
    } else{
      for(i in 1:length(moderator$label)){
        labels=addLabels(labels,moderator$label[i],moderator$name[i])

      }
    }
  }
  if(!is.null(covar)){
    for(i in seq_along(covar$name)){
      labels=addLabels(labels,paste0("C",i),covar$name[i])
    }
  }
  labels
}

#' Make table with labels
#' @param labels A list
#' @param vars A list
#' @param moderator A list
#' @param covar A list
#' @param serial A logical
#' @param bmatrix integer specifying causal relations among mediators
#' @param eq Optional string contains equation
#' @export
#' @examples
#' labels=list(X="frame",M="justify",Y="donate",W="skeptic")
#' moderator=list(name="skeptic",site=list(c("a","c")))
#' covar=list(name=c("C1","C2","C3"),site=list(c("M","Y"),c("M","Y"),c("M","Y")))
#' labels=list(X="X",M=c("M1","M2","M3"),Y="Y")
#' labels=list(X="X",M=c("M1","M2"),Y="Y")
#' moderator=list();serial=FALSE;eq=NULL
#' labels2table(labels)
#' labels2table(labels,serial=FALSE)
#' labels2table(labels,covar=covar)
#' labels2table(labels,moderator=moderator)
#' labels=list(X="X",M="M",Y="Y")
#' moderator=list(name=c("W"),site=list(c("b","c")))
#' labels2table(labels,moderator=moderator)
#' labels=list(X="baby",M="wine",Y="tile")
#' moderator=list(name=c("milk"),site=list("a"))
#' covar=list(name=c("milk","tent","sand"),site=list(c("Y"),c("M","Y"),c("M","Y")))
#' labels2table(labels,moderator=moderator,covar=covar,serial=FALSE)
labels2table=function(labels=labels,vars=list(),moderator=list(),covar=NULL,serial=TRUE,
                      bmatrix=NULL,
                      eq=NULL){
        # vars=list();eq=NULL;covar=NULL;serial=TRUE;bmatrix=NULL
     # labels=list(X="X",M="M",Y="Y")
     # moderator=list(name="X",site=list(c("b")))
  # labels=list(X="baby",M=c("wine","tent","sand"),Y="tile")
  # bmatrix=c(1,1,0,1,0,0,1,1,1,1)
  # moderator=list(name=c("milk","hair"),labels=c("W","Z"),
  #                matrix=list(c(1,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,1,0,0)))
   # labels=list(X="baby",M="wine",Y="tile")
   # vars=list()
   # moderator=list(name=c("milk"),site=list("a"))
   # covar=list(name=c("milk","tent","sand"),site=list(c("Y"),c("M","Y"),c("M","Y")))
   # serial=FALSE
   # bmatrix=NULL
   # eq=NULL

    if(is.null(eq)) {
      eq=multipleMediation(labels=labels,vars=vars,moderator=moderator,covar=covar,mode=1,
                           serial=serial,bmatrix=bmatrix)

    }
    eq
    eq=checkEquationVars(eq)
    labels = appendLabels(labels,vars,moderator,covar)


    eq
    labels
    df=equations2var(eq,labels=labels)
    df
}


#'make data.frame with equation
#'@param eq equation
#'@param labels A list
#'@export
#'@importFrom purrr map_df
#'@examples
#' labels=list(X="frame",M="justify",Y="donate",W="skeptic")
#' moderator=list(name="skeptic",site=list(c("a","c")))
#' eq=multipleMediation(labels=labels,moderator=moderator,mode=1)
#' covar=list(name=c("C1","C2","C3"),site=list(c("M","Y"),c("M","Y"),c("M","Y")))
#' eq=multipleMediation(labels=labels,covar=covar,mode=1)
#' equations2var(eq,labels=labels)
equations2var=function(eq,labels=list()){
    equations=unlist(strsplit(eq,"\n"))
    # equations
    # eq2var(equations[1],labels=labels)
    # eq2var(equations[2],labels=labels)
    purrr::map_df(equations,eq2var,labels=labels)
}

#'make data.frame with equation
#'@param eq equation
#'@param labels A list
#'@export
#'@examples
#' labels=list(X="frame",M="justify",Y="donate",W="skeptic")
#' eq="donate~justify+frame+skeptic+frame:skeptic"
#' eq2var(eq,labels=labels)
#' eq="Y~M+W+M:W+X+X:W"
#' labels=list(X="X",M="M",Y="Y")
#' eq2var(eq,labels=labels)
#' eq="wine~baby+milk+baby:milk"
#' labels=list(X="baby",M=c("wine","tent","sand"),Y="tile",W="milk")
#' eq2var(eq,labels=labels)
eq2var=function(eq,labels=list()){
    eq=str_replace_all(eq," ","")
    temp=unlist(strsplit(eq,"~"))
    y=temp[1]
    x=unique(unlist(strsplit(temp[2],"\\+")))
    y=rep(y,length(x))
    df=data.frame(y=y,x=x,stringsAsFactors = FALSE)
    df
    df$Variables=changeLabelName(df$y,labels=labels)
    df$Predictors=changeLabelName(df$x,labels=labels)
    df
    prefix=ifelse(df$Variables[1]=="Y","b","a")
    y[1]
    prefix
    df$x
    df$name=makeCoefLabel(name=df$x,dep=y[1],labels=labels,constant = "iy",prefix=prefix)
    df$name=str_replace(df$name,"'","")
    df

}
