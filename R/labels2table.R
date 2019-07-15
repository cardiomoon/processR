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
labels2table=function(labels=labels,vars=list(),moderator=list(),covar=NULL,serial=TRUE,
                      bmatrix=NULL,
                      eq=NULL){
    # moderator=list();serial=FALSE;eq=NULL
    # labels=list(X="X",M=c("M1","M2"),Y="Y")
    # vars=list(name=list(c("W","Z")),site=list("a"),arr.pos=list(c(0.5)))
    # covar=NULL;vars=list()

    if(is.null(eq)) {
      eq=multipleMediation(labels=labels,vars=vars,moderator=moderator,covar=covar,mode=1,
                           serial=serial,bmatrix=bmatrix)

    }
    eq
    labels
    if(!is.null(covar)){
        for(i in seq_along(covar$name)){
           labels[[paste0("C",i)]]=covar$name[i]
        }
    }
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
    df$name=makeCoefLabel(name=df$x,dep=y[1],labels=labels,constant = "iy",prefix=prefix)
    df$name=str_replace(df$name,"'","")
    df

}
