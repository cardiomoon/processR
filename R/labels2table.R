#' Make table with labels
#' @param labels A list
#' @param moderator A list
#' @param serial A logical
#' @param eq Optional string contains equation
#' @export
#' @examples
#' labels=list(X="frame",M="justify",Y="donate",W="skeptic")
#' moderator=list(name="skeptic",site=list(c("a","c")))
#' labels=list(X="X",M=c("M1","M2","M3"),Y="Y")
#' labels2table(labels,serial=TRUE)
#' labels2table(labels,moderator=moderator)
labels2table=function(labels=labels,moderator=list(),serial=FALSE,eq=NULL){
    # moderator=list();serial=TRUE
    if(is.null(eq)) {
      eq=multipleMediation(labels=labels,moderator=moderator,mode=1,serial=serial)
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
#' equations2var(eq)
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
#' eq2var("donate~justify+frame+skeptic+frame:skeptic")
eq2var=function(eq,labels=list()){

    temp=unlist(strsplit(eq,"~"))
    y=temp[1]
    x=unlist(strsplit(temp[2],"\\+"))
    y=rep(y,length(x))
    df=data.frame(y=y,x=x,stringsAsFactors = FALSE)
    df$Variables=changeLabelName(df$y,labels=labels)
    df$Predictors=changeLabelName(df$x,labels=labels)
    df
    df$name=makeCoefLabel(name=df$x,dep=y[1],labels=labels,constant = "iy",prefix="b")
    df$name=str_replace(df$name,"'","")
    df

}
