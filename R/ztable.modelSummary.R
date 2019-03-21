#' S3 method for class 'modelSummary'
#'@param x An object of class modelSummary
#'@param ... Further argument to be passed to ztable
#'@importFrom dplyr select
#'@importFrom ztable ztable addcgroup spanCol hlines
#'@export
ztable.modelSummary=function(x,...){
    count=ncol(x)/4
    count

    modelNames=attr(x,"modelNames")
    selected=c()
    align=c("c")
    for(i in 1:count){
        if(i<count) x[[paste0("s",i)]]=""
        start=(i-1)*4+1
        selected=c(selected,start:(start+3))
        align=c(align,c("c","c","c","r"))
        if(i<count) {
            selected=c(selected,ncol(x))
            align=c(align,"c")
        }
    }
    x1 <- x %>% select(selected)
    class(x1)="data.frame"
    align=paste0(align,collapse = "")
    z=ztable::ztable(x1,align=align,...)

    newnames=c()
    newModelNames=c()
    ncgroup=c()
    for(i in 1:count){
        newnames=c(newnames,c("Coef","SE","t","p"))
        newModelNames=c(newModelNames,modelNames[i])
        ncgroup=c(ncgroup,4)
        if(i<count) {
            newnames=c(newnames,"")
            newModelNames=c(newModelNames,"")
            ncgroup=c(ncgroup,1)
        }

    }
    z <- z %>%
        addcgroup(cgroup=newModelNames,n.cgroup=ncgroup) %>%
        addcgroup(cgroup="Consequent",n.cgroup=ncol(x1),top=TRUE) %>%
        hlines(add=nrow(z$x)-5)

    colnames(z$x)=newnames
    for(i in 1:count){
        for(j in 1:5){
            z=spanCol(z,row=nrow(x1)+1-(j-1),from=2+(i-1)*5,to=5+(i-1)*5)
        }

    }
    z

}
