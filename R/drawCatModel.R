#'Adjust y position
#'@param ypos y position
#'@param ymargin verical margin of plot
#'@param rady vertical radius of the box
#' @param maxypos maximal y position of X or W variables
#' @param minypos minimal y position of X or W variables
adjustypos=function(ypos,ymargin=0.02,rady=0.06,maxypos=0.6,minypos=0){
    yinterval=ymargin+2*rady
    starty=minypos+ymargin+rady
    yinterval=max((maxypos-starty)/(max(ypos)-1),yinterval)
    ifelse(ypos<1,ypos,starty+(ypos-1)*yinterval)
}

#' Draw statistical diagram with list of lm result
#' @param fit list of lm object
#' @param labels list of variable names
#' @param whatLabel What should the edge labels indicate in the path diagram? Choices are c("est","name")
#' @param xmargin horizontal margin between nodes
#' @param radx horizontal radius of the box.
#' @param ymargin vertical margin between nodes
#' @param rady vertical radius of the box.
#' @param maxypos maximal y position of X or W variables
#' @param minypos minimal y position of X or W variables
#' @param digits integer indicating the number of decimal places
#' @export
#' @examples
#' labels=list(X="protest",W="sexism",M="respappr",Y="liking")
#' moderator=list(name="sexism",site=list(c("a","c")))
#' data1=addCatVars(protest,"protest",mode=3)
#' eq=catMediation(X="protest",M="respappr",Y="liking",moderator=moderator,data=protest,maxylev=6,mode=1)
#' fit=eq2fit(eq,data=data1)
#' modelsSummary2(fit,labels=labels)
#' drawCatModel(fit,labels=labels,whatLabel="name")
#' drawCatModel(fit,labels=labels)
#' labels=list(X="protest",W="sexism",M="respappr",Y="liking")
#' fit=makeCatModel(labels=labels,data=protest)
#' drawCatModel(fit,labels=labels,maxypos=0.6,minypos=0.2)
#' drawCatModel(fit,labels=labels,whatLabel="name",maxypos=0.6,minypos=0.2)
drawCatModel=function(fit,labels=NULL,whatLabel="est",xmargin=0.01,radx=0.12,
                      ymargin=0.02,
                   rady=0.04,maxypos=0.6,minypos=0,digits=3){

    # whatLabel="name";xmargin=0.01;radx=0.12
    # ymargin=0.02
    # rady=0.04;maxypos=0.6;minypos=0;digits=3

    if("lm" %in%  class(fit)) fit=list(fit)
    fitcount=length(fit)
    df1=as.data.frame(summary(fit[[1]])$coef[-1,])
    df1$label=rownames(df1)
    if(!is.null(labels)) df1$label=changeLabelName(rownames(df1),labels,add=FALSE)
    names(df1)[4]="p"
    df1$lty=ifelse(df1$p<0.05,1,2)
    df1$name=paste0("a",1:nrow(df1))
    df1$start=df1$label
    df1$end=ifelse(fitcount==1,"Y","M")
    df1$est=round(df1$Estimate,digits)
    count=length(df1$label)

    if(fitcount>1){
    df2=as.data.frame(summary(fit[[2]])$coef[-1,])
    df2$label=rownames(df2)
    if(!is.null(labels)) df2$label=changeLabelName(rownames(df2),labels,add=FALSE)
    names(df2)[4]="p"
    df2$lty=ifelse(df2$p<0.05,1,2)
    df2$name=""
    ccount=length(df2$label[df2$label!="M"])
    df2$name[df2$label=="M"]="b"
    df2$name[df2$label!="M"]=paste0("c",1:ccount)
    df2$start=df2$label
    df2$end="Y"

    df2$est=round(df2$Estimate,digits)
    df2
    }

    name=c("Y","M",df1$label)
    nodes=data.frame(name=name,stringsAsFactors = FALSE)
    nodes$xpos=c(1,0.5,rep(0,count%/%2),0.05,(1:(1+count%/%2-1))/10)
    nodes$ypos=c(0.5,0.9,((2+count%/%2):3),2,rep(1,count%/%2))
    # nodes$xpos1=adjustxpos(nodes$xpos,xmargin=xmargin,radx=radx)
    nodes$ypos=adjustypos(nodes$ypos,ymargin=ymargin,rady=rady,
                          maxypos=maxypos,minypos=minypos)

    if(fitcount==1) {
        nodes=nodes[-2,]
        arrows=df1
    } else{
       arrows=rbind(df1,df2)
    }
    arrows$labelpos=0.5
    arrows$arrpos=0.8
    arrows$no=1
    arrows$label1=arrows$label
    if(whatLabel=="name") {
        arrows$label=arrows$name
        addprime=TRUE
    } else{
        arrows$label=arrows$est
        addprime=FALSE
    }

    nodes
    arrows
    openplotmat()


    for(i in 1:nrow(arrows)){
        myarrow2(nodes, from=arrows$start[i],to=arrows$end[i],
                 label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                 label.pos=arrows$labelpos[i],arr.pos=NULL,lty=arrows$lty[i],addprime=addprime)
    }

    for(i in 1:nrow(nodes)){
        xpos=nodes$xpos[i]
        xpos=adjustxpos(xpos,xmargin,radx)
        mid=c(xpos,nodes$ypos[i])

        label=nodes$name[i]

        drawtext(mid,radx=radx,rady=rady,lab=label,latent=FALSE)
    }

}
