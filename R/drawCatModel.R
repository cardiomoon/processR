#' Draw statistical diagram including categorical X
#' @param xcount integer length of categorical varables
#' @param M character name of mediator variable
#' @param W character name of moderator variable
#' @param whatLabel What should the edge labels indicate in the path diagram? Choices are c("est","name")
#' @param addDots logial.
#' @param xmargin horizontal margin between nodes
#' @param radx horizontal radius of the box.
#' @param ymargin vertical margin between nodes
#' @param xlim the x limits (min,max) of the plot
#' @param ylim the y limits (min,max) of the plot
#' @param rady vertical radius of the box.
#' @param maxypos maximal y position of X or W variables
#' @param minypos minimal y position of X or W variables
#' @param ypos  The x and y position of Y node. Default value is c(1,0.5)
#' @param mpos The x and y position of M node. Default value is c(0.5,0.9)
#' @param xinterval numeric. Horizontal intervals among labels for nodes and nodes
#' @param yinterval numeric. Vertical intervals among labels for nodes and nodes
#' @param box.col fill color of the box
#' @param xspace numeric. Horizontal distance bewteen nodes
#' @param label.pos Optional list of arrow label position
#' @importFrom graphics rect text
#' @export
#' @examples
#' drawCatModel(xcount=4)
#' drawCatModel(M="M",box.col="yellow")
#' drawCatModel(M="M",W="W",xlim=c(-0.08,1),ylim=c(0.13,0.95),ypos=c(1,0.55))
#' drawCatModel(xcount=4,M="M",W="W",xlim=c(-0.08,1),ylim=c(0.13,0.95),ypos=c(1,0.55))
drawCatModel=function(xcount=3,M=NULL,W=NULL,whatLabel="name",addDots=TRUE,
                       xmargin=0.01,radx=0.08,
                       ymargin=0.02,xlim=c(-0.2,1.2),ylim=xlim,
                       rady=0.04,maxypos=0.6,minypos=0.2,ypos=c(1,0.5),mpos=c(0.5,0.9),
                       xinterval=NULL,yinterval=NULL,box.col="white",xspace=NULL,label.pos=list()){

    # xcount=3;M="M";W="W";whatLabel="name";addDots=TRUE
    # xmargin=0.01;radx=0.12
    # ymargin=0.02;xlim=c(-0.2,1.2);ylim=xlim
    # rady=0.04;maxypos=0.6;minypos=0.2;ypos=c(1,0.5);mpos=c(0.5,0.9)
    # xinterval=NULL;yinterval=NULL;box.col="white";xspace=NULL;label.pos=list()


    X=paste0("D",1:(xcount-1))
    if(is.null(W)) X=c(X,"Dg-1")
    Y="Y"
    XW=NULL
    if(!is.null(W)) {
        XW=paste0("D",1:(xcount-1),W)
        XW=c(W,XW)
    } else{

      if(addDots) {
          count=length(X)
          X= c(X,X[count])
          X[count]="dot"
          count=length(X)
       }
    }
    X
    XW
    df1=data.frame(label=c(X,XW),stringsAsFactors = FALSE)
    df1
    df1$lty=1
    df1$name=paste0("c",1:nrow(df1))
    if(is.null(W)) df1$name[count]=("cg-1")
    df1$start=df1$label
    df1$end="Y"
    count=length(df1$label)
    df1
    if(!is.null(M)){
        df2<-df1
        df2$name=paste0("a",1:count)
        if(is.null(W)) df2$name[count]=("ag-1")
        df2$end="M"
        df2
        df3=data.frame(label="M",lty=1,name="b",start="M",end="Y",stringsAsFactors = FALSE)
        df=rbind(df1,df2,df3)
    } else{
        df=df1
    }
    df

    name=c("Y","M",c(X,XW))

    nodes=data.frame(name=name,stringsAsFactors = FALSE)
    nodes
    if(is.null(W)){
       nodes$xpos=c(ypos[1],mpos[1],rep(0,count))
       nodes$ypos=c(ypos[2],mpos[2],count:1)
    } else{
        nodes$xpos=c(ypos[1],mpos[1],rep(0,(count-1)/2),0.1,seq(from=0.2,by=0.1,length.out=(count-1)/2))
        nodes$ypos=c(ypos[2],mpos[2],((count-1)/2+2):3,2,rep(1,(count-1)/2))

    }

    nodes$ypos=adjustypos(nodes$ypos,ymargin=ymargin,rady=rady,
                          maxypos=maxypos,minypos=minypos)

    if(is.null(M)) {
        nodes=nodes[-2,]

    }
    nodes


    makeSubscript=function(x){
        res=c()
        for(i in seq_along(x)){
            if(nchar(x[i])==1){
                temp=paste0("expression(italic(",x[i],"))")
            } else if(is.null(W)){
                temp=paste0("expression(italic(",substr(x[i],1,1),"[",substr(x[i],2,nchar(x[i])),"]))")
            } else if(str_detect(x[i],paste0(W,"$"))){
                temp=paste0("expression(italic(",substr(x[i],1,1),"[",substr(x[i],2,nchar(x[i])-nchar(W)),"])*italic(",
                            substr(x[i],nchar(x[i])-nchar(W)+1,nchar(x[i])),"))")
            } else{
                temp=paste0("expression(italic(",substr(x[i],1,1),"[",substr(x[i],2,nchar(x[i])),"]))")
            }
            res=c(res,temp)
        }
        res
    }
    nodes$label=makeSubscript(nodes$name)
    nodes
    arrows=df
    arrows$labelpos=0.55
    arrows$arrpos=0.8
    arrows$no=1
    arrows$label1=arrows$label

    arrows$label=arrows$name
    addprime=TRUE
    arrows

     # print(nodes)
     # print(arrows)
    openplotmat(xlim=xlim,ylim=ylim)


    for(i in 1:nrow(arrows)){
        temppos=arrows$labelpos[i]
        if(!is.null(label.pos[[arrows$name[i]]])) temppos=label.pos[[arrows$name[i]]]
        if(arrows$start[i]!="dot"){
            myarrow2(nodes, from=arrows$start[i],to=arrows$end[i],
                     label=arrows$label[i],no=arrows$no[1],xmargin=xmargin,radx=radx,rady=rady,
                     label.pos=temppos,arr.pos=NULL,lty=arrows$lty[i],addprime=addprime,xspace=xspace)
        }
    }

    for(i in 1:nrow(nodes)){
        xpos=nodes$xpos[i]
        xpos=adjustxpos(xpos,xmargin,radx,xspace=xspace)
        mid=c(xpos,nodes$ypos[i])

        label=eval(parse(text=nodes$label[i]))

        if(nodes$name[i]!="dot"){

            drawtext(mid,radx=radx,rady=rady,lab=label,latent=FALSE,box.col=box.col)
        }

    }
    if(addDots){
        nodes
        select=which(nodes$name=="dot")
        if(length(select)==1){
        xpos=nodes$xpos[select]
        ypos=nodes$ypos[select]
        xpos=adjustxpos(xpos,xmargin,radx,xspace=xspace)
        textplain(c(xpos,mean(c(ypos,nodes$ypos[select-1]-rady))),lab=".")
        textplain(c(xpos,ypos),lab=".")
        textplain(c(xpos,mean(c(ypos,nodes$ypos[select+1]+rady))),lab=".")
        }
        xnodes=nodes[!(nodes$name %in% c("M","Y","W")),]
        if(is.null(W)){
           rect(xpos-xmargin-radx-0.01,min(xnodes$ypos)-rady-ymargin,
               xpos+xmargin+radx+0.01,max(xnodes$ypos)+rady+ymargin,lty=2)
           textplain(c(xpos-xmargin-radx-0.04,mean(xnodes$ypos)),lab=expression(italic(X)))
        } else{
            count=nrow(xnodes)
            wnodes=xnodes[(count/2+1):count,]
            xnodes=xnodes[1:(count/2),]
            xpos=xnodes$xpos[1]
            xpos=adjustxpos(xpos,xmargin,radx,xspace=xspace)
            rect(xpos-xmargin-radx-0.01,min(xnodes$ypos)-rady-ymargin,
                 xpos+xmargin+radx+0.01,max(xnodes$ypos)+rady+ymargin,lty=2)
            textplain(c(xpos-xmargin-radx-0.06,mean(xnodes$ypos)),lab=expression(italic(X)))
            xpos=wnodes$xpos[1]
            xpos2=wnodes$xpos[count/2]
            xpos=adjustxpos(xpos,xmargin,radx,xspace=xspace)
            xpos2=adjustxpos(xpos2,xmargin,radx,xspace=xspace)
            rect(xpos-xmargin-radx-0.01,min(wnodes$ypos)-rady-ymargin,
                 xpos2+xmargin+radx+0.01,max(wnodes$ypos)+rady+ymargin,lty=2)
            text((xpos+xpos2)/2,min(wnodes$ypos)-2*rady-ymargin,lab=expression(italic(X)*italic(W)))

        }

    }

}
