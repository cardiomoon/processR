#' Make Mediation Equation with one categorical variable
#' @param X Name of independent variable
#' @param Y Name of dependent variable
#' @param M Name of mediator variable
#' @param labels optional list
#' @param data A data.frame
#' @param moderator A list
#' @param covar A list of covariates
#' @param mode A numeric. 0: SEM equation, 1: regression equation
#' @param maxylev maximal unique length of categorical variable
#' @param range A logical
#' @param rangemode range mode
#' @export
#' @examples
#' labels=list(X="cyl",M="am",Y="mpg")
#' moderator=list(name=c("cyl","wt"),site=list(c("a","c"),c("c")))
#' covar=list(name=c("carb","disp"),label=c("carb","disp"),site=list(c("M","Y"),"Y","Y"))
#' cat(catMediation(labels=labels,data=mtcars))
#' cat(catMediation(X="am",Y="mpg",data=mtcars,moderator=moderator,covar=covar,maxylev=6))
#' cat(catMediation(X="am",Y="mpg",data=mtcars,moderator=moderator,covar=covar))
#' cat(catMediation(X="cyl",M="am",Y="mpg",data=mtcars))
#' cat(catMediation(X="cyl",M="am",Y="mpg",data=mtcars,moderator=moderator))
#' cat(catMediation(X="cyl",M="am",Y="mpg",data=mtcars,moderator=moderator))
#' cat(catMediation(X="am",M="hp",Y="mpg",data=mtcars,moderator=moderator,maxylev=6))
#' cat(catMediation(X="hp",M="am",Y="mpg",data=mtcars,maxylev=6))
#' cat(catMediation(X="am",M="hp",Y="mpg",data=mtcars,moderator=moderator,covar=covar))
catMediation=function(X=NULL,M=NULL,Y=NULL,labels=list(),data,moderator=list(),
                      covar=NULL,mode=0,maxylev=2,range=TRUE,rangemode=1){

      # X="X";M="M";Y="Y";cat="M";count=4;data=NULL;moderator=list();maxylev=6;
      # covar=NULL;mode=0;range=TRUE;rangemode=1
      # cat=NULL;count=NULL;moderator=list();maxylev=3
      # moderator=list(name=c("carb"),site=list(c("c")))
      # X="am";M=NULL;Y="mpg";data=mtcars;covar=NULL;mode=0;range=TRUE;rangemode=1;maxylev=2

  if(is.null(X)) X=labels$X
  if(is.null(M)) if(!is.null(labels$M)) M=labels$M
  if(is.null(Y)) Y=labels$Y

  res=c()

    xcount=length(unique(data[[X]]))
    if(!is.null(M)) {
        mcount=length(unique(data[[M]]))
    } else{
        mcount=0
    }

    vars=c(X,M,Y,moderator$name,covar$name)
    groupvars=c()
    grouplabels=list()
    for(i in seq_along(vars)) {
       count=length(unique(data[[vars[i]]]))
       if(is.factor(data[[vars[i]]])) {
          groupvars=c(groupvars,vars[i])
       } else if((count>2)&(count<=maxylev)){
          groupvars=c(groupvars,vars[i])
       }
    }
    for(i in seq_along(groupvars)){
       grouplabels[[groupvars[i]]]=LETTERS[3+i]
       attr(grouplabels[[groupvars[i]]],"length")=length(unique(data[[groupvars[i]]]))-1
    }
    # str(grouplabels)
    (XM=moderator$name[str_detect2(moderator$site,"a")])
    (MY=moderator$name[str_detect2(moderator$site,"b")])
    (XY=moderator$name[str_detect2(moderator$site,"c")])

    if(!is.null(M)){
    eq1=makeCatEquation(X=X,Y=M,W=XM,data=data,prefix="a",maxylev=maxylev,grouplabels=grouplabels,mode=mode)
    # maxylev
    eq1

    if((!is.null(covar)) & !(M %in% groupvars)){
        covar$site=lapply(covar$site,function(x) str_replace(x,"M",M))
        if(mode){
            eq1=addCovarEquation(eq1,covar,prefix=NULL)
        } else{
            eq1=addCovarEquation(eq1,covar,prefix="f")
        }
    }
    eq1

    temp1=unlist(strsplit(eq1,"\n"))
    temp1=lapply(1:length(temp1),function(i){
        unlist(strsplit(unlist(strsplit(temp1[i],"~",fixed=TRUE))[2],"+",fixed=TRUE))
    })
    temp1

    eq2=makeCatEquation(X=M,Y=Y,W=MY,data=data,prefix="b",maxylev=maxylev,grouplabels=grouplabels,mode=mode)
    eq2
    temp2=unlist(strsplit(eq2,"~"))[2]
    temp2=unlist(strsplit(temp2,"+",fixed=TRUE))
    temp2
    }


    eq3=makeCatEquation(X=X,Y=Y,W=XY,data=data,prefix="c",maxylev=maxylev,grouplabels=grouplabels,mode=mode)
    eq3
    temp3=unlist(strsplit(eq3,"~"))[2]
    temp3
    temp3=unlist(strsplit(temp3,"+",fixed=TRUE))

    if(is.null(M)) {
        eq=eq3
    } else{
        eq=sumEquation(eq2,eq3)
    }

    if((!is.null(covar)) & !(Y %in% groupvars)){
        covar$site=lapply(covar$site,function(x) str_replace(x,"Y",Y))
        if(mode){
            eq=addCovarEquation(eq,covar,prefix=NULL,grouplabels=grouplabels)
        } else{
            eq=addCovarEquation(eq,covar,prefix="g",grouplabels=grouplabels)
        }
    }

    equation=ifelse(is.null(M),eq,paste(eq1,eq,sep="\n"))


    if((mode==0)&(!is.null(M))){
        moderatorNames=moderator$name
        for(i in seq_along(moderatorNames)){
            name=moderatorNames[i]
            temp=paste0(name," ~ ",name,".mean*1\n")
            temp=paste0(temp,name," ~~ ",name,".var*",name,"\n")
            equation=paste0(equation,"\n",temp)
        }
        temp1
        temp2
        temp3
        moderatorNames
        range
        rangemode
        grouplabels

        temp=makeIndirectEquationCat(X,M,temp1,temp2,temp3,moderatorNames,range=range,data=data,rangemode=rangemode,grouplabels=grouplabels)
        temp
        equation=paste0(equation,temp)
        }


    equation


}

#'Convert equation to data.frame
#'@param eq equation seperated by linefeed
eq2df=function(eq){
    as.data.frame(matrix(unlist(lapply(unlist(strsplit(eq,"\n")),strsplit,"~")),ncol=2,byrow=TRUE))
}

#'summation of equations
#'@param eq1 A equation
#'@param eq2 A equation
#'@importFrom dplyr left_join
#'@export
sumEquation=function(eq1,eq2){
    df1=eq2df(eq1)
    df2=eq2df(eq2)
    df=dplyr::left_join(df1,df2,by="V1")
    colnames(df)=c("Y","X1","X2")
    df$eq=paste0(df$Y,"~",df$X1,"+",df$X2)
    df
    paste0(df$eq,collapse="\n")
}

#'Make indirect equation for categorical variables
#'@param X A character vector
#'@param M A character vector
#'@param temp1 A character vector
#'@param temp2 A character vector
#'@param temp3 A character vector
#'@param moderatorNames A character vector
#'@param range A logical
#' @param data A data.frame
#' @param rangemode range mode
#' @param probs numeric vector of probabilities with values in [0,1]
#'@param grouplabels A list
#'@export
makeIndirectEquationCat=function(X,M,temp1,temp2,temp3,moderatorNames,
                              range=TRUE,data=NULL,rangemode=1,probs=c(0.16,0.5,0.84),
                              grouplabels=list()){

     # data=mtcars;range=TRUE;rangemode=1;probs=c(0.16,0.5,0.84)
    equation=""

    groups=list()

    if(length(grouplabels)>0){

       count=unlist(lapply(1:length(grouplabels),function(i){
          length(unique(data[[names(grouplabels)[i]]]))-1
       }))

      for(i in seq_along(grouplabels)){
        groups[[names(grouplabels)[i]]]=paste0(grouplabels[[i]],1:count[i])
      }
    }

    if(X %in% names(groups)) X=groups[[X]]
    if(M %in% names(groups)) M=groups[[M]]
    xcount=length(X)
    mcount=length(M)


    for(i in 1:xcount){
        for(j in 1:mcount){

         # i=1;j=1
        xlabel=ifelse(xcount>1,paste0(".",X[i]),"")
        mlabel=ifelse(mcount>1,paste0(".",M[j]),"")
        temp4<-temp1[[j]]

        temp4=stringr::str_replace_all(temp4,":","*")
        ind1=strGrouping(temp4,X[i])$yes
        ind1
        temp2=stringr::str_replace_all(temp2,":","*")
        ind2=strGrouping(temp2,M[j])$yes
        ind=paste0("(",str_flatten(ind1,"+"), ")*(",str_flatten(ind2,"+"),")")
        ind
        res=treatModerator(ind,moderatorNames,data=data,rangemode=rangemode,probs=probs)
        res
        ind<-res[[1]]
        ind.below=res[[2]]
        ind.above=res[[3]]
        equation=paste0(equation,"\nindirect",xlabel,mlabel," :=",ind,"\n")
        temp3=stringr::str_replace_all(temp3,":","*")
        direct=strGrouping(temp3,X[i])$yes
        dir=paste0(str_flatten(direct,"+"))
        res=treatModerator(dir,moderatorNames,data=data,rangemode=rangemode,probs=probs)
        dir<-res[[1]]
        dir.below=res[[2]]
        dir.above=res[[3]]
        equation=paste0(equation,"direct",xlabel,mlabel," :=",dir,"\n")
        equation=paste0(equation,"total",xlabel,mlabel," := direct",xlabel,mlabel," + indirect",xlabel,mlabel,"\n")
        equation=paste0(equation,"prop.mediated",xlabel,mlabel," := indirect",xlabel,mlabel," / total",xlabel,mlabel,"\n")
        if((range)&(length(moderatorNames)>0)){
            equation=paste0(equation,"indirect",xlabel,mlabel,".below :=",ind.below,"\n")
            equation=paste0(equation,"indirect",xlabel,mlabel,".above :=",ind.above,"\n")
            equation=paste0(equation,"direct",xlabel,mlabel,".below:=",dir.below,"\n")
            equation=paste0(equation,"direct",xlabel,mlabel,".above:=",dir.above,"\n")
            equation=paste0(equation,"total",xlabel,mlabel,".below := direct",xlabel,mlabel,".below + indirect",xlabel,mlabel,".below\n",
                            "total",xlabel,mlabel,".above := direct",xlabel,mlabel,".above + indirect",xlabel,mlabel,".above\n")
            equation=paste0(equation,"prop.mediated",xlabel,mlabel,".below := indirect",xlabel,mlabel,".below / total",xlabel,mlabel,".below\n",
                            "prop.mediated",xlabel,mlabel,".above := indirect",xlabel,mlabel,".above / total",xlabel,mlabel,".above\n")
            equation
            # if(length(moderatorNames)==1) {
            #
            #     temp=ind1[str_detect(ind1,fixed("*"))]
            #     temp=unlist(str_split(temp,fixed("*")))
            #     temp
            #     if(length(temp)>2){
            #         temp[seq(1,length(temp),2)]
            #         ind2
            #         equation=paste0(equation,"index.mod.med",xlabel,mlabel," := ",
            #                         temp[seq(1,length(temp),2)],"*",str_flatten(ind2,"+"),"\n")
            #     }
            #
            # }
        }
        }
      }
   equation
}
