#' Make Equation with categorical variable
#' @param X Name of independent variable
#' @param Y Name of dependent variable
#' @param cat Which variable is categorical? Choices are c("X","Y")
#' @param W Names of moderator variables
#' @param count length of unique values of categorical variable
#' @param data A data.frame
#' @param prefix A prefix
#' @param maxylev maximal unique length of categorial variable
#' @param groupLetter A character vector
#' @export
#' @examples
#' catEquation(count=4)
#' catEquation(cat="Y",count=4,prefix="b")
#' catEquation(W="W",count=4)
#' catEquation(cat="X",W="W",count=4)
#' catEquation(W="W",cat="Y",count=4)
#' catEquation(X="cyl",Y="wt",data=mtcars)
#' catEquation(X="wt",Y="cyl",data=mtcars)
#' catEquation(X="cyl",Y="am",W="wt",data=mtcars)
catEquation=function(X="A",Y="B",cat=NULL,W=NULL,count=NULL,data=NULL,prefix="c",maxylev=6,
                     groupLetter=c("d","e")){

      # X="A";Y="B";cat=NULL;W=NULL;count=NULL;data=NULL;prefix="a";maxylev=6
      # X="cyl";Y="am";data=mtcars
    res=c()
    if(length(cat)==0){
        if(!is.null(data)){
            cat=c()
            xcount=length(unique(data[[X]]))
            ycount=length(unique(data[[Y]]))
            if(is.factor(data[[X]])) cat="X"
            else if((xcount>2)&(xcount<=maxylev)) cat="X"
            if(is.factor(data[[Y]])) cat=c(cat,"Y")
            else if((ycount>2)&(ycount<=maxylev)) cat=c(cat,"Y")

        }
    }
    # cat
    if(length(cat)==0){
        if(length(W)>0) {
            res=c(paste0(prefix,"1*",X),paste0(prefix,"2*",W),paste0(prefix,"3*",X,":",W))
        } else {
            res=paste0(prefix,"1*",X)
        }
        eq=paste0(Y,"~",paste0(res,collapse="+"))

    } else if(length(cat)==2) {  ##cat==c("X","Y")
        xcount=length(unique(data[[X]]))
        ycount=length(unique(data[[Y]]))
        # xcount
        # ycount

        eq=c()

        xgroup=paste0(groupLetter[1],1:(xcount-1))
        xgroup
        ygroup=paste0(groupLetter[2],1:(ycount-1))
        ygroup
        if(length(W)==0){
            res=c()
            for(j in 1:length(ygroup)){
                temp=c()
                for(i in 1:length(xgroup)){
                    temp=c(temp,paste0(prefix,i,j,"*",xgroup[i]))
                }
                res=c(res,paste0(ygroup[j],"~",paste0(temp,collapse="+")))
            }
            res
            eq=paste0(res,collapse="\n")
        } else{
            res=c()
            # W="W"
            for(j in 1:length(ygroup)){
                temp=c()
                for(i in 1:length(xgroup)){
                    wgroup=c(xgroup[i],W,paste0(xgroup[i],":",W))
                    temp=c(temp,paste0(prefix,i,j,1:3,"*",wgroup))
                }
                res=c(res,paste0(ygroup[j],"~",paste0(temp,collapse="+")))
            }
            res
            eq=paste0(res,collapse="\n")
        }
    } else if(cat=="X"){
        if(is.null(count)) count=length(unique(data[[X]]))
        group=paste0(groupLetter[1],1:(count-1))
        group
        res=paste0(prefix,1:(count-1),"*",group)
        res
        if(length(W)>0){
            res<-c(res,paste0(prefix,count,"*",W))
            res1=paste0(prefix,(count+1):(2*count-1),"*",group,":",W)
            res=c(res,res1)
        }
        res
        eq=paste0(Y,"~",paste0(res,collapse="+"))
    } else if(cat=="Y"){  ##cat=="Y"
        if(is.null(count)) count=length(unique(data[[Y]]))
        eq=c()
        if(length(W)==0){
            res=paste0(prefix,1:(count-1),"*",X)
            res
            eq=c(eq,paste0(groupLetter[2],1:(count-1),"~",res))
        } else{
            group=c(X,W,paste0(X,":",W))
            res=c(paste0(prefix,1:((count-1)*3),"*",group))
            res
            for(i in 1:(count-1)){
                start=3*(i-1)+1
                end=3*i
                eq=c(eq,paste0(groupLetter[2],i,"~",paste0(res[start:end],collapse="+")))
            }


        }
        eq=paste0(eq,collapse="\n")
    }
    # print(eq)
    eq
}

#' Make Mediation Equation with one categorical variable
#' @param X Name of independent variable
#' @param Y Name of dependent variable
#' @param M Name of mediator variable
#' @param cat Which variable is categorical? Choices are c("X","Y")
#' @param count length of unique values of categorical variable
#' @param data A data.frame
#' @param moderator A list
#' @param covar A list of covariates
#' @param mode A numeric. 0: SEM equation, 1: regression equation
#' @param maxylev maximal unique length of categorial variable
#' @param range A logical
#' @param rangemode range mode
#' @export
#' @examples
#' moderator=list(name=c("wt"),site=list(c("a","c")))
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list(c("M","Y"),"Y","Y"))
#' cat(catMediation(cat="X",count=4))
#' cat(catMediation(cat="X",count=4,covar=covar))
#' cat(catMediation(cat="M",count=4))
#' cat(catMediation(cat="Y",count=4))
#' cat(catMediation(cat="X",count=3,moderator=moderator))
#' cat(catMediation(cat="M",count=4,moderator=moderator))
#' cat(catMediation(cat="Y",count=4,moderator=moderator))
#' mtcars=addCatVar(mtcars,"cyl")
#' cat(catMediation(X="cyl",M="am",Y="mpg",data=mtcars,moderator=moderator))
catMediation=function(X="X",M="M",Y="Y",cat=NULL,count=NULL,data=NULL,moderator=list(),
                      covar=NULL,mode=0,maxylev=6,range=TRUE,rangemode=1){

       # X="X";M="M";Y="Y";cat="X";count=4;data=NULL;moderator=list();maxylev=6;
       # covar=NULL;mode=0;range=TRUE;rangemode=1
     # X="X";M="M";Y="Y";cat=NULL;count=NULL;data=NULL;moderator=list();maxylev=6
     # X="cyl";M="am";Y="carb";data=mtcars
     # moderator=list(name=c("W"),site=list(c("a","c")))

    res=c()
    if(is.null(cat)){
        if(!is.null(data)){
            xcount=length(unique(data[[X]]))
            mcount=length(unique(data[[M]]))
            ycount=length(unique(data[[Y]]))
            cat=c()
            if(is.factor(data[[X]])) cat=c(cat,"X")
            else if((xcount>2)&(xcount<=maxylev)) cat=c(cat,"X")
            if(is.factor(data[[M]])) cat=c(cat,"M")
            else if((mcount>2)&(mcount<=maxylev)) cat=c(cat,"M")
            if(is.factor(data[[Y]])) cat=c(cat,"Y")
            else if((ycount>2)&(ycount<=maxylev)) cat=c(cat,"Y")
        }
    }
    # cat
    (XM=moderator$name[str_detect2(moderator$site,"a")])
    (MY=moderator$name[str_detect2(moderator$site,"b")])
    (XY=moderator$name[str_detect2(moderator$site,"c")])


    cat1=c()
    if("X" %in% cat) cat1=c(cat1,"X")
    if("M" %in% cat) cat1=c(cat1,"Y")
    eq1=catEquation(X=X,Y=M,cat=cat1,W=XM,count=count,data=data,
                    prefix="a",maxylev=maxylev,groupLetter=c("d","e"))
    eq1

    if((!is.null(covar)) & !("M" %in% cat)){
        covar$site=lapply(covar$site,function(x) str_replace(x,"M",M))
        if(mode){
            eq1=addCovarEquation(eq1,covar,prefix=NULL)
        } else{
            eq1=addCovarEquation(eq1,covar)
        }
    }
    temp1=unlist(strsplit(eq1,"~"))[2]
    temp1
    temp1=unlist(strsplit(temp1,"+",fixed=TRUE))
    cat2=c()
    if("M" %in% cat) cat2=c(cat2,"X")
    if("Y" %in% cat) cat2=c(cat2,"Y")
    eq2=catEquation(X=M,Y=Y,cat=cat2,W=MY,count=count,data=data,
                    prefix="b",maxylev=maxylev,groupLetter=c("e","f"))
    eq2
    temp2=unlist(strsplit(eq2,"~"))[2]
    temp2=unlist(strsplit(temp2,"+",fixed=TRUE))
    temp2

    cat3=c()
    if("X" %in% cat) cat3=c(cat3,"X")
    if("Y" %in% cat) cat3=c(cat3,"Y")
    eq3=catEquation(X=X,Y=Y,cat=cat3,W=XY,count=count,data=data,
                    prefix="c",maxylev=maxylev,groupLetter=c("d","f"))
    eq3
    temp3=unlist(strsplit(eq3,"~"))[2]
    temp3
    temp3=unlist(strsplit(temp3,"+",fixed=TRUE))
    (eq=sumEquation(eq2,eq3))
    equation=paste(eq1,eq,sep="\n")
    equation
    if((!is.null(covar)) & !("Y" %in% cat)){
        covar$site=lapply(covar$site,function(x) str_replace(x,"Y",Y))
        if(mode){
            eq=addCovarEquation(eq,covar,prefix=NULL)
        } else{
            eq=addCovarEquation(eq,covar)
        }
    }

    if(mode==0){
        moderatorNames=moderator$name
        for(i in seq_along(moderatorNames)){
            name=moderatorNames[i]
            temp=paste0(name," ~ ",name,".mean*1\n")
            temp=paste0(temp,name," ~~ ",name,".var*",name,"\n")
            equation=paste0(equation,"\n",temp)
        }
        if(length(moderatorNames)>0){
            if("X" %in% cat) {
                if(is.null(count)) count=xcount
                X1=paste0("d",1:(count-1))
            } else{
                X1=X
            }
        temp=makeIndirectEquationCat(X1,M,temp1,temp2,temp3,moderatorNames,range=range,data=data,rangemode=rangemode)
        temp
        equation=paste0(equation,temp)
        }

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
#'@param X A character string
#'@param M A character string
#'@param temp1 A character vector
#'@param temp2 A character vector
#'@param temp3 A character vector
#'@param moderatorNames A character vector
#'@param range A logical
#' @param data A data.frame
#' @param rangemode range mode
#' @param probs numeric vector of probabilities with values in [0,1]
#'@export
#'@examples
#'X= c("d1","d2");M="am"
#'temp1=c("a1*d1","a2*d2","a3*wt","a4*d1:wt","a5*d2:wt")
#'temp2="b1*am"
#'temp3=c("c1*d1","c2*d2","c3*wt","c4*d1:wt","c5*d2:wt")
#'moderatorNames="wt"
#'cat(makeIndirectEquationCat(X,M,temp1,temp2,temp3,moderatorNames,range=TRUE))
makeIndirectEquationCat=function(X,M,temp1,temp2,temp3,moderatorNames,
                              range=TRUE,data=NULL,rangemode=1,probs=c(0.16,0.5,0.84)){
     # range=TRUE;data=NULL;rangemode=1;probs=c(0.16,0.5,0.84)
    equation=""

      for(i in 1:length(X)){
        temp1=stringr::str_replace_all(temp1,":","*")
        ind1=strGrouping(temp1,X[i])$yes
        ind1
        temp2=stringr::str_replace_all(temp2,":","*")
        ind2=strGrouping(temp2,M)$yes
        ind=paste0("(",str_flatten(ind1,"+"), ")*(",str_flatten(ind2,"+"),")")
        ind
        res=treatModerator(ind,moderatorNames,data=data,rangemode=rangemode,probs=probs)
        res
        ind<-res[[1]]
        ind.below=res[[2]]
        ind.above=res[[3]]
        equation=paste0(equation,"indirect",".",X[i]," :=",ind,"\n")
        temp3=stringr::str_replace_all(temp3,":","*")
        direct=strGrouping(temp3,X[i])$yes
        dir=paste0(str_flatten(direct,"+"))
        res=treatModerator(dir,moderatorNames,data=data,rangemode=rangemode,probs=probs)
        dir<-res[[1]]
        dir.below=res[[2]]
        dir.above=res[[3]]
        equation=paste0(equation,"direct",".",X[i]," :=",dir,"\n")
        equation=paste0(equation,"total.",X[i]," := direct.",X[i]," + indirect.",X[i],"\n")
        if(range){
            equation=paste0(equation,"indirect.",X[i],".below :=",ind.below,"\n")
            equation=paste0(equation,"indirect.",X[i],".above :=",ind.above,"\n")
            equation=paste0(equation,"direct.",X[i],".below:=",dir.below,"\n")
            equation=paste0(equation,"direct.",X[i],".above:=",dir.above,"\n")
            equation=paste0(equation,"total.",X[i],".below := direct.",X[i],".below + indirect.",X[i],".below\n",
                            "total.",X[i],".above := direct.",X[i],".above + indirect.",X[i],".above\n")
            equation=paste0(equation,"prop.mediated.",X[i],".below := indirect.",X[i],".below / total.",X[i],".below\n",
                            "prop.mediated.",X[i],".above := indirect.",X[i],".above / total.",X[i],".above\n")
            if(length(moderatorNames)==1) {
                temp=ind1[str_detect(ind1,fixed("*"))]
                temp=unlist(str_split(temp,fixed("*")))
                if(length(temp)>2){
                    temp[seq(1,length(temp),2)]
                    ind2
                    equation=paste0(equation,"index.mod.med.",X[i]," := ",
                                    temp[seq(1,length(temp),2)],"*",str_flatten(ind2,"+"),"\n")
                }

            }
        }
      }
   equation
}
