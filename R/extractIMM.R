#'Extract name of moderator from string
#'@param string A string
#'@export
#'@examples
#'string="(a1+a3*age.mean)*(b1+b3*age.mean)"
#'string="(a1+a3*age.mean)*(b)"
#'string="(a1+a3*4.12)*(b)"
#'string="(a)*(b)"
#'extractNumber(string)
#'extractModerator(string)
extractModerator=function(string){
    if(str_detect(string,"\\)\\*\\(")){
        temp=unlist(strsplit(string,"\\)\\*\\("))
        temp[1]=str_replace(temp[1],"\\(","")
        temp[2]=str_replace(temp[2],"\\)","")
    } else{
        temp=string
    }
    temp=unlist(strsplit(temp,"\\+"))
    temp=unlist(strsplit(temp,"\\*"))
    res=unique(temp[str_detect(temp,"mean")])
    if(length(res)==0) {
        res=unique(extractNumber(string))
    }
    res
}

#' extract index of moderated mediation from string
#' @param string A string
#' @export
#' @examples
#'string="(a1+a3*age.mean)*(b1+b3*age.mean)"
#'string="(a1+a3*skeptic.mean)*(b1+b2*skeptic.mean+b4*Z.mean)"
#'string="(a1+a3*age.mean)*(b)"
#'string="(a1+a3*4.12)*(b)"
#'string="(a)*(b)"
#'extractIMM(string)
extractIMM=function(string){
    mod=extractModerator(string)
    if(is.null(mod)) return(NULL)
    string=str_replace_all(string,mod,"W")
    string
    temp=unlist(strsplit(string,"\\)\\*\\("))
    temp[1]=str_replace(temp[1],"\\(","")
    temp[2]=str_replace(temp[2],"\\)","")
    templist=strsplit(temp,"\\+")
    result=c()
    for(i in seq_along(templist[[1]])){
        for(j in seq_along(templist[[2]])){
            result=c(result,paste0(templist[[1]][i],"*",templist[[2]][j]))
        }
    }
    result=result[str_detect(result,"W")]
    result=str_replace(result,"\\*W","")
    result=paste0(result,collapse="+")
    result
    if(str_detect(result,"W")) return(NULL)
    result
}

