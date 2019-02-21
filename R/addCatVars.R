#' Add dummy vars to data.frame
#' @param df A data.frame
#' @param varnames Variable name to be converted as factor and add dummies
#' @param groupLetter A character
#' @param mode if mode is 2, apply different coding system
#' @export
#' @examples
#' addCatVars(mtcars,c("cyl","carb"))
#' protest1=addCatVars(protest,"protest")
#' head(protest1)
addCatVars=function(df,varnames,groupLetter="d",mode=1){

    start=grep(groupLetter,letters[])-1

    for(i in seq_along(varnames)){
        if(is.factor(df[[varnames[i]]])) {
            temp=df[[varnames[i]]]
        } else{
            temp<-factor(df[[varnames[i]]])
        }
        res=sort(as.numeric(unique(df[[varnames[i]]])))
        if((mode==2)&(length(res)==3)){
            df[[paste0(groupLetter,"1")]]=ifelse(as.numeric(temp)==1,-2/3,1/3)
            df[[paste0(groupLetter,"2")]]=ifelse(as.numeric(temp)==1,0,
                                                 ifelse(as.numeric(temp)==2,-1/2,1/2))
        } else {
            for(j in 2:length(res)){
                df[[paste0(letters[start+i],(j-1))]]=ifelse(as.numeric(df[[varnames[i]]])==(j-1),1,0)
            }
        }
    }
    df
}

