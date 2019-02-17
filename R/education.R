
#' Data Set for process macro model
#'
#'@format A data.frame with 43 rows and 7 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{X}{name of independent variable}
#'   \item{M}{names of mediator variables}
#'   \item{Y}{name of dependent variable}
#'   \item{modName}{names of moderator variables}
#'   \item{modSite}{sites of moderators}
#'   \item{pos}{position of moderators}
#'}
"pmacro"

#' Data Set for education and income
#'
#' A dataset contains measures about the teacher's knowlege, empathy and intervention about attention-deficit hyperactivity disorder(ADHD).
#'
#'@format A data.frame with 850 rows and 4 variables:
#'\describe{
#'   \item{age}{student age}
#'   \item{number}{number of students per class}
#'   \item{duration}{eduation duration}
#'   \item{income}{income}
#'}
"education"

#' Node Data Set for drawing stastical diagram of process macro model
#'
#'@format A data.frame with 327 rows and 4 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{name}{name of node}
#'   \item{xpos}{x position}
#'   \item{ypos}{y position}
#'}
"nodes"


#' Arrow Data Set for drawing stastical diagram of process macro model
#'
#'@format A data.frame with 392 rows and 6 variables
#'\describe{
#'   \item{no}{process macro model number}
#'   \item{name}{name of arrow}
#'   \item{start}{start node}
#'   \item{end}{end node}
#'   \item{labelpos}{position of label}
#'   \item{arrowpos}{position of arrow head}
#'}
"parrows"

#' Teams data set
#'
#' @format A data.frame with 60 rows and 4 variables
#' \describe{
#'    \item{dysfunc}{Dysfunctional team behavior}
#'    \item{negtone}{Negative affective tone}
#'    \item{negexp}{Negative expressivity}
#'    \item{perform}{Team performance}
#' }
#' @source Cole, M. S., Walter, F., & Bruch, H. (2008). Affective mechanisms linking dysfunctional behavior to performance in work teams: A moderated mediation study. Journal of Applied Psychology, 93, 945-958.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"teams"

#' Protest dataset
#'
#' @format A data.frame with 129 rows and 6 variables
#' \describe{
#'    \item{subnum}{subject number}
#'    \item{protest}{experimental condition, 0 = no protest, 1 = individual protest, 2 = group protest}
#'    \item{sexism}{perceived pervasiveness of sex discrimination}
#'    \item{angry}{anger toward the attorney}
#'    \item{liking}{liking of the attorney}
#'    \item{respappr}{appropriateness of response}
#' }
#' @source Garcia, D. M., Schmitt, M. T., Branscombe, N. R., & Ellemers, N. (2010). Women's reactions to ingroup members who protest discriminatory treatment: The importance of beliefs about inequality and response appropriateness. European Journal of Social Psychology, 40, 733-745.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"protest"


#' PMI: Presumed Media Influence dataset
#'
#' @format A data.frame with 123 obs. of 6 variables
#' \describe{
#'    \item{cond}{front (1) or interior (0) page of the newspaper}
#'    \item{pmi}{presumed media influence}
#'    \item{import}{article is on an important topic}
#'    \item{reaction}{sugar purchase}
#'    \item{gender}{GENDER: female (0) or male (1)}
#'    \item{age}{age}
#' }
#' @source Tal-Or, N., Cohen, J., Tsafati, Y., & Gunther, A. C. (2010). Testing causal direction in the influence of presumed media influence. Communication Research, 37, 801-824.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"pmi"

#'Global Warming dataset
#'
#'@format A data.frame with 815 obs. of  7 variables
#'\describe{
#'   \item{govact}{Support for government action}
#'   \item{posemot}{Positive emotions about climate change}
#'   \item{negemot}{Negative emotions about climate change}
#'   \item{ideology}{Political ideology (conservatism), 1 = Very Liberal, 2 = Liberal, 3 = Somewhat Liberal, 4 = Moderate; Middle of the Road, 5 = Somewhat Conservative, 6 = Conservative, 7 = Very Conservative}
#'   \item{age}{Respondent age at last birthday}
#'   \item{sex}{female(0) or male(1)}
#'   \item{partyid}{1 = Democrat, 2 = Independent, 3= Republican}
#'}
#'@source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"glbwarm"


#' ESTRESS: Economic stress dataset
#'
#' @format A data.frame with 262 obs. of  7 variables
#' \describe{
#'    \item{tenure}{Company Tenure}
#'    \item{estress}{Economic stress}
#'    \item{affect}{Depressed affect}
#'    \item{withdraw}{Withdrawal intentions}
#'    \item{sex}{Male (1) or Female (0)}
#'    \item{age}{age}
#'    \item{ese}{Entrepreneurial self efficacy}
#' }
#' @source Pollack, J., VanEpps, E. M., & Hayes, A. F. (2012). The moderating role of social ties on entrepreneurs' depressed affect and withdrawal intentions in response to economic stress. Journal of Organizational Behavior, 33, 789-810.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"estress"

#' Disaster dataset
#'
#' @format A data.frame with 211 obs. of  5 variables
#' \describe{
#'    \item{id}{id}
#'    \item{frame}{Experimental condition. 0 = naturally caused disaster, 1 = climate change caused disaster}
#'    \item{donate}{Positive attitudes toward donating}
#'    \item{justify}{Negative justifications}
#'    \item{skeptic}{Climate change skepticism}
#' }
#' @source Chapman, D. A., & Little, B. (2016). Climate change and disasters: How framing affects justifications for giving or withholding aid to disaster victims. Social Psychological and Personality Science, 7, 13-20.
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"disaster"


#' CASKETS dataset
#'
#' @format A data.frame with 541 obs. of  7 variables
#' \describe{
#' \item{policy}{Given information about policy (0 = No information, 1 = Told About Policy)}
#' \item{interest}{Interest in viewing casket images}
#' \item{age}{Participant age}
#' \item{educ}{Participant education level, 1 = lesss than high school, 2 = high school, 3 = some college, 4 = associates or technical school, 5 = bachelor degree, 6 = some graduate school, 7 = graduate degree }
#' \item{male}{Participant sex (0 = female, 1 = male)}
#' \item{conserv}{Participant social conservatism}
#' \item{kerry}{Kerry or Bush supporter, 0 = bush supporter, 1 = kerry supporter}
#' }
#' @source Hayes, A. F., & Reineke, J. B. (2007). The effects of government censorship of war-related news coverage on interest in the censored coverage: A test of competing theories. Mass Communication and Society, 10, 423-438
#' @source \url{http://www.afhayes.com/introduction-to-mediation-moderation-and-conditional-process-analysis.html}
"caskets"

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".","pmacro","parrows"))
