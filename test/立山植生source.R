#### —§RA¶source.R

######################################
##  ƒNƒƒXƒe[ƒuƒ‹ì¬@”í“x¨”í“xŠK‹‰
######################################



#' Title
#'
#' @param cv
#'
#' @return
#' @export
#'
#' @examples
#'
#' Domin_Krajina(26.5)
#'
Domin_Krajina<-function(cv){
		if(cv==100) return(10)
		if(cv>=75)   return(9)
		if(cv>=50)   return(8)
		if(cv>=33)   return(7)
		if(cv>=20)   return(6)
		if(cv>=10)   return(5)
		if(cv>=5)   return(4)
		if(cv>=3)   return(3)
		if(cv>=2)   return(2)
		if(cv>=1)   return(1)
		if(cv>0)   return(0.5)
		if(cv==0)   return(0)
		}

#' Title
#'
#' @param r
#'
#' @return
#' @export
#'
#' @examples
hindo<-function(r) {## floor(r/0.2-0.00001)+1
	if(r>0.8)   return("‡X")
	if(r>0.6)   return("‡W")
	if(r>0.4)   return("‡V")
	if(r>0.2)   return("‡U")
	if(r>0)   	return("‡T")
	if(r==0)   	return("-")
			}
