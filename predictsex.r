#' predictsex function allows query on cell line and return corresponding sex information. 
#' 
#' predictsex function reads a dictionary which contains run_accession, sex, and cell_line. 
#' It returns: F -> female; M -> male; NA-> unknown 
#'
#' @param alist: a list of cell lines users want to query on
#'
#' @return ans: a list of sex corresponds to cell line inputs
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples predictsex(c("143B","MONOMAC6","aaa","bbb"))
#' R code here showing how your function works
library(stringr)
predictsex <- function(alist) {
   x <- lapply(alist, function(cell_line) {
  
                    sexdata <- sextable()
                    cell_line <- str_trim(cell_line)
                    if (cell_line %in% sexdata$cell_line)
                    {
                      
                      subset <- sexdata[sexdata$cell_line == cell_line,]
                      result <- unique(subset$sex)
                    }
                    
                    else
                    {
                      result <- NA
                    }
                    
                    return(as.vector(result))
                    }
                 )
   
   ans <- unlist(x)
   return(ans)
}



