#' cleanup function cleans up CCLE and HapMap database and save them as data frames.
#' CCLE databasereference: http://www.broadinstitute.org/ccle/data/browseSamples?actionMethod=pages%2Fhome.xhtml%3AbrowseSamplesBean.checkSkipFirstStep%28%29&conversationPropagation=begin
#' HapMap database reference: ftp://ftp.ncbi.nlm.nih.gov/hapmap/phase_3/relationships_w_pops_051208.txt
#'
#' @param: no param 
#'
#' @return 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples cleanup()
#' R code here showing how your function works
#' 
#' 
library('stringr')
library('magrittr')
cleanup <- function(){
  
      # clean up CCLE data and save as a data frame
      CCLE <- read.csv("~/summer 2015/CCLE.csv", stringsAsFactors = FALSE)
      save(CCLE,file="CCLE.Rda")
      ###################################################################################
      # clean up geuvadis names in cluster and save as a data frame
      geuvadis <- read.table("~/summer 2015/geuvadis.csv", quote="\"",stringsAsFactors = FALSE)
      colnames(geuvadis) <- "name"
      
      cell_line <- data.frame(str_split(geuvadis$name, "_") %>% lapply('[[',1) %>% unlist() %>% str_replace("./", ""))
      sex <- data.frame(str_split(geuvadis$name, "_") %>% lapply('[[',2) %>% unlist() %>% str_replace("female", "F") %>% str_replace("male", "M"))
      
      
      geuvadis <- cbind(cell_line,sex)
      colnames(geuvadis) <- c("cell_line","sex")
      
      save(geuvadis,file="geuvadis.Rda")
      #View(geuvadis)
      #####################################################################################
      #clean up hap map data:
      hapmap <- read.table("ftp://ftp.ncbi.nlm.nih.gov/hapmap/phase_3/relationships_w_pops_051208.txt", stringsAsFactors = FALSE, header=TRUE)
      
      hapmap$sex <- str_replace(hapmap$sex,"1","M") %>% str_replace("2", "F")
      hapmap <- data.frame(cell_line = hapmap$IID,sex = hapmap$sex) #,population = hapmap$population)
      save(hapmap,file="hapmap.Rda")
      
      #View(hapmap)
}

