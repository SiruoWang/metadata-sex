#' cross check both CCLE and SRA database and create a new table with known gender information . 
#' fields: run_accession, cell_line, sex. 
#' It returns a rex dictionary which contains run_accession, cell_line, and sex.
#' It also updates sra metadata.
#' It uses CCLE database as reference: http://www.broadinstitute.org/ccle/data/browseSamples?actionMethod=pages%2Fhome.xhtml%3AbrowseSamplesBean.checkSkipFirstStep%28%29&conversationPropagation=begin
#' It uses HapMap database as reference: ftp://ftp.ncbi.nlm.nih.gov/hapmap/phase_3/relationships_w_pops_051208.txt
#'
#' @param: no param 
#'
#' @return: a sex dictionary
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples sextable()
#' R code here showing how your function works
#' 
sextable <- function(){

        # meta_cl_sex: subset data frame from metadata with both cell line and gender fields filled
        metasub1<-data.frame(cell_line = metadata$cell_line, run_accession = metadata$run_accession,sex = metadata$sex,row.names=NULL)
        meta_cl_sex<-metasub1[!is.na(metasub1$cell_line) & !is.na(metasub1$sex),]
        
        
        # data1 & data2 come from CCLE database
        data1 <- data.frame(cell_line = CCLE$Cell.line.primary.name, sex = CCLE$Gender)
        data2 <- data.frame(cell_line = CCLE$Cell.line.aliases, sex = CCLE$Gender)
        
        # subsra come from sra database directly
        subsra <- data.frame(cell_line = metadata$cell_line, run_accession = metadata$run_accession, sex = metadata$sex)
        
        
        # merge sra database with CCLE database and geuvadis cluster files by cell name
        step1 <- merge(subsra, data1, by="cell_line", all=FALSE)
        step2 <- merge(subsra, data2, by="cell_line", all=FALSE)
        step3 <- merge(subsra, hapmap, by="cell_line", all=FALSE)
       
        sex_dictionary <- rbind(step1,step2,step3)
        sex_dictionary <- data.frame(run_accession = sex_dictionary$run_accession, cell_line = sex_dictionary$cell_line, sex = sex_dictionary$sex.y)
        
        sextable <- rbind(sex_dictionary,meta_cl_sex)
        sextable <- subset(sextable,subset = !duplicated(sextable$run_accession))
        
        ##################################################################################
    
        meta <- merge(sextable,metadata,by="run_accession",all=TRUE)
       
        sexcol <- data.frame(sex.x = meta$sex.x,sex.y = meta$sex.y)

        #sexcol$sex <-  do.call(pmax, c(sexcol, na.rm=TRUE))
        sexcol$sex <- ifelse(is.na(sexcol$sex.x), as.character(sexcol$sex.y), as.character(sexcol$sex.x))
       
        #View(sexcol[!is.na(sexcol$sex.x)|!is.na(sexcol$sex.y),])
        meta <- cbind(meta,sexcol)
        
        drops <- c("cell_line.x","sex.y","sex.x")
        updatemeta <- meta[,!(names(meta) %in% drops)]

        names(updatemeta)[names(updatemeta) == "cell_line.y"] <- "cell_line"    
        save(updatemeta, file="updatemeta.Rda")

        ##################################################################################

        return(sextable)
  }


