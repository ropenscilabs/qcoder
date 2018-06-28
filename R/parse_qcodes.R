#' Parse coded text
#'
#' Take a text document containing coded text of the form:
#' "stuff to ignore (QCODE) coded text we care about (/QCODE){#qcode} more stuff to ignore"
#' and turn it into a dataframe with one row per coded item, of the form:
#' docid,qcode,text
#'
#' Replaces newline characters with "<br>" in the captured text
#' returns an empty dataframe (no rows) if no qcodes were found.
#'
#' @param x A data frame containing the text to be coded; requires columns "doc_id" and "document_text"
#' @param ...  Other parameters optionally passed in
#' @export

parse_qcodes <- function(x, ...){
  dots <- list(...)

  #replace all newlines in the document texts
  x$document_text <- stringr::str_replace_all(x$document_text, "[\r\n]", "<br>")

  #initialise the empty data frame to fill & return
  df <- data.frame(doc = integer(), qcode = factor(),
                   text = character(), stringsAsFactors = FALSE)

  ###iterate through each document submitted
  for (i in 1:nrow(x)) {

    doc_id <- x$doc_id[i]
    #cat(paste("parsing document: ", doc_id, "\n"))

    #split the file on opening qcode tags
    #note: can't just use str_extract_all() with a nice clean regex, because qcodes can be nested
    splititems <- unlist( strsplit( x$document_text[i], "(\\(QCODE\\))") )

    ### skip this document/row if no qcodes were found
    if( length(splititems) == 1 ){
      warning("WARNING: No QCODE blocks found in document ","\n")
      next()
    }

    error_check(x$document_text[i])

    ### iterate through the split items
    extra_depth = 0
    for(i in 1:length(splititems)){

      #this is needed to handle directly-nested blocks (eg with no space/text between them)
      if (splititems[i] == ""){
        extra_depth = extra_depth + 1
      }

      ### if we've found a qcode, process it
      if( stringr::str_detect(splititems[i], "\\(/QCODE\\)\\{")){

        #split this entry on qcode close tags
        sp <- unlist( strsplit( splititems[i], "\\(/QCODE\\)\\{")  )


        ### iterate through the codes in found in this block
        for(level in length(sp):2){

          txt = "" #will hold the entire text block to return for each qcode

          ### join up all the text fragments of this block for this level

          #first add the fragments in the outer splititems[] list, from before we saw the '(/QCODE){' tag
          if(level > 2){
            for(j in (i-(level-2)-extra_depth):(i-1) ){
              txt = paste(txt, splititems[j], sep="")
            }
          } else if( level == 1 ) {
            #reset the extra_depth flag
            extra_depth = 0
          }

          #then add the fragments in this inner sp[] list
          for(k in 1:level-1){
            toadd <- stringr::str_match( sp[k], "^#.*\\}(.*)$" )[2]  #remove any qcode bits if there
            if( is.na(toadd) ){ toadd <- sp[k] }  #otherwise just add the full text
            txt = paste(txt, toadd, sep="")
          }

          ### Clean up the text block & extract its codes

          # Remove nested tags from the text block to return
          txt = stringr::str_replace_all(txt,"\\(\\/QCODE\\)\\{#.*?\\}","")

          #get the qcode(s) for this text block
          #the code block will be @ the start
          codes <- unlist( stringr::str_extract( sp[level], "^.*?\\}" ) )
          #split on the "#"
          codes <- unlist( strsplit(codes,"#") )

          #warn on qcode parsing error & remove blank first item is relevent
          if( is.na(codes[1]) ){
            warning(sep="",
                    "WARNING: encoding error detected in document ",doc_id,";
                    erroneous output is likely. Error was detected at:\n\t'",
                    sp[level],
                    "'\n")
            codes = c(NA,NA)
          }else if(codes[1] == ""){
            codes <- codes[2:length(codes)] #remove blank first vector item
          }

          #add the codes & matching text block to the df
          codes <- sapply(codes, function(x) stringr::str_replace(trimws(x), ",$|\\}$","") )
          #clean up code ends
          for(code in codes){
            rowtoadd <- data.frame(doc = doc_id, qcode = as.factor(code), text = txt)
            df <- rbind(df,rowtoadd)
          }

          # Inefficient now because of the loop but eventually this should run on save (a single text).
          if (length("dots") > 0 && !is.null(dots$code_data_frame) && !is.null(dots$save_path) ) {
            add_discovered_code(codes, dots$code_data_frame, dots$save_path)

          }
        }

      }

    }

  }

  return(df)

}

#' Check for coding errors
#' Checks the current document for coding errors.
#'
#' @param document The document to be scanned for errors.
#'
#' @export
error_check <- function(document) {
    ### basic tag error checking
    #check whether there are an equal number of (QCODE) and (/QCODE) tags
    open  = unlist( stringr::str_extract_all( document,"(\\(QCODE\\))"))
    close = unlist( stringr::str_extract_all( document,"(\\(/QCODE\\))"))
    if(length(open) != length(close)){
      warning("WARNING: number of (QCODE) and (/QCODE) tags do not match
              in document ; erroneous output is likely.\n")
    }
    #check whether there is a (/QCODE) tag missing its {#code}
    close = unlist( stringr::str_extract_all( document,
                                              "(\\(/QCODE\\)[^\\}]*?\\})"))
    for(tag in close){
      if( !stringr::str_detect(tag, "\\(/QCODE\\)\\{#.*?\\}") ){
        warning("WARNING: encoding error detected in document
                erroneous output is likely. Error was detected at:\n\t'",
                tag,"'\n")
      }
    }
}


#' Update codes data frame
#' Add discovered codes to the codes data frame
#'
#' @param codes_list A list of codes (usually from a coded document)
#' @param code_data_frame Existing data frame of QCODE codes
#' @param save_path The path where the updated code data frame should be saved
#'
#' @export
add_discovered_code <- function(codes_list = "", code_data_frame = NULL , save_path = "" ){
    old_codes <- code_data_frame %>% dplyr::pull("code") %>% as.character()
    new_codes <- unique(codes_list)
    code <- setdiff(new_codes, old_codes)
    if (length(code) > 0){
      code_id <- integer(length(code))
      code.description <- character(length(code))
      new_rows <- data.frame(code_id, code, code.description)
      code_data_frame <- rbind(code_data_frame, new_rows)
      row_n <- row.names(code_data_frame)
      code_data_frame$code_id <- ifelse(code_data_frame$code_id == 0, row_n,
                                        code_data_frame$code_id)
      saveRDS(code_data_frame, file = save_path )
    }
}

#' Extract codes from text
#' Take coded text and extract the codes, assuming they are correctly formatted.
#' @param doc_text  The text data for a single document
#' @export

get_codes <- function(doc_text){
  codes <- stringr::str_extract_all(pattern = "\\{#.*?\\}", doc_text)
  codes <- unlist(stringi::stri_split_boundaries(codes[[1]], type = "word"))
  codes <- setdiff(codes, c("{", "}", "#", "", ",", " "))
  codes
}
