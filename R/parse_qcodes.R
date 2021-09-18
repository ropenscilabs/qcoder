#' Parse coded text
#'
#' Take a data frame of coded text documents and return a data frame of the
#' codes captured within.
#'
#' This function takes a text document containing coded text of the form:
#' \preformatted{"stuff to ignore (QCODE) coded text we care about (/QCODE){#my_code}
#' more stuff to ignore"} and turns it into a data frame with one row per coded
#' item, of the form: \code{docid,qcode,text}
#'
#' \code{parse_qcodes} assumes that it is being passed a data frame, the
#' \code{\link{parse_one_document}} function is called to do the heavy lifting
#' extracting the coded text from the \code{document_text} column.
#' 
#' Newline characters are replaced with an HTML \code{<br>} in the captured text.
#' 
#' If no valid qcodes are found, \code{parse_qcodes} returns an empty data frame
#' (no rows).
#'
#' @param x A data frame containing the text to be coded; requires columns
#'          "doc_id" and "document_text"
#' @param ...  Other parameters optionally passed in
#' @return If the data frame contains coded text in the \code{document_text}
#'         column, output will be a data frame with three columns: "doc",
#'         "qcode", and "text".
#'
#'         The "doc" column is the corresponding "doc_id" value from the input
#'         data frame.
#'
#'         "qcode" is the code that the captured text was marked up with.
#'
#'         "text" is the text that was captured.
#' @examples
#' parse_qcodes(my_documents)
#'
#' # Data frames can be piped into this function
#' my_documents %>%
#'   parse_qcodes()
#' @export

parse_qcodes <- function(x, ...){
  dots <- list(...)

  #replace all newlines in the document texts
  x$document_text <- stringr::str_replace_all(x$document_text, "[\r\n]", "<br>")

  #initialise the empty data frame to fill & return
  df <- data.frame(doc = integer(), qcode = factor(),
                   text = character(), stringsAsFactors = FALSE)

  # Parse the documents
  ldf <- lapply(x$document_text, parse_one_document, df, x, dots)
  df  <- do.call(rbind, lapply(ldf, data.frame, stringsAsFactors=FALSE))
  row.names(df) <- NULL

  df
}

#' Check for coding errors
#'
#' Checks the current document for coding errors.
#'
#' This function takes a string (such as the contents of a document), and conducts some basic linting. It returns a warning if there aren't a matching number of \code{(QCODE)} tags, or if text has been marked to be captured but the capture is missing a tag (missing \code{{#my_tag}}).
#' 
#' @param document A string to be scanned for errors.
#'
#' @return A \code{warning} message as a character string.
#'
#' @examples
#' error_check("An (QCODE)unmatched set of (QCODE) gives (/QCODE){#tag} a warning.")
#' error_check("A (QCODE) qcode with a missing tag gives a warning.")
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
#' @param codes_df_path The path where the updated code data frame should be saved
#'
#'
#' @export
add_discovered_code <- function(codes_list = "", code_data_frame = NULL , codes_df_path = "" ){
  code_data_frame <- as.data.frame(code_data_frame)
  old_codes <- as.character(code_data_frame[,"code"])
  new_codes <- unique(codes_list)
  code <- setdiff(new_codes, old_codes)
  if (length(code) > 0){
    code_id <- integer(length(code))
    code.description <- character(length(code))
    new_rows <- data.frame(code_id, code, code.description)

    code_data_frame <- rbind(code_data_frame, new_rows)
    row_n <- row.names(code_data_frame)
    code_data_frame$code_id[length(old_codes):
                              (length(old_codes) + length(code))] <-
      row_n[length(old_codes):(length(old_codes) + length(code))]

    saveRDS(code_data_frame, file = codes_df_path )
  }
}

#' Extract codes from text
#' Take coded text and extract the codes, assuming they are correctly formatted.
#' @param doc_text  The text data for a single document
#' @examples
#' unlink("./my_qcoder_project", recursive=TRUE)
#' @export
get_codes <- function(doc_text){
  codes <- stringr::str_extract_all(pattern = "\\{#.*?\\}", doc_text)
  codes <- unlist(stringi::stri_split_boundaries(codes[[1]], type = "word"))
  codes <- setdiff(codes, c("{", "}", "#", "", ",", " "))
  codes
}
#' Parse one document
#' @param doc A single document from qcoder_data
#' @param df The data frame that will contain the parsed data
#' @param qcoder_documents The full documents data frame
#' @param dots Other parameters that may be passed in.
#' @examples
#' unlink("./my_qcoder_project", recursive=TRUE)
#' @export
#  change df to something more meaningful
parse_one_document <- function(doc, df, qcoder_documents, dots = NULL){

  doc_id <- qcoder_documents[doc == qcoder_documents$document_text, "doc_id"]
  #cat(paste("parsing document: ", doc_id, "\n"))

  #split the file on opening qcode tags
  #note: can't just use str_extract_all() with a nice clean regex, because qcodes can be nested
  splititems <- gsub("^$"," ",
                     unlist( strsplit( doc, "(\\(QCODE\\))") )
  )

  ### skip this document/row if no qcodes were found
  if( length(splititems) == 1 ){
    # come up with a better solution for big dfs
    #message(paste0("Message: No QCODE blocks found in document: ", doc_id, "\n"))
    return(df)
  }

  error_check(doc)

  ### iterate through the split items
  extra_depth <- 0

  # convert to parse_splitem
  ldf <- lapply(splititems, parse_splititem, df, doc_id, dots)
  df  <- do.call(rbind, lapply(ldf, data.frame, stringsAsFactors=FALSE))
  row.names(df) <- NULL

    df
  }

#' Parse a single item within a document
#' @param splititem  String usually generated by parse_one_document()
#' @param df Data frame for storing parsed data
#' @param doc_id The doc_id for the document this string is part of.
#' @param dots List of additional options passed in.

parse_splititem <- function(splititem, df, doc_id, dots){
  # This is needed to handle directly-nested blocks (with
  # no space/text between them).
  extra_depth <- 0
  if (splititem == ""){
    extra_depth = extra_depth + 1
  }

  # if we've found a qcode, process it
  if( stringr::str_detect(splititem, "\\(/QCODE\\)\\{")){

    #split this entry on qcode close tags
    sp <- unlist( strsplit( splititem, "\\(/QCODE\\)\\{")  )


    # Iterate through the codes in found in this block
    for(level in length(sp):2){

      txt <- "" #will hold the entire text block to return for each qcode

      # join up all the text fragments of this block for this level
      #First add the fragments in the outer splititems[] list, from before we
      #saw the '(/QCODE){' tag.
      # The i is from before refactoring.
      i <- 1
      if(level > 2){
        for(j in (i-(level-2) - extra_depth):(i-1) ){
           txt <- paste(txt, sp[j], sep="")
        }
      } else if( level == 1 ) {
        #reset the extra_depth flag
        extra_depth <- 0
      }
      # add back the loop?
      #then add the fragments in this inner sp[] list
      for(k in 1:level-1){
        # Remove any qcode bits if there
        toadd <- stringr::str_match( sp[k], "^#.*\\}(.*)$" )[2]
        # Otherwise just add the full text
        if( is.na(toadd) ){ toadd <- sp[k] }
        txt = paste(txt, toadd, sep="")
      }

      ### Clean up the text block & extract its codes

      # Remove nested tags from the text block to return
      txt = stringr::str_replace_all(txt,"\\(\\/QCODE\\)\\{#.*?\\}","")

      #get the qcode(s) for this text block
      #the code block will be at the start
      codes <- unlist( stringr::str_extract( sp[level], "^.*?\\}" ) )
      #split on the "#"
      codes <- unlist( strsplit(codes,"#") )

      #warn on qcode parsing error & remove blank first item is relevent
      if( is.na(codes[1]) ){
        warning(sep = "",
                "WARNING: encoding error detected in document ", doc_id,";
                erroneous output is likely. Error was detected at:\n\t'",
                sp[level],
                "'\n")
        codes <- c(NA, NA)
      } else if(codes[1] == ""){
        # Remove blank first vector item
        codes <- codes[2:length(codes)]
      }

      # Add the codes & matching text block to the df
      codes <- sapply(codes, function(x) stringr::str_replace(trimws(x),
                                                              ",$|\\}$",""))
      #clean up code ends
      for(code in codes){
        rowtoadd <- data.frame(doc = doc_id, qcode = as.factor(code), text = txt)
        df <- rbind(df,rowtoadd)
      }

      # Inefficient now because of the loop but eventually this should run on save (a single text).
      if (length("dots") > 0 & !is.null(dots$code_data_frame) & !is.null(dots$save_path) ) {
        qcoder::add_discovered_code(codes, dots$code_data_frame, dots$save_path)

      }
    }

  }
df

}
