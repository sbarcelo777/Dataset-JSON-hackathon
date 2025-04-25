ADAM001 <- function(entry, adsl_content) {
  
if (is.null(adsl_content)) {
    msg <- paste(
      "Dataset not found: ADSL"
    )
    return(list(pass = FALSE, message = msg))
}
  
content <- as.data.frame(process_json_file(entry))

# Check 2 – Variable copied from ADSL has same value for same USUBJID
common_vars <- intersect(names(content), names(adsl_content))
common_vars <- setdiff(common_vars, "USUBJID")  # exclude USUBJID itself

mismatches <- list()

if ("USUBJID" %in% names(content) && "USUBJID" %in% names(adsl_content)) {
  merged <- merge(content, adsl_content, by = "USUBJID", suffixes = c("_cur", "_adsl"))
  
  for (v in common_vars) {
    v_cur <- paste0(v, "_cur")
    v_adsl <- paste0(v, "_adsl")
    mismatch_idx <- which(!is.na(merged[[v_cur]]) & !is.na(merged[[v_adsl]]) & merged[[v_cur]] != merged[[v_adsl]])
    if (length(mismatch_idx) > 0) {
      mismatches[[v]] <- merged$USUBJID[mismatch_idx]
    }
  }
}

if (length(mismatches) > 0) {
  msg <- paste(
    "Variable(s) copied from ADSL with inconsistent values:",
    paste(
      sapply(names(mismatches), function(v) {
        paste0(v, ": ", paste(unique(mismatches[[v]]), collapse = ", "))
      }),
      collapse = "; "
    )
  )
  return(list(pass = FALSE, message = msg))
}

list(pass = TRUE, message = "")
}

# Either --SEQ variable or SRCDOM/SRCVAR/SRCSEQ should be present
# All datasets except ADSL
ADAM004 <- function(entry){
  content <- entry$content
  
  colnames <- tryCatch(
    vapply(content$columns, function(x) x$name, character(1)),
    error = function(e) stop("Error extracting column names: ", e$message)
  )
  if (!any(grepl("SEQ|VAR|DOM", colnames, ignore.case = TRUE))) {
    return(list(
      pass = FALSE,
      message = paste0("--SEQ variable not found, neither SRCDOM/SRCVAR/SRCSEQ")
    ))
  }
  list(pass = TRUE, message = "")

}

# One among RACE, SEX, AGE is not present
ADAM006 <- function(entry) {
  content <- entry$content
  colnames <- tryCatch(
    vapply(content$columns, function(x) x$name, character(1)),
    error = function(e) return(character(0))
  )
  required <- c("RACE", "SEX", "AGE")
  missing <- setdiff(required, colnames)
  if (length(missing) > 0) {
    return(list(pass = FALSE, message = paste("Missing columns:", paste(missing, collapse = ", "))))
  }
  list(pass = TRUE, message = "")
}



# ADaM dataset name should start with AD prefix. 
# AX might be allowed but not strictly required e.g., 
# Non-ADaM datasets can be identified with Class=Null in define.xml. 
# preADSL is also not a true name, rather a concept so dataset can be still named as ADxx
ADAM014 <- function(entry) {
  content <- entry$content
  name <- content$name
  prefix <- toupper(substr(name, 1, 2))
  
  if (prefix != "AD") {
    return(list(
      pass = FALSE,
      message = paste0("Dataset name '", name, "' does not start with 'AD'")
    ))
  }
  
  list(pass = TRUE, message = "")
}


# ADaM label should contain wording "Analysis Dataset"
ADAM015 <- function(entry) {
  content <- entry$content
  label <- content$label
  
  if (!any(grepl("Analysis Dataset", label, ignore.case = TRUE))) {
    return(list(
      pass = FALSE,
      message = paste0("Label '", label, "' does not contain 'Analysis Dataset'")
    ))
  }
  
  list(pass = TRUE, message = "")
}




# TS.SSTDTC (Trial Start Datae) is not same as first signed IC date in DM
SDTM002 <- function(entry, ts_content){
  if (is.null(ts_content)) {
    msg <- paste(
      "Dataset not found: TS"
    )
    return(list(pass = FALSE, message = msg))
  }
  
  content <- as.data.frame(process_json_file(entry))
  first_ic <- min(content$RFICDTC)
  sstdtc <- ts_content[ts_content$TSPARMCD %in% "SSTDTC","TSVAL"]
  # sstdtc <- "2012-11-07"
  
  if(first_ic != sstdtc){
    return(list(
      pass = FALSE,
      message = paste0("TS.SSTDTC : ", sstdtc, " != than first DM.RFICFTC : ", first_ic)
    ))
  }
  
  
  list(pass = TRUE, message = "")
}



