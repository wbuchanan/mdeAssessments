#' @title MDE Student Sex Encoding
#' @description Function to build a data.frame to use for looking up MDE
#' Assessment student sex
#' @import magrittr
#' @importFrom dplyr as_data_frame bind_cols
#' @return A tbl_df object containing the key value pairs of numeric keys
#' codes and character values
#'

mdeSex <- function() {

	# Creates numeric keys used to represent males/females
	sKeys <- 	cbind(as.integer(c(0, 1))) %>%
				as.data.frame()

	# Creates string labels used to provide semantic meaning to the numeric codes
	sValues <- cbind(c("Male", "Female")) %>%
				 as.data.frame(stringsAsFactors = FALSE)

	# Creates a data frame that serves like a Map<Integer, String> object in Java
	mdeStudentSex <- dplyr::bind_cols(sKeys, sValues) %>%
		dplyr::as_data_frame()

	# Sets the names of the variables in the data frame
	names(mdeStudentSex) <- c("key", "value")

	# Returns the data frame object
	return(Map$new(mapData))

} # End of function to construct Map of codes
