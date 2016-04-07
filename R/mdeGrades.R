#' @title MDE Grade Levels
#' @description Function to build a Map for MDE grade levels
#' @export
#' @import magrittr
#' @importFrom dplyr as_data_frame bind_cols
#' @return A tbl_df object containing the key value pairs of numeric keys
#' codes and character values
#'

mdeGrades <- function() {

	# Creates numeric keys used to represent student groups
	Keys <- cbind(as.integer(c(0:12))) %>%
			as.data.frame()

	# Creates string labels used to provide semantic meaning to the numeric codes
	Values <- 	cbind(c("Kindergarten", "1st Grade", "2nd Grade", "3rd Grade",
					    "4th Grade", "5th Grade", "6th Grade", "7th Grade",
					    "8th Grade", "9th Grade", "10th Grade", "11th Grade",
					    "12th Grade")) %>%
				as.data.frame(stringsAsFactors = FALSE)

	# Creates a data frame that serves like a Map<Integer, String> object in Java
	mapData <- dplyr::bind_cols(Keys, Values) %>%
		dplyr::as_data_frame()

	# Sets the names of the variables in the data frame
	names(mapData) <- c("key", "value")

	# Returns the data frame object
	return(mapData)

} # End of function to construct Map of codes
