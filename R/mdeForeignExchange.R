#' @title MDE Foreign Exchange Student Indicator
#' @description Function to build a Map object for Foreign Exchange Student status
#' @import magrittr
#' @importFrom dplyr as_data_frame bind_cols
#' @return A tbl_df object containing the key value pairs of numeric keys
#' codes and character values
#'

mdeForeignExchange <- function() {

	# Creates numeric keys used to represent student groups
	Keys <- cbind(as.integer(c(0, 1))) %>% as.data.frame()

	# Creates string labels used to provide semantic meaning to the numeric codes
	Values <- 	cbind(c("Not Foreign Exchange Student",
					    "Is Foreign Exchange Student")) %>%
				as.data.frame(stringsAsFactors = FALSE)

	# Creates a data frame that serves like a Map<Integer, String> object in Java
	mapData <- 	dplyr::bind_cols(Keys, Values) %>%
				dplyr::as_data_frame()

	# Sets the names of the variables in the data frame
	names(mapData) <- c("key", "value")

	# Returns the data frame object
	return(Map$new(mapData))

} # End of function to construct Map of codes
