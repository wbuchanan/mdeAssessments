#' @title MDE Ethnoracial Identity Codes
#' @description Function to build a data.frame to use for looking up MDE
#' @param used Use the US Department of Education definitions (e.g., 7 categories)
#' @import magrittr
#' @importFrom dplyr as_data_frame bind_cols
#' @return A tbl_df object containing the key value pairs of numeric keys
#' codes and character values
#'

mde <- function(used = FALSE) {

	# If user requests US Dept of Ed key value pairs
	if (used) {

		# Defines the vector containing the keys
		keys <- as.integer(c(1:7, NA))

		# Defines the vector containing the values
		labels <- c("American Indian or Alaskan Native", "Asian",
					"Hispanic/Latino", "Black or African American", "White",
					"Native Hawaiian or Other Pacific Islander",
					"Two or More Races", "Unknown Race/Ethnic Code")

	# Otherwise use locally defined values
	} else {

		# Defines the vector containing the keys
		keys <- as.integer(c(1:5, NA))

		# Defines the vector containing the values
		labels <- c("American Indian", "Asian/Pacific Islander", "Hispanic",
					"Black", "White", "Unknown Race/Ethnic Code")

	} # End ELSE Block for ethnoracial key/value definitions

	# Creates numeric keys used to represent student groups
	Keys <- cbind(keys) %>% as.data.frame()

	# Creates string labels used to provide semantic meaning to the numeric codes
	Values <- cbind(labels) %>% as.data.frame(stringsAsFactors = FALSE)

	# Creates a data frame that serves like a Map<Integer, String> object in Java
	mapData <- dplyr::bind_cols(Keys, Values) %>%
		dplyr::as_data_frame()

	# Sets the names of the variables in the data frame
	names(mapData) <- c("key", "value")

	# Returns the data frame object
	return(Map$new(mapData))

} # End of function to construct Map of codes
