#' @title MDE Disability Codes
#' @description Function to build a data.frame to use for looking up MDE
#' Assessment disability codes
#' @import magrittr
#' @importFrom dplyr as_data_frame bind_cols
#' @return A tbl_df object containing the key value pairs of numeric disability
#' codes and their respective meanings
#'

mdeSwdCodes <- function() {

	# Creates numeric keys used to encode MDE disability classifications
	swdcodeKeys <- cbind(as.integer(c(0:12, 14, 16, 54))) %>%
				   as.data.frame()

	# Creates string labels used to provide semantic meaning to the numeric codes
	swdcodeVals <- cbind(c("No IEP/IFSP/IIIP, non-disabled student",
					   "Speech/Language Impairments",
					   "Developmental Cognitive Disabilities: Mild-Moderate",
					   "Developmental Cognitive Disabilities: Moderate-Severe",
					   "Physically Impaired",
					   "Deaf – Hard of Hearing",
					   "Visually Impaired",
					   "Specific Learning Disabilities",
					   "Emotional/Behavioral Disorders",
					   "Deaf - Blind",
					   "Other Health Disabilities",
					   "Autism Spectrum Disorder",
					   "Developmental Delay",
					   "Traumatic Brain Injury",
					   "Severely Multiply Impaired",
					   "504 Accommodation Plan")) %>%
					as.data.frame(stringsAsFactors = FALSE)

	# Creates a data frame that serves like a Map<Integer, String> object in Java
	swdcodes <- dplyr::bind_cols(swdcodeKeys, swdcodeVals) %>%
				dplyr::as_data_frame()

	# Sets the names of the variables in the data frame
	names(swdcodes) <- c("key", "value")

	# Returns the data frame object
	return(Map$new(mapData))

} # End of function that builds up look up data for MDE defined disability codes

