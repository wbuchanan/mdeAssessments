#' @title map class
#' @description Provides behavior analogous to a Map<> object in Java.  Many of
#' the Java methods are implemented and the constructor allows passing either a
#' list (named or unnamed) or data.frame type object to use as the data for the
#' object
#' @docType class
#' @importFrom R6 R6Class
#' @import magrittr
#' @importFrom dplyr union add_row filter as_data_frame
#' @export
#' @keywords data
#' @return \code{\link{R6Class}} class object imitating behavior of a Java Map<>
#' @format \code{\link{R6Class}} object.
#' @examples
#'
#' # Can either pass a data frame object
#' num <- as.integer(c(1:10)) %>% as.data.frame()
#' samp <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'a', 'b', 'c') %>%
#' 			as.data.frame(stringsAsFactors = FALSE)
#' mydata <- as.data.frame(cbind(num, samp), stringsAsFactors = FALSE)
#'
#' # Create the map object
#' mymap <- Map$new(mydata)
#'
#' # Get the set of keys
#' mymap$keySet()
#'
#' # Get the set of values
#' mymap$entrySet()
#'
#' # Looks up a single value from the Map
#' mymap$get(2)
#'
#' #' @section Methods:
#' \describe {
#' 	\item{\code{get(userKey)}}{Method used to retrieve the value associated with
#' 	a given key}
#'	\item{\code{keySet(returnList)}}{Method used to return a set of the keys;
#'	returnList used to specify whether the method should return a list or a vector}
#'	\item{\code{entrySet(returnList)}}{Method used to return a set of the values;
#'	returnList used to specify whether the method should return a list or a vector}
#'	\item{\code{put(userKey, userValue)}}{Method used to add a single key/value
#'	pair to the map object}
#'	\item{\code{putAll(newValues)}}{Adds a collection of key/value pairs
#'	contained in either a list or data frame object}
#'	\item{\code{remove(userKey)}}{Removed a key/value pair from the object}
#'	\item{\code{size()}}{Gets the number of key value pairs in the object}
#'	\item{\code{containsKey(userKey)}}{Tests whether or not the key passed as an
#'	argument is a member of the map's keys}
#'	\item{\code{containsValue(userValue)}}{Tests whether or not the key passed
#'	as an argument is a member of the map's values}
#' }
#'

Map <- R6::R6Class("Map",

	# Declares public members/methods
	public = list(

		# Class constructor method
		initialize = function(df) {

			# Calls private method used to verify and store the argument passed
			# to the class constructor
			private$setMap(df)

		}, # End of constructor method declaration

		# Method used to look up/return the value of a single element
		get = function(userKey) {

			# Filters the map based on the given key and returns the value
			dplyr::filter(private$map, key == userKey) %>%
			dplyr::select(value)

		}, # End of get method definition

		# Method that returns a set of keys from the map
		keySet = function(returnList = TRUE) {

			# Gets the unique key values and returns them in a list object
			if (returnList) unique(private$map$key) %>% as.list()
			else unique(private$map$key)

		}, # End definition of keySet method

		# Method that returns a set of values
		entrySet = function(returnList = TRUE) {

			# Returns the set as a list object if the parameter is set to true
			if (returnList) unique(private$map$value) %>% as.list()

			# Or returns the vector
			else unique(private$map$value)

		}, # End of entrySet method for the object

		# Method used to add a new key/value pair
		put = function(userKey, userValue) {

			# Adds the new key/value pair to the map
			private$map %<>% dplyr::add_row(key = userKey, value = userValue)

			# Returns self to provide fluent interface
			self

		}, # End of put method for the object

		# Method that adds multiple key/value pairs contained in an object
		putAll = function(newValues) {

			# If the object is a list transform it to a data frame
			if (class(newValues) == "list") newValues %<>% private$makeDF()

			# Add those key value pairs to the existing map data
			private$map %<>% dplyr::union(private$map, newValues)

		}, # End putAll method declaration

		# Removes a key value pair from the map
		remove = function(userKey) {

			# Returns the map with the element defined by the key removed
			private$map %<>% dplyr::filter(key != userKey)

		}, # End remove method declaration

		# Method to return the size of the map object
		size = function() {

			# Gets the number of records in the data frame backing the object
			nrow(private$map)

		}, # End of Size method

		# Method to test whether or
		containsKey = function(userKey) {

			# Tests if FALSE does not appear in the test of the userKey against
			# the keySet
			(!(FALSE %in% (userKey %in% unlist(self$keySet()))))

		}, # End of containsKey method definition

		# Method to test for the presence of a given value
		containsValue = function(userValue) {

			# Tests if FALSE does not appear in the test of the userValue
			# against the entrySet
			(!(FALSE %in% (userValue %in% unlist(self$entrySet()))))

		} # End of containsValue method definition

	), # End of public member/method declarations

	# Declares private members/methods
	private = list(

		# Member where data are stored
		map = NULL,

		# Quick method to check for a valid object type passed to constructor
		valid = function(df) {

			# Returns Boolean indicating whether or not the argument passed to
			# the constructor was valid
			return(class(df)[[1]] %in% c("tbl_df", "tbl", "data.frame", "list"))

		}, # End of validation method

		# Method used to set the object data
		setMap = function(df) {

			# If a valid data frame was passed
			if (private$valid(df) && class(df) != "list") {

				# Apply standard names to the data frame
				names(df) <- c("key", "value")

				# Set the map data
				private$map <- df %>% dplyr::as_data_frame()

			# For list objects
			} else if (class(df) == "list") {

				# Stores the data in the map member
				private$map <- private$makeDF(df)

			# In all other cases
			} else {

				# Error out of the constructor method call
				stop("Can only pass a dataframe or list to the constructor")

			} # End ELSE Block for invalid data passed to constructor

		}, # End of setter method declaration

		# Method used to take a list and make it a data frame
		makeDF = function(df) {

			# If the list is unnamed
			if (is.null(names(df))) {

				# Create keys based on the element indices of the list object
				keys <- cbind(as.integer(c(1:length(df)))) %>%
					as.data.frame()

				# If the list is named
			} else {

				# Used the names for the keys
				keys <- cbind(names(df)) %>%
					as.data.frame(stringsAsFactors = FALSE)

			} # End ELSE Block for named lists

			# Creates the values that will be bound to the keys
			values <- cbind(df) %>% as.data.frame(stringsAsFactors = FALSE)

			# Makes the object a dplyr flavored data frame
			pmp <- 	cbind(keys, values) %>%
				as.data.frame(stringsAsFactors = FALSE) %>%
				dplyr::as_data_frame()

			# Applies standardized names to the data frame
			names(pmp) <- c("key", "value")

			# Returns the data frame from the list object
			return(pmp)

		} # Ends method to generate a dataframe from the list object

	) # End of Private member definitions/declarations

) # End of Class definition
