#' @title getMeta
#' @description Gets metadata from MDE file specification
#' @importFrom xlsx read.xlsx
#' @import magrittr
#' @import regexPipes
#' @export getMeta
#' @param filespec File path to the MS Excel file containing the file specification
#' @param sheet The sheet to read from the file
#' @param rowIdx A vector of row indices to import
#' @param colIdx A vector of column indices to import
#' @examples \donttest{
#' #' # Name of file containing the file specification
#' filespec <- paste0("S:/ACCESS for ELLs/2012/", "2012 Final Results/",
#' 					  "Final2012UniversalFileFormat_Nov26_2012.xlsx")
#'
#' # Will assume rows 7 - 245 and columns A - F if not specified
#' metaData <- getMeta(filespec)
#' }
#'

getMeta <- function(filespec, sheet = NULL, rowIdx = NULL, colIdx = NULL) {

	# Check for user specified value for the worksheet to use
	if(is.null(sheet) || is.na(sheet) || is.missing(sheet)) {

		# If not specified
		sheet <- "District Student Results Format"

	} # End IF Block for worksheet name

	# Check whether row indices are null/missing
	if(is.null(rowIdx) || is.na(rowIdx) || is.missing(rowIdx)) rowIdx <- c(7:245)

	# Check whether column indices are null/missing
	if(is.null(colIdx) || is.na(colIdx) || is.missing(colIdx)) colIdx <- c(1:6)

	# Reads the file specification into memory for the universal file format
	fileMeta <- xlsx::read.xlsx(filespec, sheetName = sheet, rowIndex = rowIdx,
							colIndex = colIdx, stringsAsFactors = FALSE)

	# Cleans and normalizes variable name stems
	# accom = ACCOMmodation
	# docchar = DOCument CHARacteristics
	# lab = LABel
	# pts = PoinTS
	# sch = SCHool
	# dist = DISTrict
	# num = NUMber
	# pyr = Prior YeaR
	# std = STuDent
	# lb = Lower Bound (range)
	# ub = Upper Bound (range)
	# lex = LEXile
	# lang = LANGuage
	varnames <- fileMeta$fieldname %>% tolower() %>%
	 			regexPipes::gsub(" ", "") %>% regexPipes::gsub("documentcharacteristic", "docchar") %>%
				regexPipes::gsub("accommodation", "accom") %>%
				regexPipes::gsub("score", "sc") %>% regexPipes::gsub("maximum", "max") %>%
				regexPipes::gsub("minimum", "min") %>% regexPipes::gsub("label", "lab") %>%
				regexPipes::gsub("foreignexchangestudent", "fxchng") %>%
				regexPipes::gsub("significant", "sig") %>% regexPipes::gsub("newtocountry", "ntc") %>%
				regexPipes::gsub("mtasoperationalpassage", "mtasopp") %>%
				regexPipes::gsub("prior2yearlepflag", "lepmon") %>% regexPipes::gsub("prior2yearspeflag", "swdmon") %>%
				regexPipes::gsub("indicator", "") %>% regexPipes::gsub("\\(.*\\)", "") %>%
				regexPipes::gsub("[[:punct:]]", "", perl = TRUE) %>% regexPipes::gsub("characteristic", "char") %>%
				regexPipes::gsub("testing", "") %>% regexPipes::gsub("machine", "mech") %>%
				regexPipes::gsub("number", "num") %>% regexPipes::gsub("district", "dist") %>%
				regexPipes::gsub("school", "sch") %>% regexPipes::gsub("language", "lang") %>%
				regexPipes::gsub("localstudent", "std") %>% regexPipes::gsub("ethniccode", "race") %>%
				regexPipes::gsub("name", "nm") %>% regexPipes::gsub("points", "pts") %>%
				regexPipes::gsub("secondary", "sec") %>% regexPipes::gsub("studentfirst", "first") %>%
				regexPipes::gsub("studentlast", "last") %>% regexPipes::gsub("studentmiddle", "mid") %>%
				regexPipes::gsub("prioryear", "pyr") %>% regexPipes::gsub("passage", "") %>%
				regexPipes::gsub("baseitem", "") %>% regexPipes::gsub("achievement", "prof") %>%
				regexPipes::gsub("lowerrange", "lb") %>% regexPipes::gsub("dateofbirth", "dob") %>%
				regexPipes::gsub("upperrange", "ub") %>% regexPipes::gsub("lexile", "lex") %>%
				regexPipes::gsub("level", "lev") %>% regexPipes::gsub("marss.*", "sasid") %>%
				regexPipes::gsub("adult.*", "abe") %>% regexPipes::gsub("tested", "") %>%
				regexPipes::gsub("((oct1[a-z]+)(flag))", "\\2", fixed = FALSE)

	# Meta data that can be attached as analogous to variable labels in other
	# software platforms
	varLabels <- as.list(fileMeta$description)

	# Names the list elements with the corresponding variable names
	names(varLabels) <- varnames

	# Splits the notes field into elements that can be used to construct
	# validation rules
	notes <- as.list(fileMeta$note) %>%
			plyr::llply(.fun = function(x) {
					tmp <- regexPipes::gsub(x, "Y=Yes ", "Y/") %>%
					regexPipes::gsub("N=No ", "N/") %>%
					regexPipes::gsub("(((or )|())[bB]lank.*$)", "blank") %>%
					regexPipes::gsub("blank", "NA")
			}) %>%
			plyr::llply(strsplit, "([/\n,])")

	# Applies variable names to list elements
	names(notes) <- varnames

	# Constructs a list with the meta data objects contained in it
	returnedValues <- list("fileSpecification" = fileMeta,
						   "variableNames" = varnames,
						   "variableLabels" = varLabels,
						   "validationRules" = notes)

	# Returns the list of metadata objects to the caller
	return(returnedValues)

} # End of Function declaration
