#' @title getMeta
#' @description Gets metadata from MDE file specification
#' @importFrom xlsx read.xlsx
#' @import magrittr
#' @import regexPipes
#' @export getMeta
#' @examples
#'
#' # Name of file containing the file specification
#' filepath <- paste0("S:/ACCESS for ELLs/2012/", "2012 Final Results/",
#' 					  "Final2012UniversalFileFormat_Nov26_2012.xlsx")
#'
#' # Will assume rows 7 - 245 and columns A - F if not specified
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

	fileMeta <- xlsx::read.xlsx(filespec, sheetName = sheet, rowIndex = rowIdx,
							colIndex = colIdx) %>%
				as.data.frame(stringsAsFactors = FALSE)

	varnames <- as.character(fileMeta$fieldname)
	%>%
	varnames %<>% regexPipes::gsub(" ", "") %>%
				regexPipes::gsub("documentcharacteristic", "docchar") %>%
				regexPipes::gsub("accommodation", "accom") %>%
				regexPipes::gsub("score", "sc") %>%
				regexPipes::gsub("maximum", "max") %>%
				regexPipes::gsub("minimum", "min") %>%
				regexPipes::gsub("label", "lab")

	varnames %<>%	regexPipes::gsub("foreignexchangestudent", "fxchng") %>%
				regexPipes::gsub("significant", "sig") %>%
				regexPipes::gsub("newtocountry", "ntc") %>%
				regexPipes::gsub("mtasoperationalpassage", "mtasopp") %>%
				regexPipes::gsub("prior2yearlepflag", "lepmon") %>%
				regexPipes::gsub("prior2yearspeflag", "swdmon") %>%
				regexPipes::gsub("indicator", "") %>%
				regexPipes::gsub("\\(.*\\)", "")

	varLabels <- as.list(fileMeta$description)

}
