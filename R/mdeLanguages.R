#' @title MDE Language Codes
#' @description Function to builds a data.frame to use for looking up MDE
#' Assessment language codes
#' @import magrittr
#' @importFrom dplyr as_data_frame bind_cols
#' @return A tbl_df object containing the key value pairs of numeric language
#' codes and their respective labels
#'

mdeLanguages <- function() {

	# The numeric codes defined by MDE to represent languages
	langKeys <- cbind(as.integer(c(1:29, 31:49, 51, 52, 54:416))) %>%
		 as.data.frame()

	# The languages defined in the assessment documentation from MDE
	langVals <- cbind(c("Afrikaans", "Arabic", "Armenian", "Czech",
	"Khmer (aka Cambodian)", "Cheyenne", "Chinese, Mandarin", "Dakota",
	"Danish", "Bengali", "English", "Estonian", "Farsi", "Finnish", "French",
	"Gaelic", "German", "Hebrew", "Hindi", "Hmong", "Hungarian", "Icelandic",
	"Italian", "Japanese", "Mongolian", "Korean", "Kurdish",
	"Laotian (aka Lao)", "Latvian", "Lithuanian",
	"Bahasa Indonesia (aka Indonesian)", "Dutch", "Norwegian",
	"Anishinaabemowin (aka Ojibwa, Chippewa)", "Cebuano", "Polish",
	"Portuguese", "Romanian", "Russian", "Bosnian",
	"ASL (aka American Sign Language)", "Slovak", "Slovene", "Spanish",
	"Swedish", "Thai", "Turkish", "Ukrainian", "Vietnamese", "Not Specific",
	"Greek", "Burmese", "Samoan", "English, Creolized", "Sinhala", "Yiddish",
	"Pashto", "Iu Mien", "Bhutanese (aka Dzongkha)", "Amharic", "Bulgarian",
	"Igbo", "Fulah", "Lingala", "Nepali", "Somali" , "Swahili (aka Kiswahili)",
	"Tigrinya", "Albanian", "Tibetan", "Afan Oromo (aka Oromo, Oromiffa)",
	"Yoruba", "Nuer", "Grebo", "Kabyle", "Arawak", "Quechua" , "Fon",
	"Georgian", "Adangme", "Chamor", "Haitian Creole" , "Balinese", "Kazakh",
	"Kamba", "Macedonian", "Malagasy" , "Bambara", "Maltese", "Rwanda",
	"Wolof", "Ganda (aka Luganda)" , "Uzbek", "Luba", "Bemba", "Ndebele",
	"Maay", "Bantu" , "Belarusan", "Anuak", "Tajik", "Cham, Eastern" ,
	"Cham, Western", "Ho-Chunk (aka Winnebago)", "Cantonese" , "Taiwanese",
	"Lakota", "Brahui", "Gujarati", "Kannada", "Konkani", "Malayalam",
	"Marathi", "Punjabi", "Tamil" , "Telugu", "Dari", "Hindustani", "Urdu",
	"Okinawan" , "Malay", "Ilocano", "Tagalog (aka Pilipino)", "Croatian",
	"Serbian", "Karen", "Shan", "Hawaiian", "Marshallese" , "Palauan",
	"Tongan", "Turkman", "Yao", "Harari" , "Sidamo", "Edo", "Efik", "Goemai",
	"Gokana", "Kanuri" , "Hausa", "Tigre", "Bassa", "Dan",
	"Pelle (aka Kpelle)", "Krio", "Mende", "Aymara", "Garifuna", "Guarani",
	"Nahuatl", "Ossetian", "Akan", "Dagbani", "Ewe", "Fanti" , "Ga", "Twi",
	"Javanese", "Sundanese", "Luo", "Sotho" , "Zulu", "Nyanga", "Tonga",
	"Shona", "Quichua" , "Abkhazian", "Aceh", "Acholi", "Adyghe", "Afar",
	"Ainu", "Aleut", "Altai", "Anaang", "Angika", "Aragonese", "Assamese",
	"Asturian", "Avaric", "Awadhi", "Azerbaijani", "Bakhtiari",
	"Karachai (aka Balkar)", "Baluchi", "Banda" , "Bashkir", "Basque", "Batak",
	"Bedawijet", "Belle", "Bhojpuri", "Bicolano", "Bilen", "Bislama",
	"Braj Basha", "Breton", "Bugis", "Buriat", "Carib", "Catalan", "Chechen",
	"Cherokee", "Chien", "Choctaw", "Chuukese", "Chuvash", "Corsican", "Cree",
	"Crimean Tartar" , "Cutchi-Swahili", "Dair", "Dargwa", "Dei", "Dilling",
	"Dinka", "Dogri", "Dogrib", "Domari", "Duala", "Ebira" , "Ekajuk", "Erzya",
	"Ewondo", "Fang", "Faroese", "Fijian" , "French, Creolized", "Frisian",
	"Friulian", "Fulfulde" , "Galician", "Gayo", "Gbandi (Bandi)", "Gbaya",
	"Gbii", "Gilaki", "Gio", "Gola", "Gondi", "Gorontalo", "Gubu", "Hawaiian",
	"Herero", "Hiligaynon (aka Illogo)", "Hiri Motu" , "Iban", "Ibibio",
	"Igala", "Ingush", "Irish", "Izon" , "Jingpho (aka Kachin)", "Kabardian",
	"Kadara", "Kalenjin" , "Kalmyk-Oirat", "Karakalpak", "Karelian", "Karko",
	"Kashmiri", "Karenni (aka Kayah)", "Kenuzi-Dongola", "Khasi",
	"Khmu (aka Tenh)", "Kikuyu", "Kimbundu", "Kiribati" , "Kissi (aka Gissi)",
	"Kituba", "Klao", "Kom", "Komi", "Kongo", "Koongo", "Kosraean", "Krahn",
	"Kru", "Kumyk" , "Kurux", "Kwanyama", "Kyrghyz", "Laki", "Lamba",
	"Limburgish", "Loma", "Lunda", "Luri", "Luxembourgeois" , "Maasai",
	"Madura", "Magahi", "Mah (aka Mano)", "Maithili" , "Makasar", "Maldivian",
	"Mandar", "Mandingo", "Manipuri" , "Manobo", "Maori", "Mapudungun", "Mari",
	"Marwari", "Mayan", "Mazandarani (aka Tabari)", "Meitei", "Midob",
	"Minangkabau", "Moksha", "Moldovan", "Mon" , "Mongo (aka Mongo-Nkundu)",
	"Moore", "Nande", "Nauraun" , "Navajo", "Ndonga", "Newari", "Ngbaka",
	"Nias", "Niuean" , "Nobiin", "Nogai", "Nyamwezi", "Nyanja", "Nyankore",
	"Nyoro", "Nzema", "Oriya", "Pahlavani", "Palauan" , "Pampangan",
	"Pangasinan", "Papiamentu", "Phende", "Pohnpeian", "Portuguese, creolized",
	"Qashqa'i", "Rajasthani", "Rapa Nui", "Rarotongan", "Romansh", "Romany",
	"Rundi" , "Saami", "Salkup", "Sango", "Santali", "Sapo" ,
	"Sarakole (aka Soninke)", "Sardinian", "Serer-Sine", "Shi" , "Sindhi",
	"Sogdian", "Songe", "Sranan", "Sukuma", "Susu" , "Swati", "Syriac",
	"Tamashek", "Temne", "Tereno", "Tetela", "Tetun", "Tiv", "Tok Pisin",
	"Tokelauan" , "Tonga", "Tonga Nyasa", "Tsonga", "Tswana", "Tumbuka",
	"Tuvaluan", "Tuvan", "Udmurt", "Umbundu", "Urhobo" , "Uyghur", "Vai",
	"Valencian", "Venda", "Votic" , "Wakashan", "Wali Ghana", "Wali Sudan",
	"Walloon", "Waray", "Welsh", "Wolaytta", "Xhosa", "Yakut", "Yapese",
	"Yombe", "Zande", "Zapotec", "Zaza", "Zhuang", "Zuni" , "Luba-Kasai",
	"Luba-Katanga", "Ngumba", "Basaa" , "Karen, Pwo", "Karen, S'gaw",
	"Chaldean Neo-Aramaic" , "Assyrian Neo-Aramaic", "Lisu", "Kono", "Chin")) %>%
		as.data.frame(stringsAsFactors = FALSE)

	# Creates the data frame object
	mdeLanguage <-  dplyr::bind_cols(langKeys, langVals) %>%
					dplyr::as_data_frame()

	# Adds names to the data frame object
	names(mdeLanguage) <- c("key", "value")

	# Returns the data frame object
	return(Map$new(mapData))

} # End of function that builds up look up data for MDE defined language codes
