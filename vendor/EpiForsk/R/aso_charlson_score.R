#' Charlson Score Constructor
#'
#' Charlson comorbidity score for Danish ICD-10 and ICD-8 data. This is a
#' SAS-macro ASO translated to R in March of 2022
#'
#' @param data A data.frame with at least an id variable and a variable with all
#'   diagnosis codes. The data should be in the long format (only one variable
#'   with diagnoses, but several lines per person is OK).
#' @param Person_ID <[`data-masking`][dplyr::dplyr_data_masking]> An unquoted
#'    expression naming the id variable in `data`. This variable must always be
#'    specified.
#' @param diagnosis_variable <[`data-masking`][dplyr::dplyr_data_masking]> An unquoted
#'    expression naming the diagnosis variable in `data`. This variable must
#'    always be specified.
#' @param time_variable <[`data-masking`][dplyr::dplyr_data_masking]> An unquoted
#'    expression naming the diagnosis time variable in `data` if needed. The
#'    `time_variable` must be in a date format.
#'
#'    When `time_variable` is specified, `end_date` must also be specified.
#' @param end_date <[`data-masking`][dplyr::dplyr_data_masking]> An unquoted
#'    expression naming the end of time-period to search for relevant diagnoses
#'    or a single date specifying the end date. If `end_date` names a variable,
#'    this variable must be in a date format.
#' @param days_before_end_date A numeric specifying the number of days look-back
#'   from `end_date` to search for relevant diagnoses.
#' @param amount_output A character specifying whether all created index
#'   variables should be returned. When `amount_output` is "total" (the default)
#'   only the resulting Charlson scores are returned, otherwise all disease-
#'   specific index variables are returned.
#'
#' @returns
#' If `Person_ID` and `diagnosis_variable` are the only specifications, the
#' function will calculate the different versions of the Charlson score on all
#' data available for each person, regardless of timing etc. This is OK if only
#' relevant records are included.
#'
#' @details
#' The `charlson_score()` function calculates the Charlson Charlson Comorbidity
#' Index for each person. Three different variations on the score has been
#' implemented:
#'
#' \itemize{
#'   \item cc: Article from Quan et al. (Coding Algorithms for Defining
#'   Comorbidities in ICD-9 and ICD-10 Administrative Data, Med Care 2005:43:
#'   1130-1139), the same HTR and others have used - ICD10 only
#'   \item ch: Article from Christensen et al. (Comparison of Charlson
#'   comorbidity index with SAPS and APACHE sources for prediction of mortality
#'   following intensive care, Clinical Epidemiology 2011:3 203-211), include
#'   ICD8 and ICD10 but the included diagnoses are not the same as in Quan
#'   \item cd: Article from Sundarajan et al. (New ICD-10 version of Charlson
#'   Comorbidity Index predicted in-hospital mortality, Journal of clinical
#'   Epidemiology 57 (2004) 1288-1294, include ICD10 = Charlson-Deyo including
#'   cancer
#' }
#'
#' # NOTE
#' The diagnoses to use in this function at the current state should be either
#' ICD-8, but preferably ICD-10. The ICD-10 codes should start with two letters,
#' where the first one is "D". Furthermore, the code should only have letters
#' and digits (i.e. the form "DA000" not "DA00.0")
#'
#' @author
#' ASO & ADLS
#'
#' @examples
#' \donttest{
#'
#' # An example dataset
#'
#' test_data <- data.frame(
#'   IDs = c(
#'     1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
#'     17, 18, 19, 20, 21, 22, 22, 23, 23, 24, 24, 24, 24, 24
#'   ),
#'   Diags = c(
#'     "DZ36", "DZ38", "DZ40", "DZ42", "DC20", "DI252",
#'     "DP290", "DI71", "DH340", "DG30", "DJ40", "DM353",
#'     "DK26", "DK700", "DK711", "DE106", "DE112", "DG82",
#'     "DZ940", "DC80", "DB20", "DK74", "DK704", "DE101",
#'     "DE102", "DB20", "DK74", "DK704", "DE101", "DE102"
#'  ),
#'  time = as.Date(c(
#'    "2001-01-30", "2004-05-20", "2007-01-02", "2013-12-01",
#'    "2017-04-30", "2001-01-30", "2004-05-20", "2007-01-02",
#'    "2013-12-01", "2017-04-30", "2001-01-30", "2004-05-20",
#'    "2007-01-02", "2013-12-01", "2017-04-30", "2001-01-30",
#'    "2004-05-20", "2007-01-02", "2013-12-01", "2017-04-30",
#'    "2001-01-30", "2004-05-20", "2007-01-02", "2013-12-01",
#'    "2017-04-30", "2001-01-30", "2004-05-20", "2007-01-02",
#'    "2013-12-01", "2017-04-30"
#'  )),
#'  match_date = as.Date(c(
#'    "2001-10-15", "2005-10-15", "2011-10-15", "2021-10-15",
#'    "2021-10-15", "2001-10-15", "2005-10-15", "2011-10-15",
#'    "2021-10-15", "2021-10-15", "2001-10-15", "2005-10-15",
#'    "2011-10-15", "2021-10-15", "2021-10-15", "2001-10-15",
#'    "2005-10-15", "2011-10-15", "2021-10-15", "2021-10-15",
#'    "2001-10-15", "2005-10-15", "2011-10-15", "2021-10-15",
#'    "2021-10-15", "2001-10-15", "2005-10-15", "2011-10-15",
#'    "2021-10-15", "2021-10-15"
#'  ))
#' )
#'
#' # Minimal example
#' charlson_score(
#'   data = test_data,
#'   Person_ID = IDs,
#'   diagnosis_variable = Diags
#' )
#'
#' # Minimal example with all index diagnosis variables
#' charlson_score(
#'   data = test_data,
#'   Person_ID = IDs,
#'   diagnosis_variable = Diags,
#'   amount_output = "all"
#' )
#'
#' # Imposing uniform date restrictions to diagnoses
#' charlson_score(
#'   data = test_data,
#'   Person_ID = IDs,
#'   diagnosis_variable = Diags,
#'   time_variable = time,
#'   end_date = as.Date("2012-01-01")
#' )
#'
#' # Imposing differing date restriction to diagnoses
#' charlson_score(
#'   data = test_data,
#'   Person_ID = IDs,
#'   diagnosis_variable = Diags,
#'   time_variable = time,
#'   end_date = match_date
#' )
#'
#' # Imposing both a start and end to the lookup period for
#' # relevant diagnoses
#' charlson_score(
#'   data = test_data,
#'   Person_ID = IDs,
#'   diagnosis_variable = Diags,
#'   time_variable = time,
#'   end_date = match_date,
#'   days_before_end_date = 365.25
#' )
#' }
#'
#' @export

charlson_score <- function(
    data,
    Person_ID,
    diagnosis_variable,
    time_variable = NULL,
    end_date = NULL,
    days_before_end_date = NULL,
    amount_output = "total"
) {
  # ADLS has rewritten these ifelse statements for the func_table1 construction:
  # This has been done for the following reasons:
  #   - Testing if the data masked objects are null is not a good idea, as they
  #     are not generally available in the global or function environment. This
  #     means that when specified is.null(<data masked input>) will always
  #     produce the "object <data masked input>' not found" error
  #   - I have imposed stricter error reporting to increase user friendliness:
  #     It is my opinion that a function should throw an error, rather than
  #     deliberately ignore user input.
  if (missing(data) | missing(Person_ID) | missing(diagnosis_variable)) {
    stop(
      paste0(
        "You must, as a minimum, provide\n",
        "       'data'               (data.frame),\n",
        "       'Person_ID'          (name of id variable in 'data') and\n",
        "       'diagnosis_variable' (name of ICD codes in 'data')\n"
      )
    )
  }
  # end_date_test <- try(is.character(end_date), silent = TRUE)
  # if (!missing(end_date) & !inherits(end_date_test, "try-error")) {
  #   data <- data |>
  #     dplyr::mutate("{end_Date}" = as.Date(end_date, origin = "1970-01-01"))
  # }
  if (inherits(data, "data.frame")) {
    data_names_test <-
      try(
        data |>
          dplyr::select(
            {{ Person_ID }},
            {{ diagnosis_variable }},
            {{ time_variable }}
          ),
        silent = TRUE
      )
    if (inherits(data_names_test, "try-error")) {
      stop(
        paste0(
          "One or more of the named columns does not extist in 'data'.\n",
          "The column selection gives the following error:\n",
          stringr::str_extract(
            data_names_test[1],
            "Column `.+` doesn't exist.\n"
          )
        )
      )
    }
    if (ncol(dplyr::select(data, {{ Person_ID }})) != 1L) {
      stop("Person_ID name a single id variable.")
    }
  } else {
    stop(
      paste0(
        "The input 'data' must be a data.frame with columns names by\n",
        "       'Person_ID'\n",
        "       'diagnosis_variable'\n",
        "       'time_variable'        (optional)\n",
        "       'end_date'             (optional)\n",
        "       'days_before_end_date' (optional)\n"
      )
    )
  }
  if (
    missing(time_variable) & missing(end_date) & missing(days_before_end_date)
  ) {
    func_table1 <- data |>
      dplyr::select(
        ID = {{ Person_ID }},
        Diagvar = {{ diagnosis_variable }}
      )
  } else if (missing(time_variable) | missing(end_date)) {
    stop(
      "When imposing diagnosis-time restrictions both the 'time_variable' ",
      "and the\n'end_date' must be specified."
    )
  } else if (missing(days_before_end_date)) {
    func_table1 <- data |>
      dplyr::mutate(
        ID = {{ Person_ID }},
        Diagvar = {{ diagnosis_variable }},
        diagdate = {{ time_variable }},
        stop_time = {{ end_date }}
      ) |>
      dplyr::filter((.data$stop_time - .data$diagdate) > 0) |>
      dplyr::select("ID", "Diagvar")
  } else {
    func_table1 <- data |>
      dplyr::mutate(
        ID = {{ Person_ID }},
        Diagvar = {{ diagnosis_variable }},
        diagdate = {{ time_variable }},
        stop_time = {{ end_date }},
        lookback = {{ days_before_end_date}}
      ) |>
      dplyr::filter(
        (.data$stop_time - .data$diagdate) > 0 &
          (.data$diagdate - (.data$stop_time - .data$lookback)) >= 0
      ) |>
      dplyr::select("ID", "Diagvar")
  }

  # Classify all diagnoses into different groups (some will not give any
  # points, and will therefore have 0 in all groups)
  func_table2 <- func_table1 |>
    dplyr::mutate(
      # I (ADLS) moved all sub string calculations up here
      Diagvar_2 = substr(.data$Diagvar, 1, 2),
      Diagvar_3 = substr(.data$Diagvar, 1, 3),
      Diagvar_4 = substr(.data$Diagvar, 1, 4),
      Diagvar_5 = substr(.data$Diagvar, 1, 5),

      # Parts, original Quan paper (ICD-10 only)
      Myocardial_infarction_cc = ifelse(
        .data$Diagvar_4 %in% c("DI21", "DI22") |
          .data$Diagvar_5 %in% c("DI252"),
        1L,
        0L
      ),
      Congestive_heart_failure_cc = ifelse(
        .data$Diagvar_4 %in% c("DI43", "DI50") |
          .data$Diagvar_5 %in% c(
            "DI099", "DI110", "DI130", "DI132", "DI255", "DI420", "DI425",
            "DI426", "DI427", "DI428", "DI429", "DP290"
          ),
        1L,
        0L
      ),
      Periphral_vascular_disease_cc = ifelse(
        .data$Diagvar_4 %in% c("DI70", "DI71") |
          .data$Diagvar_5 %in% c(
            "DI731", "DI738", "DI739", "DI771", "DI790", "DI792", "DK551",
            "DK558", "DK559", "DZ958", "DZ959"
          ),
        1L,
        0L
      ),
      Cerebrovascular_disease_cc = ifelse(
        .data$Diagvar_4 %in% c(
          "DG45", "DG46", "DI60", "DI61", "DI62", "DI63", "DI64", "DI65",
          "DI66", "DI67", "DI68", "DI69"
        ) |
          .data$Diagvar_5 %in% c("DH340"),
        1L,
        0L
      ),
      Dementia_cc = ifelse(
        .data$Diagvar_4 %in% c("DF00", "DF01", "DF02", "DF03", "DG30") |
          .data$Diagvar_5 %in% c("DF051", "DG311"),
        1L,
        0L
      ),
      Chronic_pulmonary_disease_cc = ifelse(
        .data$Diagvar_4 %in% c(
          "DJ40", "DJ41", "DJ42", "DJ43", "DJ44", "DJ45", "DJ46", "DJ47",
          "DJ60", "DJ61", "DJ62", "DJ63", "DJ64", "DJ65", "DJ66", "DJ67"
        ) |
          .data$Diagvar_5 %in% c("DI278", "DI279", "DJ684", "DJ701", "DJ703"),
        1L,
        0L
      ),
      Connective_tissue_diesease_cc = ifelse(
        .data$Diagvar_4 %in% c("DM05", "DM06", "DM32", "DM33", "DM34") |
          .data$Diagvar_5 %in% c("DM315", "DM351", "DM353", "DM360"),
        1L,
        0L
      ),
      Peptic_ulcer_disease_cc = ifelse(
        .data$Diagvar_4 %in% c("DK25", "DK26", "DK27", "DK28"),
        1L,
        0L
      ),
      Mild_liver_disease_cc = ifelse(
        .data$Diagvar_4 %in% c("DB18", "DK73", "DK74") |
          .data$Diagvar_5 %in% c(
            "DK700", "DK701", "DK702", "DK703", "DK709", "DK713", "DK714",
            "DK715", "DK717", "DK760", "DK762", "DK763", "DK764", "DK768",
            "DK769", "DZ944"
          ),
        1L,
        0L
      ),
      Moderate_or_severe_liver_disease_cc = ifelse(
        .data$Diagvar_5 %in% c(
          "DI850", "DI859", "DI864", "DK704", "DK711", "DK721", "DK729",
          "DK765", "DK766", "DK767"
        ),
        1L,
        0L
      ),
      Diabetes_without_complications_cc = ifelse(
        .data$Diagvar_5 %in% c(
          "DE100", "DE101", "DE106", "DE108", "DE109", "DE110", "DE111",
          "DE116", "DE118", "DE119", "DE120", "DE121", "DE126", "DE128",
          "DE129", "DE130", "DE131", "DE136", "DE138", "DE139", "DE140",
          "DE141", "DE146", "DE148", "DE149"
        ),
        1L,
        0L
      ),
      Diabetes_with_complications_cc = ifelse(
        .data$Diagvar_5 %in% c(
          "DE102", "DE103", "DE104", "DE105", "DE107", "DE112", "DE113",
          "DE114", "DE115", "DE117", "DE122", "DE123", "DE124", "DE125",
          "DE127", "DE132", "DE133", "DE134", "DE135", "DE137", "DE142",
          "DE143", "DE144", "DE145", "DE147"
        ),
        1L,
        0L
      ),
      Paraplegia_and_hemiplegia_cc = ifelse(
        .data$Diagvar_4 %in% c("DG81", "DG82") |
          .data$Diagvar_5 %in% c(
            "DG041", "DG114", "DG801", "DG802", "DG830", "DG831", "DG832",
            "DG833", "DG834", "DG839"
          ),
        1L,
        0L
      ),
      Renal_disease_cc = ifelse(
        .data$Diagvar_4 %in% c("DN18", "DN19") |
          .data$Diagvar_5 %in% c(
            "DI120", "DI131", "DN032", "DN033", "DN034", "DN035", "DN036",
            "DN037", "DN052", "DN053", "DN054", "DN055", "DN056", "DN057",
            "DN250", "DZ490", "DZ491", "DZ492", "DZ940", "DZ992"
          ),
        1L,
        0L
      ),
      Cancer_cc = ifelse(
        .data$Diagvar_4 %in% c(
          "DC20", "DC21", "DC22", "DC23", "DC24", "DC25", "DC26", "DC30",
          "DC31", "DC32", "DC33", "DC34", "DC37", "DC38", "DC39", "DC40",
          "DC41", "DC43", "DC45", "DC46", "DC47", "DC48", "DC49", "DC50",
          "DC51", "DC52", "DC53", "DC54", "DC55", "DC56", "DC57", "DC58",
          "DC70", "DC71", "DC72", "DC73", "DC74", "DC75", "DC76", "DC81",
          "DC82", "DC83", "DC84", "DC85", "DC88", "DC90", "DC91", "DC92",
          "DC93", "DC94", "DC95", "DC96", "DC97"
        ),
        1L,
        0L
      ),
      Metastatic_carcinoma_cc = ifelse(
        .data$Diagvar_4 %in% c( "DC77", "DC78", "DC79", "DC80"),
        1L,
        0L
      ),
      AIDS_HIV_cc = ifelse(
        .data$Diagvar_4 %in% c("DB20", "DB21", "DB22", "DB24"),
        1L,
        0L
      ),

      #Parts, Christensen paper
      #(ICD-10 & ICD-8 - ICD-10 not completely overlapping Quan)
      Myocardial_infarction_ch = ifelse(
        .data$Diagvar_4 %in% c("DI21", "DI22", "DI23") |
          .data$Diagvar_3 %in% c("410"),
        1L,
        0L
      ),
      Congestive_heart_failure_ch = ifelse(
        .data$Diagvar_4 %in% c("DI50") |
          .data$Diagvar_5 %in% c(
            "DI110", "DI130", "DI132", "42709", "42710", "42711", "42719",
            "42899", "78249"
          ),
        1L,
        0L
      ),
      Periphral_vascular_disease_ch = ifelse(
        .data$Diagvar_4 %in% c("DI70", "DI71", "DI72", "DI73", "DI74", "DI77") |
          .data$Diagvar_3 %in% c("440", "441", "442", "443", "444", "445"),
        1L,
        0L
      ),
      Cerebrovascular_disease_ch = ifelse(
        .data$Diagvar_4 %in% c(
          "DG45", "DG46", "DI60", "DI61", "DI62", "DI63", "DI64", "DI65",
          "DI66", "DI67", "DI68", "DI69"
        ) |
          .data$Diagvar_3 %in% c(
            "430", "431", "432", "433", "434", "435", "436", "437", "438"
          ),
        1L,
        0L
      ),
      Dementia_ch = ifelse(
        .data$Diagvar_4 %in% c("DF00", "DF01", "DF02", "DF03", "DG30") |
          .data$Diagvar_5 %in% c(
            "DF051", "29009", "29010", "29011", "29012", "29013", "29014",
            "29015", "29016", "29017", "29018", "29019", "29309"
          ),
        1L,
        0L
      ),
      Chronic_pulmonary_disease_ch = ifelse(
        .data$Diagvar_4 %in% c(
          "DJ40", "DJ41", "DJ42", "DJ43", "DJ44", "DJ45", "DJ46", "DJ47",
          "DJ60", "DJ61", "DJ62", "DJ63", "DJ64", "DJ65", "DJ66", "DJ67"
        ) |
          .data$Diagvar_3 %in% c(
            "490", "491", "492", "493", "515", "516", "517", "518"
          ) |
          .data$Diagvar_5 %in% c(
            "DJ684", "DJ701", "DJ703", "DJ841", "DJ920", "DJ961", "DJ982",
            "DJ983"
          ),
        1L,
        0L
      ),
      Connective_tissue_diesease_ch = ifelse(
        .data$Diagvar_4 %in% c(
          "DM05", "DM06", "DM08", "DM09", "DM30", "DM31", "DM32", "DM33",
          "DM34", "DM35", "DM36", "DD86"
        ) |
          .data$Diagvar_3 %in% c("712", "716", "734", "446") |
          .data$Diagvar_5 %in% c("13599"),
        1L,
        0L
      ),
      Peptic_ulcer_disease_ch = ifelse(
        .data$Diagvar_4 %in% c("DK25", "DK26", "DK27", "DK28") |
          .data$Diagvar_3 %in% c("531", "532", "533", "534") |
          .data$Diagvar_5 %in% c("DK221", "53091", "53098"),
        1L,
        0L
      ),
      Mild_liver_disease_ch = ifelse(
        .data$Diagvar_4 %in% c("DB18", "DK71", "DK73", "DK74") |
          .data$Diagvar_3 %in% c("571") |
          .data$Diagvar_5 %in% c(
            "DK700", "DK701", "DK702", "DK703", "DK709", "DK760", "57301",
            "57304"
          ),
        1L,
        0L
      ),
      Moderate_or_severe_liver_disease_ch = ifelse(
        .data$Diagvar_4 %in% c("DK72", "DI85", "4560") |
          .data$Diagvar_5 %in% c(
            "DB150", "DB160", "DB162", "DB190", "DK704", "DK766", "07000",
            "07002", "07004", "07006", "07008", "57300"
          ),
        1L,
        0L
      ),
      Diabetes_without_complications_ch = ifelse(
        .data$Diagvar_5 %in% c(
          "DE100", "DE101", "DE109", "DE110", "DE111", "DE119", "24900",
          "24906", "24907", "24909", "25000", "25006", "25007", "25009"
        ),
        1L,
        0L
      ),
      Diabetes_with_complications_ch = ifelse(
        .data$Diagvar_5 %in% c(
          "DE102", "DE103", "DE104", "DE105", "DE106", "DE107", "DE108",
          "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118",
          "24901", "24902", "24903", "24904", "24905", "24908", "25001",
          "25002", "25003", "25004", "25005", "25008"
        ),
        1L,
        0L
      ),
      Paraplegia_and_hemiplegia_ch = ifelse(
        .data$Diagvar_4 %in% c("DG81", "DG82") |
          .data$Diagvar_3 %in% c("344"),
        1L,
        0L
      ),
      Renal_disease_ch = ifelse(
        .data$Diagvar_4 %in% c(
          "DI12", "DI13", "DN00", "DN01", "DN02", "DN03", "DN04", "DN05",
          "DN07", "DN11", "DN14", "DN17", "DN18", "DN19", "DQ61"
        ) |
          .data$Diagvar_3 %in% c(
            "403", "404", "580", "581", "582", "583", "584", "792"
          ) |
          .data$Diagvar_5 %in% c(
            "59009", "59319", "75310", "75311", "75312", "75313", "75314",
            "75315", "75316", "75317", "75318", "75319"
          ),
        1L,
        0L
      ),
      Cancer_ch = ifelse(
        .data$Diagvar_4 %in% c(
          "DC70", "DC71", "DC72", "DC73", "DC74", "DC75", "DC81", "DC82",
          "DC83", "DC84", "DC85", "DC88", "DC90", "DC91", "DC92", "DC93",
          "DC94", "DC95", "DC96"
        ) |
          .data$Diagvar_2 %in% c("14", "15", "16", "17", "18") |
          .data$Diagvar_3 %in% c(
            "DC0", "DC1", "DC2", "DC3", "DC4", "DC5", "DC6", "190", "191",
            "192", "193", "194", "200", "201", "202", "203", "204", "205",
            "206", "207"
          ) |
          .data$Diagvar_5 %in% c("27559"),
        1L,
        0L
      ),
      Metastatic_carcinoma_ch = ifelse(
        .data$Diagvar_4 %in% c("DC76", "DC77", "DC78", "DC79", "DC80") |
          .data$Diagvar_3 %in% c("195", "196", "197", "198", "199"),
        1L,
        0L
      ),
      AIDS_HIV_ch = ifelse(
        .data$Diagvar_4 %in% c("DB21", "DB22", "DB23", "DB24") |
          .data$Diagvar_5 %in% c("07983"),
        1L,
        0L
      ),


      #Charlson-Deyo (ICD-10 only, not completely the same as Quan or
      #Christensen & different weights for some groups)
      Myocardial_infarction_cd = ifelse(
        .data$Diagvar_4 %in% c("DI21", "DI22") |
          .data$Diagvar_5 %in% c("DI252"),
        1L,
        0L
      ),
      Congestive_heart_failure_cd = ifelse(
        .data$Diagvar_4 %in% c("DI50"),
        1L,
        0L
      ),
      Periphral_vascular_disease_cd = ifelse(
        .data$Diagvar_4 %in% c("DI71", "DR02") |
          .data$Diagvar_5 %in% c("DI790", "DI739", "DZ958", "DZ959"),
        1L,
        0L
      ),
      Cerebrovascular_disease_cd = ifelse(
        .data$Diagvar_4 %in% c(
          "DG46", "DI60", "DI61", "DI62", "DI63", "DI64", "DI65", "DI66",
          "DI69"
        ) |
          .data$Diagvar_5 %in% c(
            "DG450", "DG451", "DG452", "DG458", "DG459", "DI670", "DI671",
            "DI672", "DI674", "DI675", "DI676", "DI677", "DI678", "DI679",
            "DI681", "DI682", "DI688"
          ),
        1L,
        0L
      ),
      Dementia_cd = ifelse(
        .data$Diagvar_4 %in% c("DF00", "DF01", "DF02") |
          .data$Diagvar_5 %in% c("DF051"),
        1L,
        0L
      ),
      Chronic_pulmonary_disease_cd = ifelse(
        .data$Diagvar_4 %in% c(
          "DJ40", "DJ41", "DJ42", "DJ43", "DJ44", "DJ45", "DJ46", "DJ47",
          "DJ60", "DJ61", "DJ62", "DJ63", "DJ64", "DJ65", "DJ66", "DJ67"
        ) |
          .data$Diagvar_5 %in% c(
            "DJ684", "DJ701", "DJ703", "DJ841", "DJ920", "DJ961", "DJ982",
            "DJ983"
          ),
        1L,
        0L
      ),
      Connective_tissue_diesease_cd = ifelse(
        .data$Diagvar_4 %in% c("DM32", "DM34") |
          .data$Diagvar_5 %in% c(
            "DM050", "DM051", "DM052", "DM053", "DM058", "DM060", "DM063",
            "DM069", "DM332", "DM353"
          ),
        1L,
        0L
      ),
      Peptic_ulcer_disease_cd = ifelse(
        .data$Diagvar_4 %in% c("DK25", "DK26", "DK27", "DK28"),
        1L,
        0L
      ),
      Mild_liver_disease_cd = ifelse(
        .data$Diagvar_4 %in% c("DK73") |
          .data$Diagvar_5 %in% c(
            "DK702", "DK703", "DK717", "DK740", "DK742", "DK743", "DK744",
            "DK745", "DK746"
          ),
        1L,
        0L
      ),
      Moderate_or_severe_liver_disease_cd = ifelse(
        .data$Diagvar_5 %in% c("DK721", "DK729", "DK766", "DK767"),
        1L,
        0L
      ),
      Diabetes_without_complications_cd = ifelse(
        .data$Diagvar_5 %in% c(
          "DE101", "DE105", "DE109", "DE111", "DE115", "DE119", "DE131",
          "DE135", "DE139", "DE141", "DE145", "DE149"
        ),
        1L,
        0L
      ),
      Diabetes_with_complications_cd = ifelse(
        .data$Diagvar_5 %in% c(
          "DE102", "DE103", "DE104", "DE112", "DE113", "DE114", "DE132",
          "DE133", "DE134", "DE142", "DE143", "DE144"
        ),
        1L,
        0L
      ),
      Paraplegia_and_hemiplegia_cd = ifelse(
        .data$Diagvar_4 %in% c("DG81") |
          .data$Diagvar_5 %in% c("DG041", "DG820", "DG821", "DG822"),
        1L,
        0L
      ),
      Renal_disease_cd = ifelse(
        .data$Diagvar_4 %in% c(
          "DN01", "DN03", "DN18", "DN19", "DN25"
        ) |
          .data$Diagvar_5 %in% c(
            "DN052", "DN053", "DN054", "DN055", "DN056", "DN072", "DN073",
            "DN074"
          ),
        1L,
        0L
      ),
      Cancer_cd = ifelse(
        .data$Diagvar_4 %in% c(
          "DC70", "DC71", "DC72", "DC73", "DC74", "DC75", "DC76", "DC81",
          "DC82", "DC83", "DC84", "DC85", "DC88", "DC90", "DC91", "DC92",
          "DC93", "DC94", "DC95", "DC96"
        ) |
          .data$Diagvar_3 %in% c(
            "DC0", "DC1", "DC2", "DC3", "DC4", "DC5", "DC6"
          ),
        1L,
        0L
      ),
      Metastatic_carcinoma_cd = ifelse(
        .data$Diagvar_4 %in% c("DC77", "DC78", "DC79", "DC80"),
        1L,
        0L
      ),
      AIDS_HIV_cd = ifelse(
        .data$Diagvar_4 %in% c("DB20", "DB21", "DB22", "DB23", "DB24"),
        1L,
        0L
      )
    )

  # Combining all records to one line per person
  # (who have data within the specified limits),
  # with all diagnosis groups as 1 or 0
  func_table3 <- func_table2 |>
    dplyr::summarise(
      dplyr::across(
        "Myocardial_infarction_cc":"AIDS_HIV_cd",
        \(x) max(x, na.rm = TRUE)
      ),
      .by = .data$ID
    )

  # Give points to all groups according the Charlson specifications AND
  # combine them to a final score (for each version)
  func_table4 <- func_table3 |>
    dplyr::mutate(
      Moderate_or_severe_liver_disease_cc =
        .data$Moderate_or_severe_liver_disease_cc * 3,
      Diabetes_with_complications_cc = .data$Diabetes_with_complications_cc * 2,
      Paraplegia_and_hemiplegia_cc = .data$Paraplegia_and_hemiplegia_cc * 2,
      Renal_disease_cc = .data$Renal_disease_cc * 2,
      Cancer_cc = .data$Cancer_cc * 2,
      Metastatic_carcinoma_cc = .data$Metastatic_carcinoma_cc * 6,
      AIDS_HIV_cc = .data$AIDS_HIV_cc * 6,
      CCI_Quan = (
        .data$Myocardial_infarction_cc +
          .data$Congestive_heart_failure_cc +
          .data$Periphral_vascular_disease_cc +
          .data$Cerebrovascular_disease_cc +
          .data$Dementia_cc +
          .data$Chronic_pulmonary_disease_cc +
          .data$Connective_tissue_diesease_cc +
          .data$Peptic_ulcer_disease_cc +
          pmax(
            .data$Mild_liver_disease_cc,
            .data$Moderate_or_severe_liver_disease_cc
          ) +
          pmax(
            .data$Diabetes_without_complications_cc,
            .data$Diabetes_with_complications_cc
          ) +
          .data$Paraplegia_and_hemiplegia_cc +
          .data$Renal_disease_cc +
          pmax(
            .data$Cancer_cc,
            .data$Metastatic_carcinoma_cc
          ) +
          .data$AIDS_HIV_cc
      ),
      Moderate_or_severe_liver_disease_ch =
        .data$Moderate_or_severe_liver_disease_ch * 3,
      Diabetes_with_complications_ch = .data$Diabetes_with_complications_ch * 2,
      Paraplegia_and_hemiplegia_ch = .data$Paraplegia_and_hemiplegia_ch * 2,
      Renal_disease_ch = .data$Renal_disease_ch * 2,
      Cancer_ch = .data$Cancer_ch * 2,
      Metastatic_carcinoma_ch = .data$Metastatic_carcinoma_ch * 6,
      AIDS_HIV_ch = .data$AIDS_HIV_ch * 6,
      CCI_Christiensen = (
        .data$Myocardial_infarction_ch +
          .data$Congestive_heart_failure_ch +
          .data$Periphral_vascular_disease_ch +
          .data$Cerebrovascular_disease_ch +
          .data$Dementia_ch +
          .data$Chronic_pulmonary_disease_ch +
          .data$Connective_tissue_diesease_ch +
          .data$Peptic_ulcer_disease_ch +
          pmax(
            .data$Mild_liver_disease_ch,
            .data$Moderate_or_severe_liver_disease_ch
          ) +
          pmax(
            .data$Diabetes_without_complications_ch,
            .data$Diabetes_with_complications_ch
          ) +
          .data$Paraplegia_and_hemiplegia_ch +
          .data$Renal_disease_ch +
          pmax(
            .data$Cancer_ch,
            .data$Metastatic_carcinoma_ch
          ) +
          .data$AIDS_HIV_ch
      ),
      Moderate_or_severe_liver_disease_cd =
        .data$Moderate_or_severe_liver_disease_cd * 3,
      Diabetes_with_complications_cd = .data$Diabetes_with_complications_cd * 2,
      Paraplegia_and_hemiplegia_cd = .data$Paraplegia_and_hemiplegia_cd * 2,
      Renal_disease_cd = .data$Renal_disease_cd * 2,
      Cancer_cd = .data$Cancer_cd * 2,
      Metastatic_carcinoma_cd = .data$Metastatic_carcinoma_cd * 3,
      AIDS_HIV_cd = .data$AIDS_HIV_cd * 6,
      CCI_Charlson_Deyo = (
        .data$Myocardial_infarction_cd +
          .data$Congestive_heart_failure_cd +
          .data$Periphral_vascular_disease_cd +
          .data$Cerebrovascular_disease_cd +
          .data$Dementia_cd +
          .data$Chronic_pulmonary_disease_cd +
          .data$Connective_tissue_diesease_cd +
          .data$Peptic_ulcer_disease_cd +
          pmax(
            .data$Mild_liver_disease_cd,
            .data$Moderate_or_severe_liver_disease_cd
          ) +
          pmax(
            .data$Diabetes_without_complications_cd,
            .data$Diabetes_with_complications_cd
          ) +
          .data$Paraplegia_and_hemiplegia_cd +
          .data$Renal_disease_cd +
          pmax(
            .data$Cancer_cd,
            .data$Metastatic_carcinoma_cd
          ) +
          .data$AIDS_HIV_cd
      )
    )

  # Select variables for output - decreased output as standard
  # (otherwise over 50 variables will be retained to the output)
  if (amount_output == "total") {
    func_table5 <- func_table4 |>
      dplyr::select("ID", "CCI_Quan", "CCI_Christiensen", "CCI_Charlson_Deyo")
  }
  else {
    func_table5 <- func_table4
  }

  # Change the name of the ID variable back to the original
  # rlang alternative: rlang::as_string(rlang::ensym(Person_ID))
  Orig_ID_name <- deparse(substitute(Person_ID))
  colnames(func_table5)[1] <- Orig_ID_name

  # Return final table
  return(func_table5)
}
