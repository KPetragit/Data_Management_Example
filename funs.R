#' Get Kobo Asset
#'
#' This function retrieves a Kobo asset based on the asset name.
#'
#' @param asset_name A character string representing the name of the asset.
#' @return A Kobo asset object.
#' @export
get_kobo_asset <- function(asset_name) {
  asset <- kobo_asset_list() |>
    filter(name == asset_name) |>
    pull(uid) |>
    first() |>
    kobo_asset()
  return(asset)
}


#' Merge Repeat Groups
#'
#' This function merges repeat groups in the data frame.
#'
#' @param df A data frame containing the data.
#' @return A list of data frames with merged repeat groups.
#' @export
# merge_repeat_groups <- function(df) {
#   df_m <- list()
#   
#   if ("main" %in% names(df)) {
#     df_m$hhmain <- df |> 
#       dm_flatten_to_tbl(.start = main, .join = left_join) |>
#       left_join(
#         df |>
#           dm_flatten_to_tbl(.start = HHmemberInfo, .join = full_join) |>
#           filter(memberPosition == 1) |>
#           select(`_uuid`, HeadAge = HH_04, HeadSex = HH_02)
#       )
#   }
#   
#   if ("HHmemberInfo" %in% names(df)) {
#     df_m$HHmemberInfo <- df |> 
#       dm_flatten_to_tbl(.start = HHmemberInfo, .join = full_join)
#   }
#   
#   if ("identification_questions_repeat" %in% names(df)) {
#     df_m$id <- df |> 
#       dm_flatten_to_tbl(.start = identification_questions_repeat, .join = full_join)
#   }
#   
#   if ("hhroster" %in% names(df)) {
#     df_m$hhroster <- df |>
#       dm_flatten_to_tbl(.start = hhroster, .join = left_join) |>
#       rename(memberPosition = hhroster_memberID) |>
#       left_join(
#         df_m$HHmemberInfo |>
#           select(`_uuid`, memberPosition, Intro_06, HH_02, HH_04),
#         by = c("_uuid", "memberPosition", "Intro_06")
#       )
#   }
#   
#   if ("plot_roster_info" %in% names(df)) {
#     df_m$plot_roster_info <- df |> 
#       dm_flatten_to_tbl(.start = plot_roster_info, .join = full_join)
#   }
#   
#   if ("ScProtec02_group" %in% names(df)) {
#     df_m$ScProtec02_group <- df |> 
#       dm_flatten_to_tbl(.start = ScProtec02_group, .join = full_join)
#   }
#   if ("gp_migrant" %in% names(df)) {
#     df_m$gp_migrant <- df |> 
#       dm_flatten_to_tbl(.start = gp_migrant, .join = full_join)
#   }
#   if ("rosterPart2" %in% names(df)) {
#     df_m$rosterPart2 <- df |> 
#       dm_flatten_to_tbl(.start = rosterPart2, .join = full_join)
#   }
#   
#   df_m <- map(df_m, as.data.frame)
#   
#   return(df_m)
# }

merge_repeat_groups <- function(df) {
  df_m <- list()
  
  # Flatten each table individually, no joins between different tables
  for (tbl_name in names(df)) {
    if (tbl_name %in% names(df)) {
      df_m[[tbl_name]] <- df |> 
        dm_flatten_to_tbl(.start = !!rlang::sym(tbl_name), .join = full_join) |>
        as.data.frame()
    }
  }
  
  return(df_m)
}


#' Count distinct interviews
#'
#' This function counts the number of distinct interviews for each group.
#'
#' @param df A data frame.
#' @param group_var A variable to group by.
#' @return A data frame with the count of distinct interviews.
#' @export
n_interviews <- function(df, group_var) {
  df <- df |>
    group_by({{ group_var }}) |>
    mutate(n_interviews_enum = n_distinct(Intro_06)) |>
    ungroup()
}



#' Pivot and match date of birth measurement
#'
#' This function pivots the data to match the date of birth (DOB) measurement for the selected child.
#'
#' @param df A data frame containing the data.
#' @param selected_child_col The column name pattern for selected child.
#' @param child_position_col The column name for child position.
#' @param birth_certificate_col The column name pattern for birth certificate age measurement.
#' @param immunization_card_col The column name pattern for immunization card age measurement.
#' @return A data frame with additional columns indicating whether the DOB was measured using a birth certificate or immunization card.
#' @export
pivot_and_match_dob <- function(df, selected_child_col = "selected_child", child_position_col = "childnametousePOSITION", 
                                birth_certificate_col = "birth_certificate_agem", immunization_card_col = "immunization_card_agem") {
  df %>%
    pivot_longer(
      cols = starts_with(selected_child_col),
      names_to = "var",
      values_to = "selected_child"
    ) %>%
    select(
      `_uuid`, `_index`, var, selected_child, all_of(child_position_col), 
      starts_with(birth_certificate_col), starts_with(immunization_card_col)
    ) %>%
    filter(selected_child == !!sym(child_position_col)) %>%
    mutate(
      birth_certificate_selected_child = if_else(var == selected_child_col & !is.na(!!sym(birth_certificate_col)), 1, 0),
      immunization_card_selected_child = if_else(var == selected_child_col & !is.na(!!sym(immunization_card_col)), 1, 0),
      dob_card_selected_child = if_else(birth_certificate_selected_child == 1 | immunization_card_selected_child == 1, 1, 0)
    )
}

#' Merge Household Information
#'
#' This function merges household information back to the main dataframe.
#'
#' @param df A data frame to merge into.
#' @param hhmain A data frame containing household main information.
#' @param columns A character vector of column names to merge.
#' @return A merged data frame.
#' @export
merge_hh_info <- function(df, hhmain, columns, df_name) {
  tryCatch({
    merged_df <- merge(df, hhmain |> select(`_uuid`, all_of(columns)), by = "_uuid")
    return(merged_df)
  }, error = function(e) {
    if (grepl("'by' must specify a uniquely valid column", e$message)) {
      stop(paste("Error in merging dataframes. Please check the '_uuid' column in the following dataframe:", df_name))
    } else {
      stop(e)
    }
  })
}

merge_hh_info_wrapper <- function(df_m, columns_to_merge) {
  dataframes <- c("hhroster", "ScProtec02_group", "plot_roster_info", "HHmemberInfo")
  
  for (df_name in dataframes) {
    if (!is.null(df_m[[df_name]])) {
      df_m[[df_name]] <- merge_hh_info(df_m[[df_name]], df_m$hhmain, columns_to_merge, df_name)
    }
  }
  
  return(df_m)
}

#' Add Key Variables for Interview Outcomes
#'
#' This function adds key variables for interview outcomes to the data frame.
#'
#' @param df A data frame containing the data.
#' @param key_vars A character vector of key variables to add.
#' @return A data frame with added key variables.
#' @export
add_key_variables <- function(df, key_vars) {
  df <- df |>
    eligibile_unit() |>
    eligibile_unit_resprate () |>
    eligibile_unit_refrate () |>
    complete_interview(key_vars) |>
    refusal_interview() |>
    denom_cooperation() |>    # ADDED THIS - PETRA
    num_cooperation() |>      # ADDED THIS - PETRA
    complete_interview_repl(key_vars) |> # ADDED THIS - PETRA
    denom_contact_rate () |> # ADDED THIS - PETRA
    num_contact_rate () |> # ADDED THIS - PETRA
    denom_replneed_rate () |> # ADDED THIS - PETRA
    num_replneed_rate () |> # ADDED THIS - PETRA
    denom_repldone_rate () |> # ADDED THIS - PETRA
    num_repldone_rate () |> # ADDED THIS - PETRA
    mutate(complete_interview_tf = case_when(
      complete_interview == 1 ~ TRUE,
      complete_interview == 0 ~ FALSE
    ))
  return(df)
}

#' Select Relevant Columns from a Data Frame
#'
#' This function selects a predefined set of columns from a data frame, including
#' variables for missingness checks, introductory questions, date/time fields, UUIDs,
#' geographic identifiers, household head information, and any additional variables specified.
#'
#' @param df A data frame from which to select columns.
#' @param additional_vars A character vector of additional variable names to include.
#'
#' @return A data frame with only the selected columns.
#' @export
#'
#' @examples
#' \dontrun{
#'   selected_df <- select_columns(df, c("custom_var1", "custom_var2"))
#' }
#'
#' @importFrom dplyr select any_of
#'
#' @seealso [dplyr::select()], [dplyr::any_of()]
select_columns <- function(df, additional_vars) {
  df %>%
    select(
      any_of(params$vars_check_missing),
      any_of(c("Intro_01", "Intro_01a", "n_interviews_enum", "Intro_06", "stratum")),
      any_of(c("start_date", "start_time", "end_date", "end_time")), # date vars
      any_of(c("_uuid", "Final_01")),
      any_of(c("Intro_03a_NUTS1", "Intro_03b_NUTS2", "Intro_03c_NUTS3")),
      any_of(c("HeadName", "HeadAge", "HeadSex")),
      any_of(additional_vars)
    )
}

#' Merge All Data Frames in a List of Lists
#'
#' This function takes a list of lists of data frames (e.g., from multiple Kobo assets),
#' aligns column types across each named data frame, and merges them using `bind_rows`.
#' It handles type coercion dynamically and preserves a `source` column to track origins.
#'
#' @param df_list A list where each element is a named list of data frames with identical structure.
#'
#' @return A named list of merged data frames.
#' @export
#'
#' @examples
#' \dontrun{
#'   merged_data <- merge_all_dataframes_dynamic(list_of_data_lists)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom haven as_factor
merge_all_dataframes_dynamic <- function(df_list) {
  # Get the names of all dataframes from the first element (all elements should have the same names)
  df_names <- names(df_list[[1]])
  
  # Create an empty list to store the merged dataframes
  df_l_merged <- list()
  
  # Loop over each dataframe name dynamically
  for (df_name in df_names) {
    # Extract the list of dataframes for the current dataframe name
    df_list_sub <- lapply(df_list, function(x) x[[df_name]])
    
    # Function to safely convert columns
    convert_column <- function(col, target_class) {
      if (target_class == "numeric") {
        return(as.numeric(as.character(col)))
      } else if (target_class == "character") {
        return(as.character(col))
      } else if (target_class == "factor") {
        return(as_factor(col))
      } else if (target_class == "haven_labelled") {
        return(as_factor(col))
      }
      return(col)
    }
    
    # Identify column types for the first dataframe
    column_types <- sapply(df_list_sub[[1]], class)
    
    # Align column types for all dataframes in the list
    df_list_sub <- lapply(df_list_sub, function(df) {
      for (col_name in names(df)) {
        target_class <- column_types[col_name]
        df[[col_name]] <- convert_column(df[[col_name]], target_class)
      }
      return(df)
    })
    
    # Merge all dataframes for the current name
    df_l_merged[[df_name]] <- bind_rows(df_list_sub, .id = "source")
  }
  
  return(df_l_merged)
}

#' Import and Process Kobo Asset Data
#'
#' This function retrieves a Kobo asset by name, converts it to a dm object if necessary,
#' merges repeat groups, and returns the processed data.
#'
#' @param asset_name A character string specifying the name of the Kobo asset.
#'
#' @return A list of data frames representing the merged Kobo data, or NULL if the asset is invalid.
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- data_import("example_asset")
#' }
#'
#' @family data import functions
#'
#' @seealso [get_kobo_asset()], [kobo_data()], [as_dm()], [merge_repeat_groups()]
#'
#' @importFrom koboloadeR get_kobo_asset kobo_data
#' @importFrom dm as_dm
#' @importFrom koboloadeR merge_repeat_groups
data_import <- function(asset_name, use_cache = TRUE) {
  cache_key <- paste0("asset_", asset_name)
  
  if (use_cache && exists(cache_key, envir = .asset_cache)) {
    return(get(cache_key, envir = .asset_cache))
  }
  
  asset <- suppressMessages(get_kobo_asset(asset_name))
  df <- suppressMessages(kobo_data(asset))
  
  if (!inherits(df, "dm")) {
    df <- tryCatch({
      as_dm(df)
    }, error = function(e) {
      message("Asset '", asset_name, "' not valid. Skipping.")
      return(NULL)
    })
  }
  
  if (is.null(df)) return(NULL)
  
  #df_l <- suppressMessages(merge_repeat_groups(df))
  
  if (use_cache) {
    assign(cache_key, df, envir = .asset_cache)
  }
  
  return(df)
}

