## Pure R XLSX writer utilities
## Provides minimal functionality to create multi-sheet XLSX workbooks
## without depending on external packages such as openxlsx. Designed to
## work inside WebR where compiled extensions are unavailable.

#' Convert an integer column index to an Excel-style column label.
#' @param index Positive integer column index (1-based).
#' @return Column label (e.g. 1 -> "A", 28 -> "AB").
excel_column_label <- function(index) {
  stopifnot(index >= 1)
  label <- ""
  while (index > 0) {
    remainder <- (index - 1) %% 26
    label <- paste0(intToUtf8(65 + remainder), label)
    index <- (index - 1) %/% 26
  }
  label
}

#' Escape XML special characters.
#' @param x Character vector.
#' @return Escaped character vector safe for XML content.
xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

#' Retrieve precomputed CRC32 lookup table.
#' @return Integer vector of length 256 with CRC32 values.
get_crc32_table <- local({
  table <- NULL
  function() {
    if (is.null(table)) {
      polynomial <- -306674912L  # 0xEDB88320 in two's complement
      tbl <- integer(256)
      for (i in 0:255) {
        crc <- as.integer(i)
        for (j in 1:8) {
          if (bitwAnd(crc, 1L) != 0L) {
            crc <- bitwXor(bitwShiftR(crc, 1L), polynomial)
          } else {
            crc <- bitwShiftR(crc, 1L)
          }
        }
        tbl[i + 1L] <- crc
      }
      table <<- tbl
    }
    table
  }
})

#' Compute CRC32 checksum for a raw vector.
#' @param raw_vec Raw vector.
#' @return Signed 32-bit integer representing CRC32.
crc32_bytes <- function(raw_vec) {
  crc_table <- get_crc32_table()
  crc <- bitwNot(0L)
  if (length(raw_vec) > 0) {
    bytes <- as.integer(raw_vec)
    for (b in bytes) {
      idx <- bitwAnd(bitwXor(crc, b), 0xFFL) + 1L
      crc <- bitwXor(bitwShiftR(crc, 8L), crc_table[idx])
    }
  }
  bitwNot(crc)
}

#' Convert POSIXct timestamp to DOS date/time components.
#' @param timestamp POSIXct timestamp.
#' @return List with `date` and `time` integer components.
to_dos_datetime <- function(timestamp) {
  if (is.na(timestamp)) {
    timestamp <- Sys.time()
  }
  lt <- as.POSIXlt(timestamp, tz = "UTC")
  year <- lt$year + 1900
  if (year < 1980) {
    year <- 1980
  }
  dos_date <- bitwShiftL(as.integer(year - 1980L), 9L) +
    bitwShiftL(as.integer(lt$mon + 1L), 5L) +
    as.integer(lt$mday)
  dos_time <- bitwShiftL(as.integer(lt$hour), 11L) +
    bitwShiftL(as.integer(lt$min), 5L) +
    as.integer(floor(lt$sec / 2))
  list(date = dos_date, time = dos_time)
}

#' Write a 16-bit little-endian value.
#' @param value Integer value (0-65535).
#' @return Raw vector of length 2.
write_le16 <- function(value) {
  value <- as.integer(value)
  if (value < 0) {
    value <- (value + 65536L) %% 65536L
  }
  lo <- value %% 256L
  hi <- (value %/% 256L) %% 256L
  as.raw(c(lo, hi))
}

#' Write a 32-bit little-endian value.
#' @param value Integer/double value (0-4294967295).
#' @return Raw vector of length 4.
write_le32 <- function(value) {
  value <- as.numeric(value)
  if (value < 0) {
    value <- value + 4294967296
  }
  bytes <- numeric(4)
  for (i in 1:4) {
    bytes[i] <- value %% 256
    value <- floor(value / 256)
  }
  as.raw(as.integer(bytes))
}

#' Create a ZIP archive without external dependencies (store method).
#' @param output_file Destination ZIP file path.
#' @param base_dir Directory containing files to archive.
#' @param files Character vector of file paths relative to `base_dir`.
write_zip_no_compress <- function(output_file, base_dir, files) {
  if (length(files) == 0) {
    stop("No files supplied to write_zip_no_compress().")
  }

  con <- file(output_file, "wb")
  on.exit(close(con), add = TRUE)

  entries <- vector("list", length(files))
  offset <- 0

  for (i in seq_along(files)) {
    rel_path <- files[i]
    file_path <- file.path(base_dir, rel_path)
    file_info <- file.info(file_path)
    if (!isTRUE(file_info$isdir %in% c(FALSE, NA))) {
      next
    }

    size <- if (is.finite(file_info$size)) as.integer(file_info$size) else 0L
    data <- readBin(file_path, "raw", n = size)
    crc <- crc32_bytes(data)
    dos_dt <- to_dos_datetime(file_info$mtime)
    name_raw <- charToRaw(rel_path)

    header <- c(
      write_le32(0x04034B50),
      write_le16(20L),
      write_le16(0L),
      write_le16(0L),
      write_le16(dos_dt$time),
      write_le16(dos_dt$date),
      write_le32(crc),
      write_le32(length(data)),
      write_le32(length(data)),
      write_le16(length(name_raw)),
      write_le16(0L)
    )

    writeBin(header, con)
    writeBin(name_raw, con)
    if (length(data) > 0) {
      writeBin(data, con)
    }

    entries[[i]] <- list(
      filename = rel_path,
      crc = crc,
      size = length(data),
      time = dos_dt$time,
      date = dos_dt$date,
      offset = offset
    )

    offset <- offset + length(header) + length(name_raw) + length(data)
  }

  central_dir_start <- offset

  valid_entries <- Filter(Negate(is.null), entries)

  for (entry in valid_entries) {
    name_raw <- charToRaw(entry$filename)
    central_header <- c(
      write_le32(0x02014B50),
      write_le16(20L),
      write_le16(20L),
      write_le16(0L),
      write_le16(0L),
      write_le16(entry$time),
      write_le16(entry$date),
      write_le32(entry$crc),
      write_le32(entry$size),
      write_le32(entry$size),
      write_le16(length(name_raw)),
      write_le16(0L),
      write_le16(0L),
      write_le16(0L),
      write_le16(0L),
      write_le32(0L),
      write_le32(entry$offset)
    )

    writeBin(central_header, con)
    writeBin(name_raw, con)

    offset <- offset + length(central_header) + length(name_raw)
  }

  central_dir_size <- offset - central_dir_start
  end_record <- c(
    write_le32(0x06054B50),
    write_le16(0L),
    write_le16(0L),
    write_le16(length(valid_entries)),
    write_le16(length(valid_entries)),
    write_le32(central_dir_size),
    write_le32(central_dir_start),
    write_le16(0L)
  )

  writeBin(end_record, con)

  invisible(output_file)
}

#' Create a ZIP archive without external dependencies (store method).
#' @param output_file Destination ZIP file path.
#' @param base_dir Directory containing files to archive.
#' @param files Character vector of file paths relative to `base_dir`.
write_zip_no_compress <- function(output_file, base_dir, files) {
  if (inherits(data, "tbl_df")) {
    data <- as.data.frame(data)
  } else if (is.vector(data) && !is.list(data)) {
    data <- data.frame(Value = data, stringsAsFactors = FALSE)
  }

  if (!inherits(data, "data.frame")) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  for (col in names(data)) {
    if (is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }
  }

  data
}

#' Gather string content from a data frame (including headers) for the
#' shared strings table.
#' @param df Data frame.
#' @return Character vector of strings (with duplicates).
gather_sheet_strings <- function(df) {
  strings <- character()

  if (ncol(df) == 0) {
    return(strings)
  }

  strings <- c(strings, colnames(df))

  for (col in seq_along(df)) {
    column <- df[[col]]

    if (inherits(column, c("Date", "POSIXct", "POSIXt", "difftime"))) {
      column <- as.character(column)
    } else if (is.logical(column)) {
      column <- ifelse(is.na(column), NA_character_, ifelse(column, "TRUE", "FALSE"))
    } else if (!is.numeric(column)) {
      column <- as.character(column)
    }

    if (!is.numeric(column)) {
      strings <- c(strings, column[!is.na(column)])
    }
  }

  strings
}

#' Build XML for a worksheet.
#' @param df Data frame representing the sheet (headers included).
#' @param strings_map Named integer vector mapping strings to indices.
#' @return Character vector with XML lines.
build_sheet_xml <- function(df, strings_map) {
  n_rows <- nrow(df)
  n_cols <- ncol(df)

  xml <- c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
    '<sheetData>'
  )

  total_rows <- if (n_cols == 0) 0 else (n_rows + 1)

  if (total_rows == 0) {
    xml <- c(xml, '</sheetData>', '</worksheet>')
    return(xml)
  }

  for (row_idx in seq_len(total_rows)) {
    cells <- character()

    for (col_idx in seq_len(n_cols)) {
      col_label <- excel_column_label(col_idx)
      cell_ref <- paste0(col_label, row_idx)

      if (row_idx == 1) {
        header_val <- colnames(df)[col_idx]
        if (nzchar(header_val)) {
          string_key <- as.character(header_val)
          str_index <- strings_map[[string_key]]
          if (!is.null(str_index)) {
            cells <- c(cells, sprintf('<c r="%s" t="s"><v>%d</v></c>', cell_ref, str_index))
          }
        }
      } else {
        value <- df[[col_idx]][row_idx - 1]

        if (is.na(value)) {
          next
        }

        if (inherits(value, c("Date", "POSIXct", "POSIXt", "difftime"))) {
          value <- as.character(value)
        }

        if (is.logical(value)) {
          value <- ifelse(value, "TRUE", "FALSE")
        }

        if (is.numeric(value)) {
          numeric_val <- format(value, scientific = FALSE, trim = TRUE)
          cells <- c(cells, sprintf('<c r="%s"><v>%s</v></c>', cell_ref, numeric_val))
        } else {
          string_val <- as.character(value)
          if (nzchar(string_val)) {
            str_index <- strings_map[[string_val]]
            if (!is.null(str_index)) {
              cells <- c(cells, sprintf('<c r="%s" t="s"><v>%d</v></c>', cell_ref, str_index))
            }
          } else {
            cells <- c(cells, sprintf('<c r="%s"/>', cell_ref))
          }
        }
      }
    }

    if (length(cells) > 0) {
      xml <- c(xml, sprintf('<row r="%d">', row_idx), cells, '</row>')
    } else {
      xml <- c(xml, sprintf('<row r="%d"/>', row_idx))
    }
  }

  xml <- c(xml, '</sheetData>', '</worksheet>')
  xml
}

#' Sanitize sheet names to comply with Excel restrictions.
#' @param names Character vector of names.
#' @return Safe, unique sheet names.
sanitize_sheet_names <- function(names) {
  safe <- gsub("[\\\\/*:?\\n\\r\\t\\[\\]]", "_", names)
  safe[nchar(safe) == 0] <- "Sheet"
  safe <- substr(safe, 1, 31)
  make.unique(safe, sep = "_")
}

#' Minimal styles XML content.
#' @return Character vector of XML lines.
styles_xml_content <- function() {
  c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
    '<fonts count="1"><font><sz val="11"/><color theme="1"/><name val="Calibri"/><family val="2"/></font></fonts>',
    '<fills count="2"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill></fills>',
    '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>',
    '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
    '<cellXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/></cellXfs>',
    '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
    '</styleSheet>'
  )
}

#' Create a simple shared strings XML document.
#' @param unique_strings Character vector of unique strings.
#' @param total_count Total number of string occurrences in workbook.
#' @return Character vector of XML lines.
shared_strings_xml <- function(unique_strings, total_count) {
  unique_count <- length(unique_strings)
  header <- sprintf('<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="%d" uniqueCount="%d">',
                    total_count, unique_count)
  xml <- c('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', header)

  if (unique_count > 0) {
    for (value in unique_strings) {
      escaped <- xml_escape(value)
      xml <- c(xml, sprintf('<si><t xml:space="preserve">%s</t></si>', escaped))
    }
  }

  c(xml, '</sst>')
}

#' Minimal styles XML content.
#' @return Character vector of XML lines.
workbook_rels_xml <- function(sheet_files) {
  rels <- c('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">')

  for (i in seq_along(sheet_files)) {
    rels <- c(rels, sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/%s"/>',
                            i, sheet_files[i]))
  }

  next_id <- length(sheet_files) + 1
  rels <- c(rels,
            sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>', next_id),
            sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>', next_id + 1),
            '</Relationships>')
  rels
}

#' Minimal styles XML content.
#' @return Character vector of XML lines.
workbook_xml <- function(sheet_names) {
  xml <- c('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
           '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
           '<sheets>')

  for (i in seq_along(sheet_names)) {
    xml <- c(xml, sprintf('<sheet name="%s" sheetId="%d" r:id="rId%d"/>', xml_escape(sheet_names[i]), i, i))
  }

  c(xml, '</sheets>', '</workbook>')
}

#' Generate [Content_Types].xml content.
#' @param sheet_files Character vector of worksheet filenames.
#' @return Character vector of XML lines.
content_types_xml <- function(sheet_files) {
  xml <- c('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
           '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
           '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
           '<Default Extension="xml" ContentType="application/xml"/>',
           '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
           '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
           '<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>',
           '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>',
           '<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>')

  for (file in sheet_files) {
    xml <- c(xml, sprintf('<Override PartName="/xl/worksheets/%s" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>', file))
  }

  c(xml, '</Types>')
}

#' Generate [Content_Types].xml content.
#' @param sheet_files Character vector of worksheet filenames.
#' @return Character vector of XML lines.
core_props_xml <- function(creator, timestamp) {
  c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
    sprintf('<dc:creator>%s</dc:creator>', xml_escape(creator)),
    sprintf('<cp:lastModifiedBy>%s</cp:lastModifiedBy>', xml_escape(creator)),
    sprintf('<dcterms:created xsi:type="dcterms:W3CDTF">%s</dcterms:created>', timestamp),
    sprintf('<dcterms:modified xsi:type="dcterms:W3CDTF">%s</dcterms:modified>', timestamp),
    '</cp:coreProperties>'
  )
}

#' Generate [Content_Types].xml content.
#' @param sheet_files Character vector of worksheet filenames.
#' @return Character vector of XML lines.
app_props_xml <- function(sheet_names) {
  sheet_count <- length(sheet_names)
  titles <- paste(sprintf('<vt:lpstr>%s</vt:lpstr>', xml_escape(sheet_names)), collapse = '')

  c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">',
    '<Application>AlloMate</Application>',
    '<DocSecurity>0</DocSecurity>',
    '<ScaleCrop>false</ScaleCrop>',
    '<HeadingPairs><vt:vector size="2" baseType="variant"><vt:variant><vt:lpstr>Worksheets</vt:lpstr></vt:variant><vt:variant><vt:i4>',
    sprintf('%d', sheet_count),
    '</vt:i4></vt:variant></vt:vector></HeadingPairs>',
    sprintf('<TitlesOfParts><vt:vector size="%d" baseType="lpstr">%s</vt:vector></TitlesOfParts>', sheet_count, titles),
    '<Company></Company>',
    '<LinksUpToDate>false</LinksUpToDate>',
    '<SharedDoc>false</SharedDoc>',
    '<HyperlinksChanged>false</HyperlinksChanged>',
    '<AppVersion>16.0000</AppVersion>',
    '</Properties>'
  )
}

#' Generate [Content_Types].xml content.
#' @param sheet_files Character vector of worksheet filenames.
#' @return Character vector of XML lines.
root_relationships_xml <- function() {
  c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
    '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>',
    '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>',
    '</Relationships>'
  )
}

#' Generate [Content_Types].xml content.
#' @param sheet_files Character vector of worksheet filenames.
#' @return Character vector of XML lines.
write_xlsx_pure <- function(file, sheets, creator = "AlloMate") {
  if (is.null(names(sheets)) || any(names(sheets) == "")) {
    stop("All sheets must be named for write_xlsx_pure().")
  }

  sheet_names <- sanitize_sheet_names(names(sheets))
  sheet_data <- lapply(sheets, coerce_sheet_data)

  string_pool <- character()
  for (df in sheet_data) {
    string_pool <- c(string_pool, gather_sheet_strings(df))
  }

  unique_strings <- unique(string_pool)
  string_count <- length(string_pool)

  if (length(unique_strings) > 0) {
    strings_map <- setNames(seq_along(unique_strings) - 1L, unique_strings)
  } else {
    strings_map <- setNames(integer(), character())
  }

  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  tmp_dir <- tempfile("allomate_xlsx_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(file.path(tmp_dir, "_rels"))
  dir.create(file.path(tmp_dir, "docProps"))
  dir.create(file.path(tmp_dir, "xl"))
  dir.create(file.path(tmp_dir, "xl", "_rels"))
  dir.create(file.path(tmp_dir, "xl", "worksheets"))

  sheet_files <- character(length(sheet_data))

  for (i in seq_along(sheet_data)) {
    sheet_file <- sprintf("sheet%d.xml", i)
    sheet_path <- file.path(tmp_dir, "xl", "worksheets", sheet_file)
    sheet_files[i] <- sheet_file

    xml_lines <- build_sheet_xml(sheet_data[[i]], strings_map)
    writeLines(xml_lines, sheet_path, useBytes = TRUE)
  }

  writeLines(styles_xml_content(), file.path(tmp_dir, "xl", "styles.xml"), useBytes = TRUE)
  writeLines(workbook_xml(sheet_names), file.path(tmp_dir, "xl", "workbook.xml"), useBytes = TRUE)
  writeLines(workbook_rels_xml(sheet_files), file.path(tmp_dir, "xl", "_rels", "workbook.xml.rels"), useBytes = TRUE)
  writeLines(shared_strings_xml(unique_strings, string_count), file.path(tmp_dir, "xl", "sharedStrings.xml"), useBytes = TRUE)
  writeLines(root_relationships_xml(), file.path(tmp_dir, "_rels", ".rels"), useBytes = TRUE)
  writeLines(content_types_xml(sheet_files), file.path(tmp_dir, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(core_props_xml(creator, timestamp), file.path(tmp_dir, "docProps", "core.xml"), useBytes = TRUE)
  writeLines(app_props_xml(sheet_names), file.path(tmp_dir, "docProps", "app.xml"), useBytes = TRUE)

  if (file.exists(file)) {
    unlink(file)
  }

  files_to_zip <- list.files(path = tmp_dir, recursive = TRUE, include.dirs = FALSE)

  if (length(files_to_zip) == 0) {
    stop("No files generated for XLSX archive.")
  }

  zip_temp <- tempfile(fileext = ".zip")
  zip_success <- FALSE
  zip_error <- NULL

  try_system_zip <- nzchar(Sys.which("zip")) || identical(.Platform$OS.type, "windows")

  if (try_system_zip) {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(tmp_dir)

    zip_success <- tryCatch({
      zip(zipfile = zip_temp, files = files_to_zip, flags = "-q")
      file.exists(zip_temp)
    }, warning = function(w) {
      zip_error <<- w$message
      FALSE
    }, error = function(e) {
      zip_error <<- e$message
      FALSE
    })

    setwd(old_wd)
  }

  if (zip_success && file.exists(zip_temp)) {
    if (!file.copy(zip_temp, file, overwrite = TRUE)) {
      stop("Unable to write XLSX file to destination path.")
    }
    unlink(zip_temp)
    return(invisible(file))
  }

  if (file.exists(zip_temp)) {
    unlink(zip_temp)
  }

  fallback_success <- tryCatch({
    write_zip_no_compress(file, tmp_dir, files_to_zip)
    TRUE
  }, error = function(e) {
    zip_error <<- e$message
    FALSE
  })

  if (!fallback_success) {
    msg <- "Failed to create ZIP archive for XLSX output."
    if (!is.null(zip_error)) {
      msg <- paste(msg, "Details:", zip_error)
    }
    stop(msg)
  }

  invisible(file)
}


