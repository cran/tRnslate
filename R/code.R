delete_comments <-
function (x, patt_begin, patt_mid, patt_pre, allow_file = FALSE) 
{
    if (missing(patt_begin)) 
        patt_begin <- "^[[:blank:]]*([#]{1}[^@]|[#]{2,})[][:alpha:][:space:]!\"$%&'()*+,./:;<=>?@[\\_`{|}~^-]*.*$"
    if (missing(patt_mid)) 
        patt_mid <- "[#]{1,}[][:alpha:][:space:]!\"$%&'()*+,./:;<=>?@[\\_`{|}~^-]*.*$"
    if (missing(patt_pre)) 
        patt_pre <- "^[[:blank:]]*[^#][][:alpha:][:space:]!\"$%&'()*+,./:;<=>?@[\\_`{|}~^-]*"
    if (allow_file) 
        if (any(file.exists(x))) 
            x <- readLines(x)
    cbeg <- 0
    cmid <- 0
    if (length(patt_begin) > 0) {
        for (i in 1:length(patt_begin)) cbeg <- cbeg + extract_comments(x, 
            pattern = patt_begin[i])
    }
    if (length(patt_mid) > 0) {
        for (j in 1:length(patt_mid)) cmid <- cmid + extract_comments(x, 
            pattern = paste(patt_pre, patt_mid[j], sep = "", 
                collapse = ""))
    }
    cbeg <- ifelse(cbeg > 0, TRUE, FALSE)
    cmid <- ifelse(cmid > 0, TRUE, FALSE)
    if (any(cmid)) 
        for (k in 1:length(patt_mid)) x[cmid] <- gsub(patt_mid[k], 
            "", x[cmid])
    if (any(cbeg)) 
        return(x[!cbeg])
    else return(x)
}
eval_r_code <-
function (x, start, end, type, envir = parent.frame()) 
{
    if (length(x) > 0) {
        o <- list()
        for (i in 1:length(x)) {
            if (type[i] == "R") {
                n <- length(1:(end[i] - start[i] + 1))
            }
            else {
                n <- length(x[[i]])
            }
            aux <- list()
            for (j in 1:n) {
                aux[[j]] <- eval(parse(text = x[[i]], n = j), 
                  envir = envir)
            }
            o[[i]] <- aux
        }
        list(output = o, envir = envir)
    }
}
eval_r_code_all <-
function (x, envir = parent.frame()) 
{
    if (length(x) > 0) {
        o <- list()
        for (i in 1:length(x)) o[[i]] <- eval(parse(text = x[[i]]), 
            envir = envir)
        list(output = o, envir = envir)
    }
}
extract_comments <-
function (x, pattern = "^[[:blank:]]*#", get_id = FALSE, allow_file = FALSE) 
{
    if (allow_file) 
        if (any(file.exists(x))) 
            x <- readLines(x)
    x <- grepl(pattern, x)
    if (get_id) 
        x <- c(1:length(x))[x]
    if (length(x) > 0) 
        return(x)
}
fix_meta <-
function (x) 
{
    meta <- c("\\\\", "\\.", "\\|", "\\(", "\\)", "\\[", "\\{", 
        "\\^", "\\$", "\\*", "\\+", "\\?")
    metaa <- c("\\\\\\\\", "\\\\.", "\\\\|", "\\\\(", "\\\\)", 
        "\\\\[", "\\\\{", "\\\\^", "\\\\$", "\\\\*", "\\\\+", 
        "\\\\?")
    for (i in 1:length(meta)) {
        x <- gsub(meta[i], metaa[i], x)
    }
    return(x)
}
get_r_chunks_from_line <-
function (x, char = "@", open = "<", close = ">") 
{
    x <- get_r_code_from_line(x, char, open, close)
    paste(paste(open, "R", char, sep = ""), x, paste(char, close, 
        sep = ""), sep = "")
}
get_r_code <-
function (x, id_code, line_type, chunk_prefix = NULL, chunk_char = "@", 
    chunk_times = 1, inline_open = "<", inline_char = "@", inline_close = ">") 
{
    if (length(x) > 0) {
        if (is.null(chunk_prefix)) 
            chunk_prefix <- ""
        if (!is.null(chunk_times)) 
            chunk_times <- paste("{", chunk_times, "}", sep = "")
        else chunk_times <- ""
        i <- 1
        i0 <- NULL
        i1 <- NULL
        lines <- list()
        while (i <= length(id_code)) {
            i0 <- c(i0, id_code[i])
            if (line_type[i] == "R") {
                j <- i
                line <- gsub(paste("^[[:blank:]]*", chunk_prefix, 
                  chunk_char, chunk_times, "[Rr]{1}[[:blank:]]*", 
                  sep = ""), "", x[j])
                while (j + 1 <= length(id_code)) {
                  if (line_type[j + 1] == "R" & id_code[j + 1] - 
                    id_code[j] == 1) {
                    addL <- gsub(paste("^[[:blank:]]*", chunk_prefix, 
                      chunk_char, chunk_times, "[Rr]?[[:blank:]]*", 
                      sep = ""), "", x[j + 1])
                    line <- paste(line, "\n", addL)
                    j <- j + 1
                    i <- j
                  }
                  else {
                    break
                  }
                }
                i <- i + 1
            }
            else {
                line <- get_r_code_from_line(x[i], inline_char, 
                  inline_open, inline_close)
                i <- i + 1
            }
            i1 <- c(i1, id_code[i - 1])
            lines[[length(lines) + 1]] <- line
        }
        type <- line_type[match(i0, id_code)]
        list(r = lines, start = i0, end = i1, type = type)
    }
}
get_r_code_from_line <-
function (x, char = "@", open = "<", close = ">") 
{
    x <- gsub(paste(char, close, sep = ""), paste(char, "R", 
        close, sep = ""), x)
    x <- strsplit(x, split = paste(open, "[Rr]", char, sep = ""))[[1]]
    x <- x[grepl(paste(char, "R", close, sep = ""), x)]
    x <- gsub(paste(char, "R", close, "[[:print:]]*$", sep = ""), 
        "", x)
    return(x)
}
get_r_lines <-
function (x, chunk_prefix = NULL, chunk_char = "@", chunk_times = 1, 
    inline_open = "<", inline_char = "@", inline_close = ">", 
    allow_file = FALSE, ...) 
{
    browser(expr = run_debug(name = "get_r_lines", ...))
    if (allow_file) 
        if (any(file.exists(x))) 
            x <- readLines(x)
    if (is.null(chunk_prefix)) 
        chunk_prefix <- ""
    if (!is.null(chunk_times)) 
        chunk_times <- paste("{", chunk_times, "}", sep = "")
    else chunk_times <- ""
    xC <- grepl(paste("^[[:blank:]]*", chunk_prefix, chunk_char, 
        chunk_times, sep = ""), x)
    xC <- grepl(paste("^[[:blank:]]*", chunk_prefix, chunk_char, 
        chunk_times, "[^", chunk_char, "][[:print:]]*$", sep = ""), 
        x) & xC
    x0 <- grepl(paste("^[[:blank:]]*", chunk_prefix, chunk_char, 
        chunk_times, "[Rr]{1}([[:space:]]{1}[[:print:]]*|[[:space:]]?)$", 
        sep = ""), x)
    x1 <- grepl(paste("^[[:blank:]]*", chunk_prefix, chunk_char, 
        chunk_times, "[^[:space:]][[:print:]]*[[:space:]]*$", 
        sep = ""), x)
    xil <- grepl(paste(inline_open, "[Rr]{1}", inline_char, "\\s*\\t*\\w[[:print:]]*", 
        inline_char, inline_close, sep = ""), x)
    ii <- NULL
    if (length(x0) > 0) {
        i0 <- c(1:length(x))[x0]
        i1 <- c(1:length(x))[x1]
        if (length(i0) > 0 & length(i1) > 0) {
            i1 <- i1[i1 > min(i0) & i1 <= min(i1[i1 > max(i0)])]
            ii <- NULL
            for (i in 1:length(i0)) {
                ii <- c(ii, i0[i]:(min(i1[i1 > i0[i]]) - 1))
            }
            rC <- grepl(paste("^[[:blank:]]*", chunk_prefix, 
                chunk_char, chunk_times, sep = ""), x[ii])
            rC <- grepl(paste("^[[:blank:]]*", chunk_prefix, 
                chunk_char, chunk_times, "[^", chunk_char, "][[:print:]]*$", 
                sep = ""), x[ii]) & rC
            ii <- ii[rC]
        }
        else {
            ii <- i0
        }
    }
    if (length(xil) > 0) {
        iil <- c(1:length(x))[xil]
        ii <- unique(c(ii, iil))
    }
    rT <- NULL
    if (length(ii) > 0) {
        for (j in 1:length(ii)) {
            if (grepl(paste(inline_open, "[Rr]{1}", inline_char, 
                "\\s*\\t*\\w[[:print:]]*", inline_char, inline_close, 
                sep = ""), x[ii[j]])) 
                rT <- c(rT, "I")
            else rT <- c(rT, "R")
        }
        ord <- order(ii)
        ii <- ii[ord]
        rT <- rT[ord]
        list(id = ii, type = rT)
    }
}
reduce_empty <-
function (x, pattern = "^$", delete_pair = FALSE) 
{
    pos <- c(1:length(x))[grepl(x, pattern = pattern)]
    op <- pos[-length(pos)]
    co <- pos[-1]
    id <- co - op
    while (any(id == 1)) {
        if (delete_pair) 
            del <- c(op[id == 1], co[id == 1])
        else del <- op[id == 1]
        x <- x[-del]
        pos <- c(1:length(x))[grepl(x, pattern = pattern)]
        op <- pos[-length(pos)]
        co <- pos[-1]
        id <- co - op
    }
    return(x)
}
replace_r_code <-
function (x, r, output, start, end, type, chunk_char = "@", inline_char = "@", 
    inline_open = "<", inline_close = ">", char_begin = "", char_clean = "<:NULL:>", 
    char_drop = "<:NULL:>", ...) 
{
    browser(expr = run_debug(name = "replace_r_code", ...))
    if (length(x) > 0) {
        xN <- x[start]
        xN <- lapply(xN, c)
        todel <- NULL
        for (i in 1:length(xN)) {
            if (type[i] == "R") {
                if (length(output[[i]]) > 0) {
                  for (j in 1:length(output[[i]])) {
                    aux <- list()
                    if (!any(grepl("<-", r[[i]]))) {
                      aux[[j]] <- paste(char_begin, output[[i]][[j]], 
                        sep = "")
                    }
                    else {
                      aux[[j]] <- char_clean
                    }
                    xN[[i]] <- aux
                  }
                }
                else {
                  xN[[i]] <- char_clean
                }
                auxdel <- start[i]:end[i]
                if (length(auxdel) > 1) {
                  todel <- c(todel, auxdel[-1])
                }
            }
            else {
                aux <- xN[[i]]
                toadd <- FALSE
                if (length(output[[i]]) > 0) {
                  for (k in 1:length(output[[i]])) {
                    if (!any(grepl("<-", r[[i]][[k]]))) {
                      aux <- gsub(paste(paste(inline_open, "[Rr]", 
                        inline_char, sep = ""), fix_meta(r[[i]][[k]]), 
                        paste(inline_char, inline_close, sep = ""), 
                        sep = ""), paste(output[[i]][[k]], sep = "", 
                        collapse = paste("<@::split::@>", char_begin, 
                          " ", sep = "")), aux)
                      if (length(output[[i]][[k]]) > 0) {
                        toadd <- TRUE
                      }
                    }
                    else {
                      aux <- gsub(paste(paste(inline_open, "[Rr]", 
                        inline_char, sep = ""), fix_meta(r[[i]][[k]]), 
                        paste(inline_char, inline_close, sep = ""), 
                        sep = ""), "", aux)
                    }
                    xN[[i]] <- aux
                  }
                }
                else {
                  xN[[i]] <- gsub(paste(paste(inline_open, "[Rr]", 
                    inline_char, sep = ""), fix_meta(r[[i]][[k]]), 
                    paste(inline_char, inline_close, sep = ""), 
                    sep = ""), "", aux)
                }
                if (toadd) {
                  xN[[i]] <- strsplit(xN[[i]], split = "<@::split::@>")[[1]]
                }
            }
        }
        o <- lapply(x, c)
        o[start] <- xN
        if (!is.null(todel)) 
            o <- o[-todel]
        o <- lapply(o, unlist)
        is0 <- function(x) ifelse(identical(x, NULL) || identical(x, 
            character(0)) || identical(x, logical(0)) || identical(x, 
            integer(0)) || identical(x, numeric(0)), TRUE, FALSE)
        o <- o[!unlist(lapply(o, is0))]
        if (!is.null(char_drop)) {
            drop <- lapply(o, grepl, pattern = char_drop)
            if (any(unlist(drop))) 
                o <- o[!unlist(drop)]
        }
        return(unlist(o))
    }
}
run_debug <-
function (name, debug) 
ifelse(missing(name) || missing(debug) || is.null(name) || is.null(debug), 
    FALSE, ifelse(is.logical(debug), debug, identical(name, debug)))
translate_r_code <-
function (x, chunk_prefix = NULL, chunk_char = "@", chunk_times = 1, 
    inline_open = "<", inline_char = "@", inline_close = ">", 
    char_begin = "", char_clean = "<:NULL:>", char_drop = "<:NULL:>", 
    envir = new.env(parent = parent.frame()), comments = TRUE, 
    reduce = TRUE, allow_file = FALSE, ...) 
{
    browser(expr = run_debug(name = "translate_r_code", ...))
    if (allow_file) 
        if (any(file.exists(x))) 
            x <- readLines(x)
    if (length(x) == 1) 
        x <- strsplit(as.character(x), split = "\n", fixed = TRUE)[[1]]
    if (!comments) 
        x <- delete_comments(x)
    lin <- c(x, "@@END @@@@@@######@@@@@@------******#")
    L <- get_r_lines(lin, chunk_prefix = chunk_prefix, chunk_char = chunk_char, 
        chunk_times = chunk_times, inline_open = inline_open, 
        inline_char = inline_char, inline_close = inline_close, 
        allow_file = FALSE, ...)
    if (!is.null(L)) {
        R <- get_r_code(lin[L$id], L$id, L$type, chunk_prefix = chunk_prefix, 
            chunk_char = chunk_char, chunk_times = chunk_times, 
            inline_open = inline_open, inline_char = inline_char, 
            inline_close = inline_close)
        E <- eval_r_code(R$r, start = R$start, end = R$end, type = R$type, 
            envir = envir)
        N <- replace_r_code(lin, R$r, output = E$output, start = R$start, 
            end = R$end, type = R$type, chunk_char = chunk_char, 
            inline_char = inline_char, inline_open = inline_open, 
            inline_close = inline_close, char_begin = char_begin, 
            char_clean = char_clean, char_drop = char_drop, ...)
        N <- N[-length(N)]
        if (reduce) 
            N <- reduce_empty(N, pattern = "^$", delete_pair = FALSE)
        return(N)
    }
    else return(x)
}
