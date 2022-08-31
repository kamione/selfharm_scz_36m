pairwise.chisq.test <- function(x, g, p.adj = p.adjust.methods, ...) {
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    p.adj <- match.arg(p.adj)
    tab <- table(g, x)
    
    compare.levels <- function(i, j) {
        table_for_test <- tab[c(i, j), ]
        chisq.test(table_for_test, simulate.p.value = TRUE)$p.value
    }
    
    p.value <- pairwise.table(compare.levels, levels(g), p.adj)

    structure(list(method = "Pearson's Chi-squared tests",
                   data.name = DNAME,
                   p.value = p.value,
                   p.adjust.method = p.adj),
              class = "pairwise.htest")
}

add_stat_pairwise <- function(data, variable, by, ...) {
    # calculate pairwise p-values
    if (is.factor(data[[variable]])) {
        pw <- pairwise.chisq.test(data[[variable]], data[[by]], p.adj = "fdr")
    } else {
        pw <- pairwise.t.test(data[[variable]], data[[by]], p.adj = "fdr")
    }
    
    # convert p-values to list
    index <- 0L
    p.value.list <- list()
    for (i in seq_len(nrow(pw$p.value))) {
        for (j in seq_len(nrow(pw$p.value))) {
            index <- index + 1L
            
            p.value.list[[index]] <- 
                c(pw$p.value[i, j]) %>%
                setNames(glue::glue("**{colnames(pw$p.value)[j]} vs. {rownames(pw$p.value)[i]}**"))
        }
    }
    
    # convert list to data frame
    p.value.list %>% 
        unlist() %>%
        purrr::discard(is.na) %>%
        t() %>%
        as.data.frame() %>%
        # formatting/roundign p-values
        dplyr::mutate(dplyr::across(everything(), style_pvalue))
}
