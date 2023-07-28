
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Functions ####################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Generate palettes of distinct colors
#'
#' @param n How many colors do we need?
#' @param ... Extra parameters passed to \code{\link[randomcoloR]{randomColor}},
#' \code{\link[randomcoloR]{distinctColorPalette}} or
#' \code{\link[Polychrome]{createPalette}}, depending on \code{random}.
#' @param pal Name of the palette to use. Use \code{names(pal_discrete)} to get
#' all palette names
#' @param is.extend When \code{n} > \code{length(pal_discrete\[\[pal\]\])},
#' whether or not to extend the colors with
#' \code{\link[grDevices:colorRamp]{colorRampPalette}}.
#' @param random Choose a method to generate random colors. Default is "no".
#' @param seed Seed for random colors.
#' @param verbose Show progress messages.
#'
#' @return A vector with \code{n} colors. For \code{setColor}, also set names as
#' factor levels.
#'
#' @importFrom scales hue_pal
#' @importFrom grDevices colorRampPalette
#' @importFrom randomcoloR distinctColorPalette randomColor
#' @importFrom Polychrome createPalette
#'
#' @export
#' @rdname setColor
getDiscreteColors <- function(
    n,
    pal = NULL,
    is.extend = TRUE,
    random = c("no", "randomColor", "distinctColorPalette", "Polychrome"),
    seed = 1234,
    verbose = FALSE,
    ...
) {
  random <- match.arg(arg = random)
  ## Random colors
  if (random != "no") {
    set.seed(seed = seed)
    if (verbose) {
      message("Use '", random, "' to get discrete colors")
    }
    return(switch(
      EXPR = random,
      "randomColor" = randomColor(count = n, ...),
      "distinctColorPalette" = distinctColorPalette(k = n, ...),
      "Polychrome" = createPalette(N = n, ...)
    ))
  }

  ## Use hue_pal like ggplot2' default
  if (is.null(x = pal)) {
    if (verbose) {
      message(
        "Use 'hue_pal()' that is default for ggplot2 ",
        "to get discrete colors"
      )
    }
    return(hue_pal(...)(n))
  }

  ## Use palettes in this package
  pal <- pal[1]
  if (!is.numeric(x = n)) {
    stop("'n' must be numeric.")
  }
  if (!pal %in% names(x = pal_discrete)) {
    stop(
      "'", pal, "' is not found in palette names: ",
      paste(names(x = pal_discrete), collapse = ", ")
    )
  }
  n.available <- names(x = pal_discrete[[pal]]) %>% as.numeric()
  n.select <- n.available[n.available >= n]
  if (length(x = n.select) > 0) {
    n.select <- min(n.select) %>% as.character()
    if (verbose) {
      message("getDiscreteColors(", n, "): '", pal, "'->", n.select)
    }
    return(head(x = pal_discrete[[pal]][[n.select]], n))
  }
  n.select <- as.character(x = 0)
  color.use <- pal_discrete[[pal]][[n.select]]
  if (!is.extend) {
    warning(
      "The default color number of '", pal, "' ", length(x = color.use),
      " < ", n, ". Set `is.extend = TRUE` to extend the output colors",
      call. = FALSE,
      immediate. = TRUE
    )
    return(color.use)
  }
  if (verbose) {
    message(
      "Extend colors from default '", pal, "' to ", n,
      " with 'colorRampPalette()'"
    )
  }
  return(colorRampPalette(colors = color.use)(n))
}

#' @param x A factor to use colors.
#'
#' @export
setColor <- function(x, pal = NULL, ...) {
  if (!is.factor(x = )) {
    x <- as.factor(x = x)
  }
  color <- getDiscreteColors(n = length(x = levels(x = x)), pal = pal, ...)
  names(x = color) <- levels(x = x)
  return(color)
}

#' @export
#' @rdname checkColorMap
#' @method checkColorMap default
checkColorMap.default <- function(x, colors = NULL, ...) {
  if (!is.factor(x = x)) {
    x <- factor(x = x)
  }
  all_levels <- levels(x = x)
  if (is.null(x = colors)) {
    warning("No 'colors' input, calling 'setColor'...", immediate. = TRUE)
    return(setColor(x = x, ...))
  }
  if (is.null(x = names(x = colors))) {
    color_names <- all_levels[1:min(length(x = all_levels), length(x = colors))]
    warning(
      "No level name found in 'colors', set color names with ",
      paste(color_names, collapse = ", "),
      immediate. = TRUE
    )
    names(x = colors) <- color_names
  }
  if (length(colors) < length(all_levels)) {
    warning(
      "Not all levels have mapped colors...",
      immediate. = TRUE
    )
    colors0 <- setColor(x, ...)
    colors0[names(x = colors)] <- colors
    return(colors0)
  }
  if (!all(all_levels %in% names(x = colors))) {
    warning(
      "No color found for ",
      paste(setdiff(all_levels, names(colors)), collapse = ", "),
      "\nCalling 'setColor'...",
      immediate. = TRUE
    )
    colors0 <- setColor(x, ...)
    cmm.nm <- fastIntersect(x = names(x = colors0), y = names(x = colors))
    colors0[cmm.nm] <- colors[cmm.nm]
    return(colors0)
  }
  colors <- colors[all_levels]
  return(colors)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Palettes #####################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' A palette list for distinct colors.
#' 
#' @export
pal_discrete <- list(
  "category10_d3" = list(
    `0` = c(
      "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
      "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"
    )
  ),
  "category20_d3" = list(
    `0` = c(
      "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
      "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF",
      "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5",
      "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5"
    )
  ),
  "jco" = list(
    `0` = c(
      "#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC",
      "#003C67", "#8F7700", "#3B3B3B", "#A73030", "#4A6990"
    ),
    `3` = c("#0073C2", "#EFC000", "#CD534C"),
    `5` = c("#0073C2", "#EFC000", "#868686", "#CD534C", "#3B3B3B")
  ),
  "Tableau_10" = list(
    `0` = c(
      "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
      "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
    )
  ),
  "Tableau_20" = list(
    `0` = c(
      "#4E79A7", "#F28E2B", "#59A14F", "#B6992D", "#499894",
      "#E15759", "#79706E", "#D37295", "#B07AA1", "#9D7660",
      "#A0CBE8", "#FFBE7D", "#8CD17D", "#F1CE63", "#86BCB6",
      "#FF9D9A", "#BAB0AC", "#FABFD2", "#D4A6C8", "#D7B5A6"
    ),
    `5` = c("#4E79A7", "#F28E2B", "#59A14F", "#B6992D", "#E15759")
  ),
  "Tableau_20pair" = list(
    `0` = c(
      "#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
      "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6",
      "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295",
      "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6"
    ),
    `10` = c(
      "#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
      "#8CD17D", "#B6992D", "#F1CE63", "#E15759", "#FF9D9A"
    )
  ),
  "calc" = list(
    `0` = c(
      "#004586", "#ff420e", "#ffd320", "#579d1c", "#7e0021",
      "#83caff", "#314004", "#aecf00", "#4b1f6f", "#ff950e",
      "#c5000b", "#0084d1"
    )
  ),
  "colorblind" = list(
    `0` = c(
      "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7"
    )
  ),
  "ppalette" = list(
    `0` = c(
      "#F7DC05", "#3d98d3", "#EC0B88", "#5e35b1", "#f9791e",
      "#3dd378", "#c6c6c6", "#444444"
    )
  ),
  "bpalette" = list(
    `0` = c(
      "#c62828", "#f44336", "#9c27b0", "#673ab7", "#3f51b5",
      "#2196f3", "#29b6f6", "#006064", "#009688", "#4caf50",
      "#8bc34a", "#ffeb3b", "#ff9800", "#795548", "#9e9e9e",
      "#607d8b"
    ),
    `2` = c("#c62828", "#009688"),
    `3` = c("#c62828", "#29b6f6", "#4caf50"),
    `5` = c("#c62828", "#29b6f6", "#4caf50", "#ffeb3b", "#3f51b5"),
    `10` = c(
      "#c62828", "#29b6f6", "#4caf50", "#ffeb3b", "#3f51b5",
      "#ff9800", "#2196f3", "#9c27b0", "#673ab7", "#795548"
    ),
    `13` = c(
      "#c62828", "#f44336", "#9c27b0", "#673ab7", "#3f51b5",
      "#2196f3", "#29b6f6", "#006064", "#009688", "#4caf50",
      "#8bc34a", "#ffeb3b", "#ff9800"
    )
  ),
  "OkabeIto_black" = list(
    `0` = c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
      "#D55E00", "#CC79A7", "#000000"
    )
  ),
  "Echo" = list(
    `0` = c(
      "#005C94", "#B50000", "#FF6600", "#804D00", "#009100",
      "#BA00FF", "#364E59", "#CCCCCC", "#2D2D2D", "#0084C8",
      "#DC0000", "#FF9900", "#B88100", "#9ADE00", "#D76CFF",
      "#9EABB0", "#666666", "#19AEFF", "#FF4141", "#FFFF3E",
      "#FFC022", "#CCFF42", "#F1CAFF", "#BDCDD4", "#0E232E"
    ),
    `3` = c("#005C94", "#B50000", "#009100"),
    `5` = c("#005C94", "#B50000", "#009100", "#F1CAFF", "#BDCDD4")
  ),
  "venn" = list(
    `0` = c(
      "#FCB4B4", "#FFCC66", "#BFE046", "#28C580", "#69A4F9",
      "#ACB9EA", "#C3C3C3"
    ),
    `2` = c("#FFBDC0", "#C7D4EE"),
    `3` = c("#FCB4B4", "#69A4F9", "#FFCC66"),
    `5` = c("#FCB4B4", "#69A4F9", "#FFCC66", "#BFE046", "#ACB9EA")
  ),
  "pie" = list(
    `0` = c(
      "#2e0a4a", "#7a1b6c", "#15ad68", "#ded531", "#db9421",
      "#14b5b5", "#ede893", "#76cfed", "#4599de", "#db2830",
      "#5F64AC", "#B271AD", "#eda4d3", "#d33c67", "#EC6925",
      "#a155f9", "#70F2D3", "#6FBA33", "#EDAC2E", "#096d42",
      "#4ec4b5", "#a36924", "#125fb2", "#7350EB", "#891a3a",
      "#bf109a", "#E8851F", "#e77def", "#4ebee5", "#69a4f9",
      "#f9cfa5", "#13D9B1", "#bfe046", "#DEB8A1", "#5B4232"
    )
  ),
  "box" = list(
    `0` = c(
      "#A2C8DC", "#F09594", "#2771A7", "#C6AFD1", "#D32421",
      "#831D20", "#3A9736", "#F3BB6F", "#A3A49E", "#5B4232"
    )
  ),
  "online1" = list(
    `0` = c(
      "#CE2930", "#76C5E1", "#D28F26", "#7A1B6C", "#21A465",
      "#D4CC38", "#2E0A4A", "#1FABAB", "#E6E191", "#458ECD",
      "#891B20", "#3A322F", "#C8CAC9", "#5F64AB", "#B172AC",
      "#E09FC5", "#D33C67", "#EB6924", "#A369F4", "#70F2D3",
      "#6EB933", "#EDAB2D", "#096D42", "#4EC4B5", "#A36924",
      "#7350EB", "#891a3a", "#bf109a", "#E8851F", "#e77def",
      "#4ebee5", "#69a4f9", "#f9cfa5", "#13D9B1", "#bfe046",
      "#bfac88", "#d2f7ba", "#24e4ed", "#A6E2BE", "#c0e9fc",
      "#f9efb4", "#fcb4b4", "#e5bcf7", "#acb9ea", "#a7dbd1",
      "#f2f225", "#c67997", "#87a822", "#d39279", "#5DA6E8"
    )
  ),
  "online2" = list(
    `0` = c(
      "#F0B142", "#62D6F6", "#F14149", "#28C580", "#FBF73E",
      "#B02F96", "#1CE8BF", "#F3854F", "#561BB5", "#C6C5E8",
      "#7ED8FA", "#FDF791", "#EFD53F", "#E46665", "#4846A6",
      "#BEDF50", "#46BBA8", "#acb9ea", "#A17817",
      "#A2E2B3", "#406BDE", "#0CB1ED", "#51EBFC", "#c4f98c",
      "#FFDC48", "#FEA8C3", "#F1835B", "#C4477A", "#821762",
      "#D544AE", "#50B5F5", "#90D1F4", "#A0F5F1", "#D1FCE4",
      "#54C4B5", "#D4E746", "#EDB349", "#FEE05B", "#F76C50",
      "#BA2D2D", "#3793DE", "#a7dbd1", "#ede893", "#76cfed",
      "#c67997", "#bfac88", "#d39279", "#9e516b", "#666b96"
    )
  ),
  "online3" = list(
    `0` = c(
      "#69a4f9", "#bfe046", "#EDAC2E", "#f2f225", "#a155f9",
      "#db2830", "#4ec4b5", "#4599de", "#E8851F", "#6FBA33",
      "#5F64AC", "#58B984", "#891a3a", "#bf109a", "#f7cb16",
      "#15ad68", "#14b5b5", "#4ebee5", "#1a497c", "#EC6925",
      "#096d42", "#7a1b6c", "#d33c67", "#db9421", "#ded531",
      "#521f84", "#87a822", "#43efce", "#11C2CC", "#fc68c7",
      "#e77def", "#76cfed", "#125fb2", "#a36924", "#c67997",
      "#B271AD", "#acb9ea", "#fcb4b4", "#f9cfa5", "#f9efb4",
      "#d2f7ba", "#8bd6d6", "#c5fcdd", "#c0e9fc", "#e5bcf7",
      "#eda4d3", "#d39279", "#d6beb2", "#e2a1ba", "#7350EB"
    )
  ),
  "tsne1" = list(
    `0` = c(
      "#00468B", "#5377A7", "#3B81AB", "#5C298F", "#6C6DA4",
      "#925E9F", "#AD729B", "#BF8099", "#D18E96", "#759EDD",
      "#76C8DC", "#0099B4", "#42C1BB", "#0F8074", "#438424",
      "#28AA6C", "#42B540", "#B8D24D", "#EDE447", "#FCCD94",
      "#FAB158", "#DCA30C", "#DD7C06", "#E39C94", "#FDAF91",
      "#E67E74", "#FF7777", "#FD0000", "#AD002A", "#792244",
      "#AF556B", "#AE8691", "#CE9573", "#B09F91", "#756455"
    ),
    `5` = c("#00468B", "#42B540", "#EDE447", "#759EDD", "#FF7777"),
    `10` = c(
      "#00468B", "#925E9F", "#0099B4", "#76D1B1", "#42B540",
      "#EDE447", "#FAB158", "#FF7777", "#AD002A", "#759EDD"
    ),
    `15` = c(
      "#00468B", "#925E9F", "#759EDD", "#0099B4", "#76D1B1",
      "#42B540", "#B8D24D", "#EDE447", "#FAB158", "#FF7777",
      "#FD0000", "#AD002A", "#AE8691", "#CE9573", "#756455"
    )
  ),
  "tsne2" = list(
    `0` = c(
      "#E4A436", "#BD685F", "#948689", "#6CA3B4", "#48B9D0",
      "#94DBE0", "#22ACA8", "#66A39D", "#1B766C", "#7CBA92",
      "#2073B7", "#83A5CD", "#A668AC", "#815A6C", "#D6B5B1",
      "#ED987F", "#703225", "#B37C79", "#9E93A7", "#8492B4",
      "#87A3B8", "#8BB4BB", "#8EC5BF", "#99BFB2", "#AB877D",
      "#DFA7A4", "#D31715", "#CE0D09", "#B5271D", "#9C4130",
      "#835B43", "#8E745D", "#CFC4B8", "#A28C74", "#D5CFAA"
    ),
    `5` = c("#E4A436", "#48B9D0", "#A668AC", "#8EC5BF", "#DFA7A4"),
    `10` = c(
      "#E4A436", "#BD685F", "#48B9D0", "#83A5CD", "#A668AC",
      "#815A6C", "#DFA7A4", "#8EC5BF", "#8E745D", "#CFC4B8"
    ),
    `14` = c(
      "#E4A436", "#BD685F", "#48B9D0", "#94DBE0", "#83A5CD",
      "#A668AC", "#815A6C", "#ED987F", "#8492B4", "#8EC5BF",
      "#D31715", "#835B43", "#A28C74", "#D5CFAA"
    )
  )
)

pal_continous <- list(
  "YlOrBr" = c(
    "#FFFFE5", "#FFF7BC", "#FEE391", "#FEC44F", "#FE9929",
    "#EC7014", "#CC4C02", "#993404", "#662506"
  ),
  "YlOrRd" = c(
    "#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C",
    "#FC4E2A", "#E31A1C", "#BD0026", "#800026"
  ),
  "YlGn" = c(
    "#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679",
    "#41AB5D", "#238443", "#006837", "#004529"
  ),
  "YlGnBu" = c(
    "#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4",
    "#1D91C0", "#225EA8", "#253494", "#081D58"
  ),
  "Reds" = c(
    "#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A",
    "#EF3B2C", "#CB181D", "#A50F15", "#67000D"
  ),
  "RdPu" = c(
    "#FFF7F3", "#FDE0DD", "#FCC5C0", "#FA9FB5", "#F768A1",
    "#DD3497", "#AE017E", "#7A0177", "#49006A"
  ),
  "Oranges" = c(
    "#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C",
    "#F16913", "#D94801", "#A63603", "#7F2704"
  ),
  "OrRd" = c(
    "#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59",
    "#EF6548", "#D7301F", "#B30000", "#7F0000"
  ),
  "Greys" = c(
    "#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696",
    "#737373", "#525252", "#252525", "#000000"
  ),
  "Greens" = c(
    "#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476",
    "#41AB5D", "#238B45", "#006D2C", "#00441B"
  ),
  "GnBu" = c(
    "#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4",
    "#4EB3D3", "#2B8CBE", "#0868AC", "#084081"
  ),
  "Blues" = c(
    "#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6",
    "#4292C6", "#2171B5", "#08519C", "#08306B"
  ),
  "BuGn" = c(
    "#F7FCFD", "#E5F5F9", "#CCECE6", "#99D8C9", "#66C2A4",
    "#41AE76", "#238B45", "#006D2C", "#00441B"
  )
)

pal_divergent <- list(
  "RdBu" = c(
    "#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
    "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC",
    "#053061"
  ),
  "RdYlBu" = c(
    "#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090",
    "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4",
    "#313695"
  ),
  "Spectral" = c(
    "#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B",
    "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD",
    "#5E4FA2"
  )
)



