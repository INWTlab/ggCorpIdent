#' Set Corporate Identity Theme for ggplot2 graphics
#'
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:theme_get]{theme_set}} and \code{\link[ggplot2:ggtheme]{theme_bw}}. In addition
#' to the specification of colors and fonts, a corporate logo (format: PNG) can be
#' specified to serve as watermark in ggplot graphics.
#' @inheritParams ggplot2::theme_bw
#' @param textColor text color for axis, labs, title and subtitle
#' @param panelBackgroundColor color for panel background
#' @param tickColor color for ticks
#' @param legendKeyColor color for legend key
#' @param panelGridMajorColor color for panel grid major
#' @param stripTextColor color for strip text
#' @param colors vector with corporate colors used for geoms; the colors are used
#' according to their position in \code{colors}; if more colors are required, the values
#' of \code{colors} will be interpolated via \code{\link[grDevices:colorRamp]{colorRampPalette}}.
#' @param logo filename for logo; the file has to be \code{PNG}.
#' @param logoSize numeric for logo rescaling
#' @param logoPosition location of logo. See 'Details'.
#' @param logoTransparency numeric alpha value to adjust logo transparency
#' @details The location may be specified by a single keyword from the
#' list \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"},
#' \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} and \code{"center"}.
#' @examples
#' \dontrun{
#' library(ggCorpIdent)
#' setThemeGgplot2()
#'
#' # default 'ggCorpIdent' settings:
#' ggplot(iris,
#'        aes(x = Sepal.Width, y = Petal.Width, col = Species)) +
#'   geom_point()
#'
#' # add corporate logo
#' logoFromPackage <- system.file("logo/logo_INWT.png", package = "ggCorpIdent", mustWork = TRUE)
#' ggCorpIdent(logo = logoFromPackage,
#'             logoSize = 0.4)
#' ggplot(iris,
#'        aes(x = Sepal.Width, y = Petal.Width, col = Species)) +
#'   geom_point()
#'
#' # change colors and font
#' ggCorpIdent(base_family = "Courier",
#'             colors = c("dodgerblue4", "violetred1", "mintcream"),
#'             logo = logoFromPackage,
#'             logoSize = 0.4) 
#' ggplot(iris,
#'        aes(x = Sepal.Width, y = Petal.Width, col = Species)) +
#'   geom_point()
#' }
#' @export
ggCorpIdent <- function(base_size = 12,
                        base_family = "Lato",
                        textColor = "#2b4894",
                        panelBackgroundColor = "#d4dbde",
                        tickColor = "white",
                        legendKeyColor = "white",
                        panelGridMajorColor = "white",
                        stripTextColor = "white",
                        colors = c("#2b4894",
                                   "#068587",
                                   "#F2B134",
                                   "#ED553B",
                                   "#4FB99F"),
                        logo = NULL,
                        logoPosition = "bottomright",
                        logoSize = 1,
                        logoTransparency = 0.2){
  
  if (!base_family %in% fonts()) {
    message("Font family `", base_family, "` not available. font family remains unchanged.\n",
            "Pleae use `extrafont::font_import` to install required fonts first.\n", sep = "")
    theme_set(
      theme_bw(base_size = base_size))  
  } else {
    theme_set(
      theme_bw(base_size = base_size,
               base_family = base_family))
  }
  
  
  theme_replace(plot.margin = unit(c(1, 1, 1, 1), "cm"),
                plot.title = element_text(colour = textColor,
                                          hjust = 0,
                                          margin = margin(b = 0.2, unit = "cm"),
                                          face = "bold"),
                plot.subtitle = element_text(colour = textColor,
                                             hjust = 0,
                                             margin = margin(b = 0.2, unit = "cm"),
                                             face = "italic"),
                axis.ticks.x = element_line(colour = tickColor),
                axis.ticks.y = element_line(colour = tickColor),
                axis.title.x = element_text(colour = textColor,
                                            face = "italic",
                                            margin = margin(t = 0.5, unit = "cm")),
                axis.title.y = element_text(colour = textColor,
                                            face = "italic",
                                            angle = 90,
                                            margin = margin(r = 0.5, unit = "cm")),
                axis.text = element_text(colour = textColor),
                axis.text.x = element_text(colour = textColor),
                axis.text.y = element_text(colour = textColor),
                legend.text = element_text(colour = textColor),
                legend.title = element_text(colour = textColor),
                legend.key = element_rect(size = 2,
                                          fill = panelBackgroundColor,
                                          color = legendKeyColor),
                legend.key.size = unit(1.5, 'lines'),
                panel.grid.major = element_line(colour = panelGridMajorColor),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = panelBackgroundColor,
                                                colour = panelBackgroundColor),
                panel.border = element_rect(colour = panelBackgroundColor, fill = NA),
                strip.background = element_rect(fill = textColor, colour = textColor),
                strip.text = element_text(colour = stripTextColor, face = "bold")
  )
  # Colors
  options(ggCorpIdentColors = colors)
  
  update_geom_defaults("point", list(colour = paletteFun(1),
                                     fill = paletteFun(1),
                                     shape = 16,
                                     size = 3))
  
  update_geom_defaults("line", list(colour = paletteFun(1),
                                    fill = paletteFun(1),
                                    shape = 16,
                                    size = 1))
  
  update_geom_defaults("segment", list(colour = paletteFun(1),
                                       fill = paletteFun(1),
                                       size = 1))
  
  update_geom_defaults("bar", list(colour = paletteFun(1),
                                   fill = paletteFun(1),
                                   size = 1))
  
  update_geom_defaults("step", list(colour = paletteFun(1),
                                    fill = paletteFun(1),
                                    size = 1))
  
  update_geom_defaults("boxplot", list(colour = "black",
                                       fill = paletteFun(1),
                                       size = 1))
  
  update_geom_defaults("ribbon", list(colour = paletteFun(1),
                                      fill = paletteFun(1),
                                      size = 1))
  
  update_geom_defaults("rect", list(colour = paletteFun(1),
                                    fill = paletteFun(1),
                                    size = 1))
  
  update_geom_defaults("errorbar", list(colour = paletteFun(1),
                                        fill = paletteFun(1),
                                        size = 1))
  
  
  
  if (!is.null(logo)){
    
    # Store logo information in options
    if (!is.null(formerFileName <- getOption("ggCorpIdentLogoFilename")) && formerFileName != logo){
      message("Former logo `", formerFileName, "` will be replaced with `", logo, "`.\n", sep = "")
    }
    options(ggCorpIdentLogoFilename = logo,
            ggCorpIdentLogoSize = logoSize,
            ggCorpIdentLogoPosition = logoPosition,
            ggCorpIdentLogoTransparency = logoTransparency)
  }
}


#' Evenly spaced INWT colours for discrete data
#'
#' @description Default color scale for categorical variables with INWT colors.
#' @param ... arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_colour_discrete <- function (...) {
  discrete_scale("colour", "brewer", palette = paletteFun, ...)
  }


#' Evenly spaced INWT colours for discrete data
#'
#' @description Default color scale for categorical variables with INWT colors.
#' @param ... arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @export
scale_fill_discrete <- function (...) {
  discrete_scale("fill", "brewer", palette = paletteFun, ...)
  }


#' Add INWT Logo
#'
#' Einbinden des INWT Logos als Wasserzeichen in ggplot Grafiken. \strong{Wichtig:}
#' Die Funktion \code{addLogo} sollte direkt im Anschluss an den Aufruf von
#' \code{\link[ggplot2]{ggplot}} erfolgen (s. Beispiele), damit das Logo als Layer hinter den Daten
#' eingebunden wird und nicht die Daten überlagert.
#' @param alpha numeric; Transparenz (s. \code{\link[grDevices]{rgb}})
#' @param position character; Position des Logos (s. Details)
#' @param size numeric; Werte zwischen [0; 1]. Ein Wert von 1 entspricht der
#' maximal möglichen Ausdehung, so dass sich das Logo (inkl. etwas Rand) über den
#' maximalen Bereich der Grafik erstreckt, ohne abgeschnitten zu werden.
#'
#' @details The position may be specified by keywords from the list \code{"default"},
#' \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"},
#' \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"}, \code{"center"}
#' and \code{"full"}. Setting \code{"full"} corresponds to \code{addLogo(position = "full", size = 1)}.
#' \code{"deafult")} ist die Verwendung des Logos wie auf der Vorderseite unserer Visitenkarten.
addLogo <- function(position = getOption("ggCorpIdentLogoPosition"),
                    alpha = 0.1,
                    size = getOption("ggCorpIdentLogoSize")){
  
  # Vorberechung der Positionen
  min <- 0
  max <- 1
  center <- (max - min) / 2
  top <- max
  bottom <- min
  right <- max
  left <- min
  
  pos <- switch(position,
                default = list(x = right,
                               y = bottom,
                               just = c(0.525, 0.5),
                               size = size * 2),
                center = list(x = center,
                              y = center,
                              just = c(0.5, 0.5)),
                top = list(x = center, y = top, just = c(0.5, 1),
                           size = size),
                topleft = list(x = left, y = top, just = c(0, 1),
                               size = size),
                topright = list(x = right, y = top, just = c(1, 1),
                                size = size),
                bottom = list(x = center, y = bottom, just = c(0.5, 0),
                              size = size),
                bottomleft = list(x = left, y = bottom, just = c(0, 0),
                                  size = size),
                bottomright = list(x = right,
                                   y = bottom,
                                   just = c(1, 0),
                                   size = size),
                right = list(x = right, y = center, just = c(1, 0.5),
                             size = size),
                left = list(x = left, y = center, just = c(0, 0.5),
                            size = size),
                full = list(x = center,
                            y = center,
                            just = c(0.5, 0.5),
                            size = 1))
  
  
  logo <- readPNG(getOption("ggCorpIdentLogoFilename"))
  
  # nolint start
  logo <- matrix(rgb(logo[,, 1],
                     logo[,, 2],
                     logo[,, 3],
                     logo[,, 4] * getOption("ggCorpIdentLogoTransparency")), # adjust alpha
                 # nolint end
                 nrow = dim(logo)[1])
  
  annotation_custom(rasterGrob(logo, x = pos$x,
                               y = pos$y,
                               just = pos$just,
                               height = pos$size))
}


#' INWT-specific ggplot function
#'
#' @description Works exactly like \code{\link[ggplot2]{ggplot}} but adds INWT
#' logo via \code{\link{addLogo}} by default. If you don't want a logo, use
#' \code{logo = FALSE} or (to affect the whole document)
#' \code{options(inwt.styles.logo = FALSE)}.
#' @param ... arguments to be passed to \code{\link[ggplot2]{ggplot}}
#' @param logo logical: show INWT logo?
#' @param logoParams list: arguments passed to \code{\link{addLogo}}
#' @export
ggplot <- function(...,
                   logo = getOption("ggCorpIdentLogoFilename"),
                   logoParams = list()) {
  if (!is.null(logo)) {
    ggplot2::ggplot(...) + do.call(addLogo, logoParams)
  } else {
    ggplot2::ggplot(...)
  }
}


#' Color palette
#'
#' @description Vector of color codes. If the number of requested colors
#' exceeds the number of available colors, interpolated colors are added.
#'
#' @param x integer: desired number of colors
#' @export
paletteFun <- function(x) {
  colors <- getOption("ggCorpIdentColors")
  if (x > length(colors)) colors <- colorRampPalette(colors)(x)
  return(colors[1:x])
}
