###########################################################
# Small Generic Functions for Eval and Testing
###########################################################

parser <- function(a){eval(parse(text = a))}

parser_lst <- function(a){
  # only split at commas without " X" following
  # to avoid splitting, eg, "Poisson (Exp, X)"
  as.list(strsplit(perl=TRUE, a, ",(?! X)")[[1]])
}

parser_vec <- function(a){
  as.vector(strsplit(a,",")[[1]])
}

isnothing <- function(x) {
  if(!is.null(x)){is.na(x)|is.nan(x)} else (is.null(x))
}

capitalizeStr <- function(str){

  firstChar <- substr(str,1,1)
  chartr(firstChar, toupper(firstChar), str)

}

sciNotTex <- function(a){
  tmp <- sprintf("%.1e", a)
  exp <- stringr::str_sub(tmp, -3, -1)
  base <- stringr::str_sub(tmp,1,3)
  paste0("{ \\small ",base,"\\text{e}^{",exp," }}")
}

roundOrShrink <- function(a){
  # are there 2 significant digits
  if((abs(round(a,2) - 0) > 1e-5 &
      abs(round(a/1e5,2) - 0) < 1e-5) || a == 0){return(round(a,2))} else{sciNotTex(a)}
}

QOISwitcher <- function(ns,distrConfig,selectedQOI){

  #idx <- which(distrDF$distrList==distrID)
  f <- parser_lst(distrConfig$QOIList)

  if(length(distrConfig) > 0){div(selectInput(
    inputId = ns("QOIid"), label = div(tags$p(tags$b("Quantity of Interest"),
                                          style = "font-size:15px; !important")),
    choices = f, selected = selectedQOI, width = "200px"),
    style = "padding-top:10px;", class = "simInput")
  } else(stop("Unknown Distribution!"))

}

handMLESwitcher <- function(distrID,distrDF,...){
  idx <- which(distrDF$distrList==distrID)

  if(length(idx) > 0){f <- if(
    distrDF$distrGroup[[idx]] %in% c("Bernoulli","Ordered", "Poisson") ){
    histAndDensityDiscrete } else {histAndDensity}
  return(f(...) )} else(stop("Unknown Distribution!"))

}

############################################################
# Tooltip maker
############################################################
helperMaker <- function(str, styleArg = ""){

  # An unholy hybrid of icons inspired by shinyhelper package
  # and popovers from good ol bootstrap
  # TODO: can we have this also trigger a rintrojs option for eg.
  # the probability model which is long
  withMathJax(div(
    tags$script(
      paste0("
      $('.shinyhelper-container').click(function(event){
        $(this).find('*').on('shown.bs.popover', function () {
          MathJax.Hub.Queue([\"Typeset\",MathJax.Hub]);
        });
      });"),
    ),
    shinyBS::popify(a(
        class = "helpercirc",
              icon(
                name = "circle-info",
                class = "shinyhelper-icon", verify_fa = F),
        tabindex = 0),
      title = str,
      content = HTML(
        (dplyr::filter(pkgEnv$tutorialText,Name == str))$content),
      placement = "right", trigger = "focus",
      options =  list(container = "body")
    ),
    class = "shinyhelper-container",
    style = styleArg,

  ))
}

helperMakerNavbar <- function(str, styleArg = ""){
  # divID <-  gsub(fixed = T,")", "",gsub(fixed = T,"(", "",gsub(" ", "", str)))
  withMathJax(div(
    class = "shinyhelper-container-navbar",
    # id = divID,
    tags$script(
      paste0("
      $('a.disabled').click(function(event){
        $(this).find('*').popover('show');
        $(this).find('*').on('shown.bs.popover', function () {
        MathJax.Hub.Queue([\"Typeset\",MathJax.Hub]);
        });
      });
      $('.shinyhelper-container-navbar').click(function(event){
        $(this).children().popover('show');
        $(this).children().on('shown.bs.popover', function () {
        MathJax.Hub.Queue([\"Typeset\",MathJax.Hub]);
        });
        event.preventDefault();
        event.stopPropagation();
      });
      $(document).click(function(event) {
      var $target = $(event.target);
      if(!$target.closest('a.disabled').length) {
      $('.shinyhelper-container-navbar').children().popover('hide');
      }});"),
    ),
    shinyBS::popify(
      a(
        class = "helpercirc-navbar", icon(
          name = "circle-info",
          class = "shinyhelper-icon-navbar", verify_fa = F),
        tabindex = 0),
      title = str,
      content = HTML(
        (dplyr::filter(pkgEnv$tutorialText, Name == str))$content),
      placement = "bottom", trigger = "click",
      options =  list(container = "body")
    ),
    style = styleArg
  ))
}


introBox <- function (..., data.step, data.intro, data.hint, data.title = "", data.position = c("bottom",
                                                                                                "auto", "top", "left", "right", "bottom", "bottom-left_aligned",
                                                                                                "bottom-middle-aligned", "bottom-right-aligned", "auto"))
{
  stopifnot(!((!missing(data.step) & missing(data.intro)) |
                (missing(data.step) & !missing(data.intro))))
  data.position <- match.arg(data.position)
  data <- match.call(expand.dots = TRUE)
  n <- length(list(...)) + 1
  names(data)[-seq_len(n)] <- gsub("\\.", "-", names(data)[-seq_len(n)])
  data[[1]] <- quote(shiny::tags$div)
  eval.parent(data)
}
