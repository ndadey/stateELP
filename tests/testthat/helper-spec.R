# Fixtures for the spec module.
#
# testthat sources every helper-*.R into ONE shared environment, so every
# builder is named for its module. Never `make_base_spec()` or another generic
# name: two helpers defining the same name silently clobber each other and the
# breakage only shows in a combined devtools::test() run.

# A minimal FULL internal spec that validates (the shape el_spec() expands the
# short form into). Tests mutate one field at a time to check that
# validate_el_spec() catches exactly that field, rather than hand-writing a
# whole broken spec per case. Short-form tests build with el_spec() directly,
# from make_spec_short().
make_spec_base <- function(...) {
  base <- list(
    schema_version = "1.0.0",
    program = list(name = "Test", state = "TS"),
    elp = list(
      instrument = "Test ELP",
      components = list(
        listening = list(kind = "domain", measures = c("scale", "level")),
        speaking  = list(kind = "domain", measures = c("scale", "level")),
        reading   = list(kind = "domain", measures = c("scale", "level")),
        writing   = list(kind = "domain", measures = c("scale", "level")),
        overall   = list(kind = "composite", measures = c("scale", "level"),
                         from = c(listening = 0.25, speaking = 0.25,
                                  reading = 0.25, writing = 0.25))
      ),
      headline    = "overall",
      level_scale = list(type = "numeric", range = c(1, 6)),
      bands       = list(breaks = c(1, 3, 5, 7),
                         labels = c("low", "mid", "high")),
      grade_clusters = c("K", "1-12"),
      cuts = NULL, alternate = NULL
    ),
    policy = list(
      exit = list(
        pathways = list(
          list(name = "composite", rule = "overall level >= 4.5",
               terms = list(list(component = "overall", measure = "level",
                                 op = ">=", value = 4.5)),
               combine = "all", discretionary = FALSE)
        ),
        combination = "any_pathway",
        types = 1L,
        source = "provisional"
      ),
      timelines = list(max_years_to_exit = 6L, essa_timeline_years = 5L,
                       exit_dating = "first_former_year",
                       initial_elp_source = "first_annual",
                       source = "provisional"),
      suppression = list(min_cell = 10L, source = "provisional")
    ),
    academic = list(
      ELA = list(grades = as.character(3:8), cuts = c(700, 735, 750, 790),
                 op = ">=", source = "provisional")
    ),
    data = list(columns = list(roster = c(STUDENT_ID = "STUDENT_ID"))),
    verification = list(status = "synthetic"),
    source_documents = list()
  )
  # A test overrides one leaf -- make_spec_base(policy = list(exit =
  # list(types = 6L))) -- and everything else stays valid. Setting a leaf to
  # NULL deletes it, which is how a test checks a "required field missing" path.
  #
  # Not utils::modifyList(): it merges recursively into UNNAMED lists too, so
  # an override of `pathways` (a list of pathways, unnamed) is silently
  # dropped and the base rule survives. Recurse only where both sides are
  # named lists; replace outright otherwise.
  base <- spec_merge(base, list(...))
  do.call(.el_spec_full, base)
}

# ELPA21 as a domain profile (Type 6): every domain at the top of the declared
# scale, a waived domain exempt. The shipped ELPA21 spec writes its rule as the
# vendor's Proficient determination (Type 1), so tests of the Type-gated
# Additional checks build the profile here. Domains and the top level come
# from the spec, never typed.
make_spec_elpa21_profile <- function() {
  spec <- read_el_spec("00_ELPA21_EL_Spec")
  top <- length(spec$elp$level_scale$levels)
  spec$policy$exit$pathways[[1]]$terms <- lapply(
    spec_components(spec, "domain"), function(d) {
      list(component = d, measure = "level", op = ">=", value = top,
           exempt_if_absent = TRUE)
    })
  spec$policy$exit$types <- 6L
  validate_el_spec(spec)
  spec
}

# A minimal SHORT-form spec, as a list of el_spec() arguments; a test
# overrides one argument and calls do.call(el_spec, ...).
make_spec_short <- function(...) {
  args <- list(
    id = "TS", status = "synthetic",
    elp = list(test = "Test ELP", score = "overall", levels = c(1, 6)),
    exit = c(overall = 4.5),
    timelines = list(max = 6)
  )
  over <- list(...)
  args[names(over)] <- over
  args
}

spec_merge <- function(x, val) {
  for (nm in names(val)) {
    v <- val[[nm]]
    both_named <- is.list(v) && !is.null(names(v)) &&
      is.list(x[[nm]]) && !is.null(names(x[[nm]]))
    x[[nm]] <- if (both_named) spec_merge(x[[nm]], v) else v
  }
  x
}
