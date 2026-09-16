# Fixtures for the spec module.
#
# testthat sources every helper-*.R into ONE shared environment, so every
# builder is named for its module. Never `make_base_spec()` or another generic
# name: two helpers defining the same name silently clobber each other and the
# breakage only shows in a combined devtools::test() run.

# A minimal spec that validates. Tests mutate one field at a time to check that
# the validator catches exactly that field, rather than hand-writing a whole
# broken spec per case.
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
    data = list(columns = list(spine = c(student_id = "student_id"))),
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
  do.call(stateELP::el_spec, base)
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
