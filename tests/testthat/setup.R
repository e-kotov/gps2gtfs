# The extraction coverage warning is intentional default behavior, but the
# fixtures deliberately drop coverage, so silence it suite-wide to keep test
# output readable. Tests that exercise the warning call the diagnostics
# helpers directly (see test-diagnostics.R).
options(gps2gtfs.diagnostics_warn = FALSE)
