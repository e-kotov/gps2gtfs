# the coverage warning and diagnostics print are stable

    Code
      cat(diagnostics_warning_message(diag))
    Output
      gps2gtfs extraction lost coverage: kept 6 trips from 2,167 input pings. Dropped pings_dropped_not_in_trip (686), pings_dropped_zero_coord (28). Inspect the full coverage table with g2g_diagnostics(result) (also attr(result, "diagnostics")). Silence with diagnostics_warn = FALSE or options(gps2gtfs.diagnostics_warn = FALSE).

---

    Code
      print(diag)
    Output
      <gps2gtfs extraction diagnostics>
      2,167 pings in -> 6 trips, 81 stop_times
        dropped:
          pings_dropped_not_in_trip: 686
          pings_dropped_duplicate: 56
          pings_dropped_zero_coord: 28
      
                 stage                        metric     n
                <char>                        <char> <int>
       1:        input                      pings_in  2167
       2:     cleaning      pings_dropped_zero_coord    28
       3:     cleaning pings_dropped_missing_vehicle    NA
       4:     cleaning       pings_dropped_duplicate    56
       5:     cleaning          pings_after_cleaning  2083
       6: segmentation rows_dropped_no_trip_identity    NA
       7: segmentation  segments_dropped_single_ping    NA
       8: segmentation   segments_dropped_stationary    NA
       9:        trips       pings_assigned_to_trips  1397
      10:        trips     pings_dropped_not_in_trip   686
      11:        trips                    trips_kept     6
      12:        trips        max_trip_duration_mins    NA
      13:        stops               stop_times_kept    81

