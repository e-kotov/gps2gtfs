#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

#ifdef HAS_RUST
extern "C" {
  void R_init_gps2gtfs_extendr(void *dll);
  void register_extendr_panic_hook(void);
}
#endif

// [[Rcpp::init]]
void my_init(DllInfo *dll) {
#ifdef HAS_RUST
  register_extendr_panic_hook();
  // Do NOT call R_init_gps2gtfs_extendr(dll) because RcppExports.cpp
  // already includes the Rust wrap__ symbols in its CallEntries table.
  // Calling R_init_gps2gtfs_extendr would call R_registerRoutines again
  // and overwrite the Rcpp symbols registration.
#endif
}

// [[Rcpp::export]]
bool is_rust_compiled_cpp() {
#ifdef HAS_RUST
    return true;
#else
    return false;
#endif
}

#ifndef HAS_RUST
extern "C" {
    SEXP wrap__assign_trip_ids_rust(SEXP bus_stops, SEXP dates, SEXP device_ids) {
      Rf_error("Rust backend is not compiled. Please install Cargo/Rust and reinstall the package.");
      return R_NilValue;
    }
    SEXP wrap__match_points_to_buffers_rust(SEXP gps_lat, SEXP gps_lon, SEXP target_lat, SEXP target_lon, SEXP target_ids, SEXP radius1, SEXP radius2, SEXP use_haversine) {
      Rf_error("Rust backend is not compiled. Please install Cargo/Rust and reinstall the package.");
      return R_NilValue;
    }
    SEXP wrap__propagate_trip_ids_rust(SEXP trip_ids) {
      Rf_error("Rust backend is not compiled. Please install Cargo/Rust and reinstall the package.");
      return R_NilValue;
    }
}
#endif

//' Haversine Distance in C++
//'
//' Calculates the great-circle distance between two points on a sphere (WGS-84).
//'
//' @param lat1 Numeric. Latitude of first point.
//' @param lon1 Numeric. Longitude of first point.
//' @param lat2 Numeric. Latitude of second point.
//' @param lon2 Numeric. Longitude of second point.
//' @return Numeric. Geodesic distance in meters.
//' @noRd
// [[Rcpp::export]]
double haversine_distance_cpp(double lat1, double lon1, double lat2, double lon2) {
  double r = 6371000.0; // Earth mean radius in meters
  double phi1 = lat1 * M_PI / 180.0;
  double phi2 = lat2 * M_PI / 180.0;
  double delta_phi = (lat2 - lat1) * M_PI / 180.0;
  double delta_lambda = (lon2 - lon1) * M_PI / 180.0;

  double a = sin(delta_phi / 2.0) * sin(delta_phi / 2.0) +
             cos(phi1) * cos(phi2) * sin(delta_lambda / 2.0) * sin(delta_lambda / 2.0);
  double c = 2.0 * atan2(sqrt(a), sqrt(1.0 - a));
  return r * c;
}

//' Match GPS Points to Spatial Buffers in C++
//'
//' Performs fast distance matching from GPS points to stop/terminal buffers using the Haversine formula.
//'
//' @param gps_lat NumericVector of GPS latitudes.
//' @param gps_lon NumericVector of GPS longitudes.
//' @param target_lat NumericVector of target latitudes.
//' @param target_lon NumericVector of target longitudes.
//' @param target_ids CharacterVector of target IDs.
//' @param radius1 Numeric. Standard radius in meters.
//' @param radius2 Numeric. Extended radius in meters (set to 0 to ignore).
//' @return CharacterVector of matched target IDs (NA if unmatched).
//' @noRd
// [[Rcpp::export]]
CharacterVector match_points_to_buffers_cpp(NumericVector gps_lat, NumericVector gps_lon,
                                             NumericVector target_lat, NumericVector target_lon,
                                             CharacterVector target_ids, double radius1, double radius2,
                                             bool use_haversine = true) {
  int n = gps_lat.size();
  int m = target_lat.size();
  CharacterVector result(n);

  for (int i = 0; i < n; ++i) {
    if (!std::isfinite(gps_lat[i]) || !std::isfinite(gps_lon[i])) {
      result[i] = CharacterVector::get_na();
      continue;
    }

    String matched_id = CharacterVector::get_na();
    bool found = false;

    // Check radius1 first
    for (int j = 0; j < m; ++j) {
      double dist;
      if (use_haversine) {
        dist = haversine_distance_cpp(gps_lat[i], gps_lon[i], target_lat[j], target_lon[j]);
      } else {
        double dx = gps_lon[i] - target_lon[j];
        double dy = gps_lat[i] - target_lat[j];
        dist = sqrt(dx * dx + dy * dy);
      }
      if (dist <= radius1) {
        matched_id = target_ids[j];
        found = true;
        break;
      }
    }

    // Check radius2 if not found and radius2 is set
    if (!found && radius2 > 0) {
      for (int j = 0; j < m; ++j) {
        double dist;
        if (use_haversine) {
          dist = haversine_distance_cpp(gps_lat[i], gps_lon[i], target_lat[j], target_lon[j]);
        } else {
          double dx = gps_lon[i] - target_lon[j];
          double dy = gps_lat[i] - target_lat[j];
          dist = sqrt(dx * dx + dy * dy);
        }
        if (dist <= radius2) {
          matched_id = target_ids[j];
          break;
        }
      }
    }

    result[i] = matched_id;
  }

  return result;
}

//' Propagate Trip IDs
//'
//' Propagates trip IDs forward from the terminal exit point to the terminal entry point.
//'
//' @param trip_ids IntegerVector of initial trip IDs (with 0s for intermediate points).
//' @return IntegerVector of propagated trip IDs.
//' @noRd
// [[Rcpp::export]]
IntegerVector propagate_trip_ids_cpp(IntegerVector trip_ids) {
  int n = trip_ids.size();
  IntegerVector result(n);
  int current_trip = 0;

  for (int i = 0; i < n; ++i) {
    if (trip_ids[i] != 0) {
      if (current_trip == 0) {
        // Start of a trip
        current_trip = trip_ids[i];
      } else if (trip_ids[i] == current_trip) {
        // End of the current trip
        result[i] = current_trip;
        current_trip = 0;
        continue;
      }
    }
    result[i] = current_trip;
  }
  return result;
}

//' Assign Trip IDs
//'
//' Pairs consecutive terminal exits and entries for the same device, date, and different terminals.
//'
//' @param bus_stops CharacterVector of terminal IDs.
//' @param dates CharacterVector of dates (as strings).
//' @param device_ids CharacterVector of device IDs.
//' @return IntegerVector of assigned trip IDs (with 0 for unmatched).
//' @noRd
// [[Rcpp::export]]
IntegerVector assign_trip_ids_cpp(CharacterVector bus_stops, CharacterVector dates, CharacterVector device_ids) {
  int n = bus_stops.size();
  IntegerVector trip_ids(n, 0);
  int trip_counter = 0;

  for (int i = 0; i < n - 1; ++i) {
    if (bus_stops[i] != bus_stops[i+1] &&
        dates[i] == dates[i+1] &&
        device_ids[i] == device_ids[i+1]) {
      trip_counter++;
      trip_ids[i] = trip_counter;
      trip_ids[i+1] = trip_counter;
      // Skip the next index since it's already paired
      i++;
    }
  }
  return trip_ids;
}
