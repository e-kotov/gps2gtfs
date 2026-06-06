use extendr_api::prelude::*;

// Haversine distance helper
fn haversine_distance(lat1: f64, lon1: f64, lat2: f64, lon2: f64) -> f64 {
    let r = 6371000.0; // Earth mean radius in meters
    let phi1 = lat1.to_radians();
    let phi2 = lat2.to_radians();
    let delta_phi = (lat2 - lat1).to_radians();
    let delta_lambda = (lon2 - lon1).to_radians();

    let a = (delta_phi / 2.0).sin().powi(2)
        + phi1.cos() * phi2.cos() * (delta_lambda / 2.0).sin().powi(2);
    let c = 2.0 * a.sqrt().atan2((1.0 - a).sqrt());

    r * c
}

/// Match GPS Points to Spatial Buffers in Rust
///
/// @param gps_lat Numeric vector of GPS latitudes.
/// @param gps_lon Numeric vector of GPS longitudes.
/// @param target_lat Numeric vector of target latitudes.
/// @param target_lon Numeric vector of target longitudes.
/// @param target_ids Character vector of target IDs.
/// @param radius1 Numeric. Standard radius in meters.
/// @param radius2 Numeric. Extended radius in meters (set to 0 to ignore).
/// @param use_haversine Logical. Use Haversine distance (true) or Euclidean/Cartesian (false).
/// @noRd
#[extendr]
fn match_points_to_buffers_rust(
    gps_lat: Vec<f64>,
    gps_lon: Vec<f64>,
    target_lat: Vec<f64>,
    target_lon: Vec<f64>,
    target_ids: Vec<String>,
    radius1: f64,
    radius2: f64,
    use_haversine: bool,
) -> Vec<Option<String>> {
    let n = gps_lat.len();
    let m = target_lat.len();

    let mut result = Vec::with_capacity(n);

    for i in 0..n {
        let lat = gps_lat[i];
        let lon = gps_lon[i];

        if !lat.is_finite() || !lon.is_finite() {
            result.push(None);
            continue;
        }

        let mut matched_id = None;

        // Check radius1 first
        for j in 0..m {
            let dist = if use_haversine {
                haversine_distance(lat, lon, target_lat[j], target_lon[j])
            } else {
                let dx = lon - target_lon[j];
                let dy = lat - target_lat[j];
                (dx * dx + dy * dy).sqrt()
            };
            if dist <= radius1 {
                matched_id = Some(target_ids[j].clone());
                break;
            }
        }

        // Check radius2 if not found and radius2 is set
        if matched_id.is_none() && radius2 > 0.0 {
            for j in 0..m {
                let dist = if use_haversine {
                    haversine_distance(lat, lon, target_lat[j], target_lon[j])
                } else {
                    let dx = lon - target_lon[j];
                    let dy = lat - target_lat[j];
                    (dx * dx + dy * dy).sqrt()
                };
                if dist <= radius2 {
                    matched_id = Some(target_ids[j].clone());
                    break;
                }
            }
        }

        result.push(matched_id);
    }

    result
}

/// Propagate Trip IDs in Rust
///
/// @param trip_ids Integer vector of initial trip IDs.
/// @noRd
#[extendr]
fn propagate_trip_ids_rust(trip_ids: Vec<i32>) -> Vec<i32> {
    let n = trip_ids.len();
    let mut result = Vec::with_capacity(n);
    let mut current_trip = 0;

    for i in 0..n {
        let val = trip_ids[i];
        if val != 0 {
            if current_trip == 0 {
                current_trip = val;
            } else if val == current_trip {
                result.push(current_trip);
                current_trip = 0;
                continue;
            }
        }
        result.push(current_trip);
    }

    result
}

/// Assign Trip IDs in Rust
///
/// @param bus_stops Character vector of terminal IDs.
/// @param dates Character vector of dates.
/// @param device_ids Character vector of device IDs.
/// @noRd
#[extendr]
fn assign_trip_ids_rust(
    bus_stops: Vec<String>,
    dates: Vec<String>,
    device_ids: Vec<String>,
) -> Vec<i32> {
    let n = bus_stops.len();
    let mut trip_ids = vec![0; n];
    let mut trip_counter = 0;

    if n < 2 {
        return trip_ids;
    }

    let mut i = 0;
    while i < n - 1 {
        if bus_stops[i] != bus_stops[i + 1]
            && dates[i] == dates[i + 1]
            && device_ids[i] == device_ids[i + 1]
        {
            trip_counter += 1;
            trip_ids[i] = trip_counter;
            trip_ids[i + 1] = trip_counter;
            i += 2;
        } else {
            i += 1;
        }
    }

    trip_ids
}

extendr_module! {
    mod gps2gtfs;
    fn match_points_to_buffers_rust;
    fn propagate_trip_ids_rust;
    fn assign_trip_ids_rust;
}
