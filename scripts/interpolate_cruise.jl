#!/usr/bin/env julia

using Pkg
Pkg.activate(joinpath(@__DIR__, "..", "julia"))
Pkg.instantiate()

using CSV
using DataFrames
using DIVAnd
using Statistics

const MAPPING_CSV = joinpath(@__DIR__, "..", "data", "ctd_datasetid_cruisename_stationname_mapping.csv")
const CLEAN_ROOT = joinpath(@__DIR__, "..", "data", "02_clean")
const INTERP_ROOT = joinpath(@__DIR__, "..", "data", "interpolated")
const HORIZ_RES_DEG = 0.03
const DEPTH_RES_M = 4.0
const MIN_GRID_POINTS = 30
const MAX_GRID_POINTS = 100
const MAX_GRID_POINTS_TOTAL = 120_000

function grid_size(span, resolution, min_points, max_points)
    n = max(min_points, round(Int, span / resolution) + 1)
    min(n, max_points)
end

function capped_grid_sizes(lon_span, lat_span, depth_span)
    n_lon = grid_size(lon_span, HORIZ_RES_DEG, MIN_GRID_POINTS, MAX_GRID_POINTS)
    n_lat = grid_size(lat_span, HORIZ_RES_DEG, MIN_GRID_POINTS, MAX_GRID_POINTS)
    n_depth = grid_size(depth_span, DEPTH_RES_M, MIN_GRID_POINTS, MAX_GRID_POINTS)

    total = n_lon * n_lat * n_depth
    if total > MAX_GRID_POINTS_TOTAL
        scale = (MAX_GRID_POINTS_TOTAL / total)^(1 / 3)
        n_lon = max(MIN_GRID_POINTS, round(Int, n_lon * scale))
        n_lat = max(MIN_GRID_POINTS, round(Int, n_lat * scale))
        n_depth = max(MIN_GRID_POINTS, round(Int, n_depth * scale))
    end

    (n_lon, n_lat, n_depth)
end

function read_ctd_mapping(csv_path::AbstractString)
    DataFrame(CSV.File(csv_path))
end

function unique_cruise_ids(mapping::DataFrame)
    sort(unique(mapping.cruise_id))
end

function clean_files_for_cruise(cruise_id::AbstractString, clean_root::AbstractString)
    prefix = "SFER_CTD_$(cruise_id)_"
    files = String[]
    for file in readdir(clean_root)
        if startswith(file, prefix) && endswith(file, ".csv") && file != "processing_log.csv"
            push!(files, joinpath(clean_root, file))
        end
    end
    sort(files)
end

function parse_float64(value)
    if value isa Number
        val = Float64(value)
        return isnan(val) ? missing : val
    elseif value isa AbstractString
        stripped = strip(value)
        if isempty(stripped)
            return missing
        end
        parsed = tryparse(Float64, stripped)
        if parsed === nothing || isnan(parsed)
            return missing
        end
        return parsed
    end
    return missing
end

function load_cruise_observations(cruise_id::AbstractString, clean_root::AbstractString)
    files = clean_files_for_cruise(cruise_id, clean_root)
    if isempty(files)
        error("No cleaned CTD files found for $cruise_id under $clean_root. Run `make process` first.")
    end

    dfs = DataFrame[]
    for file in files
        df = DataFrame(CSV.File(file))
        if !("cruise_id" in names(df))
            df.cruise_id = fill(cruise_id, nrow(df))
        end
        push!(dfs, df)
    end

    combined = vcat(dfs...)

    depth_col = "depth" in names(combined) ? :depth : :sea_water_pressure
    if !("dissolved_oxygen" in names(combined))
        error("Column dissolved_oxygen not found in cleaned data for $cruise_id")
    end

    rows = NamedTuple{(:longitude, :latitude, :depth_m, :dissolved_oxygen), Tuple{Float64, Float64, Float64, Float64}}[]
    for row in eachrow(combined)
        lon = parse_float64(row.longitude)
        lat = parse_float64(row.latitude)
        dep = parse_float64(row[depth_col])
        oxy = parse_float64(row.dissolved_oxygen)
        if !ismissing(lon) && !ismissing(lat) && !ismissing(dep) && !ismissing(oxy)
            push!(rows, (longitude = lon, latitude = lat, depth_m = dep, dissolved_oxygen = oxy))
        end
    end

    if isempty(rows)
        error("No valid dissolved oxygen observations for $cruise_id")
    end

    obs = DataFrame(rows)

    println("Loaded $(nrow(obs)) observations from $(length(files)) casts for $cruise_id")
    return obs
end

function interpolate_cruise(cruise_id::AbstractString, clean_root::AbstractString, interp_root::AbstractString)
    output_dir = joinpath(interp_root, cruise_id)
    output_file = joinpath(output_dir, "oxygen_field.csv")
    mkpath(output_dir)

    println("Interpolating oxygen field for $cruise_id")
    df = load_cruise_observations(cruise_id, clean_root)

    x = Vector{Float64}(df.longitude)
    y = Vector{Float64}(df.latitude)
    z = Vector{Float64}(df.depth_m)
    f = Vector{Float64}(df.dissolved_oxygen)

    lon_min, lon_max = extrema(x)
    lat_min, lat_max = extrema(y)
    depth_min, depth_max = extrema(z)

    n_lon, n_lat, n_depth = capped_grid_sizes(
        lon_max - lon_min,
        lat_max - lat_min,
        depth_max - depth_min,
    )

    println("Grid size: $n_lon x $n_lat x $n_depth ($(n_lon * n_lat * n_depth) points)")

    lon_range = range(lon_min, stop = lon_max, length = n_lon)
    lat_range = range(lat_min, stop = lat_max, length = n_lat)
    depth_range = range(depth_min, stop = depth_max, length = n_depth)

    mask, (pm, pn, po), (xi, yi, zi) = DIVAnd_rectdom(lon_range, lat_range, depth_range)

    len = (0.05, 0.05, 5.0)
    epsilon2 = 1.0

    f_mean = mean(f)
    println("Background (observation mean): $(round(f_mean, digits=3)) mg/L")
    fi, _ = DIVAndrun(
        mask,
        (pm, pn, po),
        (xi, yi, zi),
        (x, y, z),
        f .- f_mean,
        len,
        epsilon2,
    )
    fi .+= f_mean

    grid = DataFrame(
        longitude = vec([lon for lon in lon_range, lat in lat_range, depth in depth_range]),
        latitude = vec([lat for lon in lon_range, lat in lat_range, depth in depth_range]),
        depth_m = vec([depth for lon in lon_range, lat in lat_range, depth in depth_range]),
        dissolved_oxygen = vec(fi),
    )

    CSV.write(output_file, grid)
    println("Wrote $output_file")
end

function main()
    cruise_filter = nothing
    csv_path = MAPPING_CSV

    if length(ARGS) >= 1
        if endswith(ARGS[1], ".csv")
            csv_path = ARGS[1]
        else
            cruise_filter = ARGS[1]
        end
    end

    mapping = read_ctd_mapping(csv_path)
    cruise_ids = unique_cruise_ids(mapping)

    if !isnothing(cruise_filter)
        if !(cruise_filter in cruise_ids)
            error("Cruise $cruise_filter not found in mapping file")
        end
        cruise_ids = [cruise_filter]
    end

    for cruise_id in cruise_ids
        interpolate_cruise(cruise_id, CLEAN_ROOT, INTERP_ROOT)
    end
end

main()
