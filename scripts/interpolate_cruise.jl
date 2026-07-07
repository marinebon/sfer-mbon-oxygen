#!/usr/bin/env julia

using Pkg
Pkg.activate(joinpath(@__DIR__, "..", "julia"))
Pkg.instantiate()

using CSV
using DataFrames
using DIVAnd
using Statistics
using ArchGDAL

const MAPPING_CSV = joinpath(@__DIR__, "..", "data", "ctd_datasetid_cruisename_stationname_mapping.csv")
const CLEAN_ROOT = joinpath(@__DIR__, "..", "data", "02_clean")
const INTERP_ROOT = joinpath(@__DIR__, "..", "data", "interpolated")
const BATH_ROOT = joinpath(@__DIR__, "..", "data", "bathymetry")
const REPO_ROOT = joinpath(@__DIR__, "..")
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

function ensure_bluetopo(
    cruise_id::AbstractString,
    lon_min::Float64,
    lat_min::Float64,
    lon_max::Float64,
    lat_max::Float64,
    n_lon::Int,
    n_lat::Int,
)
    mkpath(BATH_ROOT)
    bath_file = joinpath(BATH_ROOT, "$(cruise_id)_bluetopo.tif")
    if isfile(bath_file)
        return bath_file
    end

    script = joinpath(REPO_ROOT, "scripts", "download_bluetopo.py")
    println("Fetching BlueTopo bathymetry for $(cruise_id)")
    run(
        `python3 $(script) $(lon_min) $(lat_min) $(lon_max) $(lat_max) --n-lon $(n_lon) --n-lat $(n_lat) -o $(bath_file)`,
    )
    return bath_file
end

function load_bluetopo_elevation(path::AbstractString, n_lon::Int, n_lat::Int)
    ArchGDAL.read(path) do dataset
        width = ArchGDAL.width(dataset)
        height = ArchGDAL.height(dataset)
        if width != n_lon || height != n_lat
            error("BlueTopo grid size $(width)x$(height) does not match $(n_lon)x$(n_lat)")
        end
        band = ArchGDAL.getband(dataset, 1)
        data = ArchGDAL.read(band, 0, 0, width, height)
        reverse(data, dims=2)
    end
end

function sea_mask_at_depth(elevation::AbstractMatrix{<:Real}, depth_m::Float64)
    mask = falses(size(elevation))
    for idx in eachindex(elevation)
        elev = elevation[idx]
        if isfinite(elev) && elev < 0
            mask[idx] = (-elev) >= depth_m
        end
    end
    return mask
end

function depth_slice_tolerance(depth_range)
    if length(depth_range) > 1
        return max(DEPTH_RES_M / 2, minimum(diff(collect(depth_range))) / 2)
    end
    return DEPTH_RES_M / 2
end

function interpolate_depth_slice(
    lon_range,
    lat_range,
    depth,
    depth_tol,
    x,
    y,
    z,
    f,
    len_horiz,
    epsilon2,
    sea_mask,
)
    mask_obs = abs.(z .- depth) .<= depth_tol
    n_obs = count(mask_obs)

    mask, (pm, pn), (xi, yi) = DIVAnd_rectdom(lon_range, lat_range)
    mask .= sea_mask

    if n_obs < 3
        fill_value = if n_obs == 0
            NaN
        else
            mean(f[mask_obs])
        end
        fi = fill(fill_value, size(mask))
        fi = ifelse.(sea_mask, fi, NaN)
        return fi
    end

    x2 = x[mask_obs]
    y2 = y[mask_obs]
    f2 = f[mask_obs]
    f2_mean = mean(f2)

    fi, _ = DIVAndrun(
        mask,
        (pm, pn),
        (xi, yi),
        (x2, y2),
        f2 .- f2_mean,
        len_horiz,
        epsilon2,
    )

    fi = fi .+ f2_mean
    return ifelse.(sea_mask, fi, NaN)
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
    depth_tol = depth_slice_tolerance(depth_range)

    bath_file = ensure_bluetopo(cruise_id, lon_min, lat_min, lon_max, lat_max, n_lon, n_lat)
    elevation = load_bluetopo_elevation(bath_file, n_lon, n_lat)
    println("Loaded BlueTopo bathymetry from $(basename(bath_file))")

    len_horiz = (0.05, 0.05)
    epsilon2 = 1.0

    println(
        "Interpolating independently per depth slice (±$(round(depth_tol, digits=2)) m tolerance)",
    )

    fi = Array{Float64}(undef, n_lon, n_lat, n_depth)

    for (idep, depth) in enumerate(depth_range)
        sea_mask = sea_mask_at_depth(elevation, depth)
        fi[:, :, idep] = interpolate_depth_slice(
            lon_range,
            lat_range,
            depth,
            depth_tol,
            x,
            y,
            z,
            f,
            len_horiz,
            epsilon2,
            sea_mask,
        )
    end

    # Fill depth slices with no observations from neighboring depth means (sea only).
    depth_means = [
        begin
            mask_obs = abs.(z .- depth) .<= depth_tol
            count(mask_obs) > 0 ? mean(f[mask_obs]) : NaN
        end
        for depth in depth_range
    ]
    for i in eachindex(depth_means)
        if isnan(depth_means[i])
            neighbors = [depth_means[j] for j in eachindex(depth_means) if !isnan(depth_means[j])]
            depth_means[i] = isempty(neighbors) ? mean(f) : mean(neighbors)
        end
    end
    for idep in 1:n_depth
        sea_mask = sea_mask_at_depth(elevation, depth_range[idep])
        slice = fi[:, :, idep]
        for idx in eachindex(slice)
            if !sea_mask[idx]
                slice[idx] = NaN
            elseif isnan(slice[idx])
                slice[idx] = depth_means[idep]
            end
        end
        fi[:, :, idep] = slice
    end

    grid = DataFrame(
        longitude = Float64[],
        latitude = Float64[],
        depth_m = Float64[],
        dissolved_oxygen = Float64[],
    )
    for (idep, depth) in enumerate(depth_range)
        slice = fi[:, :, idep]
        for (j, lat) in enumerate(lat_range), (i, lon) in enumerate(lon_range)
            value = slice[i, j]
            if !isnan(value)
                push!(grid, (longitude = lon, latitude = lat, depth_m = depth, dissolved_oxygen = value))
            end
        end
    end

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
