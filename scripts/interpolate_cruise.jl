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
const HORIZ_RES_DEG = 0.01
const DEPTH_RES_M = 4.0
const MIN_GRID_POINTS = 30
const MAX_GRID_POINTS = 150
const MAX_GRID_POINTS_TOTAL = 120_000
const MIN_BATHY_COVERAGE = 0.35
const BATHY_GRID_SCALE = 8
const MAX_BATHY_LON = 400
const MAX_BATHY_LAT = 400
const LAND_ELEVATION_M = -0.2
const BBOX_PADDING_DEG = 0.02
const OBS_SEA_MAX_STEPS = 4
const OBS_BRIDGE_MAX_STEPS = 40
const OBS_BRIDGE_MAX_GRID_DIST = 12
const LEN_HORIZ_DEG = 0.035
const EPSILON2 = 0.01
const LEN_HORIZ_BG_DEG = 0.08
const EPSILON2_BG = 1.0

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

    rows = NamedTuple{(:longitude, :latitude, :depth_m, :dissolved_oxygen), Tuple{Float64, Float64, Float64, Float64}}[]
    for file in files
        df = DataFrame(CSV.File(file))
        depth_col = "depth" in names(df) ? :depth : :sea_water_pressure
        if !("dissolved_oxygen" in names(df))
            error("Column dissolved_oxygen not found in $file")
        end
        for row in eachrow(df)
            lon = parse_float64(row.longitude)
            lat = parse_float64(row.latitude)
            dep = parse_float64(row[depth_col])
            oxy = parse_float64(row.dissolved_oxygen)
            if !ismissing(lon) && !ismissing(lat) && !ismissing(dep) && !ismissing(oxy)
                push!(rows, (longitude = lon, latitude = lat, depth_m = dep, dissolved_oxygen = oxy))
            end
        end
    end

    if isempty(rows)
        error("No valid dissolved oxygen observations for $cruise_id")
    end

    obs = DataFrame(rows)

    println("Loaded $(nrow(obs)) observations from $(length(files)) casts for $cruise_id")
    return obs
end

function bathymetry_grid_sizes(n_lon::Int, n_lat::Int)
    (
        min(n_lon * BATHY_GRID_SCALE, MAX_BATHY_LON),
        min(n_lat * BATHY_GRID_SCALE, MAX_BATHY_LAT),
    )
end

function ensure_land_mask(
    cruise_id::AbstractString,
    lon_min::Float64,
    lat_min::Float64,
    lon_max::Float64,
    lat_max::Float64,
    n_lon::Int,
    n_lat::Int,
)
    mkpath(BATH_ROOT)
    mask_file = joinpath(BATH_ROOT, "$(cruise_id)_land_mask_$(n_lon)x$(n_lat).tif")
    if isfile(mask_file)
        return mask_file
    end
    script = joinpath(REPO_ROOT, "scripts", "rasterize_land_mask.py")
    println("Rasterizing land mask for $(cruise_id) at $(n_lon)x$(n_lat)")
    run(
        `python3 $(script) $(lon_min) $(lat_min) $(lon_max) $(lat_max) --n-lon $(n_lon) --n-lat $(n_lat) -o $(mask_file)`,
    )
    return mask_file
end

function load_land_mask(path::AbstractString, n_lon::Int, n_lat::Int)
    ArchGDAL.read(path) do dataset
        width = ArchGDAL.width(dataset)
        height = ArchGDAL.height(dataset)
        if width != n_lon || height != n_lat
            error("Land mask grid size $(width)x$(height) does not match $(n_lon)x$(n_lat)")
        end
        band = ArchGDAL.getband(dataset, 1)
        reverse(ArchGDAL.read(band, 0, 0, width, height), dims=2)
    end
end

function apply_land_mask_to_elevation!(
    elevation::AbstractMatrix{<:Real},
    land_mask::AbstractMatrix{<:Real},
)
    for idx in eachindex(elevation)
        if land_mask[idx] >= 0.5
            elevation[idx] = NaN
        end
    end
    return elevation
end

function ensure_bluetopo(
    cruise_id::AbstractString,
    lon_min::Float64,
    lat_min::Float64,
    lon_max::Float64,
    lat_max::Float64,
    bath_n_lon::Int,
    bath_n_lat::Int,
)
    mkpath(BATH_ROOT)
    bath_file = joinpath(BATH_ROOT, "$(cruise_id)_bluetopo_$(bath_n_lon)x$(bath_n_lat).tif")
    if isfile(bath_file)
        try
            elev = load_bluetopo_elevation(bath_file, bath_n_lon, bath_n_lat)
            coverage = count(isfinite.(elev)) / length(elev)
            has_land_band = ArchGDAL.read(bath_file) do dataset
                ArchGDAL.nraster(dataset) >= 2
            end
            if coverage >= MIN_BATHY_COVERAGE && has_land_band
                return bath_file
            end
            println(
                "BlueTopo cache stale or low coverage $(round(100 * coverage, digits=1))%; re-fetching...",
            )
        catch
            println("BlueTopo cache invalid for current grid; re-fetching...")
        end
        rm(bath_file; force=true)
    end

    script = joinpath(REPO_ROOT, "scripts", "download_bluetopo.py")
    println("Fetching BlueTopo bathymetry for $(cruise_id) at $(bath_n_lon)x$(bath_n_lat)")
    run(
        `python3 $(script) $(lon_min) $(lat_min) $(lon_max) $(lat_max) --n-lon $(bath_n_lon) --n-lat $(bath_n_lat) -o $(bath_file)`,
    )
    return bath_file
end

function load_bluetopo_rasters(path::AbstractString, n_lon::Int, n_lat::Int)
    ArchGDAL.read(path) do dataset
        width = ArchGDAL.width(dataset)
        height = ArchGDAL.height(dataset)
        if width != n_lon || height != n_lat
            error("BlueTopo grid size $(width)x$(height) does not match $(n_lon)x$(n_lat)")
        end
        elev_band = ArchGDAL.getband(dataset, 1)
        elevation = ArchGDAL.read(elev_band, 0, 0, width, height)
        elevation = reverse(elevation, dims=2)
        land_mask = try
            land_band = ArchGDAL.getband(dataset, 2)
            reverse(ArchGDAL.read(land_band, 0, 0, width, height), dims=2)
        catch
            zeros(size(elevation))
        end
        (elevation, land_mask)
    end
end

function load_bluetopo_elevation(path::AbstractString, n_lon::Int, n_lat::Int)
    load_bluetopo_rasters(path, n_lon, n_lat)[1]
end

function is_land_or_unknown(elev::Real)
    !isfinite(elev) || elev >= LAND_ELEVATION_M
end

function sample_elevation_on_grid(
    elevation::AbstractMatrix{<:Real},
    land_mask::AbstractMatrix{<:Real},
    lon_range,
    lat_range;
    apply_bathy_land::Bool = true,
)
    bath_n_lon, bath_n_lat = size(elevation)
    bath_lons = range(first(lon_range), stop = last(lon_range), length = bath_n_lon)
    bath_lats = range(first(lat_range), stop = last(lat_range), length = bath_n_lat)
    n_lon = length(lon_range)
    n_lat = length(lat_range)
    coarse = Matrix{Float64}(undef, n_lon, n_lat)
    for j in 1:n_lat, i in 1:n_lon
        ii = argmin(abs.(collect(bath_lons) .- lon_range[i]))
        jj = argmin(abs.(collect(bath_lats) .- lat_range[j]))
        if apply_bathy_land && land_mask[ii, jj] >= 0.5
            coarse[i, j] = NaN
        else
            coarse[i, j] = elevation[ii, jj]
        end
    end
    return coarse
end

function sample_bathy_land_on_grid(
    land_mask::AbstractMatrix{<:Real},
    lon_range,
    lat_range,
)
    bath_n_lon, bath_n_lat = size(land_mask)
    bath_lons = range(first(lon_range), stop = last(lon_range), length = bath_n_lon)
    bath_lats = range(first(lat_range), stop = last(lat_range), length = bath_n_lat)
    n_lon = length(lon_range)
    n_lat = length(lat_range)
    coarse = Matrix{Float64}(undef, n_lon, n_lat)
    for j in 1:n_lat, i in 1:n_lon
        ii = argmin(abs.(collect(bath_lons) .- lon_range[i]))
        jj = argmin(abs.(collect(bath_lats) .- lat_range[j]))
        coarse[i, j] = land_mask[ii, jj] >= 0.5 ? 1.0 : 0.0
    end
    return coarse
end

function sea_mask_at_depth(
    elevation::AbstractMatrix{<:Real},
    depth_m::Float64,
    depth_tol::Float64,
)
    mask = falses(size(elevation))
    for idx in eachindex(elevation)
        elev = elevation[idx]
        if is_land_or_unknown(elev)
            mask[idx] = false
        else
            bottom_depth = -elev
            mask[idx] = bottom_depth + depth_tol >= depth_m
        end
    end
    return mask
end

function grid_index(lon_range, lat_range, lon::Float64, lat::Float64)
    (
        argmin(abs.(collect(lon_range) .- lon)),
        argmin(abs.(collect(lat_range) .- lat)),
    )
end

function is_water_elevation(elev::Real)
    isfinite(elev) && elev < LAND_ELEVATION_M
end

function clear_land_mask_at_observations!(
    land_mask::AbstractMatrix{<:Real},
    x,
    y,
    lon_range,
    lat_range,
)
    for (lon, lat) in zip(x, y)
        i, j = grid_index(lon_range, lat_range, lon, lat)
        land_mask[i, j] = 0
    end
    return land_mask
end

function extend_sea_mask_along_water!(
    sea_mask::AbstractMatrix{Bool},
    elevation::AbstractMatrix{<:Real},
    x,
    y,
    lon_range,
    lat_range,
    max_steps::Int,
)
    n_lon, n_lat = size(sea_mask)
    for (lon, lat) in zip(x, y)
        i, j = grid_index(lon_range, lat_range, lon, lat)
        queue = [(i, j, 0)]
        visited = falses(n_lon, n_lat)

        while !isempty(queue)
            ci, cj, steps = popfirst!(queue)
            if visited[ci, cj]
                continue
            end
            visited[ci, cj] = true

            at_obs = ci == i && cj == j
            if at_obs || is_water_elevation(elevation[ci, cj])
                sea_mask[ci, cj] = true
                if steps < max_steps
                    for (di, dj) in ((1, 0), (-1, 0), (0, 1), (0, -1))
                        ni, nj = ci + di, cj + dj
                        if 1 <= ni <= n_lon && 1 <= nj <= n_lat && !visited[ni, nj]
                            push!(queue, (ni, nj, steps + 1))
                        end
                    end
                end
            end
        end
    end
    return sea_mask
end

function is_bridge_water_cell(
    bathy_elevation::AbstractMatrix{<:Real},
    bathy_land_mask::AbstractMatrix{<:Real},
    ii::Int,
    jj::Int,
    start::Tuple{Int, Int},
    goal::Tuple{Int, Int},
)
    if (ii, jj) == start || (ii, jj) == goal
        return true
    end
    if bathy_land_mask[ii, jj] >= 0.5
        return false
    end
    elev = bathy_elevation[ii, jj]
    return isfinite(elev) && elev < LAND_ELEVATION_M
end

function bridge_path_is_valid(
    bathy_elevation::AbstractMatrix{<:Real},
    bathy_land_mask::AbstractMatrix{<:Real},
    path::AbstractVector{Tuple{Int, Int}},
    start::Tuple{Int, Int},
    goal::Tuple{Int, Int},
)
    for cell in path
        ii, jj = cell
        if !is_bridge_water_cell(bathy_elevation, bathy_land_mask, ii, jj, start, goal)
            return false
        end
    end
    return true
end

function shortest_bridge_path(
    bathy_elevation::AbstractMatrix{<:Real},
    bathy_land_mask::AbstractMatrix{<:Real},
    start::Tuple{Int, Int},
    goal::Tuple{Int, Int},
    max_steps::Int,
)
    n_lon, n_lat = size(bathy_elevation)
    si, sj = start
    gi, gj = goal

    if start == goal
        return [start]
    end

    queue = [start]
    parents = Dict{Tuple{Int, Int}, Union{Nothing, Tuple{Int, Int}}}(start => nothing)
    steps = Dict(start => 0)

    while !isempty(queue)
        ci, cj = popfirst!(queue)
        if (ci, cj) == goal
            path = Tuple{Int, Int}[]
            node::Union{Nothing, Tuple{Int, Int}} = goal
            while node !== nothing
                pushfirst!(path, node)
                node = parents[node]
            end
            return path
        end

        dist = steps[(ci, cj)]
        if dist >= max_steps
            continue
        end

        for (di, dj) in ((1, 0), (-1, 0), (0, 1), (0, -1))
            ni, nj = ci + di, cj + dj
            if 1 <= ni <= n_lon && 1 <= nj <= n_lat
                neighbor = (ni, nj)
                if haskey(parents, neighbor)
                    continue
                end
                if is_bridge_water_cell(
                    bathy_elevation,
                    bathy_land_mask,
                    ni,
                    nj,
                    start,
                    goal,
                )
                    parents[neighbor] = (ci, cj)
                    steps[neighbor] = dist + 1
                    push!(queue, neighbor)
                end
            end
        end
    end

    return nothing
end

function greedy_bridge_path(
    bathy_elevation::AbstractMatrix{<:Real},
    bathy_land_mask::AbstractMatrix{<:Real},
    start::Tuple{Int, Int},
    goal::Tuple{Int, Int},
    max_steps::Int,
)
    n_lon, n_lat = size(bathy_elevation)
    gi, gj = goal
    current = start
    path = [start]
    visited = Set([start])

    for _ in 1:max_steps
        if current == goal
            return path
        end

        ci, cj = current
        best = nothing
        best_dist = Inf
        for (di, dj) in ((1, 0), (-1, 0), (0, 1), (0, -1))
            ni, nj = ci + di, cj + dj
            if 1 <= ni <= n_lon && 1 <= nj <= n_lat
                neighbor = (ni, nj)
                if neighbor in visited
                    continue
                end
                if is_bridge_water_cell(
                    bathy_elevation,
                    bathy_land_mask,
                    ni,
                    nj,
                    start,
                    goal,
                )
                    dist = hypot(ni - gi, nj - gj)
                    if dist < best_dist
                        best_dist = dist
                        best = neighbor
                    end
                end
            end
        end

        if best === nothing
            return nothing
        end

        push!(path, best)
        push!(visited, best)
        current = best
    end

    return current == goal ? path : nothing
end

function nearest_cell_with_label(
    labels::AbstractMatrix{Int},
    sea_mask::AbstractMatrix{Bool},
    i::Int,
    j::Int,
    target_label::Int,
)
    n_lon, n_lat = size(sea_mask)
    best = nothing
    best_dist = Inf
    for ii in 1:n_lon, jj in 1:n_lat
        if sea_mask[ii, jj] && labels[ii, jj] == target_label
            dist = hypot(ii - i, jj - j)
            if dist < best_dist
                best_dist = dist
                best = (ii, jj)
            end
        end
    end
    return best, best_dist
end

function main_sea_component_label(
    labels::AbstractMatrix{Int},
    x,
    y,
    lon_range,
    lat_range,
)
    counts = Dict{Int, Int}()
    for (lon, lat) in zip(x, y)
        i, j = grid_index(lon_range, lat_range, lon, lat)
        label = labels[i, j]
        if label > 0
            counts[label] = get(counts, label, 0) + 1
        end
    end
    isempty(counts) && return 0
    return argmax(label -> counts[label], keys(counts))
end

function bridge_unconnected_observations!(
    sea_mask::AbstractMatrix{Bool},
    elevation::AbstractMatrix{<:Real},
    bathy_elevation::AbstractMatrix{<:Real},
    bathy_land_mask::AbstractMatrix{<:Real},
    land_mask::AbstractMatrix{<:Real},
    x,
    y,
    lon_range,
    lat_range,
)
    bridged_cells = 0
    connected_obs = 0
    skipped_far = 0
    labels = label_sea_components(sea_mask)
    main_label = main_sea_component_label(labels, x, y, lon_range, lat_range)
    if main_label == 0
        println("Warning: no sea components found for observation bridging")
        return sea_mask
    end

    for (lon, lat) in zip(x, y)
        labels = label_sea_components(sea_mask)
        main_label = main_sea_component_label(labels, x, y, lon_range, lat_range)
        if main_label == 0
            break
        end

        i, j = grid_index(lon_range, lat_range, lon, lat)
        obs_label = labels[i, j]

        if obs_label == main_label
            continue
        end

        target, dist = nearest_cell_with_label(labels, sea_mask, i, j, main_label)
        if target === nothing
            println(
                "Warning: no main sea component cell found near observation at ($(round(lon, digits=4)), $(round(lat, digits=4)))",
            )
            continue
        end
        if dist > OBS_BRIDGE_MAX_GRID_DIST
            skipped_far += 1
            continue
        end

        if target == (i, j)
            sea_mask[i, j] = true
            land_mask[i, j] = 0
            if !isfinite(elevation[i, j]) || elevation[i, j] >= LAND_ELEVATION_M
                elevation[i, j] = -2.0
            end
            connected_obs += 1
            continue
        end

        path = shortest_bridge_path(
            bathy_elevation,
            bathy_land_mask,
            (i, j),
            target,
            OBS_BRIDGE_MAX_STEPS,
        )
        if path === nothing
            path = greedy_bridge_path(
                bathy_elevation,
                bathy_land_mask,
                (i, j),
                target,
                OBS_BRIDGE_MAX_STEPS,
            )
        end
        if path === nothing || !bridge_path_is_valid(
            bathy_elevation,
            bathy_land_mask,
            path,
            (i, j),
            target,
        )
            println(
                "Warning: could not bridge observation at ($(round(lon, digits=4)), $(round(lat, digits=4))) within $(OBS_BRIDGE_MAX_STEPS) steps",
            )
            continue
        end

        for (pi, pj) in path
            if !sea_mask[pi, pj]
                bridged_cells += 1
            end
            sea_mask[pi, pj] = true
            land_mask[pi, pj] = 0
            if !isfinite(elevation[pi, pj]) || elevation[pi, pj] >= LAND_ELEVATION_M
                elevation[pi, pj] = -2.0
            end
        end
        connected_obs += 1
    end

    println(
        "Bridged $bridged_cells cell(s) along water paths for $connected_obs observation(s)",
    )
    if skipped_far > 0
        println(
            "Skipped bridging for $skipped_far observation(s) farther than $(OBS_BRIDGE_MAX_GRID_DIST) grid cell(s) from the main field",
        )
    end
    return sea_mask
end

function ensure_water_elevation_near_observations!(
    elevation::AbstractMatrix{<:Real},
    sea_mask::AbstractMatrix{Bool},
)
    for idx in eachindex(elevation)
        if sea_mask[idx] && !isfinite(elevation[idx])
            elevation[idx] = -2.0
        end
    end
    return elevation
end

function observation_component_labels(
    sea_mask::AbstractMatrix{Bool},
    labels::AbstractMatrix{Int},
    x,
    y,
    lon_range,
    lat_range,
    search_deg::Float64,
)
    observed_labels = Set{Int}()
    lon_step = length(lon_range) > 1 ? abs(lon_range[2] - lon_range[1]) : search_deg
    lat_step = length(lat_range) > 1 ? abs(lat_range[2] - lat_range[1]) : search_deg
    radius_i = max(1, ceil(Int, search_deg / lon_step))
    radius_j = max(1, ceil(Int, search_deg / lat_step))

    for (lon, lat) in zip(x, y)
        i, j = grid_index(lon_range, lat_range, lon, lat)
        if sea_mask[i, j] && labels[i, j] > 0
            push!(observed_labels, labels[i, j])
            continue
        end

        best_label = 0
        best_dist = Inf
        for ii in max(1, i - radius_i):min(size(sea_mask, 1), i + radius_i)
            for jj in max(1, j - radius_j):min(size(sea_mask, 2), j + radius_j)
                if sea_mask[ii, jj] && labels[ii, jj] > 0
                    dist = hypot(lon_range[ii] - lon, lat_range[jj] - lat)
                    if dist < best_dist
                        best_dist = dist
                        best_label = labels[ii, jj]
                    end
                end
            end
        end
        if best_label > 0
            push!(observed_labels, best_label)
        end
    end

    return observed_labels
end

function label_sea_components(sea_mask::AbstractMatrix{Bool})
    n_lon, n_lat = size(sea_mask)
    labels = zeros(Int, n_lon, n_lat)
    label = 0

    for i in 1:n_lon, j in 1:n_lat
        if !sea_mask[i, j] || labels[i, j] != 0
            continue
        end
        label += 1
        queue = [(i, j)]
        labels[i, j] = label
        while !isempty(queue)
            ci, cj = popfirst!(queue)
            for (di, dj) in ((1, 0), (-1, 0), (0, 1), (0, -1))
                ni, nj = ci + di, cj + dj
                if 1 <= ni <= n_lon &&
                   1 <= nj <= n_lat &&
                   sea_mask[ni, nj] &&
                   labels[ni, nj] == 0
                    labels[ni, nj] = label
                    push!(queue, (ni, nj))
                end
            end
        end
    end

    return labels
end

function mask_disconnected_components!(
    fi::AbstractMatrix{<:Real},
    sea_mask::AbstractMatrix{Bool},
    x,
    y,
    lon_range,
    lat_range,
)
    labels = label_sea_components(sea_mask)
    observed_labels = observation_component_labels(
        sea_mask,
        labels,
        x,
        y,
        lon_range,
        lat_range,
        0.05,
    )

    if isempty(observed_labels)
        println("Warning: no observation-linked sea components found; field unchanged")
        return fi
    end

    removed = 0
    for idx in eachindex(fi)
        if sea_mask[idx] && labels[idx] > 0 && !(labels[idx] in observed_labels)
            if !isnan(fi[idx])
                removed += 1
            end
            fi[idx] = NaN
        end
    end

    println(
        "Kept $(length(observed_labels)) sea component(s) with observations; ",
        "removed $removed disconnected cell(s)",
    )
    return fi
end

function depth_slice_tolerance(depth_range)
    if length(depth_range) > 1
        return max(DEPTH_RES_M / 2, minimum(diff(collect(depth_range))) / 2)
    end
    return DEPTH_RES_M / 2
end

function interpolate_surface_field(
    lon_range,
    lat_range,
    x,
    y,
    f,
    len_horiz,
    epsilon2,
    sea_mask,
)
    n_obs = length(f)

    mask, (pm, pn), (xi, yi) = DIVAnd_rectdom(lon_range, lat_range)
    mask .= sea_mask

    if n_obs == 0
        return fill(NaN, size(sea_mask))
    end

    if n_obs == 1
        fi = fill(f[1], size(mask))
        return ifelse.(sea_mask, fi, NaN)
    end

    f_mean = mean(f)
    fi, _ = DIVAndrun(
        mask,
        (pm, pn),
        (xi, yi),
        (x, y),
        f .- f_mean,
        len_horiz,
        epsilon2,
    )

    fi = fi .+ f_mean
    return ifelse.(sea_mask, fi, NaN)
end

function sample_field_at_points(
    fi::AbstractMatrix{<:Real},
    x,
    y,
    lon_range,
    lat_range,
)
    vals = Vector{Float64}(undef, length(x))
    for k in eachindex(x)
        i, j = grid_index(lon_range, lat_range, x[k], y[k])
        vals[k] = fi[i, j]
    end
    return vals
end

function interpolate_surface_field_multiscale(
    lon_range,
    lat_range,
    x,
    y,
    f,
    len_horiz,
    epsilon2,
    len_horiz_bg,
    epsilon2_bg,
    sea_mask,
)
    n_obs = length(f)
    if n_obs <= 1
        return interpolate_surface_field(
            lon_range,
            lat_range,
            x,
            y,
            f,
            len_horiz,
            epsilon2,
            sea_mask,
        )
    end

    background = interpolate_surface_field(
        lon_range,
        lat_range,
        x,
        y,
        f,
        len_horiz_bg,
        epsilon2_bg,
        sea_mask,
    )
    background_at_obs = sample_field_at_points(background, x, y, lon_range, lat_range)
    residuals = f .- background_at_obs
    fine = interpolate_surface_field(
        lon_range,
        lat_range,
        x,
        y,
        residuals,
        len_horiz,
        epsilon2,
        sea_mask,
    )
    fi = background .+ fine
    return ifelse.(sea_mask, fi, NaN)
end

function obs_indices_for_component(
    sea_mask::AbstractMatrix{Bool},
    labels::AbstractMatrix{Int},
    x,
    y,
    lon_range,
    lat_range,
    target_label::Int,
    search_deg::Float64 = 0.05,
)
    indices = Int[]
    lon_step = length(lon_range) > 1 ? abs(lon_range[2] - lon_range[1]) : search_deg
    lat_step = length(lat_range) > 1 ? abs(lat_range[2] - lat_range[1]) : search_deg
    radius_i = max(1, ceil(Int, search_deg / lon_step))
    radius_j = max(1, ceil(Int, search_deg / lat_step))

    for k in eachindex(x)
        lon = x[k]
        lat = y[k]
        i, j = grid_index(lon_range, lat_range, lon, lat)
        assigned = 0
        if sea_mask[i, j] && labels[i, j] > 0
            assigned = labels[i, j]
        else
            best_label = 0
            best_dist = Inf
            for ii in max(1, i - radius_i):min(size(sea_mask, 1), i + radius_i)
                for jj in max(1, j - radius_j):min(size(sea_mask, 2), j + radius_j)
                    if sea_mask[ii, jj] && labels[ii, jj] > 0
                        dist = hypot(lon_range[ii] - lon, lat_range[jj] - lat)
                        if dist < best_dist
                            best_dist = dist
                            best_label = labels[ii, jj]
                        end
                    end
                end
            end
            assigned = best_label
        end
        if assigned == target_label
            push!(indices, k)
        end
    end

    return indices
end

function interpolate_by_sea_component(
    lon_range,
    lat_range,
    x,
    y,
    f,
    len_horiz,
    epsilon2,
    sea_mask::AbstractMatrix{Bool},
)
    labels = label_sea_components(sea_mask)
    observed_labels = observation_component_labels(
        sea_mask,
        labels,
        x,
        y,
        lon_range,
        lat_range,
        0.05,
    )
    fi = fill(NaN, size(sea_mask))

    for target_label in observed_labels
        component_mask = sea_mask .& (labels .== target_label)
        obs_idx = obs_indices_for_component(
            sea_mask,
            labels,
            x,
            y,
            lon_range,
            lat_range,
            target_label,
        )
        isempty(obs_idx) && continue

        fi_comp = interpolate_surface_field_multiscale(
            lon_range,
            lat_range,
            x[obs_idx],
            y[obs_idx],
            f[obs_idx],
            len_horiz,
            epsilon2,
            (LEN_HORIZ_BG_DEG, LEN_HORIZ_BG_DEG),
            EPSILON2_BG,
            component_mask,
        )

        for idx in eachindex(fi)
            if component_mask[idx]
                fi[idx] = fi_comp[idx]
            end
        end
    end

    return fi
end

function report_observation_fit(
    fi::AbstractMatrix{<:Real},
    x,
    y,
    f,
    lon_range,
    lat_range,
)
    residuals = Float64[]
    for (lon, lat, obs_val) in zip(x, y, f)
        i, j = grid_index(lon_range, lat_range, lon, lat)
        field_val = fi[i, j]
        if !isnan(field_val)
            push!(residuals, obs_val - field_val)
        end
    end

    if isempty(residuals)
        println("Observation fit: no observation grid cells overlap the interpolated field")
        return
    end

    println(
        "Observation fit at observation grid cells: ",
        "mean |residual|=$(round(mean(abs.(residuals)), digits=2)), ",
        "max |residual|=$(round(maximum(abs.(residuals)), digits=2)), ",
        "RMSE=$(round(sqrt(mean(residuals .^ 2)), digits=2))",
    )
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
    x2 = x[mask_obs]
    y2 = y[mask_obs]
    f2 = f[mask_obs]
    if isempty(x2)
        return fill(NaN, size(sea_mask))
    end

    interpolate_by_sea_component(
        lon_range,
        lat_range,
        x2,
        y2,
        f2,
        len_horiz,
        epsilon2,
        sea_mask,
    )
end

function interpolate_cruise(cruise_id::AbstractString, clean_root::AbstractString, interp_root::AbstractString)
    output_dir = joinpath(interp_root, cruise_id)
    output_file = joinpath(output_dir, "oxygen_field.csv")
    mkpath(output_dir)

    println("# =========================================================")
    println("# Interpolating oxygen field for $cruise_id")
    println("# =========================================================")
    df = load_cruise_observations(cruise_id, clean_root)

    x = Vector{Float64}(df.longitude)
    y = Vector{Float64}(df.latitude)
    z = Vector{Float64}(df.depth_m)
    f = Vector{Float64}(df.dissolved_oxygen)

    lon_min, lon_max = extrema(x)
    lat_min, lat_max = extrema(y)
    depth_min, depth_max = extrema(z)
    lon_min -= BBOX_PADDING_DEG
    lon_max += BBOX_PADDING_DEG
    lat_min -= BBOX_PADDING_DEG
    lat_max += BBOX_PADDING_DEG

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

    bath_n_lon, bath_n_lat = bathymetry_grid_sizes(n_lon, n_lat)
    bath_file = ensure_bluetopo(
        cruise_id,
        lon_min,
        lat_min,
        lon_max,
        lat_max,
        bath_n_lon,
        bath_n_lat,
    )
    elevation_hi, land_mask_hi = load_bluetopo_rasters(bath_file, bath_n_lon, bath_n_lat)
    elevation = sample_elevation_on_grid(
        elevation_hi,
        land_mask_hi,
        lon_range,
        lat_range,
    )
    bathy_elevation = sample_elevation_on_grid(
        elevation_hi,
        land_mask_hi,
        lon_range,
        lat_range;
        apply_bathy_land = false,
    )
    bathy_land_mask = sample_bathy_land_on_grid(land_mask_hi, lon_range, lat_range)
    land_mask_file = ensure_land_mask(
        cruise_id,
        lon_min,
        lat_min,
        lon_max,
        lat_max,
        n_lon,
        n_lat,
    )
    land_mask = load_land_mask(land_mask_file, n_lon, n_lat)
    clear_land_mask_at_observations!(land_mask, x, y, lon_range, lat_range)
    apply_land_mask_to_elevation!(elevation, land_mask)
    land_cells = count(is_land_or_unknown.(elevation))
    println(
        "Loaded BlueTopo ($(bath_n_lon)x$(bath_n_lat) 4 m) from $(basename(bath_file)); ",
        "land/unknown cells: $(land_cells)/$(n_lon * n_lat) ",
        "(NA and elev >= $(LAND_ELEVATION_M) m treated as land)",
    )

    len_horiz = (LEN_HORIZ_DEG, LEN_HORIZ_DEG)
    epsilon2 = EPSILON2

    println(
        "Interpolating independently per depth slice (±$(round(depth_tol, digits=2)) m tolerance)",
    )
    println(
        "DIVAnd parameters: fine len_horiz=$(len_horiz), epsilon2=$(epsilon2); ",
        "background len_horiz=($(LEN_HORIZ_BG_DEG), $(LEN_HORIZ_BG_DEG)), epsilon2=$(EPSILON2_BG) ",
        "(per sea component)",
    )

    fi = Array{Float64}(undef, n_lon, n_lat, n_depth)

    for (idep, depth) in enumerate(depth_range)
        sea_mask = sea_mask_at_depth(elevation, depth, depth_tol)
        mask_obs = abs.(z .- depth) .<= depth_tol
        x2 = x[mask_obs]
        y2 = y[mask_obs]
        f2 = f[mask_obs]
        if !isempty(x2)
            extend_sea_mask_along_water!(sea_mask, elevation, x2, y2, lon_range, lat_range, OBS_SEA_MAX_STEPS)
            bridge_unconnected_observations!(
                sea_mask,
                elevation,
                bathy_elevation,
                bathy_land_mask,
                land_mask,
                x2,
                y2,
                lon_range,
                lat_range,
            )
            ensure_water_elevation_near_observations!(elevation, sea_mask)
        end
        slice = interpolate_depth_slice(
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
        if !isempty(x2)
            report_observation_fit(slice, x2, y2, f2, lon_range, lat_range)
            slice = mask_disconnected_components!(slice, sea_mask, x2, y2, lon_range, lat_range)
        end
        fi[:, :, idep] = slice
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
        sea_mask = sea_mask_at_depth(elevation, depth_range[idep], depth_tol)
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
        try
            interpolate_cruise(cruise_id, CLEAN_ROOT, INTERP_ROOT)
        catch err
            if err isa Exception && occursin("No cleaned CTD files", sprint(showerror, err))
                println("Skipping $cruise_id (no cleaned CTD data).")
            else
                rethrow()
            end
        end
    end
end

main()
