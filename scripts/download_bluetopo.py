#!/usr/bin/env python3
"""Sample NOAA BlueTopo bathymetry onto a regular lon/lat grid (no full tile download)."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

import boto3
import geopandas as gpd
import numpy as np
import rasterio
from botocore import UNSIGNED
from botocore.config import Config
from rasterio.transform import from_bounds
from rasterio.warp import Resampling, reproject
from shapely.geometry import box

BUCKET = "noaa-ocs-nationalbathymetry-pds"
TILE_SCHEME_KEY = "BlueTopo/_BlueTopo_Tile_Scheme/BlueTopo_Tile_Scheme_20260626_132625.gpkg"
ELEVATION_BAND = 1


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Sample NOAA BlueTopo elevation onto a lon/lat grid."
    )
    parser.add_argument("lon_min", type=float)
    parser.add_argument("lat_min", type=float)
    parser.add_argument("lon_max", type=float)
    parser.add_argument("lat_max", type=float)
    parser.add_argument("--n-lon", type=int, required=True)
    parser.add_argument("--n-lat", type=int, required=True)
    parser.add_argument(
        "-o",
        "--output",
        required=True,
        help="Output GeoTIFF (EPSG:4326, band 1 = BlueTopo elevation).",
    )
    parser.add_argument(
        "--cache-dir",
        default="data/bathymetry/bluetopo_cache",
        help="Directory for cached tile scheme.",
    )
    return parser.parse_args()


def s3_client():
    return boto3.client("s3", config=Config(signature_version=UNSIGNED))


def tile_scheme_path(cache_dir: Path) -> Path:
    return cache_dir / "BlueTopo_Tile_Scheme.gpkg"


def load_tile_scheme(cache_dir: Path) -> gpd.GeoDataFrame:
    scheme_path = tile_scheme_path(cache_dir)
    scheme_path.parent.mkdir(parents=True, exist_ok=True)
    if not scheme_path.exists():
        print(f"Downloading tile scheme from s3://{BUCKET}/{TILE_SCHEME_KEY}")
        s3_client().download_file(BUCKET, TILE_SCHEME_KEY, str(scheme_path))
    gdf = gpd.read_file(scheme_path)
    if gdf.crs is None:
        gdf = gdf.set_crs("EPSG:4326")
    return gdf


def tiles_for_bbox(
    gdf: gpd.GeoDataFrame,
    lon_min: float,
    lat_min: float,
    lon_max: float,
    lat_max: float,
) -> gpd.GeoDataFrame:
    aoi = box(lon_min, lat_min, lon_max, lat_max)
    hits = gdf[gdf.intersects(aoi)].copy()
    if hits.empty:
        raise RuntimeError("No BlueTopo tiles intersect the requested bounding box.")

    preferred = hits[hits["Resolution"] == "8m"]
    if not preferred.empty:
        return preferred
    return hits


def mosaic_bluetopo(
    tile_urls: list[str],
    lon_min: float,
    lat_min: float,
    lon_max: float,
    lat_max: float,
    n_lon: int,
    n_lat: int,
) -> tuple[np.ndarray, rasterio.transform.Affine]:
    dst_crs = "EPSG:4326"
    dst_transform = from_bounds(lon_min, lat_min, lon_max, lat_max, n_lon, n_lat)
    dst_shape = (n_lat, n_lon)
    dst = np.full(dst_shape, np.nan, dtype="float32")

    for url in tile_urls:
        vsicurl = f"/vsicurl/{url}"
        temp = np.full(dst_shape, np.nan, dtype="float32")
        with rasterio.open(vsicurl) as src:
            reproject(
                source=rasterio.band(src, ELEVATION_BAND),
                destination=temp,
                src_transform=src.transform,
                src_crs=src.crs,
                dst_transform=dst_transform,
                dst_crs=dst_crs,
                resampling=Resampling.bilinear,
                src_nodata=np.nan,
                dst_nodata=np.nan,
            )
        fill = np.isnan(dst) & ~np.isnan(temp)
        dst[fill] = temp[fill]
        overlap = ~np.isnan(dst) & ~np.isnan(temp)
        dst[overlap] = temp[overlap]

    return dst, dst_transform


def main() -> None:
    args = parse_args()
    lon_min, lon_max = sorted((args.lon_min, args.lon_max))
    lat_min, lat_max = sorted((args.lat_min, args.lat_max))
    cache_dir = Path(args.cache_dir)
    output_path = Path(args.output)

    gdf = load_tile_scheme(cache_dir)
    hits = tiles_for_bbox(gdf, lon_min, lat_min, lon_max, lat_max)
    urls = hits["GeoTIFF_Link"].dropna().unique().tolist()
    print(f"Sampling {len(urls)} BlueTopo tile(s) at {args.n_lon} x {args.n_lat}")

    elevation, transform = mosaic_bluetopo(
        urls,
        lon_min,
        lat_min,
        lon_max,
        lat_max,
        args.n_lon,
        args.n_lat,
    )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    profile = {
        "driver": "GTiff",
        "height": args.n_lat,
        "width": args.n_lon,
        "count": 1,
        "dtype": "float32",
        "crs": "EPSG:4326",
        "transform": transform,
        "nodata": np.nan,
    }
    with rasterio.open(output_path, "w", **profile) as dst:
        dst.write(elevation, 1)

    valid = np.isfinite(elevation)
    if not valid.any():
        raise RuntimeError("BlueTopo sampling returned no valid elevation values.")
    print(
        "Elevation range:",
        float(np.nanmin(elevation)),
        "to",
        float(np.nanmax(elevation)),
    )
    print(f"Wrote {output_path}")


if __name__ == "__main__":
    main()
