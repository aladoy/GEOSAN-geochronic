import requests
import osmnx as ox
import sys
import os
from shapely.geometry import Point

# TO USE THE FOLLOWING FUNCTIONS, YOU SHOULD GO BACK TO THE TAG
# ??? IN THE GIRAPH-functions REPOSITORY (git checkout geocoding_v2)
sys.path.append(r"/mnt/data/GEOSAN/FUNCTIONS/GIRAPH-functions/")
try:
    import shortestPaths as sp  # commit: bf78b2d
    import db_utils as db  # commit: bf78b2d
    import basic_utils as bu  # commit: bf78b2d
except FileNotFoundError:
    print("Wrong file or file path")

project_dir: str = (
    r"/mnt/data/GEOSAN/RESEARCH PROJECTS/GEOCHRONIC @ LASIG (EPFL)/GEOSAN-geochronic/"
)
geosan_db_dir: str = r"/mnt/data/GEOSAN/GEOSAN DB/data"

engine, conn, cursor = db.connect_db("geosan", "aladoy")

# Walkable roads
roads = gpd.read_file(os.sep.join(
    [project_dir, "processed_data/walkable_roads/walkable_roads_lausanne_01jan2016_2056.geojson"]))
roads = roads[["id", "geometry"]]


# HA INDICATORS FROM COMMUNE EN SANTE (ALREADY FILTERED PTOT > 3)
ha = gpd.read_postgis(
    "SELECT reli, geometry FROM geochronic.ha_characteristics", conn, geom_col="geometry")


roads = gpd.read_file(os.sep.join(
    [project_dir, "processed_data/walkable_roads/walkable_roads_lausanne_01jan2016_2056_intersections.geojson"]))
roads = roads[["id", "geometry"]]
roads.reset_index(drop=False, inplace=True)
roads['x'] = roads.geometry.x
roads['y'] = roads.geometry.y

# Perform the groupby and calculate the size/count
grouped = roads.groupby(["x", "y"]).size().reset_index(name="nb_intersections")

# Create Point geometries from x and y coordinates
grouped["geometry"] = grouped.apply(
    lambda row: Point(row["x"], row["y"]), axis=1)

# Convert to geodataframe
gdf = gpd.GeoDataFrame(grouped, geometry="geometry")
gdf.crs = 2056

# Filter only intersections where nb > 6 (more than 3-legs intersection)
gdf[gdf.nb_intersections>6].to_file("aaaaa.geojson")

ha_buff = ha.copy()
buffer_distance = 50
ha_buff["geometry"] = ha.geometry.buffer(buffer_distance)
ha_buff["buffer_area"] = ha_buff.geometry.area

roads.dtypes

roads.shape

intersections.dtypes

intersections = gpd.overlay(roads, ha_buff, how='intersection')

intersection_count = intersections.groupby('reli').apply(
    lambda g: sum([1 for geom in g['geometry'] if isinstance(
        geom, sg.MultiLineString) and len(geom) > 1 and len(geom[0].coords) > 3])
).reset_index(name='intersection_count')

final = pd.merge(ha_buff, intersection_count, how="left", on="reli")

final.to_file("final.gpkg")

intersections

intersections['intersection_count'] = intersections.geometry.apply(lambda geom: sum(
    [1 for coord in geom if isinstance(coord, sg.MultiPoint) and len(coord) > 3]))

intersections['polygon_area'] = intersections.geometry.area

# Multiply by 1,000,000 to convert to per square kilometer
intersections['intersections_per_sqkm'] = (
    intersections['intersection_count'] / intersections['polygon_area']) * 1000000

intersections


# Send the request to the Overpass Turbo API
overpass_url = "https://overpass-turbo.eu/"
response = requests.get(overpass_url, params={'data': overpass_query})

# Extract the downloaded data from the response
data = response.json()

response

POLYGON((6.574196 46.496898, 6.68559 46.497693, 6.684737 46.558856, 6.573217 46.55806, 6.574196 46.496898))
