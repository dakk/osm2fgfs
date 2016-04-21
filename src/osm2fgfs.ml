open Osm_xml.Parser;;
open Osm_xml.Types;;
open Printf;;
open Array;;

let parse_osm src = 
	let OSM osm = parse_file src in
	Printf.printf "Processing %d nodes and %d relations...\n" (OSMMap.length osm.nodes) (OSMMap.length osm.relations)
;;


let write_fgfs data dest =
	()
;;


let main () =
	Printf.printf "osm2fgfs\n";
	match Array.length Sys.argv with
	| i when i <> 3 -> 
		Printf.printf "use: %s src.xml dest.csv\n" (Array.get Sys.argv 0)
	| _ -> 
		let data = parse_osm (Array.get Sys.argv 1) in
		write_fgfs data (Array.get Sys.argv 2);
		Printf.printf "Done.\n"
;;

main ();;
