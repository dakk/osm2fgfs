open Osm_xml.Parser;;
open Osm_xml.Types;;
open Printf;;
open Array;;
open Core.Std;;
open Inspect;;

(* Object: represent a fgfs static object *)
(* OBJECT_SHARED Models/Airport/localizer.xml 121.337467 31.179872 0.0 0.0 *)
module Object = struct
	type t = { 
		model     : string ; 
		latitude  : float  ; 
		longitude : float
	}

	let to_string obj = 
		"OBJECT_SHARED " 
		^ obj.model ^ " " 
		^ string_of_float (obj.latitude) ^ " " 
		^ string_of_float (obj.longitude) ^ " 0.0 0.0"

	let from_node osmnode = 
		let rec detect_model tags =
			match (find_tag tags "amenity") with
			| Some "fuel" -> Some "models/fuel_station.ac"
			| Some _ -> None
			| None -> None
		in

		match osmnode with
		| OSMNode n ->
			match detect_model n.tags with
			| Some m -> 
				let ob = {
					latitude  = n.latitude  ;
					longitude = n.longitude ;
					model     = m
				} in 
					Printf.printf "Adding node: %f %f %s\n%!" 
						ob.latitude ob.longitude ob.model;
					Some ob
			| None -> None
end

(* Parse osm file src returning a list of objects *)
let parse_osm src = 
	let rec parse_nodes nl =
		match nl with
		| n::nl' -> (
			match Object.from_node (snd n) with
			| Some o -> o::(parse_nodes nl')
			| None -> parse_nodes nl' )
		| [] -> []
	in
	let OSM osm = parse_file src in
	Printf.printf "Processing %d nodes, %d relations and %d ways...\n%!" (OSMMap.length osm.nodes) (OSMMap.length osm.relations) (OSMMap.length osm.ways);
	parse_nodes (Map.to_alist osm.nodes)
;;


(* Write objects to file *)
let write_stg oblist dest =
	Printf.printf "Writing %d to %s...\n%!" (List.length oblist) dest;
	let rec write_objects f ol =
		match ol with 
		| ob::ol' -> 
			Printf.fprintf f "%s\n" (Object.to_string ob);
			write_objects f ol'
		| [] -> ()
	in
	let oc = open_out dest in 
		write_objects oc oblist
;;


(* osm2fgfs *)
let main () =
	Printf.printf "osm2fgfs\n%!";
	match Array.length Sys.argv with
	| i when i <> 3 -> 
		Printf.printf "use: %s src.xml dest.stg\n" (Array.get Sys.argv 0)
	| _ -> 
		let oblist = parse_osm (Array.get Sys.argv 1) in
		write_stg oblist (Array.get Sys.argv 2);
		Printf.printf "Done.\n"
;;

main ();;
