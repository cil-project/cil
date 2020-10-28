open Yojson.Safe
module Result = Ppx_deriving_yojson_runtime.Result
(* JSON-query has the following form:
        select: ...
        type: ...
        target: ...
        find: ...
        structure: ...
        constraint: ... *)

type selectable =
  | Name_sel [@name "name"]
  | Location_sel [@name "location"] [@to_yojson fun x -> `String x]
  | Type_sel [@name "type"] [@to_yojson fun x -> `String x]
  | ID_sel [@name "id"] [@to_yojson fun x -> `String x]
[@@deriving yojson]

type select = selectable list [@@deriving yojson]

type kind =
  | Var_k [@name "var"]
  | Fun_k [@name "fun"]
  | Datatype_k [@name "datatype"]
[@@deriving yojson]

type target =
  | Name_t of string [@name "name"]
  | ID_t of int [@name "id"]
  | All_t [@name "all"]
  | AllGlobVar_t [@name "all_glob_var"]
  | Or_t of string list [@name "or"]
  | And_t of string list [@name "and"]
[@@deriving yojson]

type find =
  | Uses_f [@name "uses"]
  | Decl_f [@name "decl"]
  | Defs_f [@name "defs"]
  | UsesWithVar_f of string [@name "uses_with_var"]
  | Returns_f [@name "returns"]
[@@deriving yojson]

type structure =
  | Fun_s of string [@name "fun_name"]
  | Cond_s [@name "cond"]
  | NonCond_s [@name "non-cond"]
  | None_s [@name "none"]
[@@deriving yojson]

type constr = Constraint_c of string [@name "constr"] | None_c [@name "none"]
[@@deriving yojson]

(* Type-definition of a query for mapping use *)
type query = {
  sel : select; [@key "select"]
  k : kind; [@key "type"]
  tar : target; [@key "target"]
  f : find; [@key "find"]
  str : (structure[@default None_s]); [@key "structure"]
  lim : (constr[@default None_c]); [@key "constraint"]
}
[@@deriving yojson]

(* toString-function for query *)

let to_string_q query = Yojson.Safe.to_string (query_to_yojson query)

exception Error of string

let parse_json_file filename =
  let jsonTree = from_file filename in
  let derived = query_of_yojson jsonTree in
  match derived with Result.Ok y -> y | Result.Error x -> raise (Error x)
