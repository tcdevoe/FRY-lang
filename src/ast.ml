(* Ref is List/Layout element reference *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | In | Notin | And | Or | From
type post = Inc | Dec 
type pre = Not
type ref = ListRef | LayRef

type expr = 
    StringLit of string
|   FloatLit of string
|   IntLit of int
|   BoolLit of string
|   Id of string
|   Binop of expr * op * expr
|   Postop of expr * post
|   Preop of pre * expr
|   Ref of expr * ref * expr * expr (* ID of object * reference type * index1 * index2 *)
|   Assign of string * expr
|   Call of string * expr list
|   Noexpr

type stmt =
    Block of stmt list
|   Expr of expr
|   Return of expr
|   If of expr * stmt * stmt
|   For of expr * stmt (* needs to be a X in Y binop expr *)
|   While of expr * stmt
|   SetBuild of expr * expr * expr (* [ return-layout | element-of; expression ] *)

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
}

type program = string list * func_decl list

