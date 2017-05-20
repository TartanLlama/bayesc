(*Represents the intermediate representation*)
(*Loosely based on the one outlined in Modern Compiler Design and Implementation*)
module Tree =
struct
  type label = string
  type size = int

  type exp =
      Seq of exp * exp
    | Label of label
    | Jump of label
    | Cjump of relop * exp * exp * label
    | Store of exp * exp
    | Binop of binop * exp * exp
    | Name of label
    | IConst of int
    | FConst of float
    | Call of label * exp
    | FunDef of label * exp * exp
    | Field of label * string
    | Class of label * exp
    | ArrayElt of exp * exp

  and binop = Plus | IPlus | Minus | Mul | Div

  and relop = Eq | Ne | Lt | Gt | Le | Ge

  and logop = LAnd | LOr

end

let rec dumptree t =
  Printf.printf "%s\n" (dumpe t)

and dumpes es =
  match es with
    | [] -> ""
    | (e::es) -> Printf.sprintf "(%s %s)" (dumpe e) (dumpes es)

and dumpe t =
  match t with
    | Tree.Seq (e,es) -> Printf.sprintf "(Seq %s %s)" (dumpe e) (dumpe es)
    | Tree.Label (l) -> Printf.sprintf "(Label %s)" l
    | Tree.Jump (l) -> Printf.sprintf "(Jump %s)" l
    | Tree.Cjump (op,et,ef,l) -> Printf.sprintf "(Cjump %s %s %s %s)" (dumprel op) (dumpe et) (dumpe ef) l
    | Tree.Store (e1, e2) -> Printf.sprintf "(Store %s %s)" (dumpe e1) (dumpe e2)
    | Tree.Binop (op,e1,e2) -> Printf.sprintf "(Binop %s %s %s)" (dumpbin op) (dumpe e1) (dumpe e2)
    | Tree.Name (l) -> Printf.sprintf "(Name %s)" l
    | Tree.IConst (i) -> Printf.sprintf "(IConst %d)" i
    | Tree.FConst (f) -> Printf.sprintf "(FConst %f)" f
    | Tree.Call (l,es) -> Printf.sprintf "(Call %s %s)" l (dumpe es)
    | Tree.Field (l,t) -> Printf.sprintf "(Field %s %s)" l t
    | Tree.Class (l,e) -> Printf.sprintf "(Class %s %s)" l (dumpe e)
    | Tree.FunDef (l,e,es) -> Printf.sprintf "(FunDef %s %s %s)" l (dumpe e) (dumpe es)
    | Tree.ArrayElt (x,i) -> Printf.sprintf "(ArrayElt %s %s)" (dumpe x) (dumpe i)

and dumpbin b =
  match b with
    | Tree.Plus -> "Plus"
    | Tree.IPlus -> "IPlus"
    | Tree.Minus -> "Minus"
    | Tree.Mul -> "Mul"
    | Tree.Div -> "Div"

and dumplog l =
  match l with
    | Tree.LAnd -> "LAnd"
    | Tree.LOr -> "LOr"

and dumprel r =
  match r with
    | Tree.Eq -> "Eq"
    | Tree.Ne -> "Ne"
    | Tree.Lt -> "Lt"
    | Tree.Gt -> "Gt"
    | Tree.Le -> "Le"
    | Tree.Ge -> "Ge"
