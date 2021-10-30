(** Sub-logics for the main SMT-LIB logic (see http://smtlib.cs.uiowa.edu/logics.shtml)  *)
type logic =
  | QF_AX
      (** Closed quantifier-free formulas over the theory of arrays with
      extensionality.  *)
  | QF_IDL
      (** Difference Logic over the integers. In essence, Boolean combinations of inequations of the
         form x - y < b where x and y are integer variables and b is an integer constant. *)
  | QF_UF
      (** Unquantified formulas built over a signature of uninterpreted (i.e., free) sort and
        function symbols. *)
  | QF_BV (** Closed quantifier-free formulas over the theory of fixed-size bitvectors. *)
  | QF_RDL
      (** Difference Logic over the reals. In essence, Boolean combinations of inequations of the
        form x - y < b where x and y are real variables and b is a rational constant. *)
  | QF_LIA
      (** Unquantified linear integer arithmetic. In essence, Boolean combinations of inequations
          between linear polynomials over integer variables. *)
  | QF_UFIDL
      (** Difference Logic over the integers (in essence) but with uninterpreted sort and function
        symbols. *)
  | QF_UFBV
      (** Unquantified formulas over bitvectors with uninterpreted sort function and symbols. *)
  | QF_ABV
      (** Closed quantifier-free formulas over the theory of bitvectors and bitvector arrays. *)
  | QF_LRA
      (** Unquantified linear real arithmetic. In essence, Boolean combinations of inequations
        between linear polynomials over real variables. *)
  | QF_NIA (** Quantifier-free integer arithmetic. *)
  | LIA (** Closed linear formulas over linear integer arithmetic. *)
  | QF_ALIA (** Quantifier-free arrays and linear arithmetic. *)
  | QF_UFLIA
      (** Unquantified linear integer arithmetic with uninterpreted sort and function symbols. *)
  | QF_UFLRA
      (** Unquantified linear real arithmetic with uninterpreted sort and function symbols.  *)
  | QF_AUFBV
      (** Closed quantifier-free formulas over the theory of bitvectors and bitvector arrays
          extended with free sort and function symbols. *)
  | LRA (** Closed linear formulas in linear real arithmetic. *)
  | QF_NRA (** Quantifier-free real arithmetic. *)
  | NIA (** Quantifier-free integer arithmetic. *)
  | ALIA (** Arrays and linear arithmetic. *)
  | QF_AUFLIA
      (** Closed quantifier-free linear formulas over the theory of integer arrays extended with
            free sort and function symbols. *)
  | QF_UFNIA
      (** Unquantified integer arithmetic with uninterpreted sort and function symbols.  *)
  | UFLRA
      (** Unquantified linear real arithmetic with uninterpreted sort and function symbols. *)
  | NRA (** Real arithmetic. *)
  | UFNIA
      (** Non-linear integer arithmetic with uninterpreted sort and function symbols. *)
  | AUFLIA
      (** Closed formulas over the theory of linear integer arithmetic and arrays extended with free
        sort and function symbols but restricted to arrays with integer indices and values. *)
  | AUFLIRA
      (** Closed linear formulas with free sort and function symbols over one- and two-dimentional
          arrays of integer index and real value. *)
  | AUFNIRA
      (** Closed formulas with free function and predicate symbols over a theory of arrays of arrays
        of integer index and real value. *)
  | UFDTLIA
  | UFDTLRA
  | UFDTNIA
  | UFDTNRA
  | DTNIA
  | DTLIA
  | ALL (** Default "ALL" logics  *)

let to_string (l : logic) =
  match l with
  | UFDTLIA -> "UFDTLIA"
  | UFDTLRA -> "UFDTLRA"
  | UFDTNIA -> "UFDTNIA"
  | UFDTNRA -> "UFDTNRA"
  | DTLIA -> "DTLIA"
  | DTNIA -> "DTNIA"
  | QF_AX -> "QF_AX"
  | QF_IDL -> "QF_IDL"
  | QF_UF -> "QF_UF"
  | QF_BV -> "QF_BV"
  | QF_RDL -> "QF_RDL"
  | QF_LIA -> "QF_LIA"
  | QF_UFIDL -> "QF_UFIDL"
  | QF_UFBV -> "QF_UFBV"
  | QF_ABV -> "QF_ABV"
  | QF_LRA -> "QF_LRA"
  | QF_NIA -> "QF_NIA"
  | LIA -> "LIA"
  | QF_ALIA -> "QF_ALIA"
  | QF_UFLIA -> "QF_UFLIA"
  | QF_UFLRA -> "QF_UFLRA"
  | QF_AUFBV -> "QF_AUFBV"
  | LRA -> "LRA"
  | QF_NRA -> "QF_NRA"
  | NIA -> "NIA"
  | ALIA -> "ALIA"
  | QF_AUFLIA -> "QF_AUFLIA"
  | QF_UFNIA -> "QF_UFNIA"
  | UFLRA -> "UFLRA"
  | NRA -> "NRA"
  | UFNIA -> "UFNIA"
  | AUFLIA -> "AUFLIA"
  | AUFLIRA -> "AUFLIRA"
  | AUFNIRA -> "AUFNIRA"
  | ALL -> "ALL"
;;

let of_string (l : string) =
  match l with
  | "UFDTLIA" -> UFDTLIA
  | "UFDTLRA" -> UFDTLRA
  | "UFDTNIA" -> UFDTNIA
  | "UFDTNRA" -> UFDTNIA
  | "DTLIA" -> DTLIA
  | "DTNIA" -> DTNIA
  | "QF_AX" -> QF_AX
  | "QF_IDL" -> QF_IDL
  | "QF_UF" -> QF_UF
  | "QF_BV" -> QF_BV
  | "QF_RDL" -> QF_RDL
  | "QF_LIA" -> QF_LIA
  | "QF_UFIDL" -> QF_UFIDL
  | "QF_UFBV" -> QF_UFBV
  | "QF_ABV" -> QF_ABV
  | "QF_LRA" -> QF_LRA
  | "QF_NIA" -> QF_NIA
  | "LIA" -> LIA
  | "QF_ALIA" -> QF_ALIA
  | "QF_UFLIA" -> QF_UFLIA
  | "QF_UFLRA" -> QF_UFLRA
  | "QF_AUFBV" -> QF_AUFBV
  | "LRA" -> LRA
  | "QF_NRA" -> QF_NRA
  | "NIA" -> NIA
  | "ALIA" -> ALIA
  | "QF_AUFLIA" -> QF_AUFLIA
  | "QF_UFNIA" -> QF_UFNIA
  | "UFLRA" -> UFLRA
  | "NRA" -> NRA
  | "UFNIA" -> UFNIA
  | "AUFLIA" -> AUFLIA
  | "AUFLIRA" -> AUFLIRA
  | "AUFNIRA" -> AUFNIRA
  | "ALL" -> ALL
  | _ -> ALL
;;

type theory =
  | ArraysEx
  | Core
  | FixedSizeBitVectors
  | FloatingPoint
  | Ints
  | Reals
  | Reals_Ints
  | Strings
  | All

let base_string ?(arith = true) (t : theory) =
  match t with
  | ArraysEx -> "A"
  | Core -> ""
  | FixedSizeBitVectors -> "BV"
  | FloatingPoint -> "FP"
  | Ints -> if arith then "IA" else "IDL"
  | Reals -> "RA"
  | Reals_Ints -> "IRA"
  | Strings -> "Strings"
  | All -> "All"
;;

let join_theories (t1 : theory) (t2 : theory) =
  if t1 = t2
  then t1
  else (
    match t1, t2 with
    | Ints, Reals | Reals, Ints | Reals_Ints, (Ints | Reals) | (Ints | Reals), Reals_Ints
      -> Reals_Ints
    | Core, _ -> t2
    | _, Core -> t1
    | _, ArraysEx -> ArraysEx
    | ArraysEx, _ -> ArraysEx
    | _ -> All)
;;

let combine
    ?(arith = true)
    ?(datatypes = false)
    ?(linear = true)
    ?(qf = true)
    ?(uf = false)
    (theories : theory list)
  =
  let prefix = if qf then "QF_" else "" in
  let n_or_l s = if linear then "L" ^ s else "N" ^ s in
  let l0 = if List.mem ArraysEx theories then prefix ^ "A" else prefix in
  let l1 = if uf then l0 ^ "UF" else l0 in
  let l2 = if datatypes then l1 ^ "DT" else l1 in
  if List.mem FixedSizeBitVectors theories
  then of_string (l2 ^ base_string FixedSizeBitVectors)
  else if List.mem FloatingPoint theories
  then failwith "Floating Points not supported."
  else if (not arith) && List.mem Ints theories
  then of_string (l2 ^ base_string ~arith:false Ints)
  else if List.mem Reals_Ints theories
          || (List.mem Ints theories && List.mem Reals theories)
  then of_string (l2 ^ n_or_l (base_string Reals_Ints))
  else if List.mem Ints theories
  then of_string (l2 ^ n_or_l (base_string Ints))
  else if List.mem Reals theories
  then of_string (l2 ^ n_or_l (base_string Reals))
  else ALL
;;
