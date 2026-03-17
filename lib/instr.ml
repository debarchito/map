type reg = int

type instr =
  | Nop
  | Halt

  | Mov    of reg * reg
  | Set    of reg * int
  | SetF   of reg * float

  | SetNil of reg

  | Add  of reg * reg * reg
  | Sub  of reg * reg * reg
  | Mul  of reg * reg * reg
  | Div  of reg * reg * reg
  | Mod  of reg * reg * reg
  | AddF of reg * reg * reg
  | SubF of reg * reg * reg
  | MulF of reg * reg * reg
  | DivF of reg * reg * reg

  | And  of reg * reg * reg
  | Or   of reg * reg * reg
  | Xor  of reg * reg * reg
  | Shl  of reg * reg * reg
  | Shr  of reg * reg * reg
  | ShrU of reg * reg * reg

  | Eq   of reg * reg * reg
  | EqF  of reg * reg * reg
  | Ne   of reg * reg * reg
  (* | NeF   of reg * reg * reg *)
  | Lt   of reg * reg * reg
  | LtU  of reg * reg * reg
  | LtF  of reg * reg * reg
  | Lte  of reg * reg * reg
  | LteU of reg * reg * reg
  | LteF of reg * reg * reg

  | I2F of reg * reg
  | F2I of reg * reg

  | Alloc    of reg * int * int
  | GetField of reg * reg * int
  | SetField of reg * int * reg

  | Jmp of int
  | Jz  of reg * int
  | Jnz of reg * int

  | Call  of int * reg * reg * reg
  | TCall of int * reg * reg
  | NCall of reg * reg * reg * reg
  | Ret   of reg

  | Try    of int * reg
  | EndTry
  | Throw  of reg

let string_of_instr = function
  | Nop              -> "Nop"
  | Mov    (d,s)     -> Printf.sprintf "Mov        r%d <- r%d" d s
  | Set    (d,n)     -> Printf.sprintf "Set        r%d <- %d" d n
  | SetF   (d,f)     -> Printf.sprintf "SetF       r%d <- %g" d f
  | SetNil  d        -> Printf.sprintf "SetNil     r%d" d
  | Add  (d,a,b)     -> Printf.sprintf "Add        r%d <- r%d + r%d" d a b
  | Sub  (d,a,b)     -> Printf.sprintf "Sub        r%d <- r%d - r%d" d a b
  | Mul  (d,a,b)     -> Printf.sprintf "Mul        r%d <- r%d * r%d" d a b
  | Div  (d,a,b)     -> Printf.sprintf "Div        r%d <- r%d / r%d" d a b
  | Mod  (d,a,b)     -> Printf.sprintf "Mod        r%d <- r%d %% r%d" d a b
  | AddF (d,a,b)     -> Printf.sprintf "AddF       r%d <- r%d + r%d" d a b
  | SubF (d,a,b)     -> Printf.sprintf "SubF       r%d <- r%d - r%d" d a b
  | MulF (d,a,b)     -> Printf.sprintf "MulF       r%d <- r%d * r%d" d a b
  | DivF (d,a,b)     -> Printf.sprintf "DivF       r%d <- r%d / r%d" d a b
  | Eq   (d,a,b)     -> Printf.sprintf "Eq         r%d <- r%d == r%d" d a b
  | EqF  (d,a,b)     -> Printf.sprintf "EqF        r%d <- r%d == r%d" d a b
  | Ne   (d,a,b)     -> Printf.sprintf "Ne         r%d <- r%d != r%d" d a b
  | Lt   (d,a,b)     -> Printf.sprintf "Lt         r%d <- r%d < r%d" d a b
  | LtU  (d,a,b)     -> Printf.sprintf "LtU        r%d <- r%d < r%d (u)" d a b
  | LtF  (d,a,b)     -> Printf.sprintf "LtF        r%d <- r%d < r%d" d a b
  | Lte  (d,a,b)     -> Printf.sprintf "Lte        r%d <- r%d <= r%d" d a b
  | LteU (d,a,b)     -> Printf.sprintf "LteU       r%d <- r%d <= r%d (u)" d a b
  | LteF (d,a,b)     -> Printf.sprintf "LteF       r%d <- r%d <= r%d" d a b
  | And  (d,a,b)     -> Printf.sprintf "And        r%d <- r%d & r%d" d a b
  | Or   (d,a,b)     -> Printf.sprintf "Or         r%d <- r%d | r%d" d a b
  | Xor  (d,a,b)     -> Printf.sprintf "Xor        r%d <- r%d ^ r%d" d a b
  | Shl  (d,a,b)     -> Printf.sprintf "Shl        r%d <- r%d << r%d" d a b
  | Shr  (d,a,b)     -> Printf.sprintf "Shr        r%d <- r%d >> r%d" d a b
  | ShrU (d,a,b)     -> Printf.sprintf "ShrU       r%d <- r%d >>> r%d" d a b
  | I2F  (d,s)       -> Printf.sprintf "I2F        r%d <- (float)r%d" d s
  | F2I  (d,s)       -> Printf.sprintf "F2I        r%d <- (int)r%d" d s
  | Alloc (d,t,s)    -> Printf.sprintf "Alloc      r%d tag=%d size=%d" d t s
  | GetField (d,o,f) -> Printf.sprintf "GetField   r%d <- r%d[%d]" d o f
  | SetField (o,f,s) -> Printf.sprintf "SetField   r%d[%d] <- r%d" o f s
  | Jmp   t          -> Printf.sprintf "Jmp        -> %d" t
  | Jnz  (r,t)       -> Printf.sprintf "Jnz        r%d -> %d" r t
  | Jz   (r,t)       -> Printf.sprintf "Jz         r%d -> %d" r t
  | Call (t,s,e,rd)  -> Printf.sprintf "Call       pc=%d args=[r%d..r%d] ret->r%d" t s e rd
  | TCall (t,s,e)    -> Printf.sprintf "TCall      pc=%d args=[r%d..r%d]" t s e
  | NCall (f,s,e,rd) -> Printf.sprintf "NCall      r%d args=[r%d..r%d] ret->r%d" f s e rd
  | Ret  r           -> Printf.sprintf "Ret        r%d" r
  | Try  (h,c)       -> Printf.sprintf "Try        handler=%d catch->r%d" h c
  | EndTry           -> "EndTry"
  | Throw r          -> Printf.sprintf "Throw      r%d" r
  | Halt             -> "Halt"
