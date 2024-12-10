module nat

import std/core/types
import std/core/int

abstract type nat
  Nat(i : int)

pub inline fip fun int(^x : nat) : int
  match x
    Nat(i) -> i
      
pub fun nat(x : int) : nat
  if x<0 then Nat(0) else Nat(x)
  
pub fun abs-nat(x : int) : nat
  if x<0 then Nat(~x) else Nat(x)

pub inline fip fun cmp(^x : nat, ^y : nat) : order
  if (x==y) then Eq elif (x>y) then Gt else Lt
  
pub inline fip fun(==)(^x : nat, ^y : nat) : bool
  x.int == y.int
  
pub inline fip fun(!=)(^x : nat, ^y : nat) : bool
  x.int != y.int
pub inline fip fun(<=)(^x : nat, ^y : nat) : bool
  x.int <= y.int
pub inline fip fun(>=)(^x : nat, ^y : nat) : bool
  x.int >= y.int
pub inline fip fun(<)(^x : nat, ^y : nat) : bool
  x.int < y.int
pub inline fip fun(>)(^x : nat, ^y : nat) : bool
  x.int > y.int

pub fun (+)(x : nat, y : nat) : nat
  Nat(x.int + y.int)

pub fun (-)(x : nat, y : nat ) : int
  x.int - y.int
  
pub fun (\)(x : nat, y : nat ) : nat
  nat(x - y)

pub fun (*)(x : nat, y : nat ) : nat
  Nat(x.int * y.int)
  
pub fun (/)(x : nat, y : nat ) : nat
  Nat(x.int / y.int)


pub fun is-odd(x : nat) : bool
  x.int.is-odd
  
pub fun is-zero(x : nat) : bool
  x.int.is-zero
  
pub fun inc(x : nat) : nat
  x + 1.nat
  
pub fun dec(x : nat) : nat
  x \ 1.nat
  
pub fun is-even(x : nat) : bool
  !x.is-odd


pub fun min( x : nat, y : nat ) : nat
  if x <= y then x else y

pub fun max( x : nat, y : nat ) : nat
  if x >= y then x else y

pub fun show(x : nat) : string
  x.int.show

pub fun bool(x : nat) : bool
  x != 0.nat
