Testing parsing and printing of cube variables

  $ cat >cube_vars.ny <<EOF
  > axiom A:Type
  > axiom B:Type
  > axiom b:B
  > def f : A -> B := x |-> b
  > def g (x:A) : B := b
  > def fg : Id (A -> B) f g := x0 x1 x2 |-> refl b
  > echo fg
  > echo refl fg
  > def fg' : Id (A -> B) f g := x |=> refl b
  > echo fg'
  > echo refl fg'

  $ narya cube_vars.ny
  x0 x1 x2 ↦ refl b
  
  x2 ⤇ b⁽⁰⁰⁾
  
  x ⤇ refl b
  
  x ⤇ b⁽⁰⁰⁾
  