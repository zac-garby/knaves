# knaves

A solver for Knights and Knaves logic problems. For example:

```
Alice: If Bob lies, then so does Carol.
Bob: Alice and Carol are not both lying.
Carol: Neither Alice nor Bob are truthful.
```

This can be translated into a series of logical propositions, if `A = true iff Alice tells the truth`, and similarly for `B` and `C`:

```
A <=> ~B => ~C
B <=> A | C
C <=> ~A & ~B
```

Then, the solution to the problem is a true/false value for `A`, `B`, and `C`, such that each of the formulas is satisfied. That's what this program does. It takes a list of statements, and who said them, in a form similar to the one I wrote above, and finds any assignment of truth values where all of them are satisfied.

## Usage

The best way to use it is probably in GHCi. There are two example problems defined in the source code, `prob1` and `prob2`. `prob1` is the problem I looked at earlier and is defined in Haskell as:

```haskell
prob1 =
  [ ("A", Not (V "B") ==> Not (V "C"))
  , ("B", V "A" ||| V "C")
  , ("C", Not (V "A") &&& Not (V "B"))
  ]
```

You can ask Haskell to solve it for you by entering `solve prob1`, and it will give you the answer `[[("A",True),("B",True),("C",False)]]`. This is interpreted as: "Alice and Bob are telling the truth, Carol is lying", and if you look at the original problem with this solution you can see that it does indeed work (i.e. no contradictions).
