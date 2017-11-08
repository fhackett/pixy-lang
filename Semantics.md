# Pixy Evaluation Semantics for static compilation

In the current model of Pixy evaluation designed for static compilation,
all Pixy values are represented by a tuple `<Value, Presence>`, where:
- Presence is a boolean indicating whether V has a meaninful value.
- Value is a value in the domain of the type of that Pixy value, or the special value `undef`
which should never be accessed or observed in any meaningful fashion.

This is analoguous to an optional type like Haskell's `Maybe`.

Functions defined by the various operators are composed and applied to each state of
a Pixy program in order to produce next states.

## Constants

Any constant in Pixy is defined as the literal <Constant, 1>. It always has a value
and that value does not change, so it can be used as a compile-time constant.

## next

Next is still quite simple and can be implemented without worrying about the issues highlighted
in the introduction. It simply passes through its input, except that it drops the first value
of its input that is present.

The next function will read a value tuple and one piece of boolean state called DroppedOne, represented like this:
`<<Value, Presence>, DroppedOne>`
where `DroppedOne` is initially 0.


```
next(<<Value, Presence>, DroppedOne>) =
    if Presence then
        if DroppedOne then
            <<Value, Presence>, 1>
        else
            <<undef, 0, 1>
    else
        <<undef, 0, DroppedOne>
```

## fby

There is no nice way to implement `fby` in the general case without using arbitrary amounts of memory.

This is because LHS may yield faster than RHS. I originally had a version that almost worked which used
the idea of inhibiting computation - if LHS has not yielded yet, do not evaluate RHS or any of its dependencies.
This works for some trivial cases, but deadlocks if LHS depends on the same values as RHS. Then, LHS cannot
compute its value either and `fby` waits forever.

This version of `fby` was also very complex - it is best expressed as a large truth table (available on request).

This version of `fby` is also potentially inefficient as there is instruction-level parallelism to exploit
in the simultaneous evaluation of LHS and RHS, which it ignored by blocking RHS, thus serializing the computation.

