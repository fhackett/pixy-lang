# The Pixy language

This repository contains an informal discussion of the under-construction Pixy programming language.

Pixy is a dataflow language and inherits many of its ideas and semantics from [Lucid](http://www.thefullwiki.org/Lucid_(programming_language)).
As a result, Pixy is declarative and any value represents an infinite stream of values.
One can also think of expressions as coroutines that yield an infinite series of values.
Both views can be powerful tools to understand how Pixy works.

## Goals

Pixy's goal is to take the declarative syntax and expressive semantics introduced by Lucid and make them usable in high-performance and potentially bounded-latency environments.

This introduces some constraints to the language:
- It is unacceptable for an expression's evaluation to implicitly require unbounded amounts of memory
- The core language should not require any runtime library code
- Programmers should only need to make a minimum of assumptions about concurrency or lack thereof when designing algorithms
- Typechecking should make it hard or ideally impossible to compile demonstrably malformed programs (i.e deadlocks, bad accesses)

## Current language issues


### Non-unary primitive operations

Take the example:
```
a+b
```

Are `a` and `b` synchronous or asynchronous?
What would be the most reasonable behaviour if there is not a 1-1 relationship between the rates at which a and b yield values?

A naive solution might be to buffer the inputs, but doing this violates the implicit unbounded memory usage constraint.
Many cases are much simpler, and mechanisms could be developed to let the programmer decide in cases that would otherwise require the naive approach.

### Some lucid operators can require unbounded memory

```
a fby b
```

This means "yield the first value of a followed by all the values of b".

The problem is that to produce the illusion that a and b are advancing concurrently b may need to advance before the first value of a is yielded, or even calculated.
A naive solution would be to buffer b's output but that may consume arbitrary amounts of memory depending on a and b's relative timings.
How much can be resolved reasonably via static checking?
How would a programmer control the other cases?

### Recursive functions

In Lucid all function-like constructs describe coroutines.
This means even basic stack allocation-based recursion is not feasible in the general case.
Even if we could implement fully stack-allocated coroutines in the general case the arbitrary stack usage would violate restriction on implicit unbounded memory usage - and might lead to unexpected stack overflows.

In practical Lucid, recursive function application is often used to express delays.
Since Pixy already provides iteration as a primitive, a current proposition would be to allow what the creators of Lucid described as "hiatons" - a primitive that represents the empty stream, or in terms of coroutines the act of yielding with no value.

This does not however account for cases where arbitrary memory usage is intended, and how resource management should be done is an open question.

### Typechecking

There are two (or three depending on your point of view) issues that relate to typechecking in Pixy.
There is the actual compatibility of values, there is the temporal compability of different expressions and there is resource management.

Discussions so far have yielded thoughts on a two-dimensional type system where values have types typical of values in other languages as well as a temporal type.
This temporal type would be some kind of recursive type with one or more inputs to the program as a base value.

Imagine a program that reads a UNIX domain socket. The UNIX API call would be one source of "time".
If the result of that call were then passed to an expression that yielded all of the values in its input at half speed, that constraint would be added on to the temporal type of the socket call, creating a result with a new temporal type.
Combining the value of this new temporal type with a constant would be fine since it doesn't matter at what time you take a constant's value.
Combining that value with a value of a different temporal type would require some kind of synchronisation, depending on how compatible those types are.

This typechecking might have very interesting properties in terms of enforcing very expressive asynchronous correctness guarantees.
Combining asynchronous values would fail to compile unless an appropriate synchronisation strategy was used, effectively checking that code is written to account for all sequences of events that are considered possible by the typechecker.

### Resource management

Resource management has yet to be considered in any detail.

