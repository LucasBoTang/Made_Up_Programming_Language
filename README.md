# Made_Up_Programming_Language

A simple language interpreter implemented by Racket.

## Syntax Defination

- If s is a Racket string, then (var s) is a mupl expression (a variable use).

- If n is a Racket integer, then (int n) is a mupl expression (a constant).

- If e1 and e2 are mupl expressions, then (add e1 e2) is a mupl expression (an addition).

- If s1 and s2 are Racket strings and e is a mupl expression, then (fun s1 s2 e) is a mupl expression (a function). In e, s1 is bound to the function itself (for recursion) and s2 is bound to the (one) argument. Also, (fun #f s2 e) is allowed for anonymous nonrecursive functions.

- If e1, e2, and e3, and e4 are mupl expressions, then (ifgreater e1 e2 e3 e4) is a mupl expression. It is a conditional where the result is e3 if e1 is strictly greater than e2 else the result is e4. Only one of e3 and e4 is evaluated.

- If e1 and e2 are mupl expressions, then (call e1 e2) is a mupl expression (a function call).

- If s is a Racket string and e1 and e2 are mupl expressions, then (mlet s e1 e2) is a mupl expression (a let expression where the value resulting e1 is bound to s in the evaluation of e2).

- If e1 and e2 are mupl expressions, then (apair e1 e2) is a mupl expression (a pair-creator).

- If e1 is a mupl expression, then (fst e1) is a mupl expression (getting the first part of a pair).

- If e1 is a mupl expression, then (snd e1) is a mupl expression (getting the second part of a pair).

- (aunit) is a mupl expression (holding no data, much like () in ML or null in Racket). Notice (aunit) is a mupl expression, but aunit is not.

- If e1 is a mupl expression, then (isaunit e1) is a mupl expression (testing for (aunit)).

- (closure env f) is a mupl value where f is mupl function (an expression made from fun) and env is an environment mapping variables to values. Closures do not appear in source programs; they result from evaluating functions.
