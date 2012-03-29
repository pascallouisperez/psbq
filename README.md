**Type Inference in Presence of Positive Subtyping**

Prototype implementation of the type inference system described in [this paper](https://github.com/pascallouisperez/psbq/raw/master/type-inference-positive-subtyping.pdf). You'll need [SML/NJ](http://www.smlnj.org/) to try it out.

For instance, typing the identify function `fn x => x`

    $ sml -m bq.cm
    ...
    - Bq.i "fn x => x";
    fn x => x : \X1|true.X1 -> X1
    val it = () : unit

Which correctly produces the polymorphic type `X -> X`. See the paper for additional examples you can try.
