open Types
open Parser
open TokenizerFactory
open Inference
open BQEnv

val i = Bq.i

fun p(expression : string) : unit =
  print(Expressions.toString(parse(tokenize expression)) ^ "\n")

fun s(constraint : string) : unit =
  print(toString_constraint(Constraints.simplify(parseConstraint(tokenize constraint))) ^ "\n")

fun c(constraint : string) : Constraint =
  parseConstraint (tokenize constraint)
