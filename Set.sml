structure Set =
  RedBlackSetFn
    (struct
       type ord_key = string
       val compare = String.compare
     end)