SMLofNJ.exportFn("bqbin", fn(_, args) =>
  let
    val t = case args
      of [exp] => Bq.i exp
       | [mode, exp] => (case mode
           of "ti" => Bq.i exp
            | "dc" => Bq.dc exp
            | "ws" => Bq.ws exp
            | _ => print "mode: ti, dc\n")
       | _ => print "bq expression\n"
  in
    OS.Process.success
  end)
