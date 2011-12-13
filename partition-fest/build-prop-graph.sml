structure BuildPropGraph = struct (* for use on the command line *)
    
    val usage = ref (fn () => ())
    val commands =
        [ ("build-graph",      (fn [inf, outf] => Basis.buildGraph inf outf
                                 | _ => !usage ())
          , "build-graph infile outfile"
          )
        , ("build-prop-graph", (fn [inf, outf] => Basis.buildGraph inf outf
                                 | _ => !usage ())
          , "build-prop-graph infile outfile"
          )
        ]
    val _ = usage :=
        (fn () => app (fn (_, _, s) => app print ["Usage: ", s, "\n"]) commands)

    
    val arg0 = CommandLine.name ()

    val _ = case List.filter (fn (n, _, _) => String.isSuffix n arg0) commands
              of [(_, f, _)] => f (CommandLine.arguments())
               | _           => !usage ()
end
