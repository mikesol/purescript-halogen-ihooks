let conf = ../spago.dhall

in      conf
    //  { dependencies =
              conf.dependencies
            # [ "aff-promise"
              , "argonaut-codecs"
              , "argonaut-core"
              , "node-fs"
              , "spec"
              , "random"
              , "debug"
              , "aff"
              , "arrays"
              , "avar"
              , "console"
              , "either"
              , "exceptions"
              , "integers"
              , "node-buffer"
              , "node-path"
              , "nullable"
              , "ordered-collections"
              , "partial"
              , "strings"
              , "tailrec"
              , "web-html"
              ]
        , sources = conf.sources # [ "test/**/*.purs"]
        }
