let conf = ../spago.dhall

in      conf
    //  { sources = conf.sources # [ "../src/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "aff"
              , "console"
              , "halogen-storybook"
              , "foreign-object"
              ]
        }
