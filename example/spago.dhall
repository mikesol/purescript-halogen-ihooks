let conf = ../spago.dhall

in      conf
    //  { sources = conf.sources # [ "../src/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "aff"
              , "console"
              , "event"
              , "halogen-storybook"
              , "halogen-subscriptions"
              , "foreign-object"
              ]
        }
