# TODO

## Interactive interface (issue #6)

- [ ] allow to write out changed sessions

- [ ] support commands as given in the following help text

        , "role add|del <role> <juniorRole>*"
        , "user add|del <user> <roles>*"
        , "permission add <operation> <object> <roles>*"
        , "permission del <operation>_<object>"
        , "value add|del <name> <name>+" ]

- [ ] Check the remaining commands from INCITS 359-2012

- [ ] RCL Categories: After what changes do statements need to be
  evaluated?

## RCL extensions

- [ ] allow type annotations for disambiguation

- [ ] support multiple types for user sets

- [ ] better set notation for user sets (with optional type)

        CR:Rss = {{r1, r2}, {r3, r4}}

## Access history

- [ ] extend RCL by a UP = U x P relation reflecting the access
  history

- [ ] give usage example (chinese wall, ordered access)

## Release

- [ ] make a proper release

- [ ] move git repo to public (cmaeder) github

## Papers possible

- [ ] implementation in Haskell as functional pearl

- [ ] on RCL 2020
