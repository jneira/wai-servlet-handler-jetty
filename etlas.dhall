    let prelude = https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/master/dhall/prelude.dhall

in  let types = https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/master/dhall/types.dhall

in  let v = prelude.v

in  let defaultLang =
          [ prelude.types.Languages.Haskell2010 {=} ] : Optional types.Language

in  let pkg =
            λ(name : Text)
          → λ(version-range : types.VersionRange)
          → { bounds = version-range, package = name }

in  let pkgAnyVer = λ(packageName : Text) → pkg packageName prelude.anyVersion

in  let commonDeps =
          [ pkg
            "base"
            ( prelude.intersectVersionRanges
              (prelude.orLaterVersion (v "4.8"))
              (prelude.earlierVersion (v "4.9"))
            )
          , pkgAnyVer "wai"
          , pkg "wai-servlet" (prelude.orLaterVersion (v "0.1.5"))
          ]

in  let updateRepo =
          prelude.utils.mapSourceRepos
          (   λ(srcRepo : types.SourceRepo)
            →   srcRepo
              ⫽ { tag =
                    [ "0.1.2.0" ] : Optional Text
                , kind =
                    prelude.types.RepoKind.RepoThis {=}
                }
          )

in  let project =
          prelude.utils.GitHub-project
          { owner = "jneira", repo = "wai-servlet-handler-jetty" }

in  updateRepo
    (   project
      ⫽ { description =
            "Wai handler to run wai applications in a embedded jetty server"
        , license =
            prelude.types.Licenses.BSD3 {=}
        , license-files =
            [ "LICENSE" ]
        , author =
            "Javier Neira Sanchez"
        , maintainer =
            "Javier Neira Sanchez <atreyu.bbb@gmail.com>"
        , version =
            v "0.1.2.0"
        , cabal-version =
            v "1.12"
        , category =
            "Web"
        , extra-source-files =
            [ "README.md" ]
        , stability =
            "Experimental"
        , library =
            prelude.unconditional.library
            (   prelude.defaults.Library
              ⫽ { exposed-modules =
                    [ "Network.Wai.Servlet.Handler.Jetty" ]
                , hs-source-dirs =
                    [ "src" ]
                , default-language =
                    defaultLang
                , build-depends =
                    commonDeps
                , maven-depends =
                    [ "javax.servlet:javax.servlet-api:3.1.0"
                    , "org.eclipse.jetty:jetty-server:9.4.5.v20170502"
                    ]
                }
            )
        , executables =
            [ prelude.unconditional.executable
              "wai-servlet-jetty-example"
              (   prelude.defaults.Executable
                ⫽ { build-depends =
                      commonDeps # [ pkgAnyVer "wai-servlet-handler-jetty" ]
                  , hs-source-dirs =
                      [ "examples" ]
                  , main-is =
                      "Main.hs"
                  , default-language =
                      defaultLang
                  }
              )
            ]
        }
    )
