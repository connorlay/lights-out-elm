port module Stylesheets exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.File exposing (..)


port files : CssFileStructure -> Cmd msg


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "./build/index.css"
          , Css.File.compile
                [ stylesheet
                    [ html
                        [ height (pct 100)
                        , margin zero
                        , padding zero
                        ]
                    , body
                        [ height (pct 100)
                        , margin zero
                        , padding zero
                        ]
                    ]
                ]
          )
        ]
