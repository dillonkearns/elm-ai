module Transplant exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Expression exposing (Expression, LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Elm.Writer
import Json.Encode as Encode
import Review.Project.Dependency
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ Transplant.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template dillonkearns/elm-review-transplant/example --rules Transplant
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnusedExportedFunctions" initialProjectContext
        -- Omitted, but this will collect the list of exposed modules for packages.
        -- We don't want to report functions that are exposed
        --|> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDirectDependenciesProjectVisitor directDependenciesVisitor
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


directDependenciesVisitor : Dict String Review.Project.Dependency.Dependency -> ProjectContext -> ( List asdf, ProjectContext )
directDependenciesVisitor directDependencies previousContext =
    let
        bar : Dict String (List ( String, String ))
        bar =
            directDependencies
                |> Dict.toList
                |> List.concatMap
                    (\( _, dependency ) ->
                        {-
                           { name : String
                           , comment : String
                           , unions : List Union
                           , aliases : List Alias
                           , values : List Value
                           , binops : List Binop
                           }

                        -}
                        Review.Project.Dependency.modules dependency
                            |> List.map
                                (\module_ ->
                                    ( module_.name
                                    , module_.values
                                        |> List.map
                                            (\value ->
                                                ( value.name
                                                , value.tipe
                                                    |> typeToString
                                                )
                                            )
                                    )
                                )
                    )
                |> Dict.fromList
    in
    ( []
    , { previousContext | dependencies = bar }
    )


typeToString : Elm.Type.Type -> String
typeToString type_ =
    {- Represent Elm types as values! Here are some examples:

       Int            ==> Type "Int" []
       a -> b         ==> Lambda (Var "a") (Var "b")
       ( a, b )       ==> Tuple [ Var "a", Var "b" ]
       Maybe a        ==> Type "Maybe" [ Var "a" ]
       { x : Int }    ==> Record [("x", Type "Int" [])] Nothing
       { r | x : a }  ==> Record [("x", Var "a")] (Just "r")

    -}
    case type_ of
        Elm.Type.Var string ->
            string

        Elm.Type.Lambda type1 type2 ->
            "(" ++ typeToString type1 ++ " -> " ++ typeToString type2 ++ ")"

        Elm.Type.Tuple types ->
            "(" ++ String.join ", " (List.map typeToString types) ++ ")"

        Elm.Type.Type string types ->
            cleanTypeName string ++ " " ++ String.join " " (List.map typeToString types)

        Elm.Type.Record list maybeExtensibleParameter ->
            "{ "
                ++ (case maybeExtensibleParameter of
                        Nothing ->
                            ""

                        Just string ->
                            string ++ " | "
                   )
                ++ String.join ", " (List.map (\( string, innerType ) -> string ++ " : " ++ typeToString innerType) list)
                ++ " }"


cleanTypeName : String -> String
cleanTypeName typeName =
    case typeName of
        "String.String" ->
            "String"

        "Char.Char" ->
            "Char"

        "Maybe.Maybe" ->
            "Maybe"

        "List.List" ->
            "List"

        "Result.Result" ->
            "Result"

        "Basics.Int" ->
            "Int"

        "Task.Task" ->
            "Task"

        "Basics.Never" ->
            "Never"

        "Basics.Bool" ->
            "Bool"

        _ ->
            typeName


moduleVisitor :
    Rule.ModuleRuleSchema {} ModuleContext
    -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor expressionVisitor


type alias ProjectContext =
    { targets : Dict ModuleName (List Target)
    , dependencies : Dict String (List ( String, String ))
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { targets = Dict.empty
    , dependencies = Dict.empty
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule moduleKey moduleName projectContext =
    { todoTargets = []
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleName moduleContext =
    { targets = Dict.singleton (Node.value moduleName) moduleContext.todoTargets
    , dependencies = Dict.empty
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { targets =
        Dict.merge
            (\key a -> Dict.insert key a)
            (\key a b -> Dict.insert key (a ++ b))
            (\key b -> Dict.insert key b)
            newContext.targets
            previousContext.targets
            Dict.empty
    , dependencies =
        Dict.merge
            (\key a -> Dict.insert key a)
            (\key a b -> Dict.insert key (a ++ b))
            (\key b -> Dict.insert key b)
            newContext.dependencies
            previousContext.dependencies
            Dict.empty
    }


finalEvaluationForProject : ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluationForProject projectContext =
    []



{-

   Goals:

   Find first instance of `Debug.todo "REPLACE"`. For that code,

     - Extract type annotations from all values in the current scope from this module
     - Extract direct dependencies from elm.json



-}


type alias ModuleContext =
    { todoTargets : List Target
    }


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Elm.Syntax.Expression.LetExpression info ->
            info.declarations
                |> List.filterMap
                    (\declaration ->
                        extractTypeIfReplaceTodo declaration
                    )
                |> (\newStuff ->
                        ( []
                        , { context
                            | todoTargets = newStuff ++ context.todoTargets
                          }
                        )
                   )

        _ ->
            ( [], context )


dataExtractor : ProjectContext -> Encode.Value
dataExtractor projectContext =
    Encode.object
        [ ( "targets"
          , projectContext.targets
                |> Dict.toList
                |> List.map
                    (\( moduleName, targets ) ->
                        ( String.join "." moduleName
                        , Encode.list
                            (\target ->
                                Encode.object
                                    [ ( "row"
                                      , Encode.int target.range.start.column
                                      )
                                    , ( "column"
                                      , Encode.int target.range.start.column
                                      )
                                    , ( "type"
                                      , target.annotation
                                            |> Elm.Writer.writeTypeAnnotation
                                            |> Elm.Writer.write
                                            |> Encode.string
                                      )
                                    ]
                            )
                            targets
                        )
                    )
                |> Encode.object
          )
        , ( "dependencies"
          , Encode.object
                (List.map
                    (\( key, value ) ->
                        ( key
                        , value
                            |> List.map (Tuple.mapSecond Encode.string)
                            |> Encode.object
                        )
                    )
                    (Dict.toList
                        projectContext.dependencies
                    )
                )
          )
        ]


extractTypeIfReplaceTodo : Node Elm.Syntax.Expression.LetDeclaration -> Maybe Target
extractTypeIfReplaceTodo node =
    case Node.value node of
        Elm.Syntax.Expression.LetFunction { declaration, signature } ->
            case Node.value declaration |> .expression |> Node.value of
                Elm.Syntax.Expression.Application fnApplication ->
                    case List.map Node.value fnApplication of
                        [ Elm.Syntax.Expression.FunctionOrValue [ "Debug" ] "todo", Elm.Syntax.Expression.Literal "REPLACE" ] ->
                            signature
                                |> Maybe.map
                                    (\justSignature ->
                                        { annotation =
                                            Node.value justSignature
                                                |> .typeAnnotation
                                        , range = Node.range node
                                        }
                                    )

                        _ ->
                            Nothing

                _ ->
                    Nothing

        Elm.Syntax.Expression.LetDestructuring _ _ ->
            Nothing


type alias Target =
    { annotation : Node TypeAnnotation
    , range : Range
    }
