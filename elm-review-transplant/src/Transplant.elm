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
import Elm.Writer
import Json.Encode as Encode
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
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


moduleVisitor :
    Rule.ModuleRuleSchema {} ModuleContext
    -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor expressionVisitor


type alias ProjectContext =
    { targets : Dict ModuleName (List Target)
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { targets = Dict.empty
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule moduleKey moduleName projectContext =
    { todoTargets = []
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleName moduleContext =
    { targets = Dict.singleton (Node.value moduleName) moduleContext.todoTargets
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
    projectContext.targets
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
