# elm-review-transplant

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`Transplant`](https://package.elm-lang.org/packages/dillonkearns/elm-review-transplant/1.0.0/Transplant) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import Transplant
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Transplant.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template dillonkearns/elm-review-transplant/example
```
