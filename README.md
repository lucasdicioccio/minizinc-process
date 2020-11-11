minizinc-process
================

MiniZinc is a language and a toolchain to solve discrete optimization problems.
This package offers wrappers around the `minizinc` executable to pass inputs and outputs.

Assume that a primitive MiniZinc model is available at the path `models/example001.mzn`.

```minizinc
0..100: x;
var int: y;
constraint x < y;
```

This model expects `x` as an Int and decides `y` as an Int if a solution is
found. Ideally we would like to use minizinc and this model like a function of
type `Int -> IO (Maybe Int)` function in Haskell.
This package provides building blocks to create such a mapping.

# Implementation

This package relies on JSON support for MiniZinc by using JSON as an
intermediary representation. On the Haskell side we picked the popular `aeson`
package for serializing values.

MiniZinc input binds names to variables, hence the `Int -> IO (Maybe Int)`
example above is insufficient: inputs and outputs need to translate to JSON
`Object` constructor of [Aeson's Value type](https://hackage.haskell.org/package/aeson-1.1.1.0/docs/Data-Aeson.html#t:Value).

# Example Use

The `runLastMinizincJSON` function requires some configuration object to
provide parameters like the solver backing MiniZinc, the timeout, where to
store MiniZinc data files. The `simpleMiniZinc` function provides a smart
constructor for building such an environment.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson
import Data.Hashable
import GHC.Generics
import Process.Minizinc
import Process.Minizinc.Inspect

data Input = Input {x :: Int}
  deriving (Generic)

instance ToJSON Input -- required for `runLastMinizincJSON`

instance Hashable Input -- required for `runLastMinizincJSON`

data Output = Output {y :: Int}
  deriving (Show, Generic)

instance FromJSON Output -- required for `runLastMinizincJSON`

main :: IO ()
main = do
  inspect "models/example001.mzn" >>= print
  let mzn = simpleMiniZinc @Input @Output "models/example001.mzn" 1000 Gecode
  let problem = Input 10
  runLastMinizincJSON mzn problem >>= print
```

The `@Input` and `@Output` syntax allow to pass type parameters to
`simpleMiniZinc`, this style is optional but helps the GHC compiler inference
(in our example, this type application is the only indication needed to tell
the compiler to deserialize `Output` objects).

# Usage in a project

In a typical project, you will have fixed models and varying inputs.
That is, you would like to carry the models along with the code (e.g., a web
application or gRPC server using minizinc in the background) in a same
repository as your Haskell code. One option is to leverage the support of cabal
[data-files](https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code).

You will still need some mapping functions to translate between domain objects
like `User` into the JSON values that MiniZinc requires: objects do not map
well with relations. We may consider compile-time helpers like TemplateHaskell,
but at this time it would not be immediately feasible. Be at peace with this.

For now, the implementation leverages file-system to pass the JSON object to
MiniZinc, this design means you should pay attention to disk usage and maybe
clean the clutter.

# Development

## Testing

We use [Hedgehog](hedgehog.qa) to test the overall system at once rather than having
individual tests for the internal parser and other files.

Test cases are in `.hs` files under the `test` directory whereas the `.mzn`
models are in the `models` directory.
We use a naming nomenclature to help organize what files require what
input/output types: `test{inputtype}_{testnum}.mzn` where `inputtype` pertains
to the haskell Input/Output types and testnum pertains to the test number.
Thus: all `testnum` are unique and are groupable by `inputtype`.

# Misc.

The author of this package is not affiliated with MiniZinc.
See also: [https://www.minizinc.org/](https://www.minizinc.org/).
