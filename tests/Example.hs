module Example where

import Example.CreateAtom (createAtom)
import Test.HUnit (Assertion)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

exampleTests :: Test
exampleTests = testGroup "Examples"
    [ testCase "example code to create an atom feed typechecks" typeCheckAtom
    ]

typeCheckAtom :: Assertion
typeCheckAtom = case createAtom of
                    Just ""  -> error "createAtom returned an empty Text"
                    Just _   -> return ()
                    Nothing  -> error "createAtom returned document with unresolved entities"
