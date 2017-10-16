import Test.HUnit
import AST (fromAST, toAST, Expr(..))

test1 = TestCase (assertEqual "should parse single primitive correctly" (toAST "1") (Just (Primitive 1)))
test2 = TestCase (assertEqual "should parse plus correctly" (toAST "1+2") (Just (Add(Primitive 1) (Primitive 2))))
test3 = TestCase (assertEqual "should parse subtract correctly" (toAST "1-2") (Just (Subtract(Primitive 1) (Primitive 2))))
test4 = TestCase (assertEqual "should parse multiply correctly" (toAST "1*2") (Just (Multiply(Primitive 1) (Primitive 2))))
test5 = TestCase (assertEqual "should parse divide correctly" (toAST "1/2") (Just (Divide(Primitive 1) (Primitive 2))))
test6 = TestCase (assertEqual "should parse many divides correctly" (toAST "1/2/3/4") (Just (Divide(Primitive 1) (Divide (Primitive 2) (Divide (Primitive 3) (Primitive 4)) ))))
test7 = TestCase (assertEqual "should parse many multiplies correctly" (toAST "1*2*3*4") (Just (Multiply(Primitive 1) (Multiply (Primitive 2) (Multiply (Primitive 3) (Primitive 4)) ))))
test8 = TestCase (assertEqual "should parse divides and multiplies correctly" (toAST "1*2/3*4") (Just (Multiply(Primitive 1) (Divide(Primitive 2) (Multiply (Primitive 3) (Primitive 4)) ))))

testIsolatedNodes =
  [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8]

test9 = TestCase (assertEqual "should transform '1' correctly" (fromAST <$> toAST "1") (Just "1"))
test10 = TestCase (assertEqual "should transform '1+2' correctly" (fromAST <$> toAST "1+2") (Just "1+2"))
test11 = TestCase (assertEqual "should transform '1-2' correctly" (fromAST <$> toAST "1-2") (Just "1-2"))
test12 = TestCase (assertEqual "should transform '1*2' correctly" (fromAST <$> toAST "1*2") (Just "1*2"))
test13 = TestCase (assertEqual "should transform '1/2' correctly" (fromAST <$> toAST "1/2") (Just "1/2"))

toAndFromASTTestsSingleNode = [TestLabel "test9" test9, TestLabel "test10" test10, TestLabel "test11" test11, TestLabel "test12" test12, TestLabel "test13" test13]

test14 = TestCase (assertEqual "should transform '1/2+3' correctly" (fromAST <$> toAST "1/2+3") (Just "1/2+3"))
test15 = TestCase (assertEqual "should transform '3+1/2' correctly" (fromAST <$> toAST "3+1/2") (Just "3+1/2"))
test16 = TestCase (assertEqual "should transform '3+1/2+4' correctly" (fromAST <$> toAST "3+1/2+4") (Just "3+1/2+4"))
test17 = TestCase (assertEqual "should transform '3+1*2+4/5' correctly" (fromAST <$> toAST "3+1*2+4/5") (Just "3+1*2+4/5"))
test18 = TestCase (assertEqual "should transform '1*2-4/5' correctly" (fromAST <$> toAST "1*2-4/5") (Just "1*2-4/5"))
test19 = TestCase (assertEqual "should transform '1+2-3+4-5' correctly" (fromAST <$> toAST "1+2-3+4-5") (Just "1+2-3+4-5"))

toAndFromASTManyNodes = [TestLabel "test14" test14, TestLabel "test15" test15, TestLabel "test16" test16, TestLabel "test17" test17, TestLabel "test18" test18, TestLabel "test19" test19]

tests = TestList $ testIsolatedNodes ++ toAndFromASTTestsSingleNode ++ toAndFromASTManyNodes
