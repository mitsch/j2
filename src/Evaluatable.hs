module Evaluatable ( Evaluatable, evaluate) where

import AST ( Expression(..), Statement(..) )

class Evaluatable e where
    evaluate :: e a -> m (Value, a)


instance Evaluatable Expression where
    evaluate (NoneExpr a) = return (NoneVal, a)
    evaluate (BoolExpr b a) = return (BoolVal b, a)
    evaluate (IntegerExpr i a) = return (IntegerVal i, a)
    evaluate (NumberExpr f a) = return (DecimalVal f, a)
    evaluate (StringExpr s a) = return (StringVal s, a)
    evaluate (SymbolExpr s a) = fmap (\v -> (v,a)) $ resolveName s
    evaluate (ListExpr es a) = do
        { es' <- mapM evaluate es
        ; return (ListVal $ fmap fst es', a)
        }
    evaluate (DictionaryExpr es a) = do
        { es' <- mapM (\(a,b) -> (\c d -> (fst c, fst d)) <$> evaluate a <*> evaluate b) es
        ; return (dictionaryVal es', a)
        }
    evaluate (NegateExpr e a) = do
        { (e', ea) <- evaluate e
        ; n <- anyOf
            [ (integerVal . negate) <$> expectInteger e'
            , (decimalVal . negate) <$> expectDecimal e'
            ]
        ; return (n, a)
        }
    evaluate (ComplementExpr e a) = do
        { (e', ea) <- evaluate e
        ; c <- anyOf
            [ True            <$  expectNone e'
            , not             <$> expectBool e'
            , (0==)           <$> expectInteger e'
            , (0==).numerator <$> expectDecimal e'
            , null            <$> expectString e'
            , null            <$> expectList e'
            , null            <$> expectObject e'
            , False           <$  expectFunction e'
            ]
        ; return (boolVal c, a)
        }
    evaluate (MemberExpr n e a) = do
        { (e', ea) <- evaluate e
        ; os <- expectObject e'
        ; x <- case lookup n os of
            { Just x -> return x
            ; Nothing -> F.fail $ "Cannot find member " ++ show n
            }
        ; return (x, a)
        }
    evaluate (IndexExpr i e a) = do
        { (e', ea) <- evaluate e
        ; (i', ia) <- evaluate i
        ; x <- anyOf
            [ do{ ls <- expectList e'
                ; n <- expectInteger i'
                ; case listToMaybe $ drop (fromEnum n) ls of
                    { Just x -> return x
                    ; Nothing -> F.fail $ "Cannot get index " ++ show n
                    }
                }
            , do{ os <- expectDictionary e'
                ; case lookup i' os of
                    { Just x -> return x
                      -- TODO fix formating of i'
                    ; Nothing -> F.fail $ "Cannot find " ++ "<value of i>" ++ " as key"
                    }
                }
            ]
        ; return (x, a)
        }
    evaluate (AddExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (integerVal.).(+)
                <$> expectInteger l'
                <*> expectInteger r'
            , (decimalVal.).(+)
                <$> expectDecimal l'
                <*> expectDecimal r'
            , (decimalVal.).(+)
                <$> (toRational <$> expectInteger l')
                <*> expectDecimal r'
            , (decimalVal.).(+)
                <$> expectDecimal l'
                <*> (toRational <$> expectInteger r')
            ]
        ; return (x, a)
        }
    evaluate (SubtractExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (integerVal.).(-)
                <$> expectInteger l'
                <*> expectInteger r'
            , (decimalVal.).(-)
                <$> expectDecimal l'
                <*> expectDecimal r'
            , (decimalVal.).(-)
                <$> (toRational <$> expectInteger l')
                <*> expectDecimal r'
            , (decimalVal.).(-)
                <$> expectDecimal l'
                <*> (toRational <$> expectInteger r')
            ]
            ; return (x, a)
        }
    evaluate (MultiplyExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (integerVal.).(*)
                <$> expectInteger l'
                <*> expectInteger r'
            , (decimalVal.).(*)
                <$> expectDecimal l'
                <*> expectDecimal r'
            , (decimalVal.).(*)
                <$> (toRational <$> expectInteger l')
                <*> expectDecimal r'
            , (decimalVal.).(*)
                <$> expectDecimal l'
                <*> (toRational <$> expectInteger r')
            ]
        ; return (x, a)
        }
    evaluate (DivideExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ;       (\lh rh -> (decimalVal $ lh / rh, a))
            <$> anyOf [ toRational <$> expectInteger l', expectDecimal l']
            <*> anyOf [ toRational <$> expectInteger r', expectDecimal r']
        }
    evaluate (IntegralDivideExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (integerVal.).div
                <$> expectInteger l'
                <*> expectInteger r'
            {- TODO other overloads! -} 
            ]
        ; return (x, a)
        }
    evaluate (ModuloExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (integerVal.).(mod)
                <$> expectInteger l'
                <*> expectInteger r'
            {- TODO other overloads! -}
            ]
        ; return (x, a)
        }
    evaluate (SameExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; return (boolVal $ l' == r', a)
        }
    evaluate (NotSameExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; return (boolVal $ l' /= r', a)
        }
    evaluate (LessExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (<) <$> expectInteger l' <*> expectInteger r'
            , (<) <$> expectDecimal l' <*> expectDecimal r'
            , (<) <$> (toRational <$> expectInteger l') <*> expectDecimal r'
            , (<) <$> expectDecimal l' <*> (toRational <$> expectInteger r')
            , (<) <$> expectString l' <*> expectString r'
            {- TODO List, Dictionary -}
            ]
        ; return (boolVal x, a)
        }
    evaluate (LessEqualExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (<=) <$> expectInteger l' <*> expectInteger r'
            , (<=) <$> expectDecimal l' <*> expectDecimal r'
            , (<=) <$> (toRational <$> expectInteger l') <*> expectDecimal r'
            , (<=) <$> expectDecimal l' <*> (toRational <$> expectInteger r')
            , (<=) <$> expectString l' <*> expectString r'
            {- TODO List, Dictionary -}
            ]
        ; return (boolVal x, a)
        }
    evaluate (GreaterExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (>) <$> expectInteger l' <*> expectInteger r'
            , (>) <$> expectDecimal l' <*> expectDecimal r'
            , (>) <$> (toRational <$> expectInteger l') <*> expectDecimal r'
            , (>) <$> expectDecimal l' <*> (toRational <$> expectInteger r')
            , (>) <$> expectString l' <*> expectString r'
            {- TODO List, Dictionary -}
            ]
        ; return (boolVal x, a)
        }
    evaluate (GreaterEqualExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (>=) <$> expectInteger l' <*> expectInteger r'
            , (>=) <$> expectDecimal l' <*> expectDecimal r'
            , (>=) <$> (toRational <$> expectInteger l') <*> expectDecimal r'
            , (>=) <$> expectDecimal l' <*> (toRational <$> expectInteger r')
            , (>=) <$> expectString l' <*> expectString r'
            {- TODO List, Dictionary -}
            ]
        ; return (boolVal x, a)
        }
    evaluate (InExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (l' `elem`) <$> expectList r'
            , (l' `elem`).(fmap fst) <$> expectDictionary r'
            ]
        ; return (boolVal x, a)
        }
    evaluate (NotInExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (l' `notElem`) <$> expectList r'
            , (l' `notElem`).(fmap fst) <$> expectDictionary r'
            ]
        ; return (boolVal x, a)
        }
    evaluate (IsExpr l r a) = F.fail $ "The \"is\" operator is not supported so far!"
    evaluate (IsNotExpr l r a) = F.fail $ "The \"is not\" operator is not supported so far!"
    evaluate (AndExpr l r a) = g <$> evaluate l <*> evaluate r
        where g l' r' = (boolVal $ (toBool $ fst l') && (toBool $ fst r'), a)
    evaluate (OrExpr l r a) = g <$> evaluate l <*> evaluate r
        where g l' r' = (boolVal $ (toBool $ fst l') || (toBool $ fst r'), a)
    evaluate (SliceExpr f l i e a) = F.fail $ "Slice is not supported so far!"
    {-
        =   \e' f' l' i' -> do
            { anyOf
                [ expectList
                , 
                ]
            ; 
            }
        <$> do
            { e' <- (fst <$> evaluate e)
            ; anyOf
                [  <$> expectList e'
                , 
                ]
            }
        <*> (fst <$> traverse evaluate f)
        <*> (fst <$> traverse evaluate l)
        <*> (fst <$> traverse evaluate i)
    -}
    evaluate (TernaryExpr c p n a) = do
        { (c', ca) <- evaluate c
        ; cond <- anyOf
            [ False           <$  expectNone c'
            , id              <$> expectBool c'
            , (0/=)           <$> expectInteger c'
            , (0/=).numerator <$> expectDecimal c'
            , not.null        <$> expectString c'
            , not.null        <$> expectList c'
            , not.null        <$> expectObject c'
            ]
        ; fmap (\x -> (fst x,a)) $ case cond of
            { True -> evaluate p
            ; False -> evaluate n
            }
        }
    evaluate (CallExpr ps c a) = do
        { (c', ca) <- evaluate c
        ; f <- expectFunction c'
        ; F.fail "have not implemented function call!"
        }
    evaluate (LambdaExpr ns b a) = F.fail "Lambda expression is not supported so far"
    evaluate (ComposeExpr x f a) = F.fail "Compose expression is not supported so far"
    
