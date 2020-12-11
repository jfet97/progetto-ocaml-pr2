let e1 = Empty("int");;

let ie1 = Insert(Empty("int"), CstInt(9));;

let s1 = Singleton(CstInt(9), "int");;

let r1 = Remove(s1, CstInt(9));;

let ie1 = IsEmpty(r1);;
let ie2 = IsEmpty(s1);;

eval s1 emptyEnv;;

eval ie1 emptyEnv;;

eval r1 emptyEnv;;

eval ie1 emptyEnv;;
eval ie2 emptyEnv;;

eval (Contains (s1, CstInt(9))) emptyEnv;;
eval (Contains (s1, CstInt(10))) emptyEnv;;

let subset = Insert(Insert(Empty("int"), CstInt(9)), CstInt(10));;
let set = Insert(subset, CstInt(11));;

eval (IsSubSet(subset, set)) emptyEnv;;
eval (IsSubSet(set, subset)) emptyEnv;;


eval (MaxEl set) emptyEnv;;
eval (MinEl set) emptyEnv;;


let set1 = Insert(Insert(Insert(Empty("int"), CstInt(1)), CstInt(2)), CstInt(3));;
let set2 = Insert(Insert(Insert(Empty("int"), CstInt(3)), CstInt(4)), CstInt(5));;

eval (Union(set1, set2)) emptyEnv;;
eval (Intersection(set1, set2)) emptyEnv;;
eval (Difference(set1, set2)) emptyEnv;;
eval (Difference(set2, set1)) emptyEnv;;

let set3 = Empty("int");;
let set4 = Insert(Insert(Insert(Empty("int"), CstInt(10)), CstInt(10)), CstInt(10));;
let set5 = Insert(Insert(Insert(Empty("int"), CstInt(10)), CstInt(11)), CstInt(10));;

eval (For_all(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set1)) emptyEnv;;
eval (For_all(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set2)) emptyEnv;;
eval (For_all(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set3)) emptyEnv;;
eval (For_all(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set4)) emptyEnv;;
eval (For_all(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set5)) emptyEnv;;

eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), For_all(Den("f"), set1))) emptyEnv;;
eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), For_all(Den("f"), set2))) emptyEnv;;
eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), For_all(Den("f"), set3))) emptyEnv;;
eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), For_all(Den("f"), set4))) emptyEnv;;
eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), For_all(Den("f"), set5))) emptyEnv;;



eval (Exists(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set1)) emptyEnv;;
eval (Exists(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set2)) emptyEnv;;
eval (Exists(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set3)) emptyEnv;;
eval (Exists(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set4)) emptyEnv;;
eval (Exists(Fun("x", Eq(Sub(CstInt(10), Den("x")), CstInt(0))), set5)) emptyEnv;;

eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), Exists(Den("f"), set1))) emptyEnv;;
eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), Exists(Den("f"), set2))) emptyEnv;;
eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), Exists(Den("f"), set3))) emptyEnv;;
eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), Exists(Den("f"), set4))) emptyEnv;;
eval (LetRec("f", "x", Eq(Sub(CstInt(10), Den("x")), CstInt(0)), Exists(Den("f"), set5))) emptyEnv;;


let set6 = Insert(Insert(Insert(Empty("int"), CstInt(10)), CstInt(11)), CstInt(1));;
eval set6 emptyEnv;;
eval (Filter(Fun("x", Eq(CstInt(10), Den("x"))), set6)) emptyEnv;;

eval (Map(Fun("x", Sum(CstInt(1), Den("x"))), set1)) emptyEnv;;
eval (Map(Fun("x", Times(CstInt(10), Den("x"))), set2)) emptyEnv;;