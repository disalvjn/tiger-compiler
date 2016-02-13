{-# OPTIONS_GHC -w #-}
module Parse (parse) where
import Lex(Token(..), TokenType(..))
import Data.Maybe
import AST

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27
	= HappyTerminal (Lex.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27

action_0 (31) = happyShift action_7
action_0 (35) = happyShift action_8
action_0 (36) = happyShift action_9
action_0 (39) = happyShift action_10
action_0 (40) = happyShift action_11
action_0 (43) = happyShift action_12
action_0 (64) = happyShift action_13
action_0 (68) = happyShift action_14
action_0 (69) = happyShift action_15
action_0 (70) = happyShift action_16
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 _ = happyFail

action_1 (70) = happyShift action_3
action_1 (7) = happyGoto action_2
action_1 _ = happyFail

action_2 (62) = happyShift action_40
action_2 (70) = happyShift action_41
action_2 _ = happyFail

action_3 _ = happyReduce_18

action_4 (72) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_9

action_6 (45) = happyShift action_39
action_6 (62) = happyShift action_40
action_6 (70) = happyShift action_41
action_6 _ = happyReduce_1

action_7 _ = happyReduce_12

action_8 _ = happyReduce_2

action_9 (28) = happyShift action_36
action_9 (29) = happyShift action_37
action_9 (30) = happyShift action_38
action_9 (14) = happyGoto action_26
action_9 (15) = happyGoto action_27
action_9 (16) = happyGoto action_28
action_9 (17) = happyGoto action_29
action_9 (18) = happyGoto action_30
action_9 (19) = happyGoto action_31
action_9 (20) = happyGoto action_32
action_9 (21) = happyGoto action_33
action_9 (22) = happyGoto action_34
action_9 (23) = happyGoto action_35
action_9 _ = happyFail

action_10 (70) = happyShift action_3
action_10 (7) = happyGoto action_25
action_10 _ = happyFail

action_11 (31) = happyShift action_7
action_11 (35) = happyShift action_8
action_11 (36) = happyShift action_9
action_11 (39) = happyShift action_10
action_11 (40) = happyShift action_11
action_11 (43) = happyShift action_12
action_11 (64) = happyShift action_13
action_11 (68) = happyShift action_14
action_11 (69) = happyShift action_15
action_11 (70) = happyShift action_16
action_11 (4) = happyGoto action_24
action_11 (5) = happyGoto action_5
action_11 (7) = happyGoto action_6
action_11 _ = happyFail

action_12 (31) = happyShift action_7
action_12 (35) = happyShift action_8
action_12 (36) = happyShift action_9
action_12 (39) = happyShift action_10
action_12 (40) = happyShift action_11
action_12 (43) = happyShift action_12
action_12 (64) = happyShift action_13
action_12 (68) = happyShift action_14
action_12 (69) = happyShift action_15
action_12 (70) = happyShift action_16
action_12 (4) = happyGoto action_23
action_12 (5) = happyGoto action_5
action_12 (7) = happyGoto action_6
action_12 _ = happyFail

action_13 (31) = happyShift action_7
action_13 (35) = happyShift action_8
action_13 (36) = happyShift action_9
action_13 (39) = happyShift action_10
action_13 (40) = happyShift action_11
action_13 (43) = happyShift action_12
action_13 (64) = happyShift action_13
action_13 (68) = happyShift action_14
action_13 (69) = happyShift action_15
action_13 (70) = happyShift action_16
action_13 (4) = happyGoto action_20
action_13 (5) = happyGoto action_5
action_13 (7) = happyGoto action_6
action_13 (12) = happyGoto action_21
action_13 (13) = happyGoto action_22
action_13 _ = happyFail

action_14 _ = happyReduce_4

action_15 _ = happyReduce_3

action_16 (60) = happyShift action_17
action_16 (62) = happyShift action_18
action_16 (64) = happyShift action_19
action_16 _ = happyReduce_18

action_17 (70) = happyShift action_62
action_17 (10) = happyGoto action_60
action_17 (11) = happyGoto action_61
action_17 _ = happyReduce_26

action_18 (31) = happyShift action_7
action_18 (35) = happyShift action_8
action_18 (36) = happyShift action_9
action_18 (39) = happyShift action_10
action_18 (40) = happyShift action_11
action_18 (43) = happyShift action_12
action_18 (64) = happyShift action_13
action_18 (68) = happyShift action_14
action_18 (69) = happyShift action_15
action_18 (70) = happyShift action_16
action_18 (4) = happyGoto action_59
action_18 (5) = happyGoto action_5
action_18 (7) = happyGoto action_6
action_18 _ = happyFail

action_19 (31) = happyShift action_7
action_19 (35) = happyShift action_8
action_19 (36) = happyShift action_9
action_19 (39) = happyShift action_10
action_19 (40) = happyShift action_11
action_19 (43) = happyShift action_12
action_19 (64) = happyShift action_13
action_19 (68) = happyShift action_14
action_19 (69) = happyShift action_15
action_19 (70) = happyShift action_16
action_19 (4) = happyGoto action_56
action_19 (5) = happyGoto action_5
action_19 (7) = happyGoto action_6
action_19 (8) = happyGoto action_57
action_19 (9) = happyGoto action_58
action_19 _ = happyReduce_22

action_20 _ = happyReduce_30

action_21 (63) = happyShift action_55
action_21 _ = happyFail

action_22 (65) = happyShift action_54
action_22 _ = happyReduce_29

action_23 (42) = happyShift action_53
action_23 _ = happyFail

action_24 (37) = happyShift action_52
action_24 _ = happyFail

action_25 (45) = happyShift action_51
action_25 (62) = happyShift action_40
action_25 (70) = happyShift action_41
action_25 _ = happyFail

action_26 (34) = happyShift action_50
action_26 _ = happyFail

action_27 (28) = happyShift action_36
action_27 (29) = happyShift action_37
action_27 (30) = happyShift action_38
action_27 (16) = happyGoto action_49
action_27 (17) = happyGoto action_29
action_27 (18) = happyGoto action_30
action_27 (19) = happyGoto action_31
action_27 (20) = happyGoto action_32
action_27 (21) = happyGoto action_33
action_27 (22) = happyGoto action_34
action_27 (23) = happyGoto action_35
action_27 _ = happyReduce_32

action_28 _ = happyReduce_33

action_29 _ = happyReduce_41

action_30 _ = happyReduce_35

action_31 (30) = happyShift action_38
action_31 (17) = happyGoto action_48
action_31 _ = happyReduce_40

action_32 _ = happyReduce_36

action_33 _ = happyReduce_37

action_34 (28) = happyShift action_36
action_34 (23) = happyGoto action_47
action_34 _ = happyReduce_45

action_35 _ = happyReduce_46

action_36 (70) = happyShift action_46
action_36 _ = happyFail

action_37 (70) = happyShift action_45
action_37 _ = happyFail

action_38 (70) = happyShift action_44
action_38 _ = happyFail

action_39 (31) = happyShift action_7
action_39 (35) = happyShift action_8
action_39 (36) = happyShift action_9
action_39 (39) = happyShift action_10
action_39 (40) = happyShift action_11
action_39 (43) = happyShift action_12
action_39 (64) = happyShift action_13
action_39 (68) = happyShift action_14
action_39 (69) = happyShift action_15
action_39 (70) = happyShift action_16
action_39 (4) = happyGoto action_43
action_39 (5) = happyGoto action_5
action_39 (7) = happyGoto action_6
action_39 _ = happyFail

action_40 (31) = happyShift action_7
action_40 (35) = happyShift action_8
action_40 (36) = happyShift action_9
action_40 (39) = happyShift action_10
action_40 (40) = happyShift action_11
action_40 (43) = happyShift action_12
action_40 (64) = happyShift action_13
action_40 (68) = happyShift action_14
action_40 (69) = happyShift action_15
action_40 (70) = happyShift action_16
action_40 (4) = happyGoto action_42
action_40 (5) = happyGoto action_5
action_40 (7) = happyGoto action_6
action_40 _ = happyFail

action_41 _ = happyReduce_19

action_42 (61) = happyShift action_78
action_42 _ = happyFail

action_43 _ = happyReduce_8

action_44 (64) = happyShift action_77
action_44 _ = happyFail

action_45 (45) = happyShift action_75
action_45 (66) = happyShift action_76
action_45 _ = happyFail

action_46 (53) = happyShift action_74
action_46 _ = happyFail

action_47 _ = happyReduce_47

action_48 _ = happyReduce_42

action_49 _ = happyReduce_34

action_50 (31) = happyShift action_7
action_50 (35) = happyShift action_8
action_50 (36) = happyShift action_9
action_50 (39) = happyShift action_10
action_50 (40) = happyShift action_11
action_50 (43) = happyShift action_12
action_50 (64) = happyShift action_13
action_50 (68) = happyShift action_14
action_50 (69) = happyShift action_15
action_50 (70) = happyShift action_16
action_50 (4) = happyGoto action_73
action_50 (5) = happyGoto action_5
action_50 (7) = happyGoto action_6
action_50 _ = happyFail

action_51 (31) = happyShift action_7
action_51 (35) = happyShift action_8
action_51 (36) = happyShift action_9
action_51 (39) = happyShift action_10
action_51 (40) = happyShift action_11
action_51 (43) = happyShift action_12
action_51 (64) = happyShift action_13
action_51 (68) = happyShift action_14
action_51 (69) = happyShift action_15
action_51 (70) = happyShift action_16
action_51 (4) = happyGoto action_72
action_51 (5) = happyGoto action_5
action_51 (7) = happyGoto action_6
action_51 _ = happyFail

action_52 (31) = happyShift action_7
action_52 (35) = happyShift action_8
action_52 (36) = happyShift action_9
action_52 (39) = happyShift action_10
action_52 (40) = happyShift action_11
action_52 (43) = happyShift action_12
action_52 (64) = happyShift action_13
action_52 (68) = happyShift action_14
action_52 (69) = happyShift action_15
action_52 (70) = happyShift action_16
action_52 (4) = happyGoto action_71
action_52 (5) = happyGoto action_5
action_52 (7) = happyGoto action_6
action_52 _ = happyFail

action_53 (31) = happyShift action_7
action_53 (35) = happyShift action_8
action_53 (36) = happyShift action_9
action_53 (39) = happyShift action_10
action_53 (40) = happyShift action_11
action_53 (43) = happyShift action_12
action_53 (64) = happyShift action_13
action_53 (68) = happyShift action_14
action_53 (69) = happyShift action_15
action_53 (70) = happyShift action_16
action_53 (4) = happyGoto action_70
action_53 (5) = happyGoto action_5
action_53 (7) = happyGoto action_6
action_53 _ = happyFail

action_54 (31) = happyShift action_7
action_54 (35) = happyShift action_8
action_54 (36) = happyShift action_9
action_54 (39) = happyShift action_10
action_54 (40) = happyShift action_11
action_54 (43) = happyShift action_12
action_54 (64) = happyShift action_13
action_54 (68) = happyShift action_14
action_54 (69) = happyShift action_15
action_54 (70) = happyShift action_16
action_54 (4) = happyGoto action_69
action_54 (5) = happyGoto action_5
action_54 (7) = happyGoto action_6
action_54 _ = happyFail

action_55 _ = happyReduce_7

action_56 _ = happyReduce_23

action_57 (63) = happyShift action_68
action_57 _ = happyFail

action_58 (67) = happyShift action_67
action_58 _ = happyReduce_21

action_59 (61) = happyShift action_66
action_59 _ = happyFail

action_60 (59) = happyShift action_65
action_60 _ = happyFail

action_61 (67) = happyShift action_64
action_61 _ = happyReduce_25

action_62 (53) = happyShift action_63
action_62 _ = happyFail

action_63 (31) = happyShift action_7
action_63 (35) = happyShift action_8
action_63 (36) = happyShift action_9
action_63 (39) = happyShift action_10
action_63 (40) = happyShift action_11
action_63 (43) = happyShift action_12
action_63 (64) = happyShift action_13
action_63 (68) = happyShift action_14
action_63 (69) = happyShift action_15
action_63 (70) = happyShift action_16
action_63 (4) = happyGoto action_93
action_63 (5) = happyGoto action_5
action_63 (7) = happyGoto action_6
action_63 _ = happyFail

action_64 (70) = happyShift action_92
action_64 _ = happyFail

action_65 _ = happyReduce_6

action_66 (32) = happyShift action_91
action_66 _ = happyFail

action_67 (31) = happyShift action_7
action_67 (35) = happyShift action_8
action_67 (36) = happyShift action_9
action_67 (39) = happyShift action_10
action_67 (40) = happyShift action_11
action_67 (43) = happyShift action_12
action_67 (64) = happyShift action_13
action_67 (68) = happyShift action_14
action_67 (69) = happyShift action_15
action_67 (70) = happyShift action_16
action_67 (4) = happyGoto action_90
action_67 (5) = happyGoto action_5
action_67 (7) = happyGoto action_6
action_67 _ = happyFail

action_68 _ = happyReduce_5

action_69 _ = happyReduce_31

action_70 (41) = happyShift action_89
action_70 (6) = happyGoto action_88
action_70 _ = happyReduce_16

action_71 _ = happyReduce_10

action_72 (38) = happyShift action_87
action_72 _ = happyFail

action_73 (33) = happyShift action_86
action_73 _ = happyFail

action_74 (44) = happyShift action_83
action_74 (60) = happyShift action_84
action_74 (70) = happyShift action_85
action_74 (24) = happyGoto action_82
action_74 _ = happyFail

action_75 (31) = happyShift action_7
action_75 (35) = happyShift action_8
action_75 (36) = happyShift action_9
action_75 (39) = happyShift action_10
action_75 (40) = happyShift action_11
action_75 (43) = happyShift action_12
action_75 (64) = happyShift action_13
action_75 (68) = happyShift action_14
action_75 (69) = happyShift action_15
action_75 (70) = happyShift action_16
action_75 (4) = happyGoto action_81
action_75 (5) = happyGoto action_5
action_75 (7) = happyGoto action_6
action_75 _ = happyFail

action_76 (70) = happyShift action_80
action_76 _ = happyFail

action_77 (25) = happyGoto action_79
action_77 _ = happyReduce_52

action_78 _ = happyReduce_20

action_79 (63) = happyShift action_101
action_79 (67) = happyShift action_102
action_79 _ = happyFail

action_80 (45) = happyShift action_100
action_80 _ = happyFail

action_81 _ = happyReduce_43

action_82 _ = happyReduce_48

action_83 (32) = happyShift action_99
action_83 _ = happyFail

action_84 (25) = happyGoto action_98
action_84 _ = happyReduce_52

action_85 _ = happyReduce_49

action_86 _ = happyReduce_13

action_87 (31) = happyShift action_7
action_87 (35) = happyShift action_8
action_87 (36) = happyShift action_9
action_87 (39) = happyShift action_10
action_87 (40) = happyShift action_11
action_87 (43) = happyShift action_12
action_87 (64) = happyShift action_13
action_87 (68) = happyShift action_14
action_87 (69) = happyShift action_15
action_87 (70) = happyShift action_16
action_87 (4) = happyGoto action_97
action_87 (5) = happyGoto action_5
action_87 (7) = happyGoto action_6
action_87 _ = happyFail

action_88 _ = happyReduce_15

action_89 (31) = happyShift action_7
action_89 (35) = happyShift action_8
action_89 (36) = happyShift action_9
action_89 (39) = happyShift action_10
action_89 (40) = happyShift action_11
action_89 (43) = happyShift action_12
action_89 (64) = happyShift action_13
action_89 (68) = happyShift action_14
action_89 (69) = happyShift action_15
action_89 (70) = happyShift action_16
action_89 (4) = happyGoto action_96
action_89 (5) = happyGoto action_5
action_89 (7) = happyGoto action_6
action_89 _ = happyFail

action_90 _ = happyReduce_24

action_91 (31) = happyShift action_7
action_91 (35) = happyShift action_8
action_91 (36) = happyShift action_9
action_91 (39) = happyShift action_10
action_91 (40) = happyShift action_11
action_91 (43) = happyShift action_12
action_91 (64) = happyShift action_13
action_91 (68) = happyShift action_14
action_91 (69) = happyShift action_15
action_91 (70) = happyShift action_16
action_91 (4) = happyGoto action_95
action_91 (5) = happyGoto action_5
action_91 (7) = happyGoto action_6
action_91 _ = happyFail

action_92 (53) = happyShift action_94
action_92 _ = happyFail

action_93 _ = happyReduce_27

action_94 (31) = happyShift action_7
action_94 (35) = happyShift action_8
action_94 (36) = happyShift action_9
action_94 (39) = happyShift action_10
action_94 (40) = happyShift action_11
action_94 (43) = happyShift action_12
action_94 (64) = happyShift action_13
action_94 (68) = happyShift action_14
action_94 (69) = happyShift action_15
action_94 (70) = happyShift action_16
action_94 (4) = happyGoto action_110
action_94 (5) = happyGoto action_5
action_94 (7) = happyGoto action_6
action_94 _ = happyFail

action_95 _ = happyReduce_14

action_96 _ = happyReduce_17

action_97 (37) = happyShift action_109
action_97 _ = happyFail

action_98 (59) = happyShift action_108
action_98 (67) = happyShift action_102
action_98 _ = happyFail

action_99 (70) = happyShift action_107
action_99 _ = happyFail

action_100 (31) = happyShift action_7
action_100 (35) = happyShift action_8
action_100 (36) = happyShift action_9
action_100 (39) = happyShift action_10
action_100 (40) = happyShift action_11
action_100 (43) = happyShift action_12
action_100 (64) = happyShift action_13
action_100 (68) = happyShift action_14
action_100 (69) = happyShift action_15
action_100 (70) = happyShift action_16
action_100 (4) = happyGoto action_106
action_100 (5) = happyGoto action_5
action_100 (7) = happyGoto action_6
action_100 _ = happyFail

action_101 (53) = happyShift action_104
action_101 (66) = happyShift action_105
action_101 _ = happyFail

action_102 (70) = happyShift action_103
action_102 _ = happyFail

action_103 (66) = happyShift action_114
action_103 _ = happyFail

action_104 (31) = happyShift action_7
action_104 (35) = happyShift action_8
action_104 (36) = happyShift action_9
action_104 (39) = happyShift action_10
action_104 (40) = happyShift action_11
action_104 (43) = happyShift action_12
action_104 (64) = happyShift action_13
action_104 (68) = happyShift action_14
action_104 (69) = happyShift action_15
action_104 (70) = happyShift action_16
action_104 (4) = happyGoto action_113
action_104 (5) = happyGoto action_5
action_104 (7) = happyGoto action_6
action_104 _ = happyFail

action_105 (70) = happyShift action_112
action_105 _ = happyFail

action_106 _ = happyReduce_44

action_107 _ = happyReduce_51

action_108 _ = happyReduce_50

action_109 (31) = happyShift action_7
action_109 (35) = happyShift action_8
action_109 (36) = happyShift action_9
action_109 (39) = happyShift action_10
action_109 (40) = happyShift action_11
action_109 (43) = happyShift action_12
action_109 (64) = happyShift action_13
action_109 (68) = happyShift action_14
action_109 (69) = happyShift action_15
action_109 (70) = happyShift action_16
action_109 (4) = happyGoto action_111
action_109 (5) = happyGoto action_5
action_109 (7) = happyGoto action_6
action_109 _ = happyFail

action_110 _ = happyReduce_28

action_111 _ = happyReduce_11

action_112 (53) = happyShift action_116
action_112 _ = happyFail

action_113 _ = happyReduce_38

action_114 (70) = happyShift action_115
action_114 _ = happyFail

action_115 _ = happyReduce_53

action_116 (31) = happyShift action_7
action_116 (35) = happyShift action_8
action_116 (36) = happyShift action_9
action_116 (39) = happyShift action_10
action_116 (40) = happyShift action_11
action_116 (43) = happyShift action_12
action_116 (64) = happyShift action_13
action_116 (68) = happyShift action_14
action_116 (69) = happyShift action_15
action_116 (70) = happyShift action_16
action_116 (4) = happyGoto action_117
action_116 (5) = happyGoto action_5
action_116 (7) = happyGoto action_6
action_116 _ = happyFail

action_117 _ = happyReduce_39

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (NilExp
	)

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyTerminal (Token (Int happy_var_1) _))
	 =  HappyAbsSyn4
		 (IntExp happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyTerminal (Token (String happy_var_1) _))
	 =  HappyAbsSyn4
		 (StringExp happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_1) _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (CallExp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 4 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_1) _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (RecordExp happy_var_3 happy_var_1
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (SeqExp happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (AssignExp happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  4 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 4 happyReduction_10
happyReduction_10 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (WhileExp happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 8 4 happyReduction_11
happyReduction_11 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ForExp happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  4 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn4
		 (BreakExp
	)

happyReduce_13 = happyReduce 5 4 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LetExp happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 6 4 happyReduction_14
happyReduction_14 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_1) _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ArrayExp happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 5 happyReduction_15
happyReduction_15 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfExp happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_0  6 happyReduction_16
happyReduction_16  =  HappyAbsSyn6
		 (Nothing
	)

happyReduce_17 = happySpecReduce_2  6 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Just happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 (HappyTerminal (Token (Id happy_var_1) _))
	 =  HappyAbsSyn7
		 (SimpleVar happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  7 happyReduction_19
happyReduction_19 (HappyTerminal (Token (Id happy_var_2) _))
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (FieldVar happy_var_1 happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 7 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (SubscriptVar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  8 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (reverse happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  9 happyReduction_22
happyReduction_22  =  HappyAbsSyn9
		 ([]
	)

happyReduce_23 = happySpecReduce_1  9 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  9 happyReduction_24
happyReduction_24 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  10 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (reverse happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  11 happyReduction_26
happyReduction_26  =  HappyAbsSyn11
		 ([]
	)

happyReduce_27 = happySpecReduce_3  11 happyReduction_27
happyReduction_27 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (Token (Id happy_var_1) _))
	 =  HappyAbsSyn11
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 5 11 happyReduction_28
happyReduction_28 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_3) _)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((happy_var_3, happy_var_5) : happy_var_1
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (reverse happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  13 happyReduction_30
happyReduction_30 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  14 happyReduction_32
happyReduction_32 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (reverse happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  15 happyReduction_33
happyReduction_33 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  15 happyReduction_34
happyReduction_34 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_2 : happy_var_1
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  16 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  16 happyReduction_36
happyReduction_36 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  16 happyReduction_37
happyReduction_37 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happyReduce 7 17 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyTerminal happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Fundec happy_var_2 happy_var_4 Nothing happy_var_6
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 9 17 happyReduction_39
happyReduction_39 ((HappyAbsSyn4  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_7) _)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Fundec happy_var_2 happy_var_4 (Just happy_var_7) happy_var_9
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  18 happyReduction_40
happyReduction_40 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (reverse happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  19 happyReduction_41
happyReduction_41 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  19 happyReduction_42
happyReduction_42 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_2 : happy_var_1
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 20 happyReduction_43
happyReduction_43 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (VarDec happy_var_2 Nothing happy_var_4
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 6 20 happyReduction_44
happyReduction_44 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_4) _)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (VarDec happy_var_2 (Just happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_1  21 happyReduction_45
happyReduction_45 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (reverse happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  22 happyReduction_46
happyReduction_46 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  22 happyReduction_47
happyReduction_47 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_1
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 4 23 happyReduction_48
happyReduction_48 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_1  24 happyReduction_49
happyReduction_49 (HappyTerminal (Token (Id happy_var_1) _))
	 =  HappyAbsSyn24
		 (NameTy happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  24 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (reverse happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  24 happyReduction_51
happyReduction_51 (HappyTerminal (Token (Id happy_var_3) _))
	_
	_
	 =  HappyAbsSyn24
		 (ArrayTy happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_0  25 happyReduction_52
happyReduction_52  =  HappyAbsSyn25
		 ([]
	)

happyReduce_53 = happyReduce 5 25 happyReduction_53
happyReduction_53 ((HappyTerminal (Token (Id happy_var_5) _)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_3) _)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ((Field happy_var_3 happy_var_5) : happy_var_1
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_1  26 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn26
		 (PlusOp
	)

happyReduce_55 = happySpecReduce_1  26 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn26
		 (MinusOp
	)

happyReduce_56 = happySpecReduce_1  26 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn26
		 (TimesOp
	)

happyReduce_57 = happySpecReduce_1  26 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn26
		 (DivideOp
	)

happyReduce_58 = happySpecReduce_1  26 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn26
		 (LtOp
	)

happyReduce_59 = happySpecReduce_1  26 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn26
		 (GtOp
	)

happyReduce_60 = happySpecReduce_1  26 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn26
		 (GeOp
	)

happyReduce_61 = happySpecReduce_1  26 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn26
		 (LeOp
	)

happyReduce_62 = happySpecReduce_1  26 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn26
		 (EqOp
	)

happyReduce_63 = happySpecReduce_3  27 happyReduction_63
happyReduction_63 (HappyAbsSyn4  happy_var_3)
	(HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn27
		 (OpExp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 72 72 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token Type _ -> cont 28;
	Token Var _ -> cont 29;
	Token Function _ -> cont 30;
	Token Break _ -> cont 31;
	Token Of _ -> cont 32;
	Token End _ -> cont 33;
	Token In _ -> cont 34;
	Token Nil _ -> cont 35;
	Token Let _ -> cont 36;
	Token Do _ -> cont 37;
	Token To _ -> cont 38;
	Token For _ -> cont 39;
	Token While _ -> cont 40;
	Token Else _ -> cont 41;
	Token Then _ -> cont 42;
	Token If _ -> cont 43;
	Token Array _ -> cont 44;
	Token Assign _ -> cont 45;
	Token Or _ -> cont 46;
	Token And _ -> cont 47;
	Token Ge _ -> cont 48;
	Token Gt _ -> cont 49;
	Token Le _ -> cont 50;
	Token Lt _ -> cont 51;
	Token Neq _ -> cont 52;
	Token Eq _ -> cont 53;
	Token Divide _ -> cont 54;
	Token Times _ -> cont 55;
	Token Minus _ -> cont 56;
	Token Plus _ -> cont 57;
	Token Dot _ -> cont 58;
	Token Rbrace _ -> cont 59;
	Token Lbrace _ -> cont 60;
	Token Rbrack _ -> cont 61;
	Token Lbrack _ -> cont 62;
	Token Rparen _ -> cont 63;
	Token Lparen _ -> cont 64;
	Token Semicolon _ -> cont 65;
	Token Colon _ -> cont 66;
	Token Comma _ -> cont 67;
	Token (String happy_dollar_dollar) _ -> cont 68;
	Token (Int happy_dollar_dollar) _ -> cont 69;
	Token (Id happy_dollar_dollar) _ -> cont 70;
	Token Eof _ -> cont 71;
	_ -> happyError' (tk:tks)
	}

happyError_ 72 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Lex.Token)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq



{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 45 "templates/GenericTemplate.hs" #-}








{-# LINE 66 "templates/GenericTemplate.hs" #-}

{-# LINE 76 "templates/GenericTemplate.hs" #-}

{-# LINE 85 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 154 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 255 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 321 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
