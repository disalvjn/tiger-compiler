{-# OPTIONS_GHC -w #-}
module Parse (parse) where
import Lex(Token(..), TokenType(..), AlexPosn(..))
import Data.Maybe
import AST

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
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
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30

action_0 (34) = happyShift action_11
action_0 (38) = happyShift action_12
action_0 (39) = happyShift action_13
action_0 (42) = happyShift action_14
action_0 (43) = happyShift action_15
action_0 (46) = happyShift action_16
action_0 (67) = happyShift action_17
action_0 (71) = happyShift action_18
action_0 (72) = happyShift action_19
action_0 (73) = happyShift action_20
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (27) = happyGoto action_7
action_0 (28) = happyGoto action_8
action_0 (29) = happyGoto action_9
action_0 (30) = happyGoto action_10
action_0 _ = happyFail

action_1 (73) = happyShift action_3
action_1 (7) = happyGoto action_2
action_1 _ = happyFail

action_2 (65) = happyShift action_48
action_2 (73) = happyShift action_49
action_2 _ = happyFail

action_3 _ = happyReduce_19

action_4 (75) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_9

action_6 (31) = happyReduce_70
action_6 (32) = happyReduce_70
action_6 (33) = happyReduce_70
action_6 (36) = happyReduce_70
action_6 (37) = happyReduce_70
action_6 (40) = happyReduce_70
action_6 (41) = happyReduce_70
action_6 (44) = happyReduce_70
action_6 (45) = happyReduce_70
action_6 (48) = happyShift action_47
action_6 (62) = happyReduce_70
action_6 (64) = happyReduce_70
action_6 (65) = happyShift action_48
action_6 (66) = happyReduce_70
action_6 (68) = happyReduce_70
action_6 (70) = happyReduce_70
action_6 (73) = happyShift action_49
action_6 (75) = happyReduce_70
action_6 _ = happyReduce_70

action_7 (59) = happyShift action_45
action_7 (60) = happyShift action_46
action_7 _ = happyReduce_71

action_8 (57) = happyShift action_43
action_8 (58) = happyShift action_44
action_8 _ = happyReduce_66

action_9 _ = happyReduce_69

action_10 _ = happyReduce_15

action_11 _ = happyReduce_12

action_12 _ = happyReduce_2

action_13 (31) = happyShift action_40
action_13 (32) = happyShift action_41
action_13 (33) = happyShift action_42
action_13 (14) = happyGoto action_30
action_13 (15) = happyGoto action_31
action_13 (16) = happyGoto action_32
action_13 (17) = happyGoto action_33
action_13 (18) = happyGoto action_34
action_13 (19) = happyGoto action_35
action_13 (20) = happyGoto action_36
action_13 (21) = happyGoto action_37
action_13 (22) = happyGoto action_38
action_13 (23) = happyGoto action_39
action_13 _ = happyFail

action_14 (73) = happyShift action_29
action_14 _ = happyFail

action_15 (34) = happyShift action_11
action_15 (38) = happyShift action_12
action_15 (39) = happyShift action_13
action_15 (42) = happyShift action_14
action_15 (43) = happyShift action_15
action_15 (46) = happyShift action_16
action_15 (67) = happyShift action_17
action_15 (71) = happyShift action_18
action_15 (72) = happyShift action_19
action_15 (73) = happyShift action_20
action_15 (4) = happyGoto action_28
action_15 (5) = happyGoto action_5
action_15 (7) = happyGoto action_6
action_15 (27) = happyGoto action_7
action_15 (28) = happyGoto action_8
action_15 (29) = happyGoto action_9
action_15 (30) = happyGoto action_10
action_15 _ = happyFail

action_16 (34) = happyShift action_11
action_16 (38) = happyShift action_12
action_16 (39) = happyShift action_13
action_16 (42) = happyShift action_14
action_16 (43) = happyShift action_15
action_16 (46) = happyShift action_16
action_16 (67) = happyShift action_17
action_16 (71) = happyShift action_18
action_16 (72) = happyShift action_19
action_16 (73) = happyShift action_20
action_16 (4) = happyGoto action_27
action_16 (5) = happyGoto action_5
action_16 (7) = happyGoto action_6
action_16 (27) = happyGoto action_7
action_16 (28) = happyGoto action_8
action_16 (29) = happyGoto action_9
action_16 (30) = happyGoto action_10
action_16 _ = happyFail

action_17 (34) = happyShift action_11
action_17 (38) = happyShift action_12
action_17 (39) = happyShift action_13
action_17 (42) = happyShift action_14
action_17 (43) = happyShift action_15
action_17 (46) = happyShift action_16
action_17 (67) = happyShift action_17
action_17 (71) = happyShift action_18
action_17 (72) = happyShift action_19
action_17 (73) = happyShift action_20
action_17 (4) = happyGoto action_24
action_17 (5) = happyGoto action_5
action_17 (7) = happyGoto action_6
action_17 (12) = happyGoto action_25
action_17 (13) = happyGoto action_26
action_17 (27) = happyGoto action_7
action_17 (28) = happyGoto action_8
action_17 (29) = happyGoto action_9
action_17 (30) = happyGoto action_10
action_17 _ = happyFail

action_18 _ = happyReduce_4

action_19 _ = happyReduce_3

action_20 (63) = happyShift action_21
action_20 (65) = happyShift action_22
action_20 (67) = happyShift action_23
action_20 _ = happyReduce_19

action_21 (73) = happyShift action_75
action_21 (10) = happyGoto action_73
action_21 (11) = happyGoto action_74
action_21 _ = happyReduce_27

action_22 (34) = happyShift action_11
action_22 (38) = happyShift action_12
action_22 (39) = happyShift action_13
action_22 (42) = happyShift action_14
action_22 (43) = happyShift action_15
action_22 (46) = happyShift action_16
action_22 (67) = happyShift action_17
action_22 (71) = happyShift action_18
action_22 (72) = happyShift action_19
action_22 (73) = happyShift action_20
action_22 (4) = happyGoto action_72
action_22 (5) = happyGoto action_5
action_22 (7) = happyGoto action_6
action_22 (27) = happyGoto action_7
action_22 (28) = happyGoto action_8
action_22 (29) = happyGoto action_9
action_22 (30) = happyGoto action_10
action_22 _ = happyFail

action_23 (34) = happyShift action_11
action_23 (38) = happyShift action_12
action_23 (39) = happyShift action_13
action_23 (42) = happyShift action_14
action_23 (43) = happyShift action_15
action_23 (46) = happyShift action_16
action_23 (67) = happyShift action_17
action_23 (71) = happyShift action_18
action_23 (72) = happyShift action_19
action_23 (73) = happyShift action_20
action_23 (4) = happyGoto action_69
action_23 (5) = happyGoto action_5
action_23 (7) = happyGoto action_6
action_23 (8) = happyGoto action_70
action_23 (9) = happyGoto action_71
action_23 (27) = happyGoto action_7
action_23 (28) = happyGoto action_8
action_23 (29) = happyGoto action_9
action_23 (30) = happyGoto action_10
action_23 _ = happyReduce_23

action_24 _ = happyReduce_31

action_25 (66) = happyShift action_68
action_25 _ = happyFail

action_26 (68) = happyShift action_67
action_26 _ = happyReduce_30

action_27 (45) = happyShift action_66
action_27 _ = happyFail

action_28 (40) = happyShift action_65
action_28 _ = happyFail

action_29 (48) = happyShift action_64
action_29 _ = happyFail

action_30 (37) = happyShift action_63
action_30 _ = happyFail

action_31 (31) = happyShift action_40
action_31 (32) = happyShift action_41
action_31 (33) = happyShift action_42
action_31 (16) = happyGoto action_62
action_31 (17) = happyGoto action_33
action_31 (18) = happyGoto action_34
action_31 (19) = happyGoto action_35
action_31 (20) = happyGoto action_36
action_31 (21) = happyGoto action_37
action_31 (22) = happyGoto action_38
action_31 (23) = happyGoto action_39
action_31 _ = happyReduce_33

action_32 _ = happyReduce_34

action_33 _ = happyReduce_42

action_34 _ = happyReduce_36

action_35 (33) = happyShift action_42
action_35 (17) = happyGoto action_61
action_35 _ = happyReduce_41

action_36 _ = happyReduce_37

action_37 _ = happyReduce_38

action_38 (31) = happyShift action_40
action_38 (23) = happyGoto action_60
action_38 _ = happyReduce_46

action_39 _ = happyReduce_47

action_40 (73) = happyShift action_59
action_40 _ = happyFail

action_41 (73) = happyShift action_58
action_41 _ = happyFail

action_42 (73) = happyShift action_57
action_42 _ = happyFail

action_43 (73) = happyShift action_3
action_43 (7) = happyGoto action_52
action_43 (29) = happyGoto action_56
action_43 _ = happyFail

action_44 (73) = happyShift action_3
action_44 (7) = happyGoto action_52
action_44 (29) = happyGoto action_55
action_44 _ = happyFail

action_45 (73) = happyShift action_3
action_45 (7) = happyGoto action_52
action_45 (28) = happyGoto action_54
action_45 (29) = happyGoto action_9
action_45 _ = happyFail

action_46 (73) = happyShift action_3
action_46 (7) = happyGoto action_52
action_46 (28) = happyGoto action_53
action_46 (29) = happyGoto action_9
action_46 _ = happyFail

action_47 (34) = happyShift action_11
action_47 (38) = happyShift action_12
action_47 (39) = happyShift action_13
action_47 (42) = happyShift action_14
action_47 (43) = happyShift action_15
action_47 (46) = happyShift action_16
action_47 (67) = happyShift action_17
action_47 (71) = happyShift action_18
action_47 (72) = happyShift action_19
action_47 (73) = happyShift action_20
action_47 (4) = happyGoto action_51
action_47 (5) = happyGoto action_5
action_47 (7) = happyGoto action_6
action_47 (27) = happyGoto action_7
action_47 (28) = happyGoto action_8
action_47 (29) = happyGoto action_9
action_47 (30) = happyGoto action_10
action_47 _ = happyFail

action_48 (34) = happyShift action_11
action_48 (38) = happyShift action_12
action_48 (39) = happyShift action_13
action_48 (42) = happyShift action_14
action_48 (43) = happyShift action_15
action_48 (46) = happyShift action_16
action_48 (67) = happyShift action_17
action_48 (71) = happyShift action_18
action_48 (72) = happyShift action_19
action_48 (73) = happyShift action_20
action_48 (4) = happyGoto action_50
action_48 (5) = happyGoto action_5
action_48 (7) = happyGoto action_6
action_48 (27) = happyGoto action_7
action_48 (28) = happyGoto action_8
action_48 (29) = happyGoto action_9
action_48 (30) = happyGoto action_10
action_48 _ = happyFail

action_49 _ = happyReduce_20

action_50 (64) = happyShift action_91
action_50 _ = happyFail

action_51 _ = happyReduce_8

action_52 (65) = happyShift action_48
action_52 (73) = happyShift action_49
action_52 _ = happyReduce_70

action_53 (57) = happyShift action_43
action_53 (58) = happyShift action_44
action_53 _ = happyReduce_64

action_54 (57) = happyShift action_43
action_54 (58) = happyShift action_44
action_54 _ = happyReduce_65

action_55 _ = happyReduce_67

action_56 _ = happyReduce_68

action_57 (67) = happyShift action_90
action_57 _ = happyFail

action_58 (48) = happyShift action_88
action_58 (69) = happyShift action_89
action_58 _ = happyFail

action_59 (56) = happyShift action_87
action_59 _ = happyFail

action_60 _ = happyReduce_48

action_61 _ = happyReduce_43

action_62 _ = happyReduce_35

action_63 (34) = happyShift action_11
action_63 (38) = happyShift action_12
action_63 (39) = happyShift action_13
action_63 (42) = happyShift action_14
action_63 (43) = happyShift action_15
action_63 (46) = happyShift action_16
action_63 (67) = happyShift action_17
action_63 (71) = happyShift action_18
action_63 (72) = happyShift action_19
action_63 (73) = happyShift action_20
action_63 (4) = happyGoto action_86
action_63 (5) = happyGoto action_5
action_63 (7) = happyGoto action_6
action_63 (27) = happyGoto action_7
action_63 (28) = happyGoto action_8
action_63 (29) = happyGoto action_9
action_63 (30) = happyGoto action_10
action_63 _ = happyFail

action_64 (34) = happyShift action_11
action_64 (38) = happyShift action_12
action_64 (39) = happyShift action_13
action_64 (42) = happyShift action_14
action_64 (43) = happyShift action_15
action_64 (46) = happyShift action_16
action_64 (67) = happyShift action_17
action_64 (71) = happyShift action_18
action_64 (72) = happyShift action_19
action_64 (73) = happyShift action_20
action_64 (4) = happyGoto action_85
action_64 (5) = happyGoto action_5
action_64 (7) = happyGoto action_6
action_64 (27) = happyGoto action_7
action_64 (28) = happyGoto action_8
action_64 (29) = happyGoto action_9
action_64 (30) = happyGoto action_10
action_64 _ = happyFail

action_65 (34) = happyShift action_11
action_65 (38) = happyShift action_12
action_65 (39) = happyShift action_13
action_65 (42) = happyShift action_14
action_65 (43) = happyShift action_15
action_65 (46) = happyShift action_16
action_65 (67) = happyShift action_17
action_65 (71) = happyShift action_18
action_65 (72) = happyShift action_19
action_65 (73) = happyShift action_20
action_65 (4) = happyGoto action_84
action_65 (5) = happyGoto action_5
action_65 (7) = happyGoto action_6
action_65 (27) = happyGoto action_7
action_65 (28) = happyGoto action_8
action_65 (29) = happyGoto action_9
action_65 (30) = happyGoto action_10
action_65 _ = happyFail

action_66 (34) = happyShift action_11
action_66 (38) = happyShift action_12
action_66 (39) = happyShift action_13
action_66 (42) = happyShift action_14
action_66 (43) = happyShift action_15
action_66 (46) = happyShift action_16
action_66 (67) = happyShift action_17
action_66 (71) = happyShift action_18
action_66 (72) = happyShift action_19
action_66 (73) = happyShift action_20
action_66 (4) = happyGoto action_83
action_66 (5) = happyGoto action_5
action_66 (7) = happyGoto action_6
action_66 (27) = happyGoto action_7
action_66 (28) = happyGoto action_8
action_66 (29) = happyGoto action_9
action_66 (30) = happyGoto action_10
action_66 _ = happyFail

action_67 (34) = happyShift action_11
action_67 (38) = happyShift action_12
action_67 (39) = happyShift action_13
action_67 (42) = happyShift action_14
action_67 (43) = happyShift action_15
action_67 (46) = happyShift action_16
action_67 (67) = happyShift action_17
action_67 (71) = happyShift action_18
action_67 (72) = happyShift action_19
action_67 (73) = happyShift action_20
action_67 (4) = happyGoto action_82
action_67 (5) = happyGoto action_5
action_67 (7) = happyGoto action_6
action_67 (27) = happyGoto action_7
action_67 (28) = happyGoto action_8
action_67 (29) = happyGoto action_9
action_67 (30) = happyGoto action_10
action_67 _ = happyFail

action_68 _ = happyReduce_7

action_69 _ = happyReduce_24

action_70 (66) = happyShift action_81
action_70 _ = happyFail

action_71 (70) = happyShift action_80
action_71 _ = happyReduce_22

action_72 (64) = happyShift action_79
action_72 _ = happyFail

action_73 (62) = happyShift action_78
action_73 _ = happyFail

action_74 (70) = happyShift action_77
action_74 _ = happyReduce_26

action_75 (56) = happyShift action_76
action_75 _ = happyFail

action_76 (34) = happyShift action_11
action_76 (38) = happyShift action_12
action_76 (39) = happyShift action_13
action_76 (42) = happyShift action_14
action_76 (43) = happyShift action_15
action_76 (46) = happyShift action_16
action_76 (67) = happyShift action_17
action_76 (71) = happyShift action_18
action_76 (72) = happyShift action_19
action_76 (73) = happyShift action_20
action_76 (4) = happyGoto action_106
action_76 (5) = happyGoto action_5
action_76 (7) = happyGoto action_6
action_76 (27) = happyGoto action_7
action_76 (28) = happyGoto action_8
action_76 (29) = happyGoto action_9
action_76 (30) = happyGoto action_10
action_76 _ = happyFail

action_77 (73) = happyShift action_105
action_77 _ = happyFail

action_78 _ = happyReduce_6

action_79 (35) = happyShift action_104
action_79 _ = happyFail

action_80 (34) = happyShift action_11
action_80 (38) = happyShift action_12
action_80 (39) = happyShift action_13
action_80 (42) = happyShift action_14
action_80 (43) = happyShift action_15
action_80 (46) = happyShift action_16
action_80 (67) = happyShift action_17
action_80 (71) = happyShift action_18
action_80 (72) = happyShift action_19
action_80 (73) = happyShift action_20
action_80 (4) = happyGoto action_103
action_80 (5) = happyGoto action_5
action_80 (7) = happyGoto action_6
action_80 (27) = happyGoto action_7
action_80 (28) = happyGoto action_8
action_80 (29) = happyGoto action_9
action_80 (30) = happyGoto action_10
action_80 _ = happyFail

action_81 _ = happyReduce_5

action_82 _ = happyReduce_32

action_83 (44) = happyShift action_102
action_83 (6) = happyGoto action_101
action_83 _ = happyReduce_17

action_84 _ = happyReduce_10

action_85 (41) = happyShift action_100
action_85 _ = happyFail

action_86 (36) = happyShift action_99
action_86 _ = happyFail

action_87 (47) = happyShift action_96
action_87 (63) = happyShift action_97
action_87 (73) = happyShift action_98
action_87 (24) = happyGoto action_95
action_87 _ = happyFail

action_88 (34) = happyShift action_11
action_88 (38) = happyShift action_12
action_88 (39) = happyShift action_13
action_88 (42) = happyShift action_14
action_88 (43) = happyShift action_15
action_88 (46) = happyShift action_16
action_88 (67) = happyShift action_17
action_88 (71) = happyShift action_18
action_88 (72) = happyShift action_19
action_88 (73) = happyShift action_20
action_88 (4) = happyGoto action_94
action_88 (5) = happyGoto action_5
action_88 (7) = happyGoto action_6
action_88 (27) = happyGoto action_7
action_88 (28) = happyGoto action_8
action_88 (29) = happyGoto action_9
action_88 (30) = happyGoto action_10
action_88 _ = happyFail

action_89 (73) = happyShift action_93
action_89 _ = happyFail

action_90 (25) = happyGoto action_92
action_90 _ = happyReduce_53

action_91 _ = happyReduce_21

action_92 (66) = happyShift action_114
action_92 (70) = happyShift action_115
action_92 _ = happyFail

action_93 (48) = happyShift action_113
action_93 _ = happyFail

action_94 _ = happyReduce_44

action_95 _ = happyReduce_49

action_96 (35) = happyShift action_112
action_96 _ = happyFail

action_97 (25) = happyGoto action_111
action_97 _ = happyReduce_53

action_98 _ = happyReduce_50

action_99 _ = happyReduce_13

action_100 (34) = happyShift action_11
action_100 (38) = happyShift action_12
action_100 (39) = happyShift action_13
action_100 (42) = happyShift action_14
action_100 (43) = happyShift action_15
action_100 (46) = happyShift action_16
action_100 (67) = happyShift action_17
action_100 (71) = happyShift action_18
action_100 (72) = happyShift action_19
action_100 (73) = happyShift action_20
action_100 (4) = happyGoto action_110
action_100 (5) = happyGoto action_5
action_100 (7) = happyGoto action_6
action_100 (27) = happyGoto action_7
action_100 (28) = happyGoto action_8
action_100 (29) = happyGoto action_9
action_100 (30) = happyGoto action_10
action_100 _ = happyFail

action_101 _ = happyReduce_16

action_102 (34) = happyShift action_11
action_102 (38) = happyShift action_12
action_102 (39) = happyShift action_13
action_102 (42) = happyShift action_14
action_102 (43) = happyShift action_15
action_102 (46) = happyShift action_16
action_102 (67) = happyShift action_17
action_102 (71) = happyShift action_18
action_102 (72) = happyShift action_19
action_102 (73) = happyShift action_20
action_102 (4) = happyGoto action_109
action_102 (5) = happyGoto action_5
action_102 (7) = happyGoto action_6
action_102 (27) = happyGoto action_7
action_102 (28) = happyGoto action_8
action_102 (29) = happyGoto action_9
action_102 (30) = happyGoto action_10
action_102 _ = happyFail

action_103 _ = happyReduce_25

action_104 (34) = happyShift action_11
action_104 (38) = happyShift action_12
action_104 (39) = happyShift action_13
action_104 (42) = happyShift action_14
action_104 (43) = happyShift action_15
action_104 (46) = happyShift action_16
action_104 (67) = happyShift action_17
action_104 (71) = happyShift action_18
action_104 (72) = happyShift action_19
action_104 (73) = happyShift action_20
action_104 (4) = happyGoto action_108
action_104 (5) = happyGoto action_5
action_104 (7) = happyGoto action_6
action_104 (27) = happyGoto action_7
action_104 (28) = happyGoto action_8
action_104 (29) = happyGoto action_9
action_104 (30) = happyGoto action_10
action_104 _ = happyFail

action_105 (56) = happyShift action_107
action_105 _ = happyFail

action_106 _ = happyReduce_28

action_107 (34) = happyShift action_11
action_107 (38) = happyShift action_12
action_107 (39) = happyShift action_13
action_107 (42) = happyShift action_14
action_107 (43) = happyShift action_15
action_107 (46) = happyShift action_16
action_107 (67) = happyShift action_17
action_107 (71) = happyShift action_18
action_107 (72) = happyShift action_19
action_107 (73) = happyShift action_20
action_107 (4) = happyGoto action_123
action_107 (5) = happyGoto action_5
action_107 (7) = happyGoto action_6
action_107 (27) = happyGoto action_7
action_107 (28) = happyGoto action_8
action_107 (29) = happyGoto action_9
action_107 (30) = happyGoto action_10
action_107 _ = happyFail

action_108 _ = happyReduce_14

action_109 _ = happyReduce_18

action_110 (40) = happyShift action_122
action_110 _ = happyFail

action_111 (62) = happyShift action_121
action_111 (70) = happyShift action_115
action_111 _ = happyFail

action_112 (73) = happyShift action_120
action_112 _ = happyFail

action_113 (34) = happyShift action_11
action_113 (38) = happyShift action_12
action_113 (39) = happyShift action_13
action_113 (42) = happyShift action_14
action_113 (43) = happyShift action_15
action_113 (46) = happyShift action_16
action_113 (67) = happyShift action_17
action_113 (71) = happyShift action_18
action_113 (72) = happyShift action_19
action_113 (73) = happyShift action_20
action_113 (4) = happyGoto action_119
action_113 (5) = happyGoto action_5
action_113 (7) = happyGoto action_6
action_113 (27) = happyGoto action_7
action_113 (28) = happyGoto action_8
action_113 (29) = happyGoto action_9
action_113 (30) = happyGoto action_10
action_113 _ = happyFail

action_114 (56) = happyShift action_117
action_114 (69) = happyShift action_118
action_114 _ = happyFail

action_115 (73) = happyShift action_116
action_115 _ = happyFail

action_116 (69) = happyShift action_127
action_116 _ = happyFail

action_117 (34) = happyShift action_11
action_117 (38) = happyShift action_12
action_117 (39) = happyShift action_13
action_117 (42) = happyShift action_14
action_117 (43) = happyShift action_15
action_117 (46) = happyShift action_16
action_117 (67) = happyShift action_17
action_117 (71) = happyShift action_18
action_117 (72) = happyShift action_19
action_117 (73) = happyShift action_20
action_117 (4) = happyGoto action_126
action_117 (5) = happyGoto action_5
action_117 (7) = happyGoto action_6
action_117 (27) = happyGoto action_7
action_117 (28) = happyGoto action_8
action_117 (29) = happyGoto action_9
action_117 (30) = happyGoto action_10
action_117 _ = happyFail

action_118 (73) = happyShift action_125
action_118 _ = happyFail

action_119 _ = happyReduce_45

action_120 _ = happyReduce_52

action_121 _ = happyReduce_51

action_122 (34) = happyShift action_11
action_122 (38) = happyShift action_12
action_122 (39) = happyShift action_13
action_122 (42) = happyShift action_14
action_122 (43) = happyShift action_15
action_122 (46) = happyShift action_16
action_122 (67) = happyShift action_17
action_122 (71) = happyShift action_18
action_122 (72) = happyShift action_19
action_122 (73) = happyShift action_20
action_122 (4) = happyGoto action_124
action_122 (5) = happyGoto action_5
action_122 (7) = happyGoto action_6
action_122 (27) = happyGoto action_7
action_122 (28) = happyGoto action_8
action_122 (29) = happyGoto action_9
action_122 (30) = happyGoto action_10
action_122 _ = happyFail

action_123 _ = happyReduce_29

action_124 _ = happyReduce_11

action_125 (56) = happyShift action_129
action_125 _ = happyFail

action_126 _ = happyReduce_39

action_127 (73) = happyShift action_128
action_127 _ = happyFail

action_128 _ = happyReduce_54

action_129 (34) = happyShift action_11
action_129 (38) = happyShift action_12
action_129 (39) = happyShift action_13
action_129 (42) = happyShift action_14
action_129 (43) = happyShift action_15
action_129 (46) = happyShift action_16
action_129 (67) = happyShift action_17
action_129 (71) = happyShift action_18
action_129 (72) = happyShift action_19
action_129 (73) = happyShift action_20
action_129 (4) = happyGoto action_130
action_129 (5) = happyGoto action_5
action_129 (7) = happyGoto action_6
action_129 (27) = happyGoto action_7
action_129 (28) = happyGoto action_8
action_129 (29) = happyGoto action_9
action_129 (30) = happyGoto action_10
action_129 _ = happyFail

action_130 _ = happyReduce_40

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (VarExp happy_var_1
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
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
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

happyReduce_15 = happySpecReduce_1  4 happyReduction_15
happyReduction_15 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 5 happyReduction_16
happyReduction_16 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfExp happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_0  6 happyReduction_17
happyReduction_17  =  HappyAbsSyn6
		 (Nothing
	)

happyReduce_18 = happySpecReduce_2  6 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Just happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  7 happyReduction_19
happyReduction_19 (HappyTerminal (Token (Id happy_var_1) _))
	 =  HappyAbsSyn7
		 (SimpleVar happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  7 happyReduction_20
happyReduction_20 (HappyTerminal (Token (Id happy_var_2) _))
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (FieldVar happy_var_1 happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 7 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (SubscriptVar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1  8 happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (reverse happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  9 happyReduction_23
happyReduction_23  =  HappyAbsSyn9
		 ([]
	)

happyReduce_24 = happySpecReduce_1  9 happyReduction_24
happyReduction_24 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  9 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  10 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (reverse happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  11 happyReduction_27
happyReduction_27  =  HappyAbsSyn11
		 ([]
	)

happyReduce_28 = happySpecReduce_3  11 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (Token (Id happy_var_1) _))
	 =  HappyAbsSyn11
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 5 11 happyReduction_29
happyReduction_29 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_3) _)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((happy_var_3, happy_var_5) : happy_var_1
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_1  12 happyReduction_30
happyReduction_30 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (reverse happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  13 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  13 happyReduction_32
happyReduction_32 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  14 happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (reverse happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  15 happyReduction_34
happyReduction_34 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  15 happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_2 : happy_var_1
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  16 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (FunDec happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  16 happyReduction_37
happyReduction_37 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  16 happyReduction_38
happyReduction_38 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn16
		 (TypeDec happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 7 17 happyReduction_39
happyReduction_39 ((HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Fundec happy_var_2 happy_var_4 Nothing happy_var_7
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 9 17 happyReduction_40
happyReduction_40 ((HappyAbsSyn4  happy_var_9) `HappyStk`
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

happyReduce_41 = happySpecReduce_1  18 happyReduction_41
happyReduction_41 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (reverse happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  19 happyReduction_42
happyReduction_42 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  19 happyReduction_43
happyReduction_43 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_2 : happy_var_1
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happyReduce 4 20 happyReduction_44
happyReduction_44 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (VarDec happy_var_2 Nothing happy_var_4
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 6 20 happyReduction_45
happyReduction_45 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_4) _)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (VarDec happy_var_2 (Just happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_1  21 happyReduction_46
happyReduction_46 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (reverse happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  22 happyReduction_47
happyReduction_47 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  22 happyReduction_48
happyReduction_48 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_1
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 4 23 happyReduction_49
happyReduction_49 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_2) _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_1  24 happyReduction_50
happyReduction_50 (HappyTerminal (Token (Id happy_var_1) _))
	 =  HappyAbsSyn24
		 (NameTy happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  24 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (RecordTy (reverse happy_var_2)
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  24 happyReduction_52
happyReduction_52 (HappyTerminal (Token (Id happy_var_3) _))
	_
	_
	 =  HappyAbsSyn24
		 (ArrayTy happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_0  25 happyReduction_53
happyReduction_53  =  HappyAbsSyn25
		 ([]
	)

happyReduce_54 = happyReduce 5 25 happyReduction_54
happyReduction_54 ((HappyTerminal (Token (Id happy_var_5) _)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token (Id happy_var_3) _)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ((Field happy_var_3 happy_var_5) : happy_var_1
	) `HappyStk` happyRest

happyReduce_55 = happySpecReduce_1  26 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn26
		 (PlusOp
	)

happyReduce_56 = happySpecReduce_1  26 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn26
		 (MinusOp
	)

happyReduce_57 = happySpecReduce_1  26 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn26
		 (TimesOp
	)

happyReduce_58 = happySpecReduce_1  26 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn26
		 (DivideOp
	)

happyReduce_59 = happySpecReduce_1  26 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn26
		 (LtOp
	)

happyReduce_60 = happySpecReduce_1  26 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn26
		 (GtOp
	)

happyReduce_61 = happySpecReduce_1  26 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn26
		 (GeOp
	)

happyReduce_62 = happySpecReduce_1  26 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn26
		 (LeOp
	)

happyReduce_63 = happySpecReduce_1  26 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn26
		 (EqOp
	)

happyReduce_64 = happySpecReduce_3  27 happyReduction_64
happyReduction_64 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (OpExp happy_var_1 PlusOp happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  27 happyReduction_65
happyReduction_65 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (OpExp happy_var_1 MinusOp happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  27 happyReduction_66
happyReduction_66 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  28 happyReduction_67
happyReduction_67 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (OpExp happy_var_1 TimesOp happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  28 happyReduction_68
happyReduction_68 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (OpExp happy_var_1 DivideOp happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  28 happyReduction_69
happyReduction_69 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  29 happyReduction_70
happyReduction_70 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn29
		 (VarExp happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  30 happyReduction_71
happyReduction_71 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 75 75 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token Type _ -> cont 31;
	Token Var _ -> cont 32;
	Token Function _ -> cont 33;
	Token Break _ -> cont 34;
	Token Of _ -> cont 35;
	Token End _ -> cont 36;
	Token In _ -> cont 37;
	Token Nil _ -> cont 38;
	Token Let _ -> cont 39;
	Token Do _ -> cont 40;
	Token To _ -> cont 41;
	Token For _ -> cont 42;
	Token While _ -> cont 43;
	Token Else _ -> cont 44;
	Token Then _ -> cont 45;
	Token If _ -> cont 46;
	Token Array _ -> cont 47;
	Token Assign _ -> cont 48;
	Token Or _ -> cont 49;
	Token And _ -> cont 50;
	Token Ge _ -> cont 51;
	Token Gt _ -> cont 52;
	Token Le _ -> cont 53;
	Token Lt _ -> cont 54;
	Token Neq _ -> cont 55;
	Token Eq _ -> cont 56;
	Token Divide _ -> cont 57;
	Token Times _ -> cont 58;
	Token Minus _ -> cont 59;
	Token Plus _ -> cont 60;
	Token Dot _ -> cont 61;
	Token Rbrace _ -> cont 62;
	Token Lbrace _ -> cont 63;
	Token Rbrack _ -> cont 64;
	Token Lbrack _ -> cont 65;
	Token Rparen _ -> cont 66;
	Token Lparen _ -> cont 67;
	Token Semicolon _ -> cont 68;
	Token Colon _ -> cont 69;
	Token Comma _ -> cont 70;
	Token (String happy_dollar_dollar) _ -> cont 71;
	Token (Int happy_dollar_dollar) _ -> cont 72;
	Token (Id happy_dollar_dollar) _ -> cont 73;
	Token Eof _ -> cont 74;
	_ -> happyError' (tk:tks)
	}

happyError_ 75 tk tks = happyError' tks
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
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError ((Token _ (AlexPn _ l c)) : _) =
  error $ "Parse error at line " ++ (show l) ++ " col " ++  (show c)
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
