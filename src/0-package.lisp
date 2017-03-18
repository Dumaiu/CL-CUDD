;;;; package.lisp

(in-package :cl-user)

(defpackage cl-cudd.swig-macros
  (:use :cl :cffi)
  (:documentation
   "Package containing utility functions for SWIG cffi interface generation")
  (:export #:lispify))

(defpackage cl-cudd.baseapi
  (:documentation "Low-level interface")
  (:use :cl :cffi :cl-cudd.swig-macros :alexandria)
  (:shadow #:pi)
  (:export :+CUDD-MAX-INDEX+ 
           :ADD-VAR 
           :BDD-VAR 
           :CUDD-ADD-AGREEMENT 
           :CUDD-ADD-APPLY 
           :CUDD-ADD-BDD-INTERVAL 
           :CUDD-ADD-BDD-ITH-BIT 
           :CUDD-ADD-BDD-PATTERN 
           :CUDD-ADD-BDD-STRICT-THRESHOLD 
           :CUDD-ADD-BDD-THRESHOLD 
           :CUDD-ADD-CMPL 
           :CUDD-ADD-COMPOSE 
           :CUDD-ADD-COMPUTE-CUBE 
           :CUDD-ADD-CONST 
           :CUDD-ADD-CONSTRAIN 
           :CUDD-ADD-CUBE 
           :CUDD-ADD-DIFF 
           :CUDD-ADD-DIVIDE 
           :CUDD-ADD-EVAL-CONST 
           :CUDD-ADD-EXIST-ABSTRACT 
           :CUDD-ADD-FIND-MAX 
           :CUDD-ADD-FIND-MIN 
           :CUDD-ADD-GENERAL-VECTOR-COMPOSE 
           :CUDD-ADD-HAMMING 
           :CUDD-ADD-HARWELL 
           :CUDD-ADD-HOOK 
           :CUDD-ADD-ITE 
           :CUDD-ADD-ITE-CONSTANT 
           :CUDD-ADD-ITH-BIT 
           :CUDD-ADD-ITH-VAR 
           :CUDD-ADD-LEQ 
           :CUDD-ADD-LOG 
           :CUDD-ADD-MATRIX-MULTIPLY 
           :CUDD-ADD-MAXIMUM 
           :CUDD-ADD-MINIMUM 
           :CUDD-ADD-MINUS 
           :CUDD-ADD-MONADIC-APPLY 
           :CUDD-ADD-NAND 
           :CUDD-ADD-NEGATE 
           :CUDD-ADD-NEW-VAR 
           :CUDD-ADD-NEW-VAR-AT-LEVEL 
           :CUDD-ADD-NON-SIM-COMPOSE 
           :CUDD-ADD-NOR 
           :CUDD-ADD-ONE-ZERO-MAXIMUM 
           :CUDD-ADD-OR 
           :CUDD-ADD-OR-ABSTRACT 
           :CUDD-ADD-OUTER-SUM 
           :CUDD-ADD-PERMUTE 
           :CUDD-ADD-PLUS 
           :CUDD-ADD-READ 
           :CUDD-ADD-RESIDUE 
           :CUDD-ADD-RESTRICT 
           :CUDD-ADD-ROUND-OFF 
           :CUDD-ADD-SCALAR-INVERSE 
           :CUDD-ADD-SET-NZ 
           :CUDD-ADD-SWAP-VARIABLES 
           :CUDD-ADD-THRESHOLD 
           :CUDD-ADD-TIMES 
           :CUDD-ADD-TIMES-PLUS 
           :CUDD-ADD-TRIANGLE 
           :CUDD-ADD-UNIV-ABSTRACT 
           :CUDD-ADD-VECTOR-COMPOSE 
           :CUDD-ADD-WALSH 
           :CUDD-ADD-XEQY 
           :CUDD-ADD-XNOR 
           :CUDD-ADD-XOR 
           :CUDD-AGGREGATION-TYPE 
           :CUDD-APA-ADD 
           :CUDD-APA-COMPARE 
           :CUDD-APA-COMPARE-RATIOS 
           :CUDD-APA-COPY 
           :CUDD-APA-COUNT-MINTERM 
           :CUDD-APA-INT-DIVISION 
           :CUDD-APA-NUMBER-OF-DIGITS 
           :CUDD-APA-POWER-OF-TWO 
           :CUDD-APA-PRINT-DECIMAL 
           :CUDD-APA-PRINT-DENSITY 
           :CUDD-APA-PRINT-EXPONENTIAL 
           :CUDD-APA-PRINT-HEX 
           :CUDD-APA-PRINT-MINTERM 
           :CUDD-APA-PRINT-MINTERM-EXP 
           :CUDD-APA-SET-TO-LITERAL 
           :CUDD-APA-SHIFT-RIGHT 
           :CUDD-APA-SHORT-DIVISION 
           :CUDD-APA-SUBTRACT 
           :CUDD-AUTODYN-DISABLE 
           :CUDD-AUTODYN-DISABLE-ZDD 
           :CUDD-AUTODYN-ENABLE 
           :CUDD-AUTODYN-ENABLE-ZDD 
           :CUDD-AVERAGE-DISTANCE 
           :CUDD-BDD-ADJ-PERMUTE-X 
           :CUDD-BDD-AND 
           :CUDD-BDD-AND-ABSTRACT 
           :CUDD-BDD-AND-ABSTRACT-LIMIT 
           :CUDD-BDD-AND-LIMIT 
           :CUDD-BDD-APPROX-CONJ-DECOMP 
           :CUDD-BDD-APPROX-DISJ-DECOMP 
           :CUDD-BDD-BIND-VAR 
           :CUDD-BDD-BOOLEAN-DIFF 
           :CUDD-BDD-CHAR-TO-VECT 
           :CUDD-BDD-CLIPPING-AND 
           :CUDD-BDD-CLIPPING-AND-ABSTRACT 
           :CUDD-BDD-CLOSEST-CUBE 
           :CUDD-BDD-COMPOSE 
           :CUDD-BDD-COMPUTE-CUBE 
           :CUDD-BDD-CONSTRAIN 
           :CUDD-BDD-CONSTRAIN-DECOMP 
           :CUDD-BDD-CORRELATION 
           :CUDD-BDD-CORRELATION-WEIGHTS 
           :CUDD-BDD-CUBE 
           :CUDD-BDD-EXIST-ABSTRACT 
           :CUDD-BDD-GEN-CONJ-DECOMP 
           :CUDD-BDD-GEN-DISJ-DECOMP 
           :CUDD-BDD-INTERSECT 
           :CUDD-BDD-IS-NS-VAR 
           :CUDD-BDD-IS-PI-VAR 
           :CUDD-BDD-IS-PS-VAR 
           :CUDD-BDD-IS-VAR-ESSENTIAL 
           :CUDD-BDD-IS-VAR-HARD-GROUP 
           :CUDD-BDD-IS-VAR-TO-BE-GROUPED 
           :CUDD-BDD-IS-VAR-TO-BE-UNGROUPED 
           :CUDD-BDD-ISOP 
           :CUDD-BDD-ITE 
           :CUDD-BDD-ITE-CONSTANT 
           :CUDD-BDD-ITER-CONJ-DECOMP 
           :CUDD-BDD-ITER-DISJ-DECOMP 
           :CUDD-BDD-ITH-VAR 
           :CUDD-BDD-LEQ 
           :CUDD-BDD-LEQ-UNLESS 
           :CUDD-BDD-LICOMPACTION 
           :CUDD-BDD-LITERAL-SET-INTERSECTION 
           :CUDD-BDD-MAKE-PRIME 
           :CUDD-BDD-MINIMIZE 
           :CUDD-BDD-NAND 
           :CUDD-BDD-NEW-VAR 
           :CUDD-BDD-NEW-VAR-AT-LEVEL 
           :CUDD-BDD-NOR 
           :CUDD-BDD-NOT 
           :CUDD-BDD-NPAND 
           :CUDD-BDD-OR 
           :CUDD-BDD-PERMUTE 
           :CUDD-BDD-PICK-ARBITRARY-MINTERMS 
           :CUDD-BDD-PICK-ONE-CUBE 
           :CUDD-BDD-PICK-ONE-MINTERM 
           :CUDD-BDD-PRINT-COVER 
           :CUDD-BDD-READ 
           :CUDD-BDD-READ-PAIR-INDEX 
           :CUDD-BDD-REALIGN-DISABLE 
           :CUDD-BDD-REALIGN-ENABLE 
           :CUDD-BDD-REALIGNMENT-ENABLED 
           :CUDD-BDD-RESET-VAR-TO-BE-GROUPED 
           :CUDD-BDD-RESTRICT 
           :CUDD-BDD-SET-NS-VAR 
           :CUDD-BDD-SET-PAIR-INDEX 
           :CUDD-BDD-SET-PI-VAR 
           :CUDD-BDD-SET-PS-VAR 
           :CUDD-BDD-SET-VAR-HARD-GROUP 
           :CUDD-BDD-SET-VAR-TO-BE-GROUPED 
           :CUDD-BDD-SET-VAR-TO-BE-UNGROUPED 
           :CUDD-BDD-SQUEEZE 
           :CUDD-BDD-SWAP-VARIABLES 
           :CUDD-BDD-TO-ADD 
           :CUDD-BDD-TO-CUBE-ARRAY 
           :CUDD-BDD-TRANSFER 
           :CUDD-BDD-UNBIND-VAR 
           :CUDD-BDD-UNIV-ABSTRACT 
           :CUDD-BDD-VAR-CONJ-DECOMP 
           :CUDD-BDD-VAR-DISJ-DECOMP 
           :CUDD-BDD-VAR-IS-BOUND 
           :CUDD-BDD-VAR-IS-DEPENDENT 
           :CUDD-BDD-VAR-MAP 
           :CUDD-BDD-VECTOR-COMPOSE 
           :CUDD-BDD-XNOR 
           :CUDD-BDD-XOR 
           :CUDD-BDD-XOR-EXIST-ABSTRACT 
           :CUDD-BIASED-OVER-APPROX 
           :CUDD-BIASED-UNDER-APPROX 
           :CUDD-CACHE-SLOTS 
           :CUDD-CHECK-KEYS 
           :CUDD-CHECK-ZERO-REF 
           :CUDD-CLASSIFY-SUPPORT 
           :CUDD-CLEAR-ERROR-CODE 
           :CUDD-COF-MINTERM 
           :CUDD-COFACTOR 
           :CUDD-COUNT-LEAVES 
           :CUDD-COUNT-MINTERM 
           :CUDD-COUNT-PATH 
           :CUDD-COUNT-PATHS-TO-NON-ZERO 
           :CUDD-CPROJECTION 
           :CUDD-CUBE-ARRAY-TO-BDD 
           :CUDD-DAG-SIZE 
           :CUDD-DEAD-ARE-COUNTED 
           :CUDD-DEBUG-CHECK 
           :CUDD-DECREASING 
           :CUDD-DELAYED-DEREF-BDD 
           :CUDD-DENSITY 
           :CUDD-DEREF 
           :CUDD-DISABLE-GARBAGE-COLLECTION 
           :CUDD-DISABLE-REORDERING-REPORTING 
           :CUDD-DUMP-BLIF 
           :CUDD-DUMP-BLIF-BODY 
           :CUDD-DUMP-DA-VINCI 
           :CUDD-DUMP-DDCAL 
           :CUDD-DUMP-DOT 
           :CUDD-DUMP-FACTORED-FORM 
           :CUDD-DXYGTDXZ 
           :CUDD-DXYGTDYZ 
           :CUDD-ENABLE-GARBAGE-COLLECTION 
           :CUDD-ENABLE-REORDERING-REPORTING 
           :CUDD-EPD-COUNT-MINTERM 
           :CUDD-EQUAL-SUP-NORM 
           :CUDD-EQUIV-DC 
           :CUDD-ERROR-TYPE 
           :CUDD-ESTIMATE-COFACTOR 
           :CUDD-ESTIMATE-COFACTOR-SIMPLE 
           :CUDD-EVAL 
           :CUDD-EXPECTED-USED-SLOTS 
           :CUDD-FIND-ESSENTIAL 
           :CUDD-FIND-TWO-LITERAL-CLAUSES 
           :CUDD-FIRST-CUBE 
           :CUDD-FIRST-NODE 
           :CUDD-FIRST-PRIME 
           :CUDD-FREE-TREE 
           :CUDD-FREE-ZDD-TREE 
           :CUDD-GARBAGE-COLLECTION-ENABLED 
           :CUDD-GEN-FREE 
           :CUDD-HOOK-TYPE 
           :CUDD-INCREASING 
           :CUDD-INDICES-TO-CUBE 
           :CUDD-INIT 
           :CUDD-IS-GEN-EMPTY 
           :CUDD-IS-IN-HOOK 
           :CUDD-IS-NON-CONSTANT 
           :CUDD-ITER-DEREF-BDD 
           :CUDD-LARGEST-CUBE 
           :CUDD-LAZY-GROUP-TYPE 
           :CUDD-MAKE-BDD-FROM-ZDD-COVER 
           :CUDD-MAKE-TREE-NODE 
           :CUDD-MAKE-ZDD-TREE-NODE 
           :CUDD-MANAGER 
           :CUDD-MIN-HAMMING-DIST 
           :CUDD-NEW-APA-NUMBER 
           :CUDD-NEXT-CUBE 
           :CUDD-NEXT-NODE 
           :CUDD-NEXT-PRIME 
           :CUDD-NODE 
           :CUDD-NODE-GET-ELSE 
           :CUDD-NODE-GET-REF-COUNT 
           :CUDD-NODE-GET-THEN 
           :CUDD-NODE-GET-VALUE 
           :CUDD-NODE-IS-CONSTANT 
           :CUDD-NODE-READ-INDEX 
           :CUDD-OUT-OF-MEM 
           :CUDD-OVER-APPROX 
           :CUDD-PRIME 
           :CUDD-PRINT-DEBUG 
           :CUDD-PRINT-INFO 
           :CUDD-PRINT-LINEAR 
           :CUDD-PRINT-MINTERM 
           :CUDD-PRINT-TWO-LITERAL-CLAUSES 
           :CUDD-PRINT-VERSION 
           :CUDD-PRIORITY-SELECT 
           :CUDD-QUIT 
           :CUDD-RANDOM 
           :CUDD-READ-ARCVIOLATION 
           :CUDD-READ-BACKGROUND 
           :CUDD-READ-CACHE-HITS 
           :CUDD-READ-CACHE-LOOK-UPS 
           :CUDD-READ-CACHE-SLOTS 
           :CUDD-READ-CACHE-USED-SLOTS 
           :CUDD-READ-DEAD 
           :CUDD-READ-EPSILON 
           :CUDD-READ-ERROR-CODE 
           :CUDD-READ-GARBAGE-COLLECTION-TIME 
           :CUDD-READ-GARBAGE-COLLECTIONS 
           :CUDD-READ-GROUPCHECK 
           :CUDD-READ-INV-PERM 
           :CUDD-READ-INV-PERM-ZDD 
           :CUDD-READ-ITH-CLAUSE 
           :CUDD-READ-KEYS 
           :CUDD-READ-LINEAR 
           :CUDD-READ-LOGIC-ZERO 
           :CUDD-READ-LOOSE-UP-TO 
           :CUDD-READ-MAX-CACHE 
           :CUDD-READ-MAX-CACHE-HARD 
           :CUDD-READ-MAX-GROWTH 
           :CUDD-READ-MAX-GROWTH-ALTERNATE 
           :CUDD-READ-MAX-LIVE 
           :CUDD-READ-MAX-MEMORY 
           :CUDD-READ-MEMORY-IN-USE 
           :CUDD-READ-MIN-DEAD 
           :CUDD-READ-MIN-HIT 
           :CUDD-READ-MINUS-INFINITY 
           :CUDD-READ-NEXT-REORDERING 
           :CUDD-READ-NODE-COUNT 
           :CUDD-READ-NODES-DROPPED 
           :CUDD-READ-NODES-FREED 
           :CUDD-READ-NUMBER-XOVERS 
           :CUDD-READ-ONE 
           :CUDD-READ-PEAK-LIVE-NODE-COUNT 
           :CUDD-READ-PEAK-NODE-COUNT 
           :CUDD-READ-PERM 
           :CUDD-READ-PERM-ZDD 
           :CUDD-READ-PLUS-INFINITY 
           :CUDD-READ-POPULATION-SIZE 
           :CUDD-READ-RECOMB 
           :CUDD-READ-RECURSIVE-CALLS 
           :CUDD-READ-REORDERING-CYCLE 
           :CUDD-READ-REORDERING-TIME 
           :CUDD-READ-REORDERINGS 
           :CUDD-READ-SIFT-MAX-SWAP 
           :CUDD-READ-SIFT-MAX-VAR 
           :CUDD-READ-SIZE 
           :CUDD-READ-SLOTS 
           :CUDD-READ-STDERR 
           :CUDD-READ-STDOUT 
           :CUDD-READ-SWAP-STEPS 
           :CUDD-READ-SYMMVIOLATION 
           :CUDD-READ-TREE 
           :CUDD-READ-UNIQUE-LINKS 
           :CUDD-READ-UNIQUE-LOOK-UPS 
           :CUDD-READ-USED-SLOTS 
           :CUDD-READ-VARS 
           :CUDD-READ-ZDD-ONE 
           :CUDD-READ-ZDD-SIZE 
           :CUDD-READ-ZDD-TREE 
           :CUDD-READ-ZERO 
           :CUDD-RECURSIVE-DEREF 
           :CUDD-RECURSIVE-DEREF-ZDD 
           :CUDD-REDUCE-HEAP 
           :CUDD-REF 
           :CUDD-REGULAR
           :CUDD-REMAP-OVER-APPROX 
           :CUDD-REMAP-UNDER-APPROX 
           :CUDD-REMOVE-HOOK 
           :CUDD-REORDERING-REPORTING 
           :CUDD-REORDERING-STATUS 
           :CUDD-REORDERING-STATUS-ZDD 
           :CUDD-REORDERING-TYPE 
           :CUDD-RESIDUE-DEFAULT 
           :CUDD-RESIDUE-MSB 
           :CUDD-RESIDUE-TC 
           :CUDD-SET-ARCVIOLATION 
           :CUDD-SET-BACKGROUND 
           :CUDD-SET-EPSILON 
           :CUDD-SET-GROUPCHECK 
           :CUDD-SET-LOOSE-UP-TO 
           :CUDD-SET-MAX-CACHE-HARD 
           :CUDD-SET-MAX-GROWTH 
           :CUDD-SET-MAX-GROWTH-ALTERNATE 
           :CUDD-SET-MAX-LIVE 
           :CUDD-SET-MAX-MEMORY 
           :CUDD-SET-MIN-HIT 
           :CUDD-SET-NEXT-REORDERING 
           :CUDD-SET-NUMBER-XOVERS 
           :CUDD-SET-POPULATION-SIZE 
           :CUDD-SET-RECOMB 
           :CUDD-SET-REORDERING-CYCLE 
           :CUDD-SET-SIFT-MAX-SWAP 
           :CUDD-SET-SIFT-MAX-VAR 
           :CUDD-SET-STDERR 
           :CUDD-SET-STDOUT 
           :CUDD-SET-SYMMVIOLATION 
           :CUDD-SET-TREE 
           :CUDD-SET-VAR-MAP 
           :CUDD-SET-ZDD-TREE 
           :CUDD-SHARING-SIZE 
           :CUDD-SHORTEST-LENGTH 
           :CUDD-SHORTEST-PATH 
           :CUDD-SHUFFLE-HEAP 
           :CUDD-SOLVE-EQN 
           :CUDD-SPLIT-SET 
           :CUDD-SRANDOM 
           :CUDD-STD-POST-REORD-HOOK 
           :CUDD-STD-PRE-REORD-HOOK 
           :CUDD-SUBSET-COMPRESS 
           :CUDD-SUBSET-HEAVY-BRANCH 
           :CUDD-SUBSET-SHORT-PATHS 
           :CUDD-SUBSET-WITH-MASK-VARS 
           :CUDD-SUPERSET-COMPRESS 
           :CUDD-SUPERSET-HEAVY-BRANCH 
           :CUDD-SUPERSET-SHORT-PATHS 
           :CUDD-SUPPORT 
           :CUDD-SUPPORT-INDEX 
           :CUDD-SUPPORT-SIZE 
           :CUDD-SYMM-PROFILE 
           :CUDD-TLC-INFO-FREE 
           :CUDD-TURN-OFF-COUNT-DEAD 
           :CUDD-TURN-ON-COUNT-DEAD 
           :CUDD-UNDER-APPROX 
           :CUDD-UNIQUE-SLOTS 
           :CUDD-VARIABLE-TYPE 
           :CUDD-VECTOR-SUPPORT 
           :CUDD-VECTOR-SUPPORT-INDEX 
           :CUDD-VECTOR-SUPPORT-SIZE 
           :CUDD-VERIFY-SOL 
           :CUDD-VERSION 
           :CUDD-XEQY 
           :CUDD-XGTY 
           :CUDD-ZDD-CHANGE 
           :CUDD-ZDD-COMPLEMENT 
           :CUDD-ZDD-COUNT 
           :CUDD-ZDD-COUNT-DOUBLE 
           :CUDD-ZDD-COUNT-MINTERM 
           :CUDD-ZDD-COVER-PATH-TO-STRING 
           :CUDD-ZDD-DAG-SIZE 
           :CUDD-ZDD-DIFF 
           :CUDD-ZDD-DIFF-CONST 
           :CUDD-ZDD-DIVIDE 
           :CUDD-ZDD-DIVIDE-F 
           :CUDD-ZDD-DUMP-DOT 
           :CUDD-ZDD-FIRST-PATH 
           :CUDD-ZDD-INTERSECT 
           :CUDD-ZDD-ISOP 
           :CUDD-ZDD-ITE 
           :CUDD-ZDD-ITH-VAR 
           :CUDD-ZDD-NEXT-PATH 
           :CUDD-ZDD-PORT-FROM-BDD 
           :CUDD-ZDD-PORT-TO-BDD 
           :CUDD-ZDD-PRINT-COVER 
           :CUDD-ZDD-PRINT-DEBUG 
           :CUDD-ZDD-PRINT-MINTERM 
           :CUDD-ZDD-PRINT-SUBTABLE 
           :CUDD-ZDD-PRODUCT 
           :CUDD-ZDD-READ-NODE-COUNT 
           :CUDD-ZDD-REALIGN-DISABLE 
           :CUDD-ZDD-REALIGN-ENABLE 
           :CUDD-ZDD-REALIGNMENT-ENABLED 
           :CUDD-ZDD-REDUCE-HEAP 
           :CUDD-ZDD-SHUFFLE-HEAP 
           :CUDD-ZDD-SUBSET-0 
           :CUDD-ZDD-SUBSET-1 
           :CUDD-ZDD-SYMM-PROFILE 
           :CUDD-ZDD-UNATE-PRODUCT 
           :CUDD-ZDD-UNION 
           :CUDD-ZDD-VARS-FROM-BDD-VARS 
           :CUDD-ZDD-WEAK-DIV 
           :CUDD-ZDD-WEAK-DIV-F 
           :DD-APA-BASE 
           :DD-APA-BITS 
           :DD-APA-HEXPRINT 
           :DD-APA-MASK 
           :DD-CHILDREN 
           :DD-NODE 
           :DD-NODE-TYPE 
           :DUMP-DOT 
           :E 
           :FALSE 
           :INDEX 
           :KIDS 
           :NEXT 
           :PRINT-INFO 
           :REF 
           :SIZEOF-INT 
           :SIZEOF-LONG 
           :SIZEOF-VOID-P 
           :TRUE 
           :VALUE
           :zdd-dump-dot)
  ;; other exports done by swig
  )

(defpackage cl-cudd
  (:documentation "High-level interface")
  (:use :cl :cffi :alexandria :cl-cudd.swig-macros :cl-cudd.baseapi)
  (:nicknames :cudd)
  (:export 
   #:+AGREEMENT+
   #:+AND+
   #:+DIFF+
   #:+DIVIDE+
   #:+EQUALS+
   #:+GREATER-THAN+
   #:+GREATER-THAN-EQUALS+
   #:+LESS-THAN+
   #:+LESS-THAN-EQUALS+
   #:+LOG-X-Y+
   #:+MAXIMUM+
   #:+MINIMUM+
   #:+MINUS+
   #:+MOD+
   #:+NAND+
   #:+NOR+
   #:+NOT-EQUALS+
   #:+ONE-ZERO-MAXIMUM+
   #:+OR+
   #:+PLUS+
   #:+POW+
   #:+SET-NZ+
   #:+THRESHOLD+
   #:+TIMES+
   #:+XNOR+
   #:+XOR+
   #:*MANAGER*
   #:ADD->BDD
   #:ADD-APPLY
   #:ADD-CONSTANT
   #:ADD-NEGATE
   #:ADD-NODE
   #:BDD->ADD
   #:BDD-NODE
   #:COFACTOR
   #:COUNT-LEAVES
   #:COUNT-MINTERM
   #:CUBE
   #:DAG-SIZE
   #:DISABLE-GC
   #:ENABLE-GC
   #:EXIST-ABSTRACT
   #:UNIV-ABSTRACT
   #:IF-THEN-ELSE
   #:MAKE-VAR
   #:MANAGER
   #:MANAGER-POINTER
   #:NODE
   #:NODE-POINTER
   #:NODE-COMPLEMENT
   #:NODE-EQUAL
   #:NODE-CONSTANT-P
   #:NODE-VALUE
   #:NODE-INDEX
   #:OR-ABSTRACT
   #:SWAP-VARIABLES
   #:WITH-MANAGER

   #:add->bdd-interval
   #:add->bdd-threshold
   #:add->bdd-strict-threshold

   #:node-or
   #:node-and
   #:node-else
   #:node-then

   #:plus-infinity
   #:minus-infinity
   #:epsilon

   #:min-abstract
   #:max-abstract

   #:one-node
   #:zero-node

   #:with-nodes
   #:zdd-node
   #:bdd->zdd-simple
   #:zdd->bdd-simple
   #:bdd->zdd-cover
   #:zdd->bdd-cover
   #:zdd-emptyset
   #:zdd-set-of-emptyset
   #:zdd-singleton
   #:zdd-subset-0
   #:zdd-subset-1
   #:zdd-change
   #:zdd-union
   #:zdd-intersection
   #:zdd-difference
   #:zdd-divide-unate
   #:zdd-divide-binate
   #:zdd-product-unate
   #:zdd-product-binate
   #:info
   #:peak-node-count
   #:peak-live-node-count
   #:node-count
   #:zdd-node-count))
