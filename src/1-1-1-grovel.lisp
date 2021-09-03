;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 2.0.10
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.

;;; Auto-generated -*- lisp -*- file
;;; generated from $Id:$

;; converted by Masataro Asai into CFFI-Grovel file

(in-package :cl-cudd.baseapi)

;; note: DdManager and DdNode cannot be grovelled due to NULL checking.
;; See 1-0-2-translators.lisp

;; #include <stdlib.h>
(include "stdio.h")
(include "stdlib.h")
(cc-flags #.(format nil "~{-I~A ~}"
                    (directory
                     (merge-pathnames
                      "*/"
					  ;; FIXME: Kludge:
                      (asdf:system-relative-pathname :cl-cudd "cudd/")))))

;; FIXME: Kludge:
(cc-flags #.(format nil "-I~A" (asdf:system-relative-pathname :cl-cudd "cudd/")))
;; FIXME: Added this, the build directory:
(cc-flags #.(format nil "-I~A" (asdf:system-relative-pathname :cl-cudd "build-cudd/")))
(include "config.h")
(include "cudd/cudd.h")
(include "cudd/cuddInt.h")

(ctype #.(lispify "DdHalfWord" :type) "DdHalfWord")
(ctype #.(lispify "CUDD_VALUE_TYPE" :type) "CUDD_VALUE_TYPE")

(constant (#.(lispify "CUDD_MAXINDEX" :constant) "CUDD_MAXINDEX"))
(constant (#.(lispify "CUDD_VERSION" :constant) "CUDD_VERSION"))
(constant (#.(lispify "SIZEOF_VOID_P" :constant) "SIZEOF_VOID_P"))
(constant (#.(lispify "SIZEOF_INT" :constant) "SIZEOF_INT"))
(constant (#.(lispify "SIZEOF_LONG" :constant) "SIZEOF_LONG"))
(constant (#.(lispify "CUDD_TRUE" :constant) "CUDD_TRUE"))
(constant (#.(lispify "CUDD_FALSE" :constant) "CUDD_FALSE"))
(constant (#.(lispify "CUDD_OUT_OF_MEM" :constant) "CUDD_OUT_OF_MEM"))
(constant (#.(lispify "CUDD_UNIQUE_SLOTS" :constant) "CUDD_UNIQUE_SLOTS"))
(constant (#.(lispify "CUDD_CACHE_SLOTS" :constant) "CUDD_CACHE_SLOTS"))
(constant (#.(lispify "CUDD_RESIDUE_DEFAULT" :constant) "CUDD_RESIDUE_DEFAULT"))
(constant (#.(lispify "CUDD_RESIDUE_MSB" :constant) "CUDD_RESIDUE_MSB"))
(constant (#.(lispify "CUDD_RESIDUE_TC" :constant) "CUDD_RESIDUE_TC"))
(constant (#.(lispify "DD_APA_BITS" :constant) "DD_APA_BITS"))
(constant (#.(lispify "DD_APA_BASE" :constant) "DD_APA_BASE"))
(constant (#.(lispify "DD_APA_MASK" :constant) "DD_APA_MASK"))
(constant (#.(lispify "DD_APA_HEXPRINT" :constant) "DD_APA_HEXPRINT"))
(cenum #.(lispify "Cudd_ReorderingType" :enumname)
       ((#.(lispify "CUDD_REORDER_SAME" :enumvalue :keyword) "CUDD_REORDER_SAME"))
       ((#.(lispify "CUDD_REORDER_NONE" :enumvalue :keyword) "CUDD_REORDER_NONE"))
       ((#.(lispify "CUDD_REORDER_RANDOM" :enumvalue :keyword) "CUDD_REORDER_RANDOM"))
       ((#.(lispify "CUDD_REORDER_RANDOM_PIVOT" :enumvalue :keyword) "CUDD_REORDER_RANDOM_PIVOT"))
       ((#.(lispify "CUDD_REORDER_SIFT" :enumvalue :keyword) "CUDD_REORDER_SIFT"))
       ((#.(lispify "CUDD_REORDER_SIFT_CONVERGE" :enumvalue :keyword) "CUDD_REORDER_SIFT_CONVERGE"))
       ((#.(lispify "CUDD_REORDER_SYMM_SIFT" :enumvalue :keyword) "CUDD_REORDER_SYMM_SIFT"))
       ((#.(lispify "CUDD_REORDER_SYMM_SIFT_CONV" :enumvalue :keyword) "CUDD_REORDER_SYMM_SIFT_CONV"))
       ((#.(lispify "CUDD_REORDER_WINDOW2" :enumvalue :keyword) "CUDD_REORDER_WINDOW2"))
       ((#.(lispify "CUDD_REORDER_WINDOW3" :enumvalue :keyword) "CUDD_REORDER_WINDOW3"))
       ((#.(lispify "CUDD_REORDER_WINDOW4" :enumvalue :keyword) "CUDD_REORDER_WINDOW4"))
       ((#.(lispify "CUDD_REORDER_WINDOW2_CONV" :enumvalue :keyword) "CUDD_REORDER_WINDOW2_CONV"))
       ((#.(lispify "CUDD_REORDER_WINDOW3_CONV" :enumvalue :keyword) "CUDD_REORDER_WINDOW3_CONV"))
       ((#.(lispify "CUDD_REORDER_WINDOW4_CONV" :enumvalue :keyword) "CUDD_REORDER_WINDOW4_CONV"))
       ((#.(lispify "CUDD_REORDER_GROUP_SIFT" :enumvalue :keyword) "CUDD_REORDER_GROUP_SIFT"))
       ((#.(lispify "CUDD_REORDER_GROUP_SIFT_CONV" :enumvalue :keyword) "CUDD_REORDER_GROUP_SIFT_CONV"))
       ((#.(lispify "CUDD_REORDER_ANNEALING" :enumvalue :keyword) "CUDD_REORDER_ANNEALING"))
       ((#.(lispify "CUDD_REORDER_GENETIC" :enumvalue :keyword) "CUDD_REORDER_GENETIC"))
       ((#.(lispify "CUDD_REORDER_LINEAR" :enumvalue :keyword) "CUDD_REORDER_LINEAR"))
       ((#.(lispify "CUDD_REORDER_LINEAR_CONVERGE" :enumvalue :keyword) "CUDD_REORDER_LINEAR_CONVERGE"))
       ((#.(lispify "CUDD_REORDER_LAZY_SIFT" :enumvalue :keyword) "CUDD_REORDER_LAZY_SIFT"))
       ((#.(lispify "CUDD_REORDER_EXACT" :enumvalue :keyword) "CUDD_REORDER_EXACT")))
(cenum #.(lispify "Cudd_AggregationType" :enumname)
       ((#.(lispify "CUDD_NO_CHECK" :enumvalue :keyword) "CUDD_NO_CHECK"))
       ((#.(lispify "CUDD_GROUP_CHECK" :enumvalue :keyword) "CUDD_GROUP_CHECK"))
       ((#.(lispify "CUDD_GROUP_CHECK2" :enumvalue :keyword) "CUDD_GROUP_CHECK2"))
       ((#.(lispify "CUDD_GROUP_CHECK3" :enumvalue :keyword) "CUDD_GROUP_CHECK3"))
       ((#.(lispify "CUDD_GROUP_CHECK4" :enumvalue :keyword) "CUDD_GROUP_CHECK4"))
       ((#.(lispify "CUDD_GROUP_CHECK5" :enumvalue :keyword) "CUDD_GROUP_CHECK5"))
       ((#.(lispify "CUDD_GROUP_CHECK6" :enumvalue :keyword) "CUDD_GROUP_CHECK6"))
       ((#.(lispify "CUDD_GROUP_CHECK7" :enumvalue :keyword) "CUDD_GROUP_CHECK7"))
       ((#.(lispify "CUDD_GROUP_CHECK8" :enumvalue :keyword) "CUDD_GROUP_CHECK8"))
       ((#.(lispify "CUDD_GROUP_CHECK9" :enumvalue :keyword) "CUDD_GROUP_CHECK9")))
(cenum #.(lispify "Cudd_HookType" :enumname)
       ((#.(lispify "CUDD_PRE_GC_HOOK" :enumvalue :keyword) "CUDD_PRE_GC_HOOK"))
       ((#.(lispify "CUDD_POST_GC_HOOK" :enumvalue :keyword) "CUDD_POST_GC_HOOK"))
       ((#.(lispify "CUDD_PRE_REORDERING_HOOK" :enumvalue :keyword) "CUDD_PRE_REORDERING_HOOK"))
       ((#.(lispify "CUDD_POST_REORDERING_HOOK" :enumvalue :keyword) "CUDD_POST_REORDERING_HOOK")))
(cenum #.(lispify "Cudd_ErrorType" :enumname)
       ((#.(lispify "CUDD_NO_ERROR" :enumvalue :keyword) "CUDD_NO_ERROR"))
       ((#.(lispify "CUDD_MEMORY_OUT" :enumvalue :keyword) "CUDD_MEMORY_OUT"))
       ((#.(lispify "CUDD_TOO_MANY_NODES" :enumvalue :keyword) "CUDD_TOO_MANY_NODES"))
       ((#.(lispify "CUDD_MAX_MEM_EXCEEDED" :enumvalue :keyword) "CUDD_MAX_MEM_EXCEEDED"))
       ((#.(lispify "CUDD_INVALID_ARG" :enumvalue :keyword) "CUDD_INVALID_ARG"))
       ((#.(lispify "CUDD_INTERNAL_ERROR" :enumvalue :keyword) "CUDD_INTERNAL_ERROR")))
(cenum #.(lispify "Cudd_LazyGroupType" :enumname)
       ((#.(lispify "CUDD_LAZY_NONE" :enumvalue :keyword) "CUDD_LAZY_NONE"))
       ((#.(lispify "CUDD_LAZY_SOFT_GROUP" :enumvalue :keyword) "CUDD_LAZY_SOFT_GROUP"))
       ((#.(lispify "CUDD_LAZY_HARD_GROUP" :enumvalue :keyword) "CUDD_LAZY_HARD_GROUP"))
       ((#.(lispify "CUDD_LAZY_UNGROUP" :enumvalue :keyword) "CUDD_LAZY_UNGROUP")))
(cenum #.(lispify "Cudd_VariableType" :enumname)
       ((#.(lispify "CUDD_VAR_PRIMARY_INPUT" :enumvalue :keyword) "CUDD_VAR_PRIMARY_INPUT"))
       ((#.(lispify "CUDD_VAR_PRESENT_STATE" :enumvalue :keyword) "CUDD_VAR_PRESENT_STATE"))
       ((#.(lispify "CUDD_VAR_NEXT_STATE" :enumvalue :keyword) "CUDD_VAR_NEXT_STATE")))

;; forward declarations

(cstruct dd-node "DdNode")
(cstruct dd-children "DdChildren")
(cstruct dd-gen "DdGen")
(cstruct dd-hook "DdHook")
(cstruct dd-local-cahce-item "DdLocalCacheItem")
(cstruct dd-local-cache "DdLocalCache")
(cstruct dd-hash-item "DdHashItem")
(cstruct dd-hash-table "DdHashTable")
(cstruct dd-cache "DdCache")
(cstruct dd-subtable "DdSubtable")
(cstruct dd-manager "DdManager")
(cstruct move "Move")
(cstruct index-key "IndexKey")
(cstruct dd-queue-item "DdQueueItem")
(cstruct dd-level-queue "DdLevelQueue")

;; definitions --- putting only the necessary nodes.
;; the offsets are automatically found by the groveller.

(cstruct dd-children "DdChildren"
         (t "T" :type (:pointer (:struct dd-node)))
         (e "E" :type (:pointer (:struct dd-node))))
(cstruct dd-manager "DdManager"
         (size "size" :type :int)
         (size-z "sizeZ" :type :int)
         (max-size "maxSize" :type :int)
         (max-size-z "maxSizeZ" :type :int)
         (error-code "errorCode" :type cudd-error-type))

#+enable_this_when_cffi_upstream_patch_is_approved
(cstruct dd-node "DdNode"
         (ref "ref" :type dd-half-word)
         (type "type" :type (:union
                             (value cudd-value-type)
                             (kids dd-children))))

;;; mtr interface

(include "mtr/mtrInt.h")

(cenum mtr-type
       ((#.(LISPIFY "MTR_DEFAULT" :ENUMVALUE :KEYWORD) "MTR_DEFAULT"))
       ((#.(LISPIFY "MTR_FIXED" :ENUMVALUE :KEYWORD) "MTR_FIXED")))

(bitfield mtr-flags
          ((#.(LISPIFY "MTR_DEFAULT" :ENUMVALUE :KEYWORD) "MTR_DEFAULT"))
          ((#.(LISPIFY "MTR_FIXED" :ENUMVALUE :KEYWORD) "MTR_FIXED"))
          ((#.(LISPIFY "MTR_TERMINAL" :ENUMVALUE :KEYWORD) "MTR_TERMINAL"))
          ((#.(LISPIFY "MTR_SOFT" :ENUMVALUE :KEYWORD) "MTR_SOFT"))
          ((#.(LISPIFY "MTR_NEWNODE" :ENUMVALUE :KEYWORD) "MTR_NEWNODE")))

(ctype mtr-half-word "MtrHalfWord")

(cstruct mtr-node "MtrNode"
         (flags "flags" :type mtr-flags)
         (low "low" :type mtr-half-word)
         (size "size" :type mtr-half-word)
         (index "index" :type mtr-half-word)
         (parent "parent" :type (:pointer (:struct mtr-node)))
         (child "child" :type (:pointer (:struct mtr-node)))
         (elder "elder" :type (:pointer (:struct mtr-node)))
         (younger "younger" :type (:pointer (:struct mtr-node))))

;;; cache interface

(ctype tertiary-operator "ptruint")
(ctype binary-operator "DD_CTFP")
(ctype unary-operator "DD_CTFP1")

