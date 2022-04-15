(in-package :cudd)



(when (null *manager*)
  (manager-initf *manager*)
  ;; (break "Count: ~D" (cudd-node-ref-count (cudd-read-logic-zero %mp%)))
  )

;; Do a sanity check on the Boolean constants' reference counts.
;; The comparison probably shouldn't be strict; see [README](README.md#ref-count-constants).
(with-cudd-critical-section
  (let ((1-count (cudd-node-ref-count (cudd-read-one %mp%)))
        (0-count (cudd-node-ref-count (cudd-read-logic-zero %mp%))))
    (assert* (>= 1-count 1) (1-count)
             "Assert failed: (cudd-node-ref-count (cudd-read-one %mp%)) should be at least 1, but we have ~D." 1-count)
    (assert* (>= 0-count 1) (0-count)
             "Assert failed: (cudd-node-ref-count (cudd-read-logic-zero %mp%)) should always be at least 1, but we have ~D." 0-count)))
