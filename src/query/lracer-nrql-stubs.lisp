(in-package racer)

;;;
;;;--------------------------------------
;;;  Automatically Generated nRQL Stubs  
;;;          Version: 1.9.1  
;;;--------------------------------------
;;;


(defun |store-substrate-for-current-abox| (|filename|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|store-substrate-for-current-abox|
            (mapcar #'transform-s-expr (list |filename|))
            (mapcar #'transform-s-expr nil)))))


(defun |restore-all-substrates| (|filename|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|restore-all-substrates|
            (mapcar #'transform-s-expr (list |filename|))
            (mapcar #'transform-s-expr nil)))))


(defun |restore-substrate| (|filename|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|restore-substrate|
            (mapcar #'transform-s-expr (list |filename|))
            (mapcar #'transform-s-expr nil)))))


(defun |store-all-substrates| (|filename|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|store-all-substrates|
            (mapcar #'transform-s-expr (list |filename|))
            (mapcar #'transform-s-expr nil)))))


(defun |store-substrate-for-abox|
       (|filename| |&optional| |for-abox| |type-of-substrate|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|store-substrate-for-abox|
            (mapcar #'transform-s-expr
                    (list |filename|
                          |&optional|
                          |for-abox|
                          |type-of-substrate|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-nrql-version| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|get-nrql-version|))))


(defun |del-rcc-edge1| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|del-rcc-edge1|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |del-rcc-node1| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|del-rcc-node1|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rcc-edge-label1| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rcc-edge-label1|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rcc-node-label1| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rcc-node-label1|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rcc-edge1| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rcc-edge1|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rcc-node1| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rcc-node1|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rcc-related1| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rcc-related1|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rcc-instance1| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rcc-instance1|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rcc-consistent-p| (|&optional| |abox| |type-of-substrate|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rcc-consistent-p|
            (mapcar #'transform-s-expr
                    (list |&optional| |abox| |type-of-substrate|))
            (mapcar #'transform-s-expr nil)))))


(defun |create-rcc-edge| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|create-rcc-edge|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |create-rcc-node| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|create-rcc-node|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |set-rcc-box| (|name| |&optional| |rcc-type|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|set-rcc-box|
            (mapcar #'transform-s-expr
                    (list |name| |&optional| |rcc-type|))
            (mapcar #'transform-s-expr nil)))))


(defun |set-mirror-data-box| (|name|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|set-mirror-data-box|
            (mapcar #'transform-s-expr (list |name|))
            (mapcar #'transform-s-expr nil)))))


(defun |description-implies-p| (\a \b)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|description-implies-p|
            (mapcar #'transform-s-expr (list \a \b))
            (mapcar #'transform-s-expr nil)))))


(defun |set-data-box| (|name|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|set-data-box|
            (mapcar #'transform-s-expr (list |name|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-data-edge-label|
       (|from| |to| |&key| |abox| |type-of-substrate|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-data-edge-label|
            (mapcar #'transform-s-expr
                    (list |from|
                          |to|
                          |&key|
                          |abox|
                          |type-of-substrate|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-data-node-label| (|name| |&key| |abox| |type-of-substrate|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-data-node-label|
            (mapcar #'transform-s-expr
                    (list |name| |&key| |abox| |type-of-substrate|))
            (mapcar #'transform-s-expr nil)))))


(defun |delete-data-edge|
       (|from| |to| |&key| |abox| |type-of-substrate| |told-info-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|delete-data-edge|
            (mapcar #'transform-s-expr
                    (list |from|
                          |to|
                          |&key|
                          |abox|
                          |type-of-substrate|
                          |told-info-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |delete-data-node|
       (|name| |&key| |abox| |type-of-substrate| |told-info-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|delete-data-node|
            (mapcar #'transform-s-expr
                    (list |name|
                          |&key|
                          |abox|
                          |type-of-substrate|
                          |told-info-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |create-data-edge|
       (|from| |to| |descr| |&key| |abox| |type-of-substrate|
        |racer-descr| |told-info-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|create-data-edge|
            (mapcar #'transform-s-expr
                    (list |from|
                          |to|
                          |descr|
                          |&key|
                          |abox|
                          |type-of-substrate|
                          |racer-descr|
                          |told-info-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |create-data-node|
       (|name| |&key| |abox| |type-of-substrate| |racer-descr| |descr|
        |told-info-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|create-data-node|
            (mapcar #'transform-s-expr
                    (list |name|
                          |&key|
                          |abox|
                          |type-of-substrate|
                          |racer-descr|
                          |descr|
                          |told-info-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-process-pool-size| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|get-process-pool-size|))))


(defun |get-maximum-size-of-process-pool| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|get-maximum-size-of-process-pool|))))


(defun |get-initial-size-of-process-pool| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|get-initial-size-of-process-pool|))))


(defun |set-maximum-size-of-process-pool| (\n)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|set-maximum-size-of-process-pool|
            (mapcar #'transform-s-expr (list \n))
            (mapcar #'transform-s-expr nil)))))


(defun |set-initial-size-of-process-pool| (\n)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|set-initial-size-of-process-pool|
            (mapcar #'transform-s-expr (list \n))
            (mapcar #'transform-s-expr nil)))))


(defun |set-rewrite-defined-concepts| (|val|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|set-rewrite-defined-concepts|
            (mapcar #'transform-s-expr (list |val|))
            (mapcar #'transform-s-expr nil)))))


(defun |set-nrql-mode| (|mode|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|set-nrql-mode|
            (mapcar #'transform-s-expr (list |mode|))
            (mapcar #'transform-s-expr nil)))))


(defun |show-current-qbox| (|&optional| |definitions-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|show-current-qbox|
            (mapcar #'transform-s-expr
                    (list |&optional| |definitions-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-abox-of-current-qbox| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|get-abox-of-current-qbox|))))


(defun |disable-query-realization| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|disable-query-realization|))))


(defun |enable-query-realization| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|enable-query-realization|))))


(defun |optimizer-dont-use-cardinality-heuristics| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|optimizer-dont-use-cardinality-heuristics|))))


(defun |optimizer-use-cardinality-heuristics| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|optimizer-use-cardinality-heuristics|))))


(defun |disable-query-optimization| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|disable-query-optimization|))))


(defun |enable-query-optimization| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|enable-query-optimization|))))


(defun |disable-query-repository| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|disable-query-repository|))))


(defun |enable-query-repository| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|enable-query-repository|))))


(defun |dont-report-tautological-queries| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|dont-report-tautological-queries|))))


(defun |report-tautological-queries| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|report-tautological-queries|))))


(defun |dont-report-inconsistent-queries| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|dont-report-inconsistent-queries|))))


(defun |report-inconsistent-queries| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|report-inconsistent-queries|))))


(defun |describe-query-processing-mode| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|describe-query-processing-mode|))))


(defun |describe-current-substrate| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|describe-current-substrate|))))


(defun |include-permutations| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|include-permutations|))))


(defun |exclude-permutations| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|exclude-permutations|))))


(defun |dont-add-rule-consequences-automatically| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|dont-add-rule-consequences-automatically|))))


(defun |add-rule-consequences-automatically| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|add-rule-consequences-automatically|))))


(defun |process-set-at-a-time| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|process-set-at-a-time|))))


(defun |process-tuple-at-a-time| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|process-tuple-at-a-time|))))


(defun |get-max-no-of-tuples-bound| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|get-max-no-of-tuples-bound|))))


(defun |set-max-no-of-tuples-bound| (|&optional| \n)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|set-max-no-of-tuples-bound|
            (mapcar #'transform-s-expr (list |&optional| \n))
            (mapcar #'transform-s-expr nil)))))


(defun |dont-check-abox-consistency-before-querying| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A)"
            '|dont-check-abox-consistency-before-querying|))))


(defun |check-abox-consistency-before-querying| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|check-abox-consistency-before-querying|))))


(defun |enable-lazy-tuple-computation| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-lazy-tuple-computation|))))


(defun |enable-eager-tuple-computation| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-eager-tuple-computation|))))


(defun |restore-standard-settings| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|restore-standard-settings|))))


(defun |dont-add-role-assertions-for-datatype-properties| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A)"
            '|dont-add-role-assertions-for-datatype-properties|))))


(defun |add-role-assertions-for-datatype-properties| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A)"
            '|add-role-assertions-for-datatype-properties|))))


(defun |disable-told-information-querying| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|disable-told-information-querying|))))


(defun |enable-told-information-querying| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-told-information-querying|))))


(defun |disable-nrql-warnings| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|disable-nrql-warnings|))))


(defun |enable-nrql-warnings| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|enable-nrql-warnings|))))


(defun |disable-kb-has-changed-warning-tokens| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|disable-kb-has-changed-warning-tokens|))))


(defun |enable-kb-has-changed-warning-tokens| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-kb-has-changed-warning-tokens|))))


(defun |disable-phase-two-starts-warning-tokens| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|disable-phase-two-starts-warning-tokens|))))


(defun |enable-phase-two-starts-warning-tokens| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-phase-two-starts-warning-tokens|))))


(defun |disable-two-phase-query-processing-mode| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|disable-two-phase-query-processing-mode|))))


(defun |enable-two-phase-query-processing-mode| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-two-phase-query-processing-mode|))))


(defun |disable-abox-mirroring| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|disable-abox-mirroring|))))


(defun |enable-very-smart-abox-mirroring| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-very-smart-abox-mirroring|))))


(defun |enable-smart-abox-mirroring| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-smart-abox-mirroring|))))


(defun |enable-abox-mirroring| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|enable-abox-mirroring|))))


(defun |disable-sql-data-substrate-mirroring| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|disable-sql-data-substrate-mirroring|))))


(defun |enable-sql-data-substrate-mirroring| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-sql-data-substrate-mirroring|))))


(defun |disable-data-substrate-mirroring| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|disable-data-substrate-mirroring|))))


(defun |enable-data-substrate-mirroring| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|enable-data-substrate-mirroring|))))


(defun |wait-for-rules-to-terminate| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|wait-for-rules-to-terminate|))))


(defun |wait-for-queries-to-terminate| ()
  (with-standard-io-syntax-1
   (service-request
    (format nil "(~A)" '|wait-for-queries-to-terminate|))))


(defun |describe-all-rules| (|&optional| |rewritten-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|describe-all-rules|
            (mapcar #'transform-s-expr
                    (list |&optional| |rewritten-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |describe-all-queries| (|&optional| |rewritten-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|describe-all-queries|
            (mapcar #'transform-s-expr
                    (list |&optional| |rewritten-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-all-answers| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|get-all-answers|))))


(defun |get-answer-size| (|query| |&optional| |execute-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-answer-size|
            (mapcar #'transform-s-expr
                    (list |query| |&optional| |execute-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |run-all-rules| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|run-all-rules|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |reexecute-all-rules| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|reexecute-all-rules|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |execute-all-rules| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|execute-all-rules|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |run-all-queries| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|run-all-queries|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |reexecute-all-queries| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|reexecute-all-queries|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |execute-all-queries| (|&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|execute-all-queries|
            (mapcar #'transform-s-expr (list |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |abort-all-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|abort-all-rules|))))


(defun |abort-all-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|abort-all-queries|))))


(defun |terminated-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|terminated-rules|))))


(defun |inactive-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|inactive-rules|))))


(defun |processed-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|processed-rules|))))


(defun |terminated-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|terminated-queries|))))


(defun |inactive-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|inactive-queries|))))


(defun |processed-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|processed-queries|))))


(defun |waiting-expensive-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|waiting-expensive-rules|))))


(defun |waiting-cheap-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|waiting-cheap-rules|))))


(defun |waiting-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|waiting-rules|))))


(defun |waiting-expensive-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|waiting-expensive-queries|))))


(defun |waiting-cheap-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|waiting-cheap-queries|))))


(defun |waiting-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|waiting-queries|))))


(defun |running-expensive-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|running-expensive-rules|))))


(defun |running-cheap-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|running-cheap-rules|))))


(defun |running-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|running-rules|))))


(defun |running-expensive-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|running-expensive-queries|))))


(defun |running-cheap-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|running-cheap-queries|))))


(defun |running-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|running-queries|))))


(defun |active-expensive-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|active-expensive-rules|))))


(defun |active-cheap-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|active-cheap-rules|))))


(defun |active-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|active-rules|))))


(defun |active-expensive-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|active-expensive-queries|))))


(defun |active-cheap-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|active-cheap-queries|))))


(defun |active-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|active-queries|))))


(defun |prepared-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|prepared-rules|))))


(defun |ready-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|ready-rules|))))


(defun |prepared-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|prepared-queries|))))


(defun |ready-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|ready-queries|))))


(defun |expensive-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|expensive-rules|))))


(defun |cheap-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|cheap-rules|))))


(defun |inaccurate-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|inaccurate-rules|))))


(defun |accurate-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|accurate-rules|))))


(defun |all-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|all-rules|))))


(defun |expensive-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|expensive-queries|))))


(defun |cheap-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|cheap-queries|))))


(defun |inaccurate-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|inaccurate-queries|))))


(defun |accurate-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|accurate-queries|))))


(defun |all-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|all-queries|))))


(defun |describe-all-definitions| (|&key| |tbox|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|describe-all-definitions|
            (mapcar #'transform-s-expr (list |&key| |tbox|))
            (mapcar #'transform-s-expr nil)))))


(defun |describe-definition| (|name| |&key| |tbox|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|describe-definition|
            (mapcar #'transform-s-expr (list |name| |&key| |tbox|))
            (mapcar #'transform-s-expr nil)))))


(defun |delete-all-definitions| (|&key| |tbox|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|delete-all-definitions|
            (mapcar #'transform-s-expr (list |&key| |tbox|))
            (mapcar #'transform-s-expr nil)))))


(defun |undefine-query| (|name| |&key| |tbox|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|undefine-query|
            (mapcar #'transform-s-expr (list |name| |&key| |tbox|))
            (mapcar #'transform-s-expr nil)))))


(defun |define-and-prepare-query| (|name| |head| |body| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|define-and-prepare-query|
            (mapcar #'transform-s-expr
                    (list |name| |head| |body| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |define-and-execute-query| (|name| |head| |body| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|define-and-execute-query|
            (mapcar #'transform-s-expr
                    (list |name| |head| |body| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |define-query|
       (|name| |head| |body| |&rest| |args| |&key| |keep-p| |tbox|
        |&allow-other-keys|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|define-query|
            (mapcar #'transform-s-expr
                    (list |name|
                          |head|
                          |body|
                          |&rest|
                          |args|
                          |&key|
                          |keep-p|
                          |tbox|
                          |&allow-other-keys|))
            (mapcar #'transform-s-expr nil)))))


(defun |racer-prepare-tbox-query| (|res-args| |query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|racer-prepare-tbox-query|
            (mapcar #'transform-s-expr
                    (list |res-args| |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |racer-answer-tbox-query| (|res-args| |query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|racer-answer-tbox-query|
            (mapcar #'transform-s-expr
                    (list |res-args| |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |racer-prepare-rule| (|query| |res-args| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|racer-prepare-rule|
            (mapcar #'transform-s-expr
                    (list |query| |res-args| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |racer-apply-rule| (|query| |res-args| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|racer-apply-rule|
            (mapcar #'transform-s-expr
                    (list |query| |res-args| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |prepare-nrql-engine| (|abox| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|prepare-nrql-engine|
            (mapcar #'transform-s-expr (list |abox| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |racer-prepare-query| (|res-args| |query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|racer-prepare-query|
            (mapcar #'transform-s-expr
                    (list |res-args| |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |racer-answer-query-under-premise|
       (|premise| |res-args| |query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|racer-answer-query-under-premise|
            (mapcar #'transform-s-expr
                    (list |premise| |res-args| |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |racer-answer-query| (|res-args| |query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|racer-answer-query|
            (mapcar #'transform-s-expr
                    (list |res-args| |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |full-reset| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|full-reset|))))


(defun |reset-nrql-engine| (|&key| |full-reset-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|reset-nrql-engine|
            (mapcar #'transform-s-expr (list |&key| |full-reset-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |reset-all-substrates| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|reset-all-substrates|))))


(defun |delete-all-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|delete-all-rules|))))


(defun |delete-all-queries| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|delete-all-queries|))))


(defun |describe-rule| (|query| |&optional| |rewritten-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|describe-rule|
            (mapcar #'transform-s-expr
                    (list |query| |&optional| |rewritten-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |describe-query| (|query| |&optional| |rewritten-p|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|describe-query|
            (mapcar #'transform-s-expr
                    (list |query| |&optional| |rewritten-p|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-dag-of-qbox-for-abox|
       (|substrate| |&optional| |type-of-substrate|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-dag-of-qbox-for-abox|
            (mapcar #'transform-s-expr
                    (list |substrate| |&optional| |type-of-substrate|))
            (mapcar #'transform-s-expr nil)))))


(defun |show-qbox-for-abox|
       (|abox| |&optional| |definitions-p| |type-of-substrate|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|show-qbox-for-abox|
            (mapcar #'transform-s-expr
                    (list |abox|
                          |&optional|
                          |definitions-p|
                          |type-of-substrate|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-nodes-in-qbox-for-abox|
       (|abox| |&optional| |type-of-substrate|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-nodes-in-qbox-for-abox|
            (mapcar #'transform-s-expr
                    (list |abox| |&optional| |type-of-substrate|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-nodes-in-current-qbox| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|get-nodes-in-current-qbox|))))


(defun |get-dag-of-current-qbox| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|get-dag-of-current-qbox|))))


(defun |query-equivalents| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-equivalents|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-descendants| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-descendants|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-children| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-children|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-ancestors| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-ancestors|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-parents| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-parents|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-equivalent-p|
       (\a \b |&rest| |args| |&key| |&allow-other-keys|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-equivalent-p|
            (mapcar #'transform-s-expr
                    (list \a
                          \b
                          |&rest|
                          |args|
                          |&key|
                          |&allow-other-keys|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-entails-p|
       (\a \b |&rest| |args| |&key| |&allow-other-keys|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-entails-p|
            (mapcar #'transform-s-expr
                    (list \a
                          \b
                          |&rest|
                          |args|
                          |&key|
                          |&allow-other-keys|))
            (mapcar #'transform-s-expr nil)))))


(defun |classify-query| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|classify-query|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-tautological-p| (|query| |&key| |&allow-other-keys|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-tautological-p|
            (mapcar #'transform-s-expr
                    (list |query| |&key| |&allow-other-keys|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-inconsistent-p| (|query| |&key| |&allow-other-keys|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-inconsistent-p|
            (mapcar #'transform-s-expr
                    (list |query| |&key| |&allow-other-keys|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-consistent-p| (|query| |&key| |&allow-other-keys|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-consistent-p|
            (mapcar #'transform-s-expr
                    (list |query| |&key| |&allow-other-keys|))
            (mapcar #'transform-s-expr nil)))))


(defun |abort-rule| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|abort-rule|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |abort-query| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|abort-query|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |original-rule-body| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|original-rule-body|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |original-query-body| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|original-query-body|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-body| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-body|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-body| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-body|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |original-rule-head| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|original-rule-head|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |original-query-head| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|original-query-head|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-head| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-head|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-head| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-head|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-accurate-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-accurate-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-accurate-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-accurate-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-answer| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-answer|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-all-remaining-sets-of-rule-consequences| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-all-remaining-sets-of-rule-consequences|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-all-remaining-tuples| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-all-remaining-tuples|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-next-n-remaining-sets-of-rule-consequences|
       (|query| |&optional| \n)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-next-n-remaining-sets-of-rule-consequences|
            (mapcar #'transform-s-expr (list |query| |&optional| \n))
            (mapcar #'transform-s-expr nil)))))


(defun |get-next-n-remaining-tuples| (|query| |&optional| \n)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-next-n-remaining-tuples|
            (mapcar #'transform-s-expr (list |query| |&optional| \n))
            (mapcar #'transform-s-expr nil)))))


(defun |describe-rule-status| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|describe-rule-status|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |describe-query-status| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|describe-query-status|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-inactive-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-inactive-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-processed-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-processed-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-inactive-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-inactive-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-processed-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-processed-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |active-expensive-rule-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|active-expensive-rule-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |active-expensive-query-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|active-expensive-query-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |cheap-rule-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|cheap-rule-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |cheap-query-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|cheap-query-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-active-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-active-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-active-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-active-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-waiting-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-waiting-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-waiting-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-waiting-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |execute-applicable-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|execute-applicable-rules|))))


(defun |unapplicable-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|unapplicable-rules|))))


(defun |applicable-rules| ()
  (with-standard-io-syntax-1
   (service-request (format nil "(~A)" '|applicable-rules|))))


(defun |add-chosen-sets-of-rule-consequences| (|query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|add-chosen-sets-of-rule-consequences|
            (mapcar #'transform-s-expr (list |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |choose-current-set-of-rule-consequences|
       (|query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|choose-current-set-of-rule-consequences|
            (mapcar #'transform-s-expr (list |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-applicable-p| (|query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-applicable-p|
            (mapcar #'transform-s-expr (list |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-prepared-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-prepared-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-prepared-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-prepared-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |reexecute-rule| (|query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|reexecute-rule|
            (mapcar #'transform-s-expr (list |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |reexecute-query| (|query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|reexecute-query|
            (mapcar #'transform-s-expr (list |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |reprepare-rule| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|reprepare-rule|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |reprepare-query| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|reprepare-query|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |execute-rule| (|query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|execute-rule|
            (mapcar #'transform-s-expr (list |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |execute-query| (|query| |&rest| |args|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|execute-query|
            (mapcar #'transform-s-expr (list |query| |&rest| |args|))
            (mapcar #'transform-s-expr nil)))))


(defun |rule-ready-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|rule-ready-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |query-ready-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|query-ready-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |next-set-of-rule-consequences-available-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|next-set-of-rule-consequences-available-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |next-tuple-available-p| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|next-tuple-available-p|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-current-set-of-rule-consequences| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-current-set-of-rule-consequences|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-next-set-of-rule-consequences| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-next-set-of-rule-consequences|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-current-tuple| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-current-tuple|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |get-next-tuple| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|get-next-tuple|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |delete-rule| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|delete-rule|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defun |delete-query| (|query|)
  (with-standard-io-syntax-1
   (service-request
    (format nil
            "(~A~{ ~S~}~{ ~S~})"
            '|delete-query|
            (mapcar #'transform-s-expr (list |query|))
            (mapcar #'transform-s-expr nil)))))


(defmacro |del-rcc-edge| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|del-rcc-edge|
              (mapcar #'transform-s-expr args)))))


(defmacro |del-rcc-node| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|del-rcc-node|
              (mapcar #'transform-s-expr args)))))


(defmacro |rcc-edge-label| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|rcc-edge-label|
              (mapcar #'transform-s-expr args)))))


(defmacro |rcc-node-label| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|rcc-node-label|
              (mapcar #'transform-s-expr args)))))


(defmacro |rcc-edge| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|rcc-edge|
              (mapcar #'transform-s-expr args)))))


(defmacro |rcc-node| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|rcc-node|
              (mapcar #'transform-s-expr args)))))


(defmacro |rcc-related| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|rcc-related|
              (mapcar #'transform-s-expr args)))))


(defmacro |rcc-instance| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|rcc-instance|
              (mapcar #'transform-s-expr args)))))


(defmacro |rcc-consistent?| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|rcc-consistent?|
              (mapcar #'transform-s-expr args)))))


(defmacro |in-rcc-box| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|in-rcc-box|
              (mapcar #'transform-s-expr args)))))


(defmacro |in-mirror-data-box| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|in-mirror-data-box|
              (mapcar #'transform-s-expr args)))))


(defmacro |description-implies?| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|description-implies?|
              (mapcar #'transform-s-expr args)))))


(defmacro |edge-label| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|edge-label|
              (mapcar #'transform-s-expr args)))))


(defmacro |node-label| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|node-label|
              (mapcar #'transform-s-expr args)))))


(defmacro |del-data-edge| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|del-data-edge|
              (mapcar #'transform-s-expr args)))))


(defmacro |del-data-node| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|del-data-node|
              (mapcar #'transform-s-expr args)))))


(defmacro |data-edge| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|data-edge|
              (mapcar #'transform-s-expr args)))))


(defmacro |data-node| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|data-node|
              (mapcar #'transform-s-expr args)))))


(defmacro |in-data-box| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|in-data-box|
              (mapcar #'transform-s-expr args)))))


(defmacro |undefquery| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|undefquery|
              (mapcar #'transform-s-expr args)))))


(defmacro |def-and-exec-query| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|def-and-exec-query|
              (mapcar #'transform-s-expr args)))))


(defmacro |def-and-prep-query| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|def-and-prep-query|
              (mapcar #'transform-s-expr args)))))


(defmacro |defquery| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|defquery|
              (mapcar #'transform-s-expr args)))))


(defmacro |preprule| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|preprule|
              (mapcar #'transform-s-expr args)))))


(defmacro |firerule| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|firerule|
              (mapcar #'transform-s-expr args)))))


(defmacro |prepare-abox-rule| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|prepare-abox-rule|
              (mapcar #'transform-s-expr args)))))


(defmacro |prepare-tbox-query| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|prepare-tbox-query|
              (mapcar #'transform-s-expr args)))))


(defmacro |prepare-abox-query| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|prepare-abox-query|
              (mapcar #'transform-s-expr args)))))


(defmacro |apply-abox-rule| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|apply-abox-rule|
              (mapcar #'transform-s-expr args)))))


(defmacro |tbox-retrieve| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|tbox-retrieve|
              (mapcar #'transform-s-expr args)))))


(defmacro |retrieve-under-premise| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|retrieve-under-premise|
              (mapcar #'transform-s-expr args)))))


(defmacro |retrieve| (&rest args)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A~{ ~S~})"
              '|retrieve|
              (mapcar #'transform-s-expr args)))))


(defmacro |with-nrql-settings| ((&rest args) &body body)
  (with-standard-io-syntax-1
   `(service-request
     ,(format nil
              "(~A (~{ ~S~})~{ ~S~})"
              '|with-nrql-settings|
              (transform-s-expr args)
              (transform-s-expr body)))))

