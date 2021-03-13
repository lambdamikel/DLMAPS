(in-package cl-user)

;;;
;;;--------------------------------------------
;;;  Automatically Generated nRQL Server Case  
;;;          Version: 1.9.1  
;;;--------------------------------------------
;;;


(defun process-nrql-request (expr stream n state output-string-stream)
  (case (first expr)
    ((xml-output)
     (process-racer-expr (second expr)
                         nil
                         n
                         state
                         output-string-stream)
     (let ((expr2
            (if *last-error*
                (lisp-to-xml (format nil "~a" *last-error*)
                             stream
                             :newlines-p
                             nil
                             :ascii-p
                             nil
                             :indentation-p
                             nil
                             :top-level-attributes
                             (format nil "id=\"~d\" type=\"error\"" n))
              (lisp-to-xml (list (format nil "~s" *last-answer*)
                                 *last-answer*)
                           stream
                           :newlines-p
                           nil
                           :ascii-p
                           t
                           :indentation-p
                           nil
                           :top-level-attributes
                           (format nil
                                   "id=\"~d\" type=\"answer\""
                                   n)))))
       (answer expr state stream n expr2 output-string-stream)))
    ((xml-native-output)
     (process-racer-expr (second expr)
                         nil
                         n
                         state
                         output-string-stream)
     (let ((expr2
            (if *last-error*
                (lisp-to-xml (format nil "~a" *last-error*)
                             stream
                             :newlines-p
                             nil
                             :ascii-p
                             nil
                             :indentation-p
                             nil
                             :top-level-attributes
                             (format nil "id=\"~d\" type=\"error\"" n))
              (lisp-to-xml (format nil "~s" *last-answer*)
                           stream
                           :newlines-p
                           nil
                           :ascii-p
                           t
                           :indentation-p
                           nil
                           :top-level-attributes
                           (format nil
                                   "id=\"~d\" type=\"answer\""
                                   n)))))
       (answer expr state stream n expr2 output-string-stream)))
    ((del-rcc-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-rcc-edge1) (rest expr)))
               output-string-stream)))
    ((del-rcc-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-rcc-node1) (rest expr)))
               output-string-stream)))
    ((rcc-edge-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-edge-label1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-node-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-node-label1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-edge1) (rest expr)))
               output-string-stream)))
    ((rcc-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-node1) (rest expr)))
               output-string-stream)))
    ((rcc-related)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-related1) (rest expr)))
               output-string-stream)))
    ((rcc-instance)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-instance1) (rest expr)))
               output-string-stream)))
    ((rcc-consistent?)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-consistent-p)
                        (rest expr)))
               output-string-stream)))
    ((in-rcc-box)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'set-rcc-box) (rest expr)))
               output-string-stream)))
    ((in-mirror-data-box)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'set-mirror-data-box)
                        (rest expr)))
               output-string-stream)))
    ((description-implies?)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'thematic-substrate::implies-p)
                        (rest expr)))
               output-string-stream)))
    ((edge-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'thematic-substrate::edge-label1)
                        (rest expr)))
               output-string-stream)))
    ((node-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'thematic-substrate::node-label1)
                        (rest expr)))
               output-string-stream)))
    ((del-data-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'thematic-substrate::del-data-edge1)
                        (rest expr)))
               output-string-stream)))
    ((del-data-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'thematic-substrate::del-data-node1)
                        (rest expr)))
               output-string-stream)))
    ((data-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'thematic-substrate::data-edge1)
                        (rest expr)))
               output-string-stream)))
    ((data-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'thematic-substrate::data-node1)
                        (rest expr)))
               output-string-stream)))
    ((in-data-box)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'set-data-box) (rest expr)))
               output-string-stream)))
    ((undefquery)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'undefine-query) (rest expr)))
               output-string-stream)))
    ((def-and-exec-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define-and-execute-query)
                        (rest expr)))
               output-string-stream)))
    ((def-and-prep-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define-and-prepare-query)
                        (rest expr)))
               output-string-stream)))
    ((defquery)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define-query) (rest expr)))
               output-string-stream)))
    ((preprule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule)
                        (rest expr)))
               output-string-stream)))
    ((firerule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-rule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule)
                        (rest expr)))
               output-string-stream)))
    ((prepare-tbox-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-tbox-query)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-query)
                        (rest expr)))
               output-string-stream)))
    ((apply-abox-rule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule)
                        (rest expr)))
               output-string-stream)))
    ((tbox-retrieve)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-tbox-query)
                        (rest expr)))
               output-string-stream)))
    ((retrieve-under-premise)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query-under-premise)
                        (rest expr)))
               output-string-stream)))
    ((retrieve)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query)
                        (rest expr)))
               output-string-stream)))
    ((with-nrql-settings)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-nrql-settings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((describe-rule describe-query
                    get-dag-of-qbox-for-abox
                    show-qbox-for-abox
                    get-nodes-in-qbox-for-abox
                    get-nodes-in-current-qbox
                    get-dag-of-current-qbox
                    query-equivalents
                    query-descendants
                    query-children
                    query-ancestors
                    query-parents
                    query-equivalent-p
                    query-entails-p
                    classify-query
                    query-tautological-p
                    query-inconsistent-p
                    query-consistent-p
                    abort-rule
                    abort-query
                    original-rule-body
                    original-query-body
                    rule-body
                    query-body
                    original-rule-head
                    original-query-head
                    rule-head
                    query-head
                    rule-accurate-p
                    query-accurate-p
                    get-answer
                    get-all-remaining-sets-of-rule-consequences
                    get-all-remaining-tuples
                    get-next-n-remaining-sets-of-rule-consequences
                    get-next-n-remaining-tuples
                    describe-rule-status
                    describe-query-status
                    rule-inactive-p
                    rule-processed-p
                    query-inactive-p
                    query-processed-p
                    active-expensive-rule-p
                    active-expensive-query-p
                    cheap-rule-p
                    cheap-query-p
                    rule-active-p
                    query-active-p
                    rule-waiting-p
                    query-waiting-p
                    execute-applicable-rules
                    unapplicable-rules
                    applicable-rules
                    add-chosen-sets-of-rule-consequences
                    choose-current-set-of-rule-consequences
                    rule-applicable-p
                    rule-prepared-p
                    query-prepared-p
                    reexecute-rule
                    reexecute-query
                    reprepare-rule
                    reprepare-query
                    execute-rule
                    execute-query
                    rule-ready-p
                    query-ready-p
                    next-set-of-rule-consequences-available-p
                    next-tuple-available-p
                    get-current-set-of-rule-consequences
                    get-next-set-of-rule-consequences
                    get-current-tuple
                    get-next-tuple
                    delete-rule
                    delete-query
                    store-substrate-for-current-abox
                    restore-all-substrates
                    restore-substrate
                    store-all-substrates
                    store-substrate-for-abox
                    get-nrql-version
                    del-rcc-edge1
                    del-rcc-node1
                    rcc-edge-label1
                    rcc-node-label1
                    rcc-edge1
                    rcc-node1
                    rcc-related1
                    rcc-instance1
                    rcc-consistent-p
                    create-rcc-edge
                    create-rcc-node
                    set-rcc-box
                    set-mirror-data-box
                    description-implies-p
                    set-data-box
                    get-data-edge-label
                    get-data-node-label
                    delete-data-edge
                    delete-data-node
                    create-data-edge
                    create-data-node
                    get-process-pool-size
                    get-maximum-size-of-process-pool
                    get-initial-size-of-process-pool
                    set-maximum-size-of-process-pool
                    set-initial-size-of-process-pool
                    set-rewrite-defined-concepts
                    set-nrql-mode
                    show-current-qbox
                    get-abox-of-current-qbox
                    disable-query-realization
                    enable-query-realization
                    optimizer-dont-use-cardinality-heuristics
                    optimizer-use-cardinality-heuristics
                    disable-query-optimization
                    enable-query-optimization
                    disable-query-repository
                    enable-query-repository
                    dont-report-tautological-queries
                    report-tautological-queries
                    dont-report-inconsistent-queries
                    report-inconsistent-queries
                    describe-query-processing-mode
                    describe-current-substrate
                    include-permutations
                    exclude-permutations
                    dont-add-rule-consequences-automatically
                    add-rule-consequences-automatically
                    process-set-at-a-time
                    process-tuple-at-a-time
                    get-max-no-of-tuples-bound
                    set-max-no-of-tuples-bound
                    dont-check-abox-consistency-before-querying
                    check-abox-consistency-before-querying
                    enable-lazy-tuple-computation
                    enable-eager-tuple-computation
                    restore-standard-settings
                    dont-add-role-assertions-for-datatype-properties
                    add-role-assertions-for-datatype-properties
                    disable-told-information-querying
                    enable-told-information-querying
                    disable-nrql-warnings
                    enable-nrql-warnings
                    disable-kb-has-changed-warning-tokens
                    enable-kb-has-changed-warning-tokens
                    disable-phase-two-starts-warning-tokens
                    enable-phase-two-starts-warning-tokens
                    disable-two-phase-query-processing-mode
                    enable-two-phase-query-processing-mode
                    disable-abox-mirroring
                    enable-very-smart-abox-mirroring
                    enable-smart-abox-mirroring
                    enable-abox-mirroring
                    disable-sql-data-substrate-mirroring
                    enable-sql-data-substrate-mirroring
                    disable-data-substrate-mirroring
                    enable-data-substrate-mirroring
                    wait-for-rules-to-terminate
                    wait-for-queries-to-terminate
                    describe-all-rules
                    describe-all-queries
                    get-all-answers
                    get-answer-size
                    run-all-rules
                    reexecute-all-rules
                    execute-all-rules
                    run-all-queries
                    reexecute-all-queries
                    execute-all-queries
                    abort-all-rules
                    abort-all-queries
                    terminated-rules
                    inactive-rules
                    processed-rules
                    terminated-queries
                    inactive-queries
                    processed-queries
                    waiting-expensive-rules
                    waiting-cheap-rules
                    waiting-rules
                    waiting-expensive-queries
                    waiting-cheap-queries
                    waiting-queries
                    running-expensive-rules
                    running-cheap-rules
                    running-rules
                    running-expensive-queries
                    running-cheap-queries
                    running-queries
                    active-expensive-rules
                    active-cheap-rules
                    active-rules
                    active-expensive-queries
                    active-cheap-queries
                    active-queries
                    prepared-rules
                    ready-rules
                    prepared-queries
                    ready-queries
                    expensive-rules
                    cheap-rules
                    inaccurate-rules
                    accurate-rules
                    all-rules
                    expensive-queries
                    cheap-queries
                    inaccurate-queries
                    accurate-queries
                    all-queries
                    describe-all-definitions
                    describe-definition
                    delete-all-definitions
                    undefine-query
                    define-and-prepare-query
                    define-and-execute-query
                    define-query
                    racer-prepare-tbox-query
                    racer-answer-tbox-query
                    racer-prepare-rule
                    racer-apply-rule
                    prepare-nrql-engine
                    racer-prepare-query
                    racer-answer-query-under-premise
                    racer-answer-query
                    full-reset
                    reset-nrql-engine
                    reset-all-substrates
                    delete-all-rules
                    delete-all-queries)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function (first expr)) (rest expr)))
               output-string-stream)))
    (otherwise (error "Illegal operator in ~A" expr))))