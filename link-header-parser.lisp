(in-package #:org.shirakumo.tooter.link-header-parser)

(a:define-constant +tokens+ "(,)|(;)|(=)|(<[^<>;\\s]+>)|(\"[^\"]+\")|([^\\s=;,]+)|(\\s*)"
  :test #'string=)
;;                            0   1   2        3             4            5         6

(defun next-token (data)
  (multiple-value-bind (matched registers)
      (re:scan-to-strings +tokens+ data)
    (when matched
      (let ((position-matched (position-if-not #'null registers)))
        (values
         (ecase position-matched
           (0 (list :record-separator matched))
           (1 (list :field-separator matched))
           (2 (list :parameter-separator matched))
           (3 (list :url matched))
           (4 (list :parameter-value matched))
           (5 (list :string matched))
           (6 (list :blanks matched)))
         (subseq data (length matched)))))))

;; link-record      := url blanks (field-sep blanks parameter)? | link-record
;; parameter        := single-parameter | (single-parameter blanks field-sep parameter)
;; single-parameter := parameter-key blanks parameter-separator blanks parameter-value
;; url              := "<[^<>;\\s]+>"
;; blanks           := "\\s*"
;; parameter-key    := string
;; paramerer-value  := "\"[^\"]+\"" | string
;; string           := "[^\\s=;,]+"

(defun token= (token token-symbol)
  (eq (first token) token-symbol))

(defun token-value (token)
  (second token))

(defstruct link-header
  (records))

(defstruct link-record
  (url)
  (parameters))

(defun parse (data)
  (let ((results (make-link-header)))
    (link-record data results)
    (link-header-records results)))

(defun signal-error (expected-token data)
  (error "expected ~a at the start of ~s" expected-token data))

(defun consume-blanks (data)
  (multiple-value-bind (token rest-data)
      (next-token data)
    (if (and token
             (token= token :blanks))
        rest-data
        data)))

(defun field-separator (data)
  (multiple-value-bind (token rest-data)
      (next-token data)
    (if (and token
             (token= token :field-separator))
        (values t rest-data)
        nil)))

(defun record-separator (data)
  (multiple-value-bind (token rest-data)
      (next-token data)
    (if (and token
             (token= token :record-separator))
        (values t rest-data)
        nil)))

(defun link-record (data results)
  (let* ((record-results (make-link-record))
         (url-rest-data  (url data record-results))
         (no-blanks-rest-data (consume-blanks url-rest-data)))
    (multiple-value-bind (field-separator-found field-separator-rest-data)
        (field-separator no-blanks-rest-data)
      (let ((next-record no-blanks-rest-data))
        (if field-separator-found
          (let* ((no-blanks-rest-data  (consume-blanks field-separator-rest-data))
                 (parameters-rest-data (parameters no-blanks-rest-data record-results)))
            (push record-results (link-header-records results))
            (setf next-record parameters-rest-data))
          (push record-results (link-header-records results)))
        (let* ((rest-after-blanks (consume-blanks next-record)))
          (multiple-value-bind (record-separator-found record-separator-rest-data)
              (record-separator rest-after-blanks)
            (if record-separator-found
                (let ((new-records (consume-blanks record-separator-rest-data)))
                  (link-record new-records results))
                url-rest-data)))))))

(a:define-constant +url-wrapper-chars+ '(#\< #\>) :test #'equalp)

(defun url (data record-results)
  (multiple-value-bind (token no-url-data)
      (next-token data)
    (cond
      ((not (and token
                  (token= token :url)))
       (signal-error :url no-url-data))
      (t
       (setf (link-record-url record-results)
             (string-trim +url-wrapper-chars+ (token-value token)))
       no-url-data))))

(defun consume-parameter-separator (data)
  (multiple-value-bind (token rest-data)
      (next-token data)
    (if (and token
             (token= token :parameter-separator))
        rest-data
        (signal-error :parameter-separator data))))

(a:define-constant +parameter-value-wrapper-char+ '(#\") :test #'equalp)

(defun parameters (data parameters-results)
  (multiple-value-bind (token rest-data)
      (next-token data)
    (let ((key   nil)
          (value nil))
      (cond
        ((not (and token
                   (token= token :string)))
         (signal-error :string data))
        (t
         (setf key (token-value token))
         (let* ((no-blanks-rest-data        (consume-blanks rest-data))
                (no-parameter-sep-rest-data (consume-parameter-separator no-blanks-rest-data))
                (no-blanks-after-rest-data  (consume-blanks no-parameter-sep-rest-data)))
           (multiple-value-bind (parameter-value-token no-value-rest-data)
               (next-token no-blanks-after-rest-data)
             (cond
               ((not (and parameter-value-token
                          (or (token= parameter-value-token :parameter-value)
                              (token= parameter-value-token :string))))
                (signal-error '(or :parameter-value :string)
                              no-blanks-after-rest-data))
               (t
                (setf value (string-trim +parameter-value-wrapper-char+
                                         (token-value parameter-value-token)))
                (push (cons key value) (link-record-parameters parameters-results))
                (let ((no-blanks-rest-data (consume-blanks no-value-rest-data)))
                  (multiple-value-bind (field-separator-found field-separator-rest-data)
                      (field-separator no-blanks-rest-data)
                    (if field-separator-found
                        (let ((no-blanks-rest-data (consume-blanks field-separator-rest-data)))
                          (parameters no-blanks-rest-data parameters-results))
                        no-blanks-rest-data))))))))))))

(a:define-constant +pagination-parameter-key+            "rel" :test #'string=)

(a:define-constant +pagination-parameter-next-page+     "next" :test #'string=)

(a:define-constant +pagination-parameter-previous-page+ "prev" :test #'string=)

(defun find-link-header (headers)
  (cdr (assoc :link headers :test #'eq)))

(defun find-rel-links (parsed-link-header)
  (remove-if-not (lambda (a)
                   (assoc +pagination-parameter-key+
                          (link-record-parameters a)
                          :test #'string=))
                 parsed-link-header))

(defun find-pagination-link (parsed-link-header direction)
  (a:when-let* ((rel-links       (find-rel-links parsed-link-header))
                (direction-links (remove-if-not (lambda (link)
                                                  (find-if (lambda (parameters)
                                                             (string= (cdr parameters)
                                                                      direction))
                                                           (link-record-parameters link)))
                                                rel-links)))
               (link-record-url (first direction-links))))

(defun find-link-to-next-page (parsed-link-header)
  (find-pagination-link parsed-link-header +pagination-parameter-next-page+))

(defun find-link-to-previous-page (parsed-link-header)
  (find-pagination-link parsed-link-header +pagination-parameter-previous-page+))

(defun find-pagination-links (headers)
  (a:when-let* ((link-header   (find-link-header headers))
                (parsed-header (ignore-errors (parse link-header))))
    (values (find-link-to-next-page parsed-header)
            (find-link-to-previous-page parsed-header))))
