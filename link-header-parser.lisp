(in-package :link-header-parser)

(defparameter *tokens* "(,)|(;)|(=)|(<[^<>;\\s]+>)|(\"[^\"]+\")|([^\\s=]+)|(\\s*)")
;                        0   1   2       3              4            5        6

(defun next-token (data)
  (multiple-value-bind (matched registers)
      (cl-ppcre:scan-to-strings *tokens* data)
    (when matched
      (let ((position-matched (position-if-not #'null registers)))
        (values
         (ecase position-matched
           (0 (list :record-separator matched))
           (1 (list :field-separator matched))
           (2 (list :parameter-separator matched))
           (3 (list :url matched))
           (4 (list :parameter-value matched))
           (5 (list :parameter-key matched))
           (6 (list :blanks matched)))
         (subseq data (length matched)))))))

;; link-record      := url blanks (field-sep blanks parameter)? | link-record
;; parameter        := single-parameter | (single-parameter blanks field-sep parameter)
;; single-parameter := parameter-key blanks parameter-separator blanks parameter-value
;; url              := "<[^<>;\\s]+>"
;; blanks           := "\\s*"
;; parameter-key    := "[^\\s=]+"
;; paramerer-value  := "\"[^\"]+\""

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

(defconstant +url-wrapper-chars+ '(#\< #\>))

(defun url (data record-results)
  (multiple-value-bind (token no-url-data)
      (next-token data)
    (if (not (and token
                  (token= token :url)))
        (signal-error :url no-url-data)
        (progn
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

(defconstant +parameter-value-wrapper-char+ '(#\"))

(defun parameters (data parameters-results)
  (multiple-value-bind (token rest-data)
      (next-token data)
    (let ((key   nil)
          (value nil))
      (if (not (and token
                    (token= token :parameter-key)))
          (signal-error :parameter-key data)
          (progn
            (setf key (token-value token))
            (let* ((no-blanks-rest-data        (consume-blanks rest-data))
                   (no-parameter-sep-rest-data (consume-parameter-separator no-blanks-rest-data))
                   (no-blanks-after-rest-data  (consume-blanks no-parameter-sep-rest-data)))
              (multiple-value-bind (parameter-value-token no-value-rest-data)
                  (next-token no-blanks-after-rest-data)
                (if (not (and parameter-value-token
                              (token= parameter-value-token :parameter-value)))
                    (signal-error :parameter-value no-blanks-after-rest-data)
                    (progn
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
