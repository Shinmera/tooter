(in-package #:tooter-client-v6)

(tooter:define-entity quote-state
  (state :translate-with #'tooter:to-keyword))

(tooter:define-entity (status-quote (quote-state))
  (status :translate-with #'decode-status))

(defmethod print-object ((object status-quote) stream)
  (print-unreadable-object (object stream :type T)
    (format t "quote state ~a " (state object))
    (tooter::present (status object) stream)))

(tooter:define-entity (status-shallow-quote (quote-state))
  (status-id :field "status_id"))

(defmethod print-object ((object status-shallow-quote) stream)
  (print-unreadable-object (object stream :type T)
    (format t
            "quote state ~a id ~a"
            (state object)
            (status-id object))))

(defun %decode-quoted-status (quoted-data)
  (let ((shallowp (tooter::getj quoted-data "status_id")))
    (if shallowp
        (tooter:decode-entity 'status-shallow-quote quoted-data)
        (tooter:decode-entity 'status-quote quoted-data))))

(tooter:define-entity (status (tooter-objects:status))
  (quoted-status :fields "quote" :nullable T :translate-with #'%decode-quoted-status))
