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

(define-entity instance-rule
  (id)
  (text)
  (hint)
  (translations :nullable T))

(defmethod print-object ((instance-rule instance-rule) stream)
  (print-unreadable-object (instance-rule stream :type T)
    (format stream
            "rule #~a text ~s hint ~s translations ~s"
            (id instance-rule)
            (text instance-rule)
            (hint instance-rule)
            (translations instance-rule))))

(defun %decode-registrations (data)
  (let ((registrations data))
    (when (hash-table-p registrations)
      (list :enabled (gethash "enabled" registrations)
            :approval-required (gethash "approval_required" registrations)
            :message (gethash "message" registrations)
            :min-age (gethash "min_age" registrations)
            :reason-required (gethash "reason_required" registrations)))))

(define-entity instance
  (domain)
  (title)
  (version)
  (source-url :field "source_url")
  (description)
  (usage :translate-with #'tooter::%decode-usage)
  (thumbnail :nullable T :translate-with #'tooter::%decode-thumbnail)
  (icon :translate-with #'tooter::decode-instance-icon)
  (languages :translate-with #'tooter::translate-languages)
  (configuration :translate-with #'tooter::%decode-configuration)
  (registrations :translate-with #'%decode-registrations)
  (api-versions :field "api_versions" :translate-with #'tooter::%decode-api-versions)
  (contact :translate-with #'tooter::%decode-contact)
  (rules :translate-with #'tooter::decode-instance-rule))

(defmethod print-object ((instance instance) stream)
  (print-unreadable-object (instance stream :type T)
    (format stream
            "~s ~a configuration ~a"
            (title instance)
            (domain instance)
            (configuration instance))))
