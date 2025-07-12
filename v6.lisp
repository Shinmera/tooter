(in-package #:org.shirakumo.tooter)

(defclass v6:client (v2:client) ())

(define-entity quote-state
  (state :translate-with #'to-keyword))

(define-entity (status-quote quote-state)
  (status :translate-with #'decode-status))

(defmethod print-object ((object status-quote) stream)
  (print-unreadable-object (object stream :type T)
    (format stream "~a " (state object))
    (present (status object) stream)))

(define-entity (status-shallow-quote quote-state)
  (status-id :field "status_id"))

(defmethod print-object ((object status-shallow-quote) stream)
  (print-unreadable-object (object stream :type T)
    (format stream "~a id ~a" (state object) (status-id object))))

(defun %decode-quoted-status (quoted-data)
  (let ((shallowp (getj quoted-data "status_id")))
    (if shallowp
        (decode-entity 'status-shallow-quote quoted-data)
        (decode-entity 'status-quote quoted-data))))

(define-entity (v6:status status)
  (quoted-status :fields "quote" :nullable T :translate-with #'%decode-quoted-status))

(define-entity (v6:instance-rule instance-rule)
  (translations :nullable T))

(defmethod print-object ((instance-rule v6:instance-rule) stream)
  (print-unreadable-object (instance-rule stream :type T)
    (format stream
            "rule #~a text ~s hint ~s translations ~s"
            (id instance-rule)
            (text instance-rule)
            (hint instance-rule)
            (translations instance-rule))))

(defun v6::%decode-registrations (data)
  (let ((registrations data))
    (when (hash-table-p registrations)
      (list :enabled (gethash "enabled" registrations)
            :approval-required (gethash "approval_required" registrations)
            :message (gethash "message" registrations)
            :min-age (gethash "min_age" registrations)
            :reason-required (gethash "reason_required" registrations)))))

(define-entity (v6:instance instance)
  (registrations :translate-with #'v6::%decode-registrations))

(defmethod update-credentials ((client v6:client)
                               &key
                                 display-name
                                 note
                                 avatar
                                 header
                                 (locked NIL l-p)
                                 fields
                                 language
                                 (privacy :public)
                                 (sensitive NIL s-p)
                                 (indexable NIL i-p)
                                 (attribution-domains '()))
  (check-type display-name (or null string))
  (check-type note (or null string))
  (check-type avatar (or null pathname))
  (check-type header (or null pathname))
  (check-type language (or null string))
  (assert (member privacy '(:public :unlisted :private)))
  (check-type fields list)
  (setf (account client)
        (decode-account (apply #'submit client "/api/v1/accounts/update_credentials"
                               :http-method :patch
                               :display-name display-name
                               :note note
                               :avatar avatar
                               :header header
                               :locked (coerce-boolean locked l-p)
                               :indexable (coerce-boolean indexable i-p)
                               (loop for i from 0
                                     for (key . val) in fields
                                     collect (format NIL "fields_attributes[~a][name]" i)
                                     collect key
                                     collect (format NIL "fields_attributes[~a][value]" i)
                                     collect val)
                               (loop for attributed-domain in attribution-domains
                                     collect
                                     (format nil "attribution_domains[]")
                                     collect attributed-domain)
                               (when language
                                 (list "source[language]" language))
                               (when privacy
                                 (list "source[privacy]" privacy))
                               (when sensitive
                                 (list "source[sensitive]"
                                       (coerce-boolean sensitive s-p)))))))

(defmethod check-filter-action ((object v6:client) value)
  (assert (member value '("warn" "hide" "blur") :test #'string=)))

(defmethod instance ((client v6:client))
  (v6:decode-instance (query client "/api/v2/instance")))

(defmethod delete-media ((client v6:client) (attachment string))
  (submit client
          (format NIL "/api/v1/media/~a" attachment)
          :http-method :delete))

(defmethod delete-media ((client v6:client) (attachment attachment))
  (submit client
          (format NIL "/api/v1/media/~a" (id attachment))
          :http-method :delete))

(defmethod find-status ((client v6:client) (id string))
  (v6:decode-status (query client (format NIL "/api/v1/statuses/~a" id))))

(defmethod delete-status ((client v6:client) (status string)
                          &key (delete-media NIL da-p) &allow-other-keys)
  (submit client
          (format NIL "/api/v1/statuses/~a" status)
          :delete-media (coerce-boolean delete-media da-p)
          :http-method :delete))

(defmethod delete-status ((client v6:client) (status status)
                          &key (delete-media NIL) &allow-other-keys)
  (delete-status client (id status) :delete-media delete-media))

(defmethod featured-accounts ((client v6:client) (account-id string))
  (decode-account (query client
                         (format NIL
                                 "/api/v1/accounts/~a/endorsements"
                                 account-id))))

(defmethod feature-account ((client v6:client) (account-id string))
  (decode-relationship (submit client
                               (format NIL
                                       "/api/v1/accounts/~a/endorse"
                                       account-id))))

(defmethod unfeature-account ((client v6:client) (account-id string))
  (decode-relationship (submit client
                               (format NIL
                                       "/api/v1/accounts/~a/unendorse"
                                       account-id))))

(defmethod set-private-note ((client v6:client) (account-id string) (comment string))
  (decode-relationship (submit
                        client
                        (format NIL
                                "/api/v1/accounts/~a/note"
                                account-id)
                        :comment "")))

(defmethod feature-tag ((client v6:client) (tag string))
  (decode-relationship (submit client
                               (format NIL
                                       "/api/v1/tags/~a/feature"
                                       tag))))

(defmethod feature-tag ((client v6:client) (tag tag))
  (feature-tag client (name tag)))

(defmethod unfeature-tag ((client v6:client) (tag string))
  (decode-relationship (submit client
                               (format NIL
                                       "/api/v1/tags/~a/unfeature"
                                       tag))))

(defmethod unfeature-tag ((client v6:client) (tag tag))
  (unfeature-tag client (name tag)))

(defmethod %timeline ((client v6:client) url &key (local NIL l-p) (only-media NIL o-p) max-id since-id min-id (limit 20) (other-args nil))
  (v6:decode-status (apply #'query
                           client
                           (format NIL "/api/v1/timelines/~a" url)
                           :local (coerce-boolean local l-p)
                           :only-media (coerce-boolean only-media o-p)
                           :max-id max-id
                           :since-id since-id
                           :min-id min-id
                           :limit limit
                           other-args)))
